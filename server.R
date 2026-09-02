# global.R provides: config, participants_emails, respostes,
# compute_metrics(), compute_classif_metrics(), write_result_atomic(),
# read_all_results(), results_state(), normalize_email()

server <- function(input, output, session) {

  # -------------------------------------------------------------
  # Live benchmark: read-only view of everyone's results.
  # checkFunc is a cheap directory fingerprint; valueFunc re-reads
  # only when a new CSV appears (or the RData changes).
  # -------------------------------------------------------------
  results_data <- reactivePoll(
    intervalMillis = config$poll_ms,
    session        = session,
    checkFunc      = results_state,
    valueFunc      = read_all_results
  )

  # -------------------------------------------------------------
  # Idle-time consolidation: once per minute, check whether the
  # newest pending CSV is older than the configured idle window.
  # If so, consolidate (RData + move to processed/). The lock in
  # try_consolidate_with_lock() guarantees a single consolidator
  # even with many sessions open. This does NOT close submissions:
  # any later attempt is simply picked up by the next cycle (or at
  # the next app startup).
  # -------------------------------------------------------------
  observe({
    invalidateLater(60 * 1000, session)
    pending <- list.files(config$results_path, pattern = "\\.csv$",
                          full.names = TRUE)
    if (length(pending) == 0) return(invisible(NULL))
    newest_mtime <- max(file.info(pending)$mtime, na.rm = TRUE)
    idle_minutes <- as.numeric(difftime(Sys.time(), newest_mtime,
                                        units = "mins"))
    if (idle_minutes >= config$consolidate_after_idle_minutes) {
      try_consolidate_with_lock()
    }
  })

  # Last accepted attempt of THIS session (for the red cross + feedback)
  attempt_result <- reactiveVal(NULL)

  # Server-side cooldown state (client-side disabling can be bypassed)
  last_accepted <- reactiveVal(NULL)

  # -------------------------------------------------------------
  # Load user file
  # -------------------------------------------------------------
  loadDF <- reactive({
    req(input$file1)
    tryCatch({
      if (input$filetype != "excel") {
        read.csv(
          input$file1$datapath,
          header = TRUE,
          sep    = input$sep,
          dec    = input$dec
        )
      } else {
        read_excel(input$file1$datapath)
      }
    },
    error = function(e) {
      shinyalert(
        "Could not read your file",
        paste0("Check that the file type (Excel/CSV) and the separators ",
               "match your file.\n\nTechnical detail: ",
               conditionMessage(e)),
        type = "error"
      )
      NULL
    })
  })

  # Informative validation of the student's file. Returns NULL + alert
  # if something is wrong; returns the cleaned df otherwise.
  validate_student_df <- function(df) {
    needed <- c(config$id_column, config$prediction_column)
    missing_cols <- needed[!needed %in% names(df)]
    if (length(missing_cols) > 0) {
      shinyalert(
        "Missing columns",
        paste0("Your file must contain the column(s): ",
               paste(missing_cols, collapse = ", "),
               ".\n\nColumns found in your file: ",
               paste(names(df), collapse = ", ")),
        type = "error"
      )
      return(NULL)
    }
    df
  }

  # -------------------------------------------------------------
  # THE single point where an attempt is computed AND saved.
  # One click -> at most one CSV, written atomically.
  # -------------------------------------------------------------
  observeEvent(input$button, {

    # --- Cooldown (server side) ---
    now <- Sys.time()
    if (!is.null(last_accepted()) &&
        as.numeric(difftime(now, last_accepted(), units = "secs")) <
        config$cooldown_seconds) {
      remaining <- ceiling(
        config$cooldown_seconds -
          as.numeric(difftime(now, last_accepted(), units = "secs"))
      )
      shinyalert(
        "Please wait",
        paste0("Your previous attempt was just recorded. You can submit ",
               "again in ", remaining, " seconds."),
        type = "info"
      )
      return(NULL)
    }

    # --- Participant check (normalized: case/whitespace tolerant) ---
    user_norm <- normalize_email(input$user)
    if (!(user_norm %in% participants_emails)) {
      shinyalert("Oops!", "Your email is not in the participant list",
                 type = "error")
      return(NULL)
    }

    df <- loadDF()
    if (is.null(df)) return(NULL)
    df <- validate_student_df(df)
    if (is.null(df)) return(NULL)

    tryCatch({

      # Keep last prediction per id
      estudiant <- df %>%
        select(all_of(config$id_column),
               all_of(config$prediction_column)) %>%
        group_by(across(all_of(config$id_column))) %>%
        slice_tail(n = 1) %>%
        ungroup() %>%
        mutate(across(all_of(config$id_column), as.character))

      solucions <- respostes %>%
        select(all_of(config$id_column),
               all_of(config$response_column)) %>%
        mutate(across(all_of(config$id_column), as.character))

      check_answer <- left_join(
        solucions,
        estudiant,
        by = config$id_column
      )

      obs  <- check_answer[[config$response_column]]
      pred <- check_answer[[config$prediction_column]]

      # If the join matched nothing, the ids do not correspond:
      # tell the student instead of silently penalizing everything.
      if (all(is.na(pred)) && nrow(estudiant) > 0) {
        shinyalert(
          "Wrong file?",
          paste0("The file you uploaded does not contain the ids of the ",
                 "TEST dataset. You may have uploaded the TRAIN file ",
                 "instead of the TEST file with your predictions."),
          type = "error"
        )
        return(NULL)
      }

      if (config$task_type == "regression") {
        m <- compute_metrics(obs, pred)
        metric_row <- pivot_wider(m$metrics,
                                  names_from  = metric,
                                  values_from = value)
      } else {
        m <- compute_classif_metrics(
          check_answer,
          response_col = config$response_column,
          pred_col     = config$prediction_column
        )
        metric_row <- m$metrics
      }

      result <- data.frame(
        user     = user_norm,
        time     = Sys.time(),          # full precision (tie-breaking)
        workshop = config$workshop_name,
        metric_row,
        check.names      = FALSE,
        stringsAsFactors = FALSE
      )

      attr(result, "n_missing") <- m$n_missing
      attr(result, "n_total")   <- m$n_total

      # --- Persist: unique name, atomic write, exactly once ---
      write_result_atomic(result, config$results_path, user_norm)

      last_accepted(now)
      attempt_result(result)

      # --- Client-side cooldown: disable the button visibly ---
      shinyjs::disable("button")
      shinyjs::delay(config$cooldown_seconds * 1000,
                     shinyjs::enable("button"))

    },
    error = function(e) {
      shinyalert(
        "Could not process your result",
        paste0("Technical detail: ", conditionMessage(e)),
        type = "error"
      )
    })
  })

  # -------------------------------------------------------------
  # Show uploaded data
  # -------------------------------------------------------------
  output$contents <- renderTable({
    req(input$file1)
    df <- loadDF()
    req(df)
    if (input$disp == "head") head(df) else df
  })

  # -------------------------------------------------------------
  # Benchmark plot: always shows the live class distribution;
  # overlays this session's last attempt (red) and the best (blue).
  # -------------------------------------------------------------
  output$chart <- renderPlot({
    resultats <- results_data()
    if (is.null(resultats) || nrow(resultats) == 0) return(NULL)

    result <- attempt_result()   # may be NULL before first submission

    if (config$task_type == "regression") {

      req(all(c("RMSE", "MAE") %in% names(resultats)))

      resultats <- resultats %>%
        mutate(RMSE = as.numeric(RMSE),
               MAE  = as.numeric(MAE))

      rmse_min       <- min(resultats$RMSE, na.rm = TRUE)
      rmse_median    <- median(resultats$RMSE, na.rm = TRUE)
      rmse_threshold <- rmse_median + 3 * (rmse_median - rmse_min)

      mae_min       <- min(resultats$MAE, na.rm = TRUE)
      mae_median    <- median(resultats$MAE, na.rm = TRUE)
      mae_threshold <- mae_median + 3 * (mae_median - mae_min)

      resultats_filtrats <- resultats %>%
        filter(RMSE <= rmse_threshold,
               MAE  <= mae_threshold)

      punts_extra <- tibble()

      if (!is.null(result)) {
        dflog_long <- result %>%
          mutate(RMSE = as.numeric(RMSE),
                 MAE  = as.numeric(MAE)) %>%
          filter(RMSE <= rmse_threshold,
                 MAE  <= mae_threshold) %>%
          select(user, RMSE, MAE) %>%
          pivot_longer(-user, names_to = "metric") %>%
          mutate(type = "Your attempt")
        punts_extra <- bind_rows(punts_extra, dflog_long)
      }

      best_score <- resultats_filtrats %>%
        mutate(total_error = RMSE + MAE) %>%
        slice_min(total_error, n = 1, with_ties = FALSE) %>%
        select(user, RMSE, MAE) %>%
        pivot_longer(-user, names_to = "metric") %>%
        mutate(type = "Best (lowest avg)")
      punts_extra <- bind_rows(punts_extra, best_score)

      p <- resultats_filtrats %>%
        select(user, RMSE, MAE) %>%
        pivot_longer(-user, names_to = "metric") %>%
        ggplot(aes(x = metric, y = value)) +
        geom_boxplot(outlier.shape = NA) +
        labs(
          title    = "Benchmark of your results",
          subtitle = "Your result (red) vs. best result (blue)",
          color    = "Legend",
          shape    = "Legend"
        ) +
        coord_cartesian(
          ylim = c(0, max(c(resultats_filtrats$RMSE,
                            resultats_filtrats$MAE), na.rm = TRUE) * 1.2)
        )

      if (nrow(punts_extra) > 0) {
        p <- p +
          geom_point(
            data   = punts_extra,
            aes(x = metric, y = value, color = type, shape = type),
            stroke = 2,
            size   = 3
          )
      }

      p +
        scale_color_manual(values = c("Your attempt"      = "red",
                                      "Best (lowest avg)" = "blue")) +
        scale_shape_manual(values = c("Your attempt"      = 4,
                                      "Best (lowest avg)" = 4)) +
        theme(
          text          = element_text(size = 16),
          axis.text     = element_text(size = 14),
          axis.title    = element_text(size = 16),
          legend.text   = element_text(size = 14),
          legend.title  = element_text(size = 15),
          plot.title    = element_text(size = 18, face = "bold"),
          plot.subtitle = element_text(size = 15)
        )

    } else if (config$task_type == "classification") {

      metric_cols <- intersect(VALID_METRICS$classification,
                               names(resultats))
      if (length(metric_cols) == 0) return(NULL)

      main_metric <- config$metric

      resultats_long <- resultats %>%
        mutate(across(all_of(metric_cols), as.numeric)) %>%
        select(user, all_of(metric_cols)) %>%
        pivot_longer(-user, names_to = "metric", values_to = "value")

      punts_extra <- tibble()

      if (!is.null(result)) {
        result_long <- result %>%
          select(user, any_of(metric_cols)) %>%
          pivot_longer(-user, names_to = "metric", values_to = "value") %>%
          mutate(value = as.numeric(value),
                 type  = "Your attempt")
        punts_extra <- bind_rows(punts_extra, result_long)
      }

      best_score <- resultats %>%
        mutate(across(all_of(metric_cols), as.numeric)) %>%
        slice_max(.data[[main_metric]], n = 1, with_ties = FALSE) %>%
        select(user, all_of(metric_cols)) %>%
        pivot_longer(-user, names_to = "metric", values_to = "value") %>%
        mutate(type = "Best")
      punts_extra <- bind_rows(punts_extra, best_score)

      p <- resultats_long %>%
        ggplot(aes(x = metric, y = value)) +
        geom_boxplot(outlier.shape = NA) +
        labs(
          title    = "Benchmark of your results",
          subtitle = sprintf(
            "Your result (red) vs. best result (blue) – main metric: %s",
            main_metric
          ),
          color = "Legend",
          shape = "Legend"
        ) +
        coord_cartesian(ylim = c(0, 1))

      if (nrow(punts_extra) > 0) {
        p <- p +
          geom_point(
            data   = punts_extra,
            aes(x = metric, y = value, color = type, shape = type),
            stroke = 2,
            size   = 3
          )
      }

      p +
        scale_color_manual(values = c("Your attempt" = "red",
                                      "Best"         = "blue")) +
        scale_shape_manual(values = c("Your attempt" = 4,
                                      "Best"         = 4)) +
        theme(
          text          = element_text(size = 16),
          axis.text     = element_text(size = 14),
          axis.title    = element_text(size = 16),
          legend.text   = element_text(size = 14),
          legend.title  = element_text(size = 15),
          plot.title    = element_text(size = 18, face = "bold"),
          plot.subtitle = element_text(size = 15)
        )
    }
  })

  # -------------------------------------------------------------
  # Text feedback: pure display, no side effects, no recomputation.
  # Uses the attributes stored with the accepted attempt.
  # -------------------------------------------------------------
  output$Answer <- renderUI({
    result <- attempt_result()
    if (is.null(result)) {
      return(HTML(paste0(
        "<em>The chart below shows the live class benchmark. ",
        "Upload your file and press 'Get result' to see your attempt.</em>"
      )))
    }

    n_missing <- attr(result, "n_missing")
    n_total   <- attr(result, "n_total")

    metric_cols <- setdiff(names(result), c("user", "time", "workshop"))
    metric_lines <- map_chr(
      metric_cols,
      ~ sprintf("<strong>&#9989; %s:</strong> %s", .x, result[[.x]])
    )
    message <- paste(metric_lines, collapse = "<br>")

    if (!is.null(n_missing) && n_missing > 0) {
      percent_missing <- round(100 * n_missing / n_total, 1)
      missing_note <- if (config$task_type == "regression") {
        "These missing values have been penalized in your score."
      } else {
        "These missing values were excluded from the metric computation."
      }
      message <- paste0(
        message,
        "<br><br>&#8505;&#65039; <strong>Note:</strong> You missed <strong>",
        n_missing, "</strong> out of ", n_total, " predictions (",
        percent_missing, "%).<br>", missing_note
      )
    }

    HTML(message)
  })
}

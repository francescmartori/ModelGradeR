# global.R provides: config, participants_emails, respostes,
# compute_metrics(), compute_classif_metrics(), write_result_atomic(),
# read_consolidated(), read_attempt_csv(), normalize_results(),
# results_state(), rdata_stamp(), normalize_email()

server <- function(input, output, session) {

  # -------------------------------------------------------------
  # Live benchmark: read-only view of everyone's results.
  # The benchmark is read ON DEMAND, not polled.
  #
  # With a reactivePoll, a single submission invalidated every open
  # session and all of them redrew the chart: with 25 sessions that is 25
  # ggplot renders per attempt, which saturated the CPU and left the
  # students' own submissions queueing behind the redraws. Now the chart
  # depends only on this session's own attempt, so one submission costs
  # exactly one render, done by the session that submitted.
  #
  # Each pending CSV is still read at most once per session and the RData
  # only when its mtime changes, so reading the store stays cheap.
  # Plain (non-reactive) variables on purpose.
  bench_base  <- NULL
  bench_stamp <- ""
  bench_seen  <- character(0)
  bench_extra <- list()

  llegeix_benchmark <- function() {
    stamp <- rdata_stamp()
    if (!identical(stamp, bench_stamp)) {
      bench_base  <<- read_consolidated()
      bench_stamp <<- stamp
    }

    pending   <- list.files(config$results_path, pattern = "\\.csv$",
                            full.names = TRUE)
    new_files <- setdiff(pending, bench_seen)
    if (length(new_files) > 0) {
      rows <- lapply(new_files, read_attempt_csv)
      rows <- rows[!vapply(rows, is.null, logical(1))]
      if (length(rows) > 0) {
        bench_extra <<- c(bench_extra, list(normalize_results(bind_rows(rows))))
      }
      bench_seen <<- c(bench_seen, new_files)
    }

    # Consolidated rows may also still be in bench_extra; distinct()
    # dedupes them because both sides are normalized.
    out <- bind_rows(bench_base, bind_rows(bench_extra))
    if (is.null(out) || nrow(out) == 0) return(out)
    distinct(out)
  }

  # -------------------------------------------------------------
  # The best attempt in the store, under the same rule the chart uses for
  # the blue cross: in regression the lowest RMSE + MAE, in classification
  # the highest main metric. Returns a one-row data frame with the metric
  # columns, or NULL if there is nothing to compare against yet.
  #
  # The chart picks the best among non-aberrant attempts, but the aberrant
  # filter only trims errors that are too HIGH, so the minimum is never
  # one of the rows it drops: the row found here is the one on the chart.
  # -------------------------------------------------------------
  # The attempts that make up the competition. With
  # config$competition_requires_model, an attempt without the Gretl output
  # attached is exploratory: it is computed and recorded, and its author
  # sees it, but it does not shape the benchmark and cannot be the target.
  #
  # A store written before has_evidence existed has no such column; there
  # the flag cannot be applied and every attempt stays eligible.
  intents_elegibles <- function(resultats) {
    if (is.null(resultats) || nrow(resultats) == 0) return(resultats)
    if (!isTRUE(config$competition_requires_model)) return(resultats)
    if (!"has_evidence" %in% names(resultats)) return(resultats)
    filter(resultats, as.logical(has_evidence) %in% TRUE)
  }

  # Returns a list, or NULL if there is nothing to compare against yet:
  #   metrics  one-row data frame with the metric columns of the best attempt
  #   score    its score, in the direction the task is scored
  #   n_tied   how many DISTINCT users sit at that score
  #
  # n_tied matters: in simple-regression workshops many groups converge on
  # the same specification and end up with exactly the same error, so being
  # "the best" is often shared.
  millor_intent <- function(resultats) {
    resultats <- intents_elegibles(resultats)
    if (is.null(resultats) || nrow(resultats) == 0) return(NULL)
    tol <- 1e-9   # the store round-trips through CSV: compare loosely

    if (config$task_type == "regression") {
      if (!all(c("RMSE", "MAE") %in% names(resultats))) return(NULL)
      r <- resultats %>%
        mutate(RMSE = as.numeric(RMSE),
               MAE  = as.numeric(MAE)) %>%
        filter(!is.na(RMSE), !is.na(MAE)) %>%
        mutate(total_error = RMSE + MAE)
      if (nrow(r) == 0) return(NULL)

      best_val <- min(r$total_error)
      list(
        metrics = r %>%
          slice_min(total_error, n = 1, with_ties = FALSE) %>%
          select(RMSE, MAE),
        score  = best_val,
        n_tied = n_distinct(r$user[r$total_error <= best_val + tol])
      )

    } else {
      metric_cols <- intersect(VALID_METRICS$classification,
                               names(resultats))
      if (length(metric_cols) == 0) return(NULL)
      main_metric <- if (config$metric %in% metric_cols) {
        config$metric
      } else {
        metric_cols[1]
      }
      r <- resultats %>%
        mutate(across(all_of(metric_cols), as.numeric)) %>%
        filter(!is.na(.data[[main_metric]]))
      if (nrow(r) == 0) return(NULL)

      best_val <- max(r[[main_metric]])
      list(
        metrics = r %>%
          slice_max(.data[[main_metric]], n = 1, with_ties = FALSE) %>%
          select(all_of(metric_cols)),
        score  = best_val,
        n_tied = n_distinct(r$user[r[[main_metric]] >= best_val - tol])
      )
    }
  }

  # This attempt's own score, on the same scale as millor_intent()$score,
  # or NA if it cannot be computed.
  puntuacio_intent <- function(result) {
    if (is.null(result)) return(NA_real_)
    if (config$task_type == "regression") {
      if (!all(c("RMSE", "MAE") %in% names(result))) return(NA_real_)
      as.numeric(result$RMSE) + as.numeric(result$MAE)
    } else {
      metric_cols <- intersect(VALID_METRICS$classification, names(result))
      if (length(metric_cols) == 0) return(NA_real_)
      main_metric <- if (config$metric %in% metric_cols) {
        config$metric
      } else {
        metric_cols[1]
      }
      as.numeric(result[[main_metric]])
    }
  }

  # Four significant digits, never scientific: readable for errors in the
  # hundreds (prices) and for rates between 0 and 1 alike.
  fmt_metric <- function(x) {
    format(signif(as.numeric(x), 4), scientific = FALSE, trim = TRUE)
  }

  # The line that tells the student what to beat. The author of the best
  # attempt is never named. If this attempt IS the best, the line
  # congratulates instead, and says so differently when the top is shared.
  #
  # The attempt is already on disk when the feedback renders, so it is part
  # of the benchmark: a student in the lead always matches the best score
  # exactly rather than beating it.
  linia_millor <- function(result = NULL, resultats = NULL) {
    if (is.null(resultats)) resultats <- llegeix_benchmark()
    best <- millor_intent(resultats)
    if (is.null(best) || nrow(best$metrics) == 0) return("")

    vals <- vapply(
      names(best$metrics),
      function(nm) sprintf("%s: <strong>%s</strong>", nm,
                           fmt_metric(best$metrics[[nm]])),
      character(1)
    )
    vals <- paste(vals, collapse = " &nbsp;&middot;&nbsp; ")

    # An attempt that cannot be the best must never be congratulated as
    # such: without this, an unattached attempt with the lowest numbers of
    # the class scores better than the best ELIGIBLE attempt and would
    # otherwise pass the comparison below.
    elegible <- !isTRUE(config$competition_requires_model) ||
      isTRUE(result$has_evidence)

    meu <- puntuacio_intent(result)
    lidera <- elegible && !is.na(meu) && (
      if (config$task_type == "regression") meu <= best$score + 1e-9
      else                                  meu >= best$score - 1e-9
    )

    capcalera <- if (!lidera) {
      "&#127919; <strong>Best attempt so far.</strong>"
    } else if (best$n_tied <= 1) {
      "&#127942; <strong>Congratulations! The best attempt so far is yours.</strong>"
    } else {
      sprintf(paste0("&#127942; <strong>Well done! You are at the top, ",
                     "tied with %d other%s.</strong>"),
              best$n_tied - 1L, if (best$n_tied - 1L == 1L) "" else "s")
    }

    paste0(capcalera, "<br>", vals)
  }

  # The chart trims attempts whose error is far above the median, so that one
  # broken submission does not flatten the boxplot for everyone. When the
  # student's own attempt is one of them it simply vanishes from the chart,
  # with no red cross and no explanation. This says so: the attempt counts,
  # it is only too far out to be drawn.
  #
  # Same thresholds as renderPlot, on the same population.
  linia_fora_grafic <- function(result, resultats) {
    if (is.null(result) || config$task_type != "regression") return("")
    r <- intents_elegibles(resultats)
    if (is.null(r) || nrow(r) == 0) return("")
    if (!all(c("RMSE", "MAE") %in% names(r))) return("")

    rmse <- as.numeric(r$RMSE); mae <- as.numeric(r$MAE)
    ok <- is.finite(rmse) & is.finite(mae)
    rmse <- rmse[ok]; mae <- mae[ok]
    if (length(rmse) == 0) return("")

    llindar_rmse <- median(rmse) + 3 * (median(rmse) - min(rmse))
    llindar_mae  <- median(mae)  + 3 * (median(mae)  - min(mae))
    meu_rmse <- as.numeric(result$RMSE); meu_mae <- as.numeric(result$MAE)
    if (!is.finite(meu_rmse) || !is.finite(meu_mae)) return("")
    if (meu_rmse <= llindar_rmse && meu_mae <= llindar_mae) return("")

    paste0("&#9888;&#65039; <strong>Your attempt is not drawn on the chart.</strong> ",
           "It is recorded and it counts, but its error is too far above the ",
           "rest of the class to fit in the plot. Check your predictions: ",
           "this usually means a mistake in the file or a specification that ",
           "is far off.")
  }

  # Score of every row of the store, in the direction the task is scored, so
  # that it can be compared with puntuacio_intent().
  puntuacions <- function(resultats) {
    if (config$task_type == "regression") {
      if (!all(c("RMSE", "MAE") %in% names(resultats))) return(NULL)
      as.numeric(resultats$RMSE) + as.numeric(resultats$MAE)
    } else {
      metric_cols <- intersect(VALID_METRICS$classification, names(resultats))
      if (length(metric_cols) == 0) return(NULL)
      main_metric <- if (config$metric %in% metric_cols) {
        config$metric
      } else {
        metric_cols[1]
      }
      as.numeric(resultats[[main_metric]])
    }
  }

  # Where this attempt stands, among students and among the student's own
  # attempts. The ranking across students compares each student's BEST
  # attempt, which is what the competition is about; ranking raw attempts
  # would just reward whoever submitted most.
  #
  # Only shown for an attempt that is part of the competition: an attempt
  # without the model attached has no position, and the notice below the
  # metrics already says why.
  linia_posicio <- function(result, resultats) {
    if (is.null(result)) return("")
    if (isTRUE(config$competition_requires_model) &&
        !isTRUE(result$has_evidence)) return("")

    r <- intents_elegibles(resultats)
    if (is.null(r) || nrow(r) == 0 || !"user" %in% names(r)) return("")
    sc <- puntuacions(r)
    meu <- puntuacio_intent(result)
    if (is.null(sc) || is.na(meu)) return("")

    ok <- is.finite(sc)
    r <- r[ok, , drop = FALSE]; sc <- sc[ok]
    if (length(sc) == 0) return("")
    millor_es_menys <- config$task_type == "regression"
    tol <- 1e-9

    # Posicio entre alumnes, comparant el millor intent de cadascun
    millors <- tapply(sc, r$user, if (millor_es_menys) min else max)
    n_alum  <- length(millors)
    guanyen <- if (millor_es_menys) sum(millors < meu - tol) else
      sum(millors > meu + tol)
    pos_global <- guanyen + 1L

    # Posicio entre els intents propis
    meus <- sc[r$user == result$user]
    if (length(meus) == 0) meus <- meu
    g2 <- if (millor_es_menys) sum(meus < meu - tol) else sum(meus > meu + tol)
    pos_propi <- g2 + 1L

    paste0("&#128202; This attempt ranks <strong>", pos_global,
           "</strong> of ", n_alum, " students",
           if (length(meus) > 1)
             paste0(", and <strong>", pos_propi, "</strong> of your ",
                    length(meus), " attempts") else "",
           ".")
  }

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

      # --- Model evidence (pasted Gretl output) ---
      # Only presence is checked here; content is verified asynchronously.
      model_text   <- if (is.null(input$model_text)) "" else input$model_text
      has_evidence <- has_model_evidence(model_text)

      result <- data.frame(
        user         = user_norm,
        time         = Sys.time(),          # full precision (tie-breaking)
        workshop     = config$workshop_name,
        has_evidence = has_evidence,        # TRUE: model text was attached
        metric_row,
        check.names      = FALSE,
        stringsAsFactors = FALSE
      )

      attr(result, "n_missing") <- m$n_missing
      attr(result, "n_total")   <- m$n_total

      # --- Persist: unique name, atomic write, exactly once ---
      write_result_atomic(
        result, config$results_path, user_norm,
        model_text = if (has_evidence) model_text else ""
      )

      last_accepted(now)
      attempt_result(result)
      updateTextAreaInput(session, "model_text", value = "")

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
  # Benchmark plot: the class distribution as of this session's last
  # submission (or as of opening the app), with this student's attempt
  # (red) and the best (blue) overlaid once there is one.
  # -------------------------------------------------------------
  output$chart <- renderPlot({
    # Only dependency: this session's own accepted attempt. So the chart is
    # drawn once when the session opens (showing the class distribution the
    # student is competing against) and then only when this student submits.
    # Other people's submissions never trigger a redraw here.
    result <- attempt_result()   # NULL until the first submission

    resultats <- llegeix_benchmark()
    if (is.null(resultats) || nrow(resultats) == 0) return(NULL)

    if (config$task_type == "regression") {

      req(all(c("RMSE", "MAE") %in% names(resultats)))

      resultats <- resultats %>%
        mutate(RMSE = as.numeric(RMSE),
               MAE  = as.numeric(MAE))

      # The box describes the competition, not the exploration.
      resultats <- intents_elegibles(resultats)
      if (is.null(resultats) || nrow(resultats) == 0) return(NULL)

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

      # Same eligibility rule as the written target, so the blue cross and
      # the number in the text always point at the same attempt.
      candidats <- intents_elegibles(resultats_filtrats)
      if (!is.null(candidats) && nrow(candidats) > 0) {
        best_score <- candidats %>%
          mutate(total_error = RMSE + MAE) %>%
          slice_min(total_error, n = 1, with_ties = FALSE) %>%
          select(user, RMSE, MAE) %>%
          pivot_longer(-user, names_to = "metric") %>%
          mutate(type = "Best (lowest avg)")
        punts_extra <- bind_rows(punts_extra, best_score)
      }

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

      # Guard against stores where the configured metric column is
      # absent (e.g. legacy-schema rows only): fall back gracefully
      # instead of crashing the chart.
      main_metric <- if (config$metric %in% metric_cols) {
        config$metric
      } else {
        metric_cols[1]
      }

      # Same population rule as the regression branch above.
      resultats <- intents_elegibles(resultats)
      if (is.null(resultats) || nrow(resultats) == 0) return(NULL)

      resultats_long <- resultats %>%
        mutate(across(all_of(metric_cols), as.numeric)) %>%
        select(user, all_of(metric_cols)) %>%
        pivot_longer(-user, names_to = "metric", values_to = "value") %>%
        filter(!is.na(value))

      punts_extra <- tibble()

      if (!is.null(result)) {
        result_long <- result %>%
          select(user, any_of(metric_cols)) %>%
          pivot_longer(-user, names_to = "metric", values_to = "value") %>%
          mutate(value = as.numeric(value),
                 type  = "Your attempt")
        punts_extra <- bind_rows(punts_extra, result_long)
      }

      # Same eligibility rule as the written target (see the regression
      # branch): the blue cross marks an attempt that could actually win.
      candidats <- intents_elegibles(resultats)
      if (!is.null(candidats) && nrow(candidats) > 0) {
        best_score <- candidats %>%
          mutate(across(all_of(metric_cols), as.numeric)) %>%
          slice_max(.data[[main_metric]], n = 1, with_ties = FALSE,
                    na_rm = TRUE) %>%
          select(user, all_of(metric_cols)) %>%
          pivot_longer(-user, names_to = "metric", values_to = "value") %>%
          mutate(type = "Best")
        punts_extra <- bind_rows(punts_extra, best_score)
      }

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
    # Una sola lectura del magatzem per a les dues linies.
    benchmark <- llegeix_benchmark()
    millor    <- linia_millor(result, benchmark)
    posicio   <- linia_posicio(result, benchmark)
    fora      <- linia_fora_grafic(result, benchmark)

    if (is.null(result)) {
      intro <- paste0(
        "<em>The chart below shows the live class benchmark. ",
        "Upload your file and press 'Get result' to see your attempt.</em>"
      )
      if (nzchar(millor)) intro <- paste0(intro, "<br><br>", millor)
      return(HTML(intro))
    }

    n_missing <- attr(result, "n_missing")
    n_total   <- attr(result, "n_total")

    metric_cols <- setdiff(names(result),
                           c("user", "time", "workshop", "has_evidence",
                             "attempt_id"))
    metric_lines <- map_chr(
      metric_cols,
      ~ sprintf("<strong>&#9989; %s:</strong> %s", .x, result[[.x]])
    )
    message <- paste(metric_lines, collapse = "<br>")

    if (nzchar(fora)) {
      message <- paste0(message, "<br><br>", fora)
    }

    # What they are competing against, in writing. Includes this very
    # attempt, so if they just took the lead the two agree.
    if (nzchar(millor)) {
      message <- paste0(message, "<br><br>", millor)
    }
    if (nzchar(posicio)) {
      message <- paste0(message, "<br>", posicio)
    }

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

    # Unconditional, stateless notice: shown on every attempt without
    # model evidence, regardless of how good the attempt is. No lookup
    # against the benchmark, no session memory.
    if (!isTRUE(result$has_evidence)) {
      message <- paste0(
        message,
        "<br><br>&#9888;&#65039; <strong>No model attached.</strong> ",
        "This attempt is recorded and you can see how it compares, ",
        "but it will not count towards your grade. Paste the Gretl ",
        "output of your model in the box on the left to make an ",
        "attempt count."
      )
    }

    HTML(message)
  })
}

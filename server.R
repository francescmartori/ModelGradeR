library(shiny)
library(tidyverse)
library(readxl)
library(shinyalert)
library(caret)
library(yaml)

# 🔧 Configuració del workshop
admin_password <- "iqs2024"
config <- yaml::read_yaml("workshop_config.yaml")

if (is.null(config$task_type)) config$task_type <- "regression"
if (is.null(config$metric)) {
  config$metric <- if (config$task_type == "regression") "RMSE" else "F1-Score"
}

# 🔁 Consolidació de resultats
consolidar_resultats <- function(path = config$results_path, output_file = config$resultats_rdata) {
  file_names <- list.files(path, pattern = "\\.csv$", full.names = TRUE)
  
  if (file.exists(output_file)) {
    load(output_file)  # carrega 'resultats'
  } else {
    resultats <- data.frame()
  }
  
  if (length(file_names) > 0) {
    nous_resultats <- purrr::map_dfr(file_names, readr::read_csv, show_col_types = FALSE)
    resultats <- dplyr::bind_rows(resultats, nous_resultats)
    save(resultats, file = output_file)
    # file.remove(file_names)
  }
}

consolidar_resultats()

# 📥 Dades
participants <- readr::read_delim("participants.csv", delim = ";")
respostes <- readxl::read_excel(config$response_file)

# 🧮 Mètriques segons tipus de tasca
compute_metrics <- function(task_type, obs, pred) {
  if (task_type == "regression") {
    penalized_pred <- ifelse(is.na(pred), max(obs, na.rm = TRUE) * 1.5, pred)
    errors <- obs - penalized_pred
    
    rmse <- round(sqrt(mean(errors^2, na.rm = TRUE)), 3)
    mae  <- round(mean(abs(errors), na.rm = TRUE), 3)
    
    list(
      metrics = tibble::tibble(
        metric = c("RMSE", "MAE"),
        value  = c(rmse, mae)
      ),
      n_missing = sum(is.na(pred)),
      n_total   = length(obs)
    )
    
  } else if (task_type == "classification") {
    
    # obs a 0/1
    obs_bin <- suppressWarnings(as.numeric(as.character(obs)))
    if (any(is.na(obs_bin))) {
      obs_bin <- as.numeric(factor(obs, levels = unique(obs))) - 1
    }
    
    # pred: probabilitats o etiquetes 0/1
    pred_num <- suppressWarnings(as.numeric(pred))
    pred_num[is.na(pred_num)] <- 0
    y_hat <- ifelse(is.na(pred_num), 0, ifelse(pred_num >= 0.5, 1, 0))
    
    tab <- table(
      factor(y_hat, levels = c(0, 1)),
      factor(obs_bin, levels = c(0, 1))
    )
    TN <- tab[1,1]; FP <- tab[2,1]; FN <- tab[1,2]; TP <- tab[2,2]
    
    safe_div <- function(num, den) ifelse(den == 0, NA_real_, num / den)
    
    accuracy    <- safe_div(TP + TN, sum(tab))
    precision   <- safe_div(TP, TP + FP)
    sensitivity <- safe_div(TP, TP + FN)
    specificity <- safe_div(TN, TN + FP)
    f1          <- ifelse(
      is.na(precision) | is.na(sensitivity) | (precision + sensitivity) == 0,
      NA_real_,
      2 * precision * sensitivity / (precision + sensitivity)
    )
    
    list(
      metrics = tibble::tibble(
        metric = c("Accuracy", "Precision", "Sensitivity", "Specificity", "F1-Score"),
        value  = round(c(accuracy, precision, sensitivity, specificity, f1), 3)
      ),
      n_missing = sum(is.na(pred)),
      n_total   = length(obs)
    )
    
  } else {
    stop("Task_type desconegut")
  }
}

# 🧠 Server
server <- function(input, output) {
  
  if (file.exists(config$resultats_rdata)) {
    load(config$resultats_rdata)  # carrega 'resultats'
  } else {
    resultats <- data.frame()
  }
  
  loadDF <- reactive({
    tryCatch({
      req(input$file1)
      if (input$filetype != "excel") {
        df <- read.csv(input$file1$datapath,
                       header = TRUE,
                       sep = input$sep,
                       dec = input$dec)
      } else {
        df <- read_excel(input$file1$datapath)
      }
      df
    },
    error = function(e){
      shinyalert("Oops!", "Error loading your file.", type = "error")
      stop(safeError(e))
    })
  })
  
  getAnswers <- eventReactive(input$button, {
    tryCatch({
      df <- loadDF()
      if (!(input$user %in% participants[[config$user_column]])) {
        shinyalert("Oops!", "Your email is not in the participant list", type = "error")
        return(NULL)
      }
      
      estudiant <- dplyr::select(df, dplyr::all_of(config$id_column), dplyr::all_of(config$prediction_column))
      solucions <- dplyr::select(respostes, dplyr::all_of(config$id_column), dplyr::all_of(config$response_column))
      check_answer <- dplyr::left_join(solucions, estudiant, by = config$id_column)
      
      obs  <- check_answer[[config$response_column]]
      pred <- check_answer[[config$prediction_column]]
      
      m <- compute_metrics(config$task_type, obs, pred)
      
      metric_row <- tidyr::pivot_wider(
        m$metrics,
        names_from = metric,
        values_from = value
      )
      
      result <- data.frame(
        user = input$user,
        time = as.POSIXct(format(Sys.time(), "%Y-%m-%d %H:%M:00")),
        metric_row,
        stringsAsFactors = FALSE
      )
      
      attr(result, "n_missing") <- m$n_missing
      attr(result, "n_total")   <- m$n_total
      
      result
    },
    error = function(e) {
      shinyalert("Oops!", "Could not process your result.", type = "error")
      stop(safeError(e))
    })
  })
  
  output$contents <- renderTable({
    req(input$file1)
    df <- loadDF()
    if (input$disp == "head") head(df) else df
  })
  
  output$chart <- renderPlot({
    result <- getAnswers()
    req(result)
    req(nrow(resultats) > 0)
    
    metric_cols <- setdiff(names(resultats), c("user", "time"))
    if (length(metric_cols) == 0) return(NULL)
    
    main_metric <- config$metric
    
    resultats_long <- resultats %>%
      dplyr::mutate(dplyr::across(all_of(metric_cols), as.numeric)) %>%
      dplyr::select(-time) %>%
      tidyr::pivot_longer(-user, names_to = "metric", values_to = "value")
    
    result_long <- result %>%
      dplyr::select(-time) %>%
      tidyr::pivot_longer(-user, names_to = "metric", values_to = "value") %>%
      dplyr::mutate(type = "Your attempt")
    
    if (config$task_type == "regression") {
      best_row <- resultats %>%
        dplyr::mutate(dplyr::across(all_of(metric_cols), as.numeric)) %>%
        dplyr::slice_min(.data[[main_metric]], n = 1, with_ties = FALSE)
    } else {
      best_row <- resultats %>%
        dplyr::mutate(dplyr::across(all_of(metric_cols), as.numeric)) %>%
        dplyr::slice_max(.data[[main_metric]], n = 1, with_ties = FALSE)
    }
    
    best_score <- best_row %>%
      dplyr::select(-time) %>%
      tidyr::pivot_longer(-user, names_to = "metric", values_to = "value") %>%
      dplyr::mutate(type = "Best")
    
    punts_extra <- dplyr::bind_rows(result_long, best_score)
    
    p <- resultats_long %>%
      ggplot2::ggplot(ggplot2::aes(x = metric, y = value)) +
      ggplot2::geom_boxplot(outlier.shape = NA) +
      ggplot2::labs(
        title = "Benchmark of your results",
        subtitle = sprintf("Your result (red) vs. best (blue) – main metric: %s", main_metric),
        color = "Legend",
        shape = "Legend"
      ) +
      ggplot2::ylim(0, max(c(resultats_long$value, punts_extra$value), na.rm = TRUE) * 1.2)
    
    if (nrow(punts_extra) > 0) {
      p <- p +
        ggplot2::geom_point(
          data = punts_extra,
          ggplot2::aes(x = metric, y = value, color = type, shape = type),
          stroke = 2, size = 3
        )
    }
    
    p +
      ggplot2::scale_color_manual(values = c("Your attempt" = "red", "Best" = "blue")) +
      ggplot2::scale_shape_manual(values = c("Your attempt" = 4, "Best" = 4)) +
      ggplot2::theme(
        text = ggplot2::element_text(size = 16),
        axis.text = ggplot2::element_text(size = 14),
        axis.title = ggplot2::element_text(size = 16),
        legend.text = ggplot2::element_text(size = 14),
        legend.title = ggplot2::element_text(size = 15),
        plot.title = ggplot2::element_text(size = 18, face = "bold"),
        plot.subtitle = ggplot2::element_text(size = 15)
      )
  })
  
  observeEvent(input$updateChart, {
    if (input$admin_pw == admin_password) {
      consolidar_resultats(path = config$results_path,
                           output_file = config$resultats_rdata)
      showNotification("✅ Results updated!", type = "message")
    } else {
      showNotification("❌ Incorrect password. Update not allowed.", type = "error")
    }
  })
  
  output$Answer <- renderUI({
    result <- getAnswers()
    
    if (!is.null(result) && input$user %in% participants[[config$user_column]]) {
      readr::write_csv(
        result,
        file = paste0(config$results_path, input$user, "-dflog", as.integer(Sys.time()), ".csv")
      )
      
      df <- loadDF()
      estudiant <- dplyr::select(df, dplyr::all_of(config$id_column), dplyr::all_of(config$prediction_column))
      solucions <- dplyr::select(respostes, dplyr::all_of(config$id_column), dplyr::all_of(config$response_column))
      check_answer <- dplyr::left_join(solucions, estudiant, by = config$id_column)
      n_missing <- sum(is.na(check_answer[[config$prediction_column]]))
      n_total <- nrow(check_answer)
      
      metric_cols <- setdiff(names(result), c("user", "time"))
      
      if (config$task_type == "regression") {
        message <- glue::glue(
          "<strong>✅ Your RMSE:</strong> {result$RMSE}<br>
           <strong>✅ Your MAE:</strong> {result$MAE}",
          .envir = environment()
        )
      } else {
        metric_lines <- purrr::map_chr(
          metric_cols,
          ~ sprintf("<strong>✅ %s:</strong> %s", .x, result[[.x]])
        )
        message <- paste(metric_lines, collapse = "<br>")
      }
      
      if (n_missing > 0) {
        percent_missing <- round(100 * n_missing / n_total, 1)
        message <- paste0(
          message,
          "<br><br>ℹ️ <strong>Note:</strong> You missed <strong>", n_missing, "</strong> out of ", n_total, " predictions (", percent_missing, "%).",
          "<br>These missing values have been penalized in your score."
        )
      }
      
      HTML(message)
    } else {
      HTML("❌ It was not possible to calculate a result.")
    }
  })
}

library(shiny)
library(tidyverse)
library(readxl)
library(shinyalert)
library(caret)
library(yaml)

# Workshop configuration
admin_password <- "iqs2024"
config <- read_yaml("workshop_config.yaml")

if (is.null(config$task_type)) config$task_type <- "regression"
if (is.null(config$metric)) {
  config$metric <- if (config$task_type == "regression") "RMSE" else "F1-Score"
}

# Result consolidation
consolidar_resultats <- function(path = config$results_path,
                                 output_file = config$resultats_rdata) {
  
  file_names <- list.files(path, pattern = "\\.csv$", full.names = TRUE)
  
  if (file.exists(output_file)) {
    load(output_file)    # loads 'resultats' into this scope
  } else {
    resultats <- data.frame()
  }
  
  if (length(file_names) > 0) {
    nous_resultats <- map_dfr(file_names, read_csv, show_col_types = FALSE)
    resultats <- bind_rows(resultats, nous_resultats)
    save(resultats, file = output_file)
    # file.remove(file_names)  # optional: remove processed CSV files
  }
  
  resultats
}

consolidar_resultats()

# Input data
participants <- read_delim("participants.csv", delim = ";")
respostes    <- read_excel(config$response_file)

# Regression metrics (RMSE and MAE)
compute_metrics <- function(obs, pred) {
  penalized_pred <- ifelse(is.na(pred), max(obs, na.rm = TRUE) * 1.5, pred)
  errors <- obs - penalized_pred
  
  rmse <- round(sqrt(mean(errors^2, na.rm = TRUE)), 3)
  mae  <- round(mean(abs(errors), na.rm = TRUE), 3)
  
  list(
    metrics = tibble(
      metric = c("RMSE", "MAE"),
      value  = c(rmse, mae)
    ),
    n_missing = sum(is.na(pred)),
    n_total   = length(obs)
  )
}

# Classification metrics using caret::confusionMatrix
compute_classif_metrics <- function(check_answer, response_col, pred_col) {
  
  df_cm <- tibble(
    response   = check_answer[[response_col]],
    prediction = check_answer[[pred_col]]
  )
  
  n_total   <- nrow(df_cm)
  n_missing <- sum(is.na(df_cm$prediction))
  
  # Remove rows with NA for metric computation
  df_cm <- df_cm %>%
    filter(!is.na(response), !is.na(prediction))
  
  if (nrow(df_cm) == 0) {
    stop("No valid (non-NA) pairs of observed/predicted values for classification.")
  }
  
  # Response: must be 0/1 numeric or factor with levels 0/1
  resp_fac <- factor(df_cm$response, levels = c(0, 1))
  if (any(is.na(resp_fac))) {
    stop("response_column must contain only 0/1 values for classification.")
  }
  
  # Prediction: 0/1 labels or probabilities in [0,1]
  pred_raw <- df_cm$prediction
  if (is.factor(pred_raw)) pred_raw <- as.character(pred_raw)
  
  pred_num     <- suppressWarnings(as.numeric(pred_raw))
  non_na_pred  <- pred_num[!is.na(pred_num)]
  is_prob      <- length(non_na_pred) > 0 &&
    all(non_na_pred >= 0 & non_na_pred <= 1) &&
    any(!(non_na_pred %in% c(0, 1)))
  
  if (is_prob) {
    # Probabilities -> 0/1 labels (threshold 0.5)
    pred_lab <- ifelse(pred_num >= 0.5, 1, 0)
  } else {
    # Should be 0/1 labels
    if (any(!non_na_pred %in% c(0, 1))) {
      stop("prediction_column must be probabilities in [0,1] or labels 0/1.")
    }
    pred_lab <- pred_num
  }
  
  pred_fac <- factor(pred_lab, levels = c(0, 1))
  
  cm <- confusionMatrix(
    data      = pred_fac,
    reference = resp_fac,
    positive  = "1"
  )
  
  accuracy    <- unname(cm$overall["Accuracy"])
  sensitivity <- unname(cm$byClass["Sensitivity"])
  specificity <- unname(cm$byClass["Specificity"])
  precision   <- unname(cm$byClass["Pos Pred Value"])
  
  f1 <- if (!is.na(precision) && !is.na(sensitivity) && (precision + sensitivity) > 0) {
    2 * precision * sensitivity / (precision + sensitivity)
  } else {
    NA_real_
  }
  
  metrics <- tibble(
    Accuracy    = round(accuracy, 3),
    Precision   = round(precision, 3),
    Sensitivity = round(sensitivity, 3),
    Specificity = round(specificity, 3),
    `F1-Score`  = round(f1, 3)
  )
  
  list(
    metrics   = metrics,
    n_missing = n_missing,
    n_total   = n_total
  )
}

# Server
server <- function(input, output) {
  
  if (file.exists(config$resultats_rdata)) {
    load(config$resultats_rdata)   # loads 'resultats'
  } else {
    resultats <- data.frame()
  }
  
  # Load user file
  loadDF <- reactive({
    tryCatch({
      req(input$file1)
      if (input$filetype != "excel") {
        df <- read.csv(
          input$file1$datapath,
          header = TRUE,
          sep    = input$sep,
          dec    = input$dec
        )
      } else {
        df <- read_excel(input$file1$datapath)
      }
      df
    },
    error = function(e) {
      shinyalert("Oops!", "Error loading your file.", type = "error")
      stop(safeError(e))
    })
  })
  
  # Compute metrics for current attempt
  getAnswers <- eventReactive(input$button, {
    tryCatch({
      df <- loadDF()
      
      if (!(input$user %in% participants[[config$user_column]])) {
        shinyalert("Oops!", "Your email is not in the participant list", type = "error")
        return(NULL)
      }
      
      # Join by ID defined in YAML
      estudiant <- df %>%
        select(all_of(config$id_column),
               all_of(config$prediction_column)) %>%
        group_by(across(all_of(config$id_column))) %>%
        slice_tail(n = 1) %>%              # keep last prediction per id
        ungroup()
      
      solucions <- respostes %>%
        select(all_of(config$id_column),
               all_of(config$response_column))
      
      check_answer <- left_join(
        solucions,
        estudiant,
        by = config$id_column
      )
      
      obs  <- check_answer[[config$response_column]]
      pred <- check_answer[[config$prediction_column]]
      
      # Debug info
      message(">> DEBUG join")
      message("nrow(solucions) = ", nrow(solucions))
      message("nrow(estudiant) = ", nrow(estudiant))
      message("nrow(check_answer) = ", nrow(check_answer))
      message("NAs in pred after join: ", sum(is.na(pred)))
      
      if (config$task_type == "regression") {
        
        m <- compute_metrics(obs, pred)
        
        metric_row <- pivot_wider(
          m$metrics,
          names_from  = metric,
          values_from = value
        )
        
        result <- data.frame(
          user  = input$user,
          time  = as.POSIXct(format(Sys.time(), "%Y-%m-%d %H:%M:00")),
          metric_row,
          stringsAsFactors = FALSE
        )
        
        attr(result, "n_missing") <- m$n_missing
        attr(result, "n_total")   <- m$n_total
        
        result
        
      } else if (config$task_type == "classification") {
        
        m <- compute_classif_metrics(
          check_answer,
          response_col = config$response_column,
          pred_col     = config$prediction_column
        )
        
        result <- data.frame(
          user  = input$user,
          time  = as.POSIXct(format(Sys.time(), "%Y-%m-%d %H:%M:00")),
          m$metrics,
          stringsAsFactors = FALSE
        )
        
        attr(result, "n_missing") <- m$n_missing
        attr(result, "n_total")   <- m$n_total
        
        result
        
      } else {
        stop("Unknown task_type in YAML.")
      }
      
    },
    error = function(e) {
      shinyalert("Oops!", "Could not process your result.", type = "error")
      stop(safeError(e))
    })
  })
  
  # Show uploaded data
  output$contents <- renderTable({
    req(input$file1)
    df <- loadDF()
    if (input$disp == "head") head(df) else df
  })
  
  # Plot benchmark (regression or classification)
  output$chart <- renderPlot({
    result <- getAnswers()
    req(result)
    
    if (!exists("resultats") || nrow(resultats) == 0) return(NULL)
    
    if (config$task_type == "regression") {
      # Regression: original RMSE/MAE benchmark
      
      req(all(c("RMSE", "MAE") %in% names(resultats)))
      
      rmse_min      <- min(resultats$RMSE, na.rm = TRUE)
      rmse_median   <- median(resultats$RMSE, na.rm = TRUE)
      rmse_threshold <- rmse_median + 3 * (rmse_median - rmse_min)
      
      mae_min       <- min(resultats$MAE, na.rm = TRUE)
      mae_median    <- median(resultats$MAE, na.rm = TRUE)
      mae_threshold <- mae_median + 3 * (mae_median - mae_min)
      
      resultats_filtrats <- resultats %>%
        mutate(
          RMSE = as.numeric(RMSE),
          MAE  = as.numeric(MAE)
        ) %>%
        filter(RMSE <= rmse_threshold,
               MAE  <= mae_threshold)
      
      result_filtrat <- result %>%
        mutate(
          RMSE = as.numeric(RMSE),
          MAE  = as.numeric(MAE)
        ) %>%
        filter(RMSE <= rmse_threshold,
               MAE  <= mae_threshold)
      
      dflog_long <- result_filtrat %>%
        select(-time) %>%
        pivot_longer(-user, names_to = "metric") %>%
        mutate(type = "Your attempt")
      
      best_row <- resultats_filtrats %>%
        mutate(total_error = RMSE + MAE) %>%
        slice_min(total_error, n = 1, with_ties = FALSE)
      
      best_score <- best_row %>%
        select(-time, -total_error) %>%
        pivot_longer(-user, names_to = "metric") %>%
        mutate(type = "Best (lowest avg)")
      
      punts_extra <- bind_rows(dflog_long, best_score)
      
      p <- resultats_filtrats %>%
        select(-time) %>%
        pivot_longer(-user, names_to = "metric") %>%
        ggplot(aes(x = metric, y = value)) +
        geom_boxplot(outlier.shape = NA) +
        labs(
          title    = "Benchmark of your results",
          subtitle = "Your result (red) vs. best result (blue)",
          color    = "Legend",
          shape    = "Legend"
        ) +
        ylim(0, max(c(resultats_filtrats$RMSE,
                      resultats_filtrats$MAE), na.rm = TRUE) * 1.2)
      
      if (nrow(punts_extra) > 0) {
        p <- p +
          geom_point(
            data  = punts_extra,
            aes(x = metric, y = value, color = type, shape = type),
            stroke = 2,
            size   = 3
          )
      }
      
      p +
        scale_color_manual(values = c("Your attempt" = "red",
                                      "Best (lowest avg)" = "blue")) +
        scale_shape_manual(values = c("Your attempt" = 4,
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
      # Classification: boxplots 0–1 + crosses for user and best
      
      metric_cols <- setdiff(names(resultats), c("user", "time"))
      metric_cols <- intersect(metric_cols, setdiff(names(result), c("user", "time")))
      if (length(metric_cols) == 0) return(NULL)
      
      main_metric <- config$metric
      if (!(main_metric %in% metric_cols)) {
        main_metric <- metric_cols[1]
      }
      
      resultats_long <- resultats %>%
        mutate(across(all_of(metric_cols), as.numeric)) %>%
        select(-time) %>%
        pivot_longer(-user, names_to = "metric", values_to = "value")
      
      result_long <- result %>%
        select(-time) %>%
        pivot_longer(-user, names_to = "metric", values_to = "value") %>%
        filter(metric %in% metric_cols) %>%
        mutate(type = "Your attempt")
      
      best_row <- resultats %>%
        mutate(across(all_of(metric_cols), as.numeric)) %>%
        slice_max(.data[[main_metric]], n = 1, with_ties = FALSE)
      
      best_score <- best_row %>%
        select(-time) %>%
        pivot_longer(-user, names_to = "metric", values_to = "value") %>%
        filter(metric %in% metric_cols) %>%
        mutate(type = "Best")
      
      punts_extra <- bind_rows(result_long, best_score)
      
      p <- resultats_long %>%
        filter(metric %in% metric_cols) %>%
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
        ylim(0, 1)
      
      if (nrow(punts_extra) > 0) {
        p <- p +
          geom_point(
            data  = punts_extra,
            aes(x = metric, y = value, color = type, shape = type),
            stroke = 2,
            size   = 3
          )
      }
      
      p +
        scale_color_manual(values = c("Your attempt" = "red",
                                      "Best"          = "blue")) +
        scale_shape_manual(values = c("Your attempt" = 4,
                                      "Best"          = 4)) +
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
  
  # Text feedback
  output$Answer <- renderUI({
    result <- getAnswers()
    
    if (!is.null(result) && input$user %in% participants[[config$user_column]]) {
      write_csv(
        result,
        file = paste0(config$results_path,
                      input$user, "-dflog", as.integer(Sys.time()), ".csv")
      )
      
      df <- loadDF()
      estudiant <- df %>%
        select(all_of(config$id_column),
               all_of(config$prediction_column))
      
      solucions <- respostes %>%
        select(all_of(config$id_column),
               all_of(config$response_column))
      
      check_answer <- left_join(solucions, estudiant, by = config$id_column)
      n_missing    <- sum(is.na(check_answer[[config$prediction_column]]))
      n_total      <- nrow(check_answer)
      
      metric_cols <- setdiff(names(result), c("user", "time"))
      
      if (config$task_type == "regression") {
        message <- glue::glue(
          "<strong>✅ Your RMSE:</strong> {result$RMSE}<br>
           <strong>✅ Your MAE:</strong> {result$MAE}",
          .envir = environment()
        )
      } else {
        metric_lines <- map_chr(
          metric_cols,
          ~ sprintf("<strong>✅ %s:</strong> %s", .x, result[[.x]])
        )
        message <- paste(metric_lines, collapse = "<br>")
      }
      
      if (n_missing > 0) {
        percent_missing <- round(100 * n_missing / n_total, 1)
        message <- paste0(
          message,
          "<br><br>ℹ️ <strong>Note:</strong> You missed <strong>",
          n_missing, "</strong> out of ", n_total, " predictions (",
          percent_missing, "%).",
          "<br>These missing values have been penalized in your score."
        )
      }
      
      HTML(message)
    } else {
      HTML("❌ It was not possible to calculate a result.")
    }
  })
}

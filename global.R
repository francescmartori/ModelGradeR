library(shiny)
library(shinyjs)
library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(tibble)
library(ggplot2)
library(readxl)
library(shinyalert)
library(yaml)

# ---------------------------------------------------------------
# Configuration: load and validate (fail fast, at deployment time)
# ---------------------------------------------------------------

config <- read_yaml("workshop_config.yaml")

# Defaults
if (is.null(config$task_type)) config$task_type <- "regression"
if (is.null(config$metric)) {
  config$metric <- if (config$task_type == "regression") "RMSE" else "F1-Score"
}
if (is.null(config$cooldown_seconds)) config$cooldown_seconds <- 20
if (is.null(config$poll_ms)) config$poll_ms <- 2500
if (is.null(config$consolidate_after_idle_minutes)) {
  config$consolidate_after_idle_minutes <- 10
}
if (is.null(config$evidence_min_chars)) config$evidence_min_chars <- 30

VALID_METRICS <- list(
  regression     = c("RMSE", "MAE"),
  classification = c("Accuracy", "Precision", "Sensitivity",
                     "Specificity", "F1-Score")
)

validate_config <- function(cfg) {
  required <- c("workshop_name", "response_file", "response_column",
                "id_column", "user_column", "prediction_column",
                "results_path", "resultats_rdata")
  missing_fields <- required[!required %in% names(cfg)]
  if (length(missing_fields) > 0) {
    stop("workshop_config.yaml: missing required fields: ",
         paste(missing_fields, collapse = ", "))
  }

  if (!cfg$task_type %in% c("regression", "classification")) {
    stop("workshop_config.yaml: task_type must be 'regression' or ",
         "'classification' (got '", cfg$task_type, "').")
  }

  if (!cfg$metric %in% VALID_METRICS[[cfg$task_type]]) {
    stop("workshop_config.yaml: metric '", cfg$metric,
         "' is not valid for task_type '", cfg$task_type, "'. ",
         "Valid options: ",
         paste(VALID_METRICS[[cfg$task_type]], collapse = ", "), ".")
  }

  if (!file.exists(cfg$response_file)) {
    stop("Solutions file not found: ", cfg$response_file)
  }
  if (!file.exists("participants.csv")) {
    stop("participants.csv not found in app directory.")
  }

  invisible(TRUE)
}

validate_config(config)

# Results directories
dir.create(config$results_path, showWarnings = FALSE, recursive = TRUE)
processed_path <- file.path(config$results_path, "processed")
dir.create(processed_path, showWarnings = FALSE, recursive = TRUE)

# ---------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------

normalize_email <- function(x) tolower(trimws(x))

# Unique filename + atomic write: write to .csv.tmp, then rename.
# list.files(pattern = "\\.csv$") never sees half-written files.
# If model_text is non-empty, it is saved as a sibling file with the
# same base name and a .model.txt extension, so the model evidence of
# each attempt can be found by name (by a human or by the asynchronous
# verification script). Never mixed into the CSV: Gretl output is
# multi-line and would break the tabular results store.
write_result_atomic <- function(result, dir_path, user, model_text = "") {
  safe_user <- gsub("[^A-Za-z0-9._@-]", "_", user)
  stamp  <- format(Sys.time(), "%Y%m%d-%H%M%OS3")   # millisecond precision
  suffix <- paste0(sample(c(letters, 0:9), 6, replace = TRUE), collapse = "")
  base   <- file.path(dir_path, paste0(safe_user, "-", stamp, "-", suffix))

  if (nzchar(model_text)) {
    model_final <- paste0(base, ".model.txt")
    model_tmp   <- paste0(model_final, ".tmp")
    writeLines(model_text, model_tmp)
    file.rename(model_tmp, model_final)
  }

  # The CSV is written last: an attempt "exists" only once its CSV does,
  # so a model file can never be orphaned in the other direction.
  final <- paste0(base, ".csv")
  tmp   <- paste0(final, ".tmp")
  write_csv(result, tmp)
  file.rename(tmp, final)
  invisible(final)
}

# Cheap plausibility check, deliberately NOT a validation of content:
# only whether there is enough text to be a pasted model output.
# Content verification (recomputing metrics from the declared model)
# happens asynchronously, outside the app, on the attempts that count.
has_model_evidence <- function(model_text) {
  nchar(trimws(model_text)) >= config$evidence_min_chars
}

# ---------------------------------------------------------------
# Consolidation: SINGLE WRITER, runs only at app startup
# (never during class; student sessions are read-only).
# Raw CSVs are moved to processed/, never deleted: they remain the
# recoverable source of truth if the RData is ever corrupted.
# ---------------------------------------------------------------

consolidar_resultats <- function(path        = config$results_path,
                                 output_file = config$resultats_rdata,
                                 processed   = processed_path) {

  file_names <- list.files(path, pattern = "\\.csv$", full.names = TRUE)

  if (file.exists(output_file)) {
    load(output_file)                 # loads 'resultats'
    resultats <- normalize_results(resultats)
  } else {
    resultats <- data.frame()
  }

  if (length(file_names) > 0) {
    nous <- lapply(file_names, read_attempt_csv)
    nous <- nous[!vapply(nous, is.null, logical(1))]
    nous_resultats <- normalize_results(bind_rows(nous))
    resultats <- distinct(bind_rows(resultats, nous_resultats))
    save(resultats, file = output_file)

    # Move processed files only after the RData is safely written.
    # Sibling .model.txt files (model evidence) travel with their CSV
    # so the pairing by base name is preserved in processed/.
    file.rename(file_names,
                file.path(processed, basename(file_names)))
    model_files <- sub("\\.csv$", ".model.txt", file_names)
    model_files <- model_files[file.exists(model_files)]
    if (length(model_files) > 0) {
      file.rename(model_files,
                  file.path(processed, basename(model_files)))
    }
  }

  invisible(resultats)
}

# Idle-time consolidation, safe under concurrency.
# Several sessions may detect the idle condition at the same time;
# dir.create() is atomic, so only one of them acquires the lock and
# consolidates. The others silently skip.
try_consolidate_with_lock <- function() {
  lock <- file.path(config$results_path, ".consolidate.lock")
  if (!dir.create(lock, showWarnings = FALSE)) {
    return(invisible(FALSE))          # someone else is consolidating
  }
  on.exit(unlink(lock, recursive = TRUE), add = TRUE)
  consolidar_resultats()
  invisible(TRUE)
}

# Fast reader for the one-row attempt CSVs: readr's per-call overhead
# dominates when hundreds of files are polled by dozens of sessions.
read_attempt_csv <- function(path) {
  tryCatch(
    utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE),
    error = function(e) NULL
  )
}

# The RData holds `time` as POSIXct and a freshly read CSV as
# character; bind_rows refuses to combine them. Text on both sides also
# lets distinct() dedupe across the two sources.
normalize_results <- function(d) {
  if (is.null(d) || nrow(d) == 0) return(d)
  if ("time" %in% names(d)) d$time <- as.character(d$time)
  d
}

read_consolidated <- function(output_file = config$resultats_rdata) {
  if (file.exists(output_file)) {
    load(output_file)
    normalize_results(resultats)
  } else {
    data.frame()
  }
}

# Read-only view: consolidated RData + pending CSVs. For grading
# scripts; the app builds the benchmark incrementally (see server.R).
read_all_results <- function(path        = config$results_path,
                             output_file = config$resultats_rdata) {

  resultats <- read_consolidated(output_file)

  pending <- list.files(path, pattern = "\\.csv$", full.names = TRUE)
  if (length(pending) > 0) {
    nous <- lapply(pending, read_attempt_csv)
    nous <- nous[!vapply(nous, is.null, logical(1))]
    if (length(nous) > 0) {
      resultats <- bind_rows(resultats,
                             normalize_results(bind_rows(nous)))
    }
  }

  distinct(resultats)
}

# Cheap fingerprint for reactivePoll's checkFunc: one readdir, no file
# contents read. Changes iff a CSV appears or the RData is rewritten.
results_state <- function(path        = config$results_path,
                          output_file = config$resultats_rdata) {
  files <- list.files(path, pattern = "\\.csv$", full.names = TRUE)
  paste(c(rdata_stamp(output_file), files), collapse = "|")
}

rdata_stamp <- function(output_file = config$resultats_rdata) {
  if (file.exists(output_file)) {
    paste0(output_file, file.info(output_file)$mtime)
  } else ""
}

# ---------------------------------------------------------------
# Metrics
# ---------------------------------------------------------------

# Regression metrics (RMSE and MAE); NA predictions are penalized
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

# Classification metrics computed directly from the 2x2 confusion
# table (positive class = 1). This replaces caret::confusionMatrix,
# which pulled a large dependency tree for four cell counts.
# NOTE: NA predictions are EXCLUDED here, not penalized (unlike regression).
compute_classif_metrics <- function(check_answer, response_col, pred_col) {

  df_cm <- tibble(
    response   = check_answer[[response_col]],
    prediction = check_answer[[pred_col]]
  )

  n_total   <- nrow(df_cm)
  n_missing <- sum(is.na(df_cm$prediction))

  df_cm <- df_cm %>%
    filter(!is.na(response), !is.na(prediction))

  if (nrow(df_cm) == 0) {
    stop("No valid (non-NA) pairs of observed/predicted values for classification.")
  }

  resp_fac <- factor(df_cm$response, levels = c(0, 1))
  if (any(is.na(resp_fac))) {
    stop("response_column must contain only 0/1 values for classification.")
  }
  resp_num <- as.numeric(as.character(resp_fac))

  pred_raw <- df_cm$prediction
  if (is.factor(pred_raw)) pred_raw <- as.character(pred_raw)

  pred_num    <- suppressWarnings(as.numeric(pred_raw))
  non_na_pred <- pred_num[!is.na(pred_num)]
  is_prob     <- length(non_na_pred) > 0 &&
    all(non_na_pred >= 0 & non_na_pred <= 1) &&
    any(!(non_na_pred %in% c(0, 1)))

  if (is_prob) {
    pred_lab <- ifelse(pred_num >= 0.5, 1, 0)
  } else {
    if (any(!non_na_pred %in% c(0, 1))) {
      stop("prediction_column must be probabilities in [0,1] or labels 0/1.")
    }
    pred_lab <- pred_num
  }

  # 2x2 confusion table, positive class = 1
  tp <- sum(pred_lab == 1 & resp_num == 1)
  tn <- sum(pred_lab == 0 & resp_num == 0)
  fp <- sum(pred_lab == 1 & resp_num == 0)
  fn <- sum(pred_lab == 0 & resp_num == 1)

  safe_div <- function(num, den) if (den > 0) num / den else NA_real_

  accuracy    <- safe_div(tp + tn, tp + tn + fp + fn)
  sensitivity <- safe_div(tp, tp + fn)   # recall / true positive rate
  specificity <- safe_div(tn, tn + fp)   # true negative rate
  precision   <- safe_div(tp, tp + fp)   # positive predictive value

  f1 <- if (!is.na(precision) && !is.na(sensitivity) &&
            (precision + sensitivity) > 0) {
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

# ---------------------------------------------------------------
# Startup: load inputs.
# No consolidation here: Shiny Server runs this file once per
# connection, so it would run concurrently during a live session.
# Consolidation happens only via try_consolidate_with_lock().
# ---------------------------------------------------------------

participants <- read_delim("participants.csv", delim = ";",
                           show_col_types = FALSE)
if (!config$user_column %in% names(participants)) {
  stop("participants.csv does not contain the column '",
       config$user_column, "' defined in workshop_config.yaml.")
}
participants_emails <- normalize_email(participants[[config$user_column]])

respostes <- read_excel(config$response_file)
if (!all(c(config$id_column, config$response_column) %in% names(respostes))) {
  stop("Solutions file '", config$response_file,
       "' must contain columns '", config$id_column, "' and '",
       config$response_column, "'.")
}

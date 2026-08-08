ModelGradeR

A Shiny app for automated scoring of student predictions in competitive, in-class prediction workshops. Students upload a file with their predictions (Excel or CSV), receive their metrics immediately, and see their attempt positioned within the live distribution of the whole class (boxplot benchmark with their attempt and the current best marked as crosses).

Designed for linear models / data analysis courses where students do not program: models are estimated in Gretl, applied manually in Excel, and this app only scores and gives feedback. Supports regression (RMSE, MAE) and binary classification (Accuracy, Precision, Sensitivity, Specificity, F1-Score) workshops.

Developed at IQS School of Management, Universitat Ramon Llull.

How it works
Each workshop is defined entirely by workshop_config.yaml: no code changes are needed to run a new workshop or reuse the app in another course.
Every submitted attempt is written as an individual CSV file with a guaranteed-unique name (millisecond timestamp + random suffix), using an atomic write (temp file + rename). One click produces at most one file; no attempt can be lost or half-written.
Student sessions are strictly read-only with respect to the results store. The benchmark chart refreshes automatically (default every 2.5 s) via reactivePoll, reading the consolidated .RData plus any pending CSVs.
Consolidation (append pending CSVs to the .RData, then move them to processed/) runs only at app startup and, automatically, after a configurable period with no new submissions (default 10 minutes). A filesystem lock guarantees a single consolidator even with many open sessions. Raw CSVs are moved, never deleted: they remain the recoverable source of truth.
A server-side cooldown (default 20 s, mirrored client-side by disabling the button) prevents accidental duplicate submissions from double clicks.
Uploaded files are validated with student-facing error messages: missing columns (listing the columns actually found), unreadable files, and the most common real-world mistake — uploading the TRAIN file instead of the TEST file with predictions (detected when no ids match).
Files
File	Role
global.R	Config loading and validation, metric functions, atomic write, consolidation, read-only results view
server.R	Reactivity: submission handling, cooldown, live benchmark, feedback
ui.R	Interface (CSV parsing options only appear if CSV is selected)
workshop_config.yaml	Per-workshop configuration
Configuration (workshop_config.yaml)
yaml
workshop_name: "Workshop 11 - Classification"
response_file: "W11_test_solutions.xlsx"   # solutions file (Excel)
response_column: "Churn_sol"               # column with correct answers
id_column: "id"                            # join key
user_column: "Email"                       # column in participants.csv
prediction_column: "Churn"                 # column students must provide
results_path: "./AdD/"                     # where attempt CSVs are stored
resultats_rdata: "W11.RData"               # consolidated results store
task_type: "classification"                # regression | classification
cooldown_seconds: 20
poll_ms: 2500
consolidate_after_idle_minutes: 10
# metric: "F1-Score"   # optional; defines the "Best" attempt in the
#                      # classification chart (default F1-Score).
#                      # Not used in regression.

Configuration is validated at startup and the app refuses to launch with a clear error message if a required field is missing, the metric is inconsistent with the task type, or an input file is not found.

Required files in the app directory
participants.csv (semicolon-separated) containing the column named in user_column. Only listed emails can record attempts; matching is case- and whitespace-insensitive.
The solutions file named in response_file, containing id_column and response_column.
www/iqslogo.png (or replace with your institution's logo).
Scoring details
Regression: RMSE and MAE. Missing predictions are penalized by imputing 1.5 × max(observed).
Classification: response must be 0/1; predictions may be 0/1 labels or probabilities in [0, 1] (thresholded at 0.5). Missing predictions are excluded from the metric computation (their count is reported to the student).
If a student's file contains repeated ids, only the last prediction per id is kept. The id column is coerced to character on both sides of the join, so numeric/text id mismatches (a frequent Excel artifact) cannot silently produce empty joins.
The benchmark chart filters aberrant attempts (worse than median + 3 × (median − best)) in regression so that the boxplot remains readable.
Data recorded per attempt

user (normalized email), time (full-precision timestamp, allowing arrival-order tie-breaking), workshop, and the metric columns for the task type. Grade computation is performed outside the app by separate scripts; for robustness these should read the consolidated .RData plus any pending CSVs (see read_all_results() in global.R, which can be reused as-is).

Deployment notes
Three-file structure (global.R / ui.R / server.R) for standard Shiny Server deployment.
Dependencies: shiny, shinyjs, tidyverse, readxl, shinyalert, caret, yaml.
Idle-time consolidation runs inside sessions: at least one browser must keep the app open past the idle window for it to trigger. Otherwise, consolidation simply happens at the next app startup — no data is lost either way.

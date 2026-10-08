# ---------------------------------------------------------------------------
# notes_workshop.R
#
# Grades a workshop, by group, from the attempts recorded by ModelGradeR.
# Handles both regression (RMSE, MAE) and classification (Accuracy,
# Precision, Sensitivity, Specificity, F1-Score) workshops.
#
# Grade = average of two components, both mapped to [NOTA_MIN, NOTA_MAX]:
#   - position: percentile of the group's error among all groups
#   - distance: relative excess over the best error, (e - best) / best,
#     linear down to the floor at E_MAX
# plus a bonus for the number of attempts, relative to the median group.
#
# The two components are complementary. Position alone is unfair when every
# group converges on the same model (last place scores 3 for a 3% gap);
# distance alone is unfair when a problem spreads the field (second of forty
# scores low for a large but hard-won gap). E_MAX is an absolute reference,
# so it does not need recalibrating per workshop.
#
# A group's error is that of the BEST attempt by any of its members.
# The distribution is computed over all groups of all class groups together.
#
# Data path: attempt (email) -> participants.csv (email <-> ID)
#            -> ASSIGNACIO (ID <-> Grup_final) -> group grade
# ---------------------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(readxl)

# ======================= CONFIGURATION =====================================

WORKSHOP  <- "W1"
TASK_TYPE <- "regression"        # "regression" | "classification"

RDATA_FILE        <- "W01.RData"
ASSIGNACIO_FILE   <- "grups_W1.xlsx"
ASSIGNACIO_SHEET  <- "ASSIGNACIO"
PARTICIPANTS_CSV  <- "participants.csv"      # Número ID ; Email

# Optional name <-> ID cross-check. Set to NA to skip it. Catches rows whose
# name and ID no longer match (e.g. names moved between rows by hand), which
# would otherwise silently place students in the wrong group.
PARTICIPANTS_XLSX <- "participants.xlsx"     # Nom, ID

# Session windows, in local time. Only attempts inside one of these count.
# COURTESY_MIN is added to the end of every window.
WINDOWS <- list(
  c("2026-09-29 13:00:00", "2026-09-29 14:00:00"),
  c("2026-09-30 13:00:00", "2026-09-30 14:00:00"),
  c("2026-09-30 16:00:00", "2026-09-30 17:00:00")
)
COURTESY_MIN <- 5
TZ           <- "Europe/Madrid"

EXCLUDE_USERS <- c("test", "francesc.martori@iqs.url.edu")

NOTA_MIN   <- 3
NOTA_MAX   <- 10
E_MAX      <- 0.30     # excess over the best at which a group hits the floor
                       # (0.30 = 30% worse than the best => NOTA_MIN)
W_POS      <- 0.5      # weight of the position component; 1 - W_POS distance
BONUS_UP   <- 1.0      # reward for attempts above the group median
BONUS_DOWN <- 0.25     # penalty below it, deliberately smaller

W_ACC <- 0.50          # classification: weight of Accuracy; the other four
                       # metrics share the rest equally

OUT_FILE <- paste0("notes_", WORKSHOP, ".csv")

# ======================= HELPERS ===========================================

norm_id <- function(x) {
  x <- trimws(as.character(x))
  out <- sub("^0+", "", x)                 # xlsx drops leading zeros, csv may not
  ifelse(out == "", x, out)
}

norm_email <- function(x) tolower(trimws(as.character(x)))

# Map a score in [0,1] to the grade range
to_grade <- function(p) NOTA_MIN + (NOTA_MAX - NOTA_MIN) * p

say <- function(...) cat(..., "\n", sep = "")

# ======================= 1. LOAD ===========================================

load(RDATA_FILE)                             # provides 'resultats'
say("Attempts in store: ", nrow(resultats))

# The store keeps `time` as text (full precision, UTC); older stores may hold
# POSIXct. Accept both.
if (is.character(resultats$time)) {
  t_utc <- ymd_hms(resultats$time, tz = "UTC", quiet = TRUE)
} else {
  t_utc <- with_tz(resultats$time, "UTC")
}
resultats <- resultats %>%
  mutate(time_local = with_tz(t_utc, TZ),
         user        = norm_email(user))

if (any(is.na(resultats$time_local))) {
  say("WARNING: ", sum(is.na(resultats$time_local)),
      " attempts with an unparseable timestamp; they are dropped.")
  resultats <- resultats %>% filter(!is.na(time_local))
}

# Normalize the metric column names: read.csv(check.names = FALSE) keeps
# "F1-Score" while older stores hold "F1.Score".
names(resultats)[names(resultats) == "F1.Score"] <- "F1-Score"

if ("workshop" %in% names(resultats)) {
  other <- setdiff(unique(resultats$workshop), NA)
  if (length(other) > 1) {
    say("NOTE: store holds attempts from several workshops: ",
        paste(other, collapse = " | "))
  }
}

# ======================= 2. TIME WINDOWS ===================================

in_any_window <- function(t) {
  keep <- rep(FALSE, length(t))
  for (w in WINDOWS) {
    from <- ymd_hms(w[1], tz = TZ)
    to   <- ymd_hms(w[2], tz = TZ) + minutes(COURTESY_MIN)
    keep <- keep | (t >= from & t <= to)
  }
  keep
}

n_before <- nrow(resultats)
resultats <- resultats %>%
  filter(!user %in% norm_email(EXCLUDE_USERS)) %>%
  filter(in_any_window(time_local))

say("Attempts inside session windows: ", nrow(resultats),
    " (dropped ", n_before - nrow(resultats), ")")

if (nrow(resultats) == 0) stop("No attempts left after filtering. Check WINDOWS.")

# ======================= 3. GROUPS =========================================

participants <- read_delim(PARTICIPANTS_CSV, delim = ";",
                           show_col_types = FALSE) %>%
  rename(id = 1, email = 2) %>%
  transmute(id = norm_id(id), email = norm_email(email))

assignacio <- read_excel(ASSIGNACIO_FILE, sheet = ASSIGNACIO_SHEET) %>%
  transmute(
    classe = as.character(Classe),
    id     = norm_id(Usuari),
    nom    = as.character(Nom),
    grup   = toupper(trimws(as.character(Grup_final)))
  ) %>%
  filter(!is.na(grup), grup != "")

say("Students assigned: ", nrow(assignacio),
    " in ", n_distinct(assignacio$grup), " groups")

# Optional cross-check: does each ID still sit next to its own name?
if (!is.na(PARTICIPANTS_XLSX) && file.exists(PARTICIPANTS_XLSX)) {
  flat <- function(s) {
    s <- iconv(trimws(as.character(s)), to = "ASCII//TRANSLIT")
    tolower(gsub("\\s+", " ", s))
  }
  noms <- read_excel(PARTICIPANTS_XLSX) %>%
    transmute(nom_key = flat(Nom), id_ref = norm_id(ID))
  bad <- assignacio %>%
    mutate(nom_key = flat(nom)) %>%
    inner_join(noms, by = "nom_key") %>%
    filter(id != id_ref)
  if (nrow(bad) > 0) {
    say("WARNING: ", nrow(bad), " row(s) in ", ASSIGNACIO_SHEET,
        " whose name and ID do not match. Grades will follow the ID, so",
        " these students are counted in the wrong group. Fix the file:")
    bad %>%
      transmute(nom, grup, id_in_file = id, id_should_be = id_ref) %>%
      as.data.frame() %>% print(row.names = FALSE)
  }
}

grups <- assignacio %>%
  left_join(participants, by = "id")

if (any(is.na(grups$email))) {
  say("WARNING: ", sum(is.na(grups$email)),
      " assigned student(s) without an email in ", PARTICIPANTS_CSV, ":")
  grups %>% filter(is.na(email)) %>%
    transmute(nom, id, grup) %>% as.data.frame() %>% print(row.names = FALSE)
}

intents <- resultats %>%
  left_join(grups %>% select(email, id, nom, grup, classe),
            by = c("user" = "email"))

orfes <- intents %>% filter(is.na(grup)) %>% distinct(user)
if (nrow(orfes) > 0) {
  say("WARNING: ", nrow(orfes),
      " email(s) with attempts but no group; their attempts are excluded:")
  print(orfes$user)
}
intents <- intents %>% filter(!is.na(grup))

# ======================= 4. ERROR MEASURE ==================================

if (TASK_TYPE == "regression") {
  metric_cols <- c("RMSE", "MAE")
  stopifnot(all(metric_cols %in% names(intents)))
  intents <- intents %>%
    mutate(across(all_of(metric_cols), as.numeric),
           total_error = RMSE + MAE)
} else if (TASK_TYPE == "classification") {
  metric_cols <- c("Accuracy", "Precision", "Sensitivity",
                   "Specificity", "F1-Score")
  stopifnot(all(metric_cols %in% names(intents)))
  w_other <- (1 - W_ACC) / 4
  intents <- intents %>%
    mutate(across(all_of(metric_cols), as.numeric),
           score_total = W_ACC * Accuracy +
             w_other * (Precision + Sensitivity + Specificity + `F1-Score`),
           total_error = 1 - score_total)
} else {
  stop("TASK_TYPE must be 'regression' or 'classification'.")
}

# Resubmitting an identical result is not a new attempt: keep one row per
# (student, set of metric values).
intents <- intents %>% filter(!is.na(total_error))
intents <- intents[!duplicated(intents[, c("user", metric_cols)]), ]

# Aberrant attempts (wrong file, broken predictions) are NOT dropped. The
# grading formula already floors anything beyond E_MAX at NOTA_MIN, so a
# group whose only attempts were broken still gets a grade, and a single
# absurd value cannot distort anyone else's: every comparison is anchored to
# the best result, never to the spread.
say("Attempts used: ", nrow(intents))

# ======================= 5. BEST ATTEMPT PER GROUP =========================

millors <- intents %>%
  group_by(grup) %>%
  slice_min(total_error, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  rename(best_user = user, best_nom = nom, best_time = time_local)

sense_intents <- setdiff(unique(grups$grup), millors$grup)
if (length(sense_intents) > 0) {
  say("NOTE: ", length(sense_intents),
      " group(s) with no valid attempt, left ungraded: ",
      paste(sort(sense_intents), collapse = ", "))
}

say("Groups graded: ", nrow(millors))

# How spread out is the field? A very small maximum excess means the groups
# converged on the same model, and most of the grade range will come from the
# position component.
say("Excess over the best result: median ",
    round(100 * median((millors$total_error - min(millors$total_error)) /
                         min(millors$total_error)), 2), "%, max ",
    round(100 * max((millors$total_error - min(millors$total_error)) /
                      min(millors$total_error)), 2), "%")

# ======================= 6. POSITION AND DISTANCE ==========================

millors <- millors %>%
  arrange(total_error) %>%
  mutate(
    # percent_rank is NaN with a single group
    percentil = if (n() > 1) 1 - percent_rank(total_error) else 1,
    nota_pos  = to_grade(percentil)
  )

best_error <- min(millors$total_error, na.rm = TRUE)

millors <- millors %>%
  mutate(
    exces     = (total_error - best_error) / best_error,
    nota_dist = to_grade(1 - pmin(exces / E_MAX, 1))
  )

# ======================= 7. ATTEMPT BONUS ==================================
# Group effort: every attempt by every member counts.

n_intents <- intents %>%
  group_by(grup) %>%
  summarise(intents = n(), .groups = "drop")

min_i <- min(n_intents$intents)
max_i <- max(n_intents$intents)
med_i <- median(n_intents$intents)

bonus <- n_intents %>%
  mutate(
    bonificacio = case_when(
      intents > med_i & max_i > med_i ~
        (intents - med_i) / (max_i - med_i) * BONUS_UP,
      intents < med_i & med_i > min_i ~
        -(med_i - intents) / (med_i - min_i) * BONUS_DOWN,
      TRUE ~ 0
    )
  )

say("Attempts per group: min ", min_i, ", median ", med_i, ", max ", max_i)

# ======================= 8. FINAL GRADE ====================================

membres <- grups %>%
  group_by(grup) %>%
  summarise(
    classe  = paste(sort(unique(classe)), collapse = "/"),
    n_membres = n(),
    membres = paste(sort(nom), collapse = "; "),
    .groups = "drop"
  )

notes_grup <- millors %>%
  left_join(bonus, by = "grup") %>%
  select(-any_of("classe")) %>%        # classe comes from membres, not from
  left_join(membres, by = "grup") %>%  # the member who made the best attempt
  mutate(
    nota_base  = pmin(pmax(W_POS * nota_pos + (1 - W_POS) * nota_dist,
                           NOTA_MIN), NOTA_MAX),
    nota_final = round(pmin(pmax(nota_base + bonificacio,
                                 NOTA_MIN), NOTA_MAX), 1)
  ) %>%
  arrange(desc(nota_final), total_error) %>%
  select(grup, classe, n_membres, membres,
         all_of(metric_cols), total_error, exces, percentil,
         nota_pos, nota_dist, nota_base,
         intents, bonificacio, nota_final,
         best_user, best_nom, best_time,
         any_of(c("has_evidence", "attempt_id")))

# Model evidence: diagnostic only for now, it does not affect the grade.
if ("has_evidence" %in% names(intents)) {
  ev <- intents %>%
    group_by(grup) %>%
    summarise(amb_model = sum(as.logical(has_evidence), na.rm = TRUE),
              total     = n(), .groups = "drop")
  say("Model evidence: ",
      sum(ev$amb_model), " of ", sum(ev$total), " attempts; ",
      sum(ev$amb_model > 0), " of ", nrow(ev), " groups attached at least one")
}

# ======================= 9. OUTPUT =========================================

say("\nGrade summary:")
print(summary(notes_grup$nota_final))

write.table(notes_grup, file = OUT_FILE,
            sep = "\t", dec = ",", row.names = FALSE,
            col.names = TRUE, quote = FALSE, na = "")
say("\nWritten: ", OUT_FILE)

# One row per student, for uploading to the gradebook
notes_alumne <- grups %>%
  select(id, nom, grup, classe, email) %>%
  left_join(notes_grup %>% select(grup, nota_final), by = "grup")

write.table(notes_alumne,
            file = paste0("notes_", WORKSHOP, "_alumnes.csv"),
            sep = "\t", dec = ",", row.names = FALSE,
            col.names = TRUE, quote = FALSE, na = "")
say("Written: notes_", WORKSHOP, "_alumnes.csv")

# Quick visual check
notes_grup %>%
  pivot_longer(c(nota_pos, nota_dist, nota_base, nota_final),
               names_to = "nota", values_to = "score") %>%
  ggplot(aes(nota, score)) +
  geom_boxplot() +
  labs(title = paste("Grade components —", WORKSHOP), x = NULL, y = NULL)

#!/usr/bin/env Rscript

# ---------------------------------------------------------------------------
# parteix_train_test.R
#
# D'un únic Excel amb totes les dades del workshop, en genera els tres fitxers
# que necessita l'activitat:
#
#   <base>_Train.xlsx           dades d'entrenament, amb la variable resposta
#   <base>_Test_Alumnos.xlsx    test per als alumnes, sense respostes i amb una
#                               columna buida perquè hi escriguin les prediccions
#   <base>_Test_Solucions.xlsx  el mateix test amb les respostes, per a l'app
#
# Train i test són complementaris: cap fila és als dos.
#
# Ús:
#   Rscript parteix_train_test.R dades.xlsx
#   Rscript parteix_train_test.R dades.xlsx quality
#   Rscript parteix_train_test.R dades.xlsx quality sortida_W3
#
#   1r argument: fitxer Excel d'entrada
#   2n argument (opcional): nom de la columna resposta
#   3r argument (opcional): carpeta de sortida
#
# Si no passes arguments, fa servir els valors de la secció CONFIGURACIÓ.
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(readxl)
  library(openxlsx)
})

# --------------------------- CONFIGURACIÓ ----------------------------------

FITXER_DEFECTE   <- "W2_2026-27.xlsx"
COL_RESPOSTA     <- "medv"     # columna a predir, tal com es diu a l'Excel
PESTANYA         <- 1             # número o nom de la pestanya amb les dades

PROPORCIO_TRAIN  <- 0.80          # la resta va a test
LLAVOR           <- 2026          # mateixa llavor, mateixa partició

COL_ID           <- "id"          # nom del camp identificador
PREFIX_ID        <- "id"          # valors: id1, id2, id3...

COL_PREDICCIO    <- "Respuestas"  # columna buida que omplen els alumnes
                                  # (al fitxer de solucions, la resposta manté
                                  #  el nom original de la variable)

SUFIX_TRAIN      <- "_Train"
SUFIX_TEST_ALU   <- "_Test_Alumnos"
SUFIX_TEST_SOL   <- "_Test_Solucions"

FORMAT_DEFECTE   <- "xlsx"        # "xlsx" o "csv"
CARPETA_DEFECTE  <- NULL          # NULL: una carpeta amb el nom del fitxer

CSV_SEP <- ","
CSV_DEC <- "."

# ---------------------------------------------------------------------------

args <- commandArgs(trailingOnly = TRUE)

fitxer   <- if (length(args) >= 1) args[1] else FITXER_DEFECTE
resposta <- if (length(args) >= 2) args[2] else COL_RESPOSTA
carpeta  <- if (length(args) >= 3) args[3] else CARPETA_DEFECTE
format   <- tolower(FORMAT_DEFECTE)

if (!file.exists(fitxer)) {
  stop("No trobo el fitxer: ", fitxer,
       "\nDirectori de treball actual: ", getwd())
}
if (!(format %in% c("xlsx", "csv"))) {
  stop("El format ha de ser 'xlsx' o 'csv'. He rebut: ", format)
}
if (!(PROPORCIO_TRAIN > 0 && PROPORCIO_TRAIN < 1)) {
  stop("PROPORCIO_TRAIN ha de ser un valor entre 0 i 1.")
}

base <- tools::file_path_sans_ext(basename(fitxer))
if (is.null(carpeta)) carpeta <- base
if (!dir.exists(carpeta)) {
  dir.create(carpeta, recursive = TRUE)
  cat("He creat la carpeta:", carpeta, "\n")
}

# --------------------------- LECTURA ---------------------------------------

dades <- suppressMessages(
  as.data.frame(readxl::read_excel(fitxer, sheet = PESTANYA, guess_max = 100000),
                stringsAsFactors = FALSE)
)

if (!(resposta %in% names(dades))) {
  stop("No hi ha cap columna '", resposta, "' al fitxer.",
       "\nColumnes disponibles: ", paste(names(dades), collapse = ", "))
}
if (COL_ID %in% names(dades)) {
  stop("El fitxer ja té una columna '", COL_ID,
       "'. Canvia COL_ID o treu-la de l'Excel, per no trepitjar-la.")
}
if (nrow(dades) < 10) stop("El fitxer només té ", nrow(dades), " files.")

# --------------------------- PARTICIÓ --------------------------------------

# Barregem abans d'assignar els identificadors: així l'ordre original del
# fitxer (sovint ordenat per alguna variable) no queda codificat a l'id.
set.seed(LLAVOR)
dades <- dades[sample(nrow(dades)), , drop = FALSE]
rownames(dades) <- NULL
dades[[COL_ID]] <- paste0(PREFIX_ID, seq_len(nrow(dades)))

n_train <- round(nrow(dades) * PROPORCIO_TRAIN)
train <- dades[seq_len(n_train), , drop = FALSE]
test  <- dades[(n_train + 1):nrow(dades), , drop = FALSE]

# Train: tot menys l'id, que només té sentit al test i confondria els alumnes
ids_train <- train[[COL_ID]]
train[[COL_ID]] <- NULL

# Test per als alumnes: sense la resposta, amb la columna buida a omplir
test_alu <- test
test_alu[[resposta]] <- NULL
test_alu[[COL_PREDICCIO]] <- NA
test_alu <- test_alu[, c(COL_ID,
                         setdiff(names(test_alu), c(COL_ID, COL_PREDICCIO)),
                         COL_PREDICCIO), drop = FALSE]

# Test amb solucions: el mateix test amb la resposta, que conserva el nom
# original de la variable. Al YAML de l'app:
#   response_column: <nom original>   prediction_column: Respuestas
test_sol <- test[, c(COL_ID,
                     setdiff(names(test), c(COL_ID, resposta)),
                     resposta), drop = FALSE]

# Comprovació: cap fila compartida, cap fila perduda
stopifnot(length(intersect(ids_train, test[[COL_ID]])) == 0,
          nrow(train) + nrow(test) == nrow(dades))

# --------------------------- ESCRIPTURA ------------------------------------

escriu <- function(d, sufix, nom_pestanya) {
  nom <- file.path(carpeta, paste0(base, sufix, ".", format))
  if (format == "xlsx") {
    wb <- createWorkbook()
    addWorksheet(wb, substr(nom_pestanya, 1, 31))
    writeData(wb, 1, d, headerStyle = createStyle(textDecoration = "bold"))
    setColWidths(wb, 1, cols = seq_len(max(1, ncol(d))), widths = "auto")
    freezePane(wb, 1, firstRow = TRUE)
    saveWorkbook(wb, nom, overwrite = TRUE)
  } else {
    utils::write.table(d, nom, sep = CSV_SEP, dec = CSV_DEC,
                       row.names = FALSE, na = "", qmethod = "double",
                       fileEncoding = "UTF-8")
  }
  cat(sprintf("  %-26s %4d files x %2d columnes  ->  %s\n",
              nom_pestanya, nrow(d), ncol(d), basename(nom)))
  nom
}

cat("\nFitxer d'entrada: ", fitxer, "\n", sep = "")
cat("Columna resposta: ", resposta, "   |   llavor: ", LLAVOR, "\n", sep = "")
cat(strrep("-", 70), "\n")

escriu(train,    SUFIX_TRAIN,    "train")
escriu(test_alu, SUFIX_TEST_ALU, "test alumnes")
escriu(test_sol, SUFIX_TEST_SOL, "test solucions")

cat(strrep("-", 70), "\n")
cat(sprintf("%d files en total: %d train (%.0f%%) i %d test (%.0f%%)\n",
            nrow(dades), nrow(train), 100 * nrow(train) / nrow(dades),
            nrow(test), 100 * nrow(test) / nrow(dades)))

# --------------------------- DIAGNÒSTIC ------------------------------------
# Mirar que les dues parts s'assemblin: si no, canvia la llavor.

y_tr <- train[[resposta]]
y_te <- test_sol[[resposta]]

if (is.numeric(y_tr) && length(unique(y_tr)) > 10) {
  cat(sprintf("\n%-16s %8s %8s %8s %8s\n", resposta, "mitjana", "sd", "min", "max"))
  cat(sprintf("%-16s %8.3f %8.3f %8.3f %8.3f\n", "train",
              mean(y_tr, na.rm = TRUE), sd(y_tr, na.rm = TRUE),
              min(y_tr, na.rm = TRUE), max(y_tr, na.rm = TRUE)))
  cat(sprintf("%-16s %8.3f %8.3f %8.3f %8.3f\n", "test",
              mean(y_te, na.rm = TRUE), sd(y_te, na.rm = TRUE),
              min(y_te, na.rm = TRUE), max(y_te, na.rm = TRUE)))
} else {
  cat("\nDistribució de ", resposta, ":\n", sep = "")
  tt <- rbind(train = table(y_tr), test = table(y_te))
  print(tt)
  print(chisq.test(tt))
  cat("Si alguna categoria queda molt desequilibrada al test, prova",
      "una altra llavor.\n")
}

nas <- colSums(is.na(test_alu))
nas <- nas[names(nas) != COL_PREDICCIO & nas > 0]
if (length(nas) > 0) {
  cat("\nValors absents al test dels alumnes: ",
      paste(sprintf("%s (%d)", names(nas), nas), collapse = ", "), "\n", sep = "")
}

cat("\nFitxers a: ", normalizePath(carpeta), "\n\n", sep = "")

#!/usr/bin/env Rscript

# ---------------------------------------------------------------------------
# separa_pestanyes.R
#
# Separa cada pestanya d'un fitxer Excel en un fitxer independent. Pensat per
# al material dels workshops: d'un únic Excel amb les dades de train, el test
# dels alumnes i les solucions, en surt un fitxer per a cada cosa.
#
# Ús:
#   Rscript separa_pestanyes.R workshop1.xlsx
#   Rscript separa_pestanyes.R workshop1.xlsx csv
#   Rscript separa_pestanyes.R workshop1.xlsx xlsx sortida_W1
#
#   1r argument: fitxer Excel d'entrada (.xlsx o .xls)
#   2n argument (opcional): format de sortida, "xlsx" o "csv"
#   3r argument (opcional): carpeta de sortida
#
# Si no passes arguments, fa servir els valors de la secció CONFIGURACIÓ.
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(readxl)
  library(openxlsx)
})

# --------------------------- CONFIGURACIÓ ----------------------------------

FITXER_DEFECTE <- "W1_2026-27"
FORMAT_DEFECTE <- "xlsx"        # "xlsx" o "csv"
CARPETA_DEFECTE <- NULL         # NULL: una carpeta amb el nom del fitxer

# Prefix dels fitxers generats. NULL fa servir el nom del fitxer d'entrada.
# Posa "" si no en vols cap i prefereixes que es diguin com la pestanya.
PREFIX <- NULL

# Pestanyes a exportar. NULL vol dir totes.
# Exemple: NOMES <- c("train", "test")
NOMES <- NULL

# Pestanyes a deixar fora, encara que NOMES sigui NULL.
# Exemple: EXCLOU <- c("notes", "esborrany")
EXCLOU <- NULL

# Per als fitxers CSV: separador i decimal.
CSV_SEP <- ","
CSV_DEC <- "."

# ---------------------------------------------------------------------------

args <- commandArgs(trailingOnly = TRUE)

fitxer  <- if (length(args) >= 1) args[1] else FITXER_DEFECTE
format  <- if (length(args) >= 2) tolower(args[2]) else tolower(FORMAT_DEFECTE)
carpeta <- if (length(args) >= 3) args[3] else CARPETA_DEFECTE

if (!nzchar(fitxer)) {
  cat("Ús: Rscript separa_pestanyes.R <fitxer.xlsx> [xlsx|csv] [carpeta]\n")
  cat("   o bé omple FITXER_DEFECTE a la secció CONFIGURACIÓ.\n")
  quit(status = 1)
}

if (!file.exists(fitxer)) {
  stop("No trobo el fitxer: ", fitxer,
       "\nDirectori de treball actual: ", getwd())
}

if (!(format %in% c("xlsx", "csv"))) {
  stop("El format ha de ser 'xlsx' o 'csv'. He rebut: ", format)
}

base <- tools::file_path_sans_ext(basename(fitxer))
if (is.null(carpeta)) carpeta <- base
if (!dir.exists(carpeta)) {
  dir.create(carpeta, recursive = TRUE)
  cat("He creat la carpeta:", carpeta, "\n")
}

prefix <- if (is.null(PREFIX)) paste0(base, "_") else
  if (nzchar(PREFIX)) paste0(PREFIX, "_") else ""

# --------------------------- SELECCIÓ DE PESTANYES -------------------------

pestanyes <- readxl::excel_sheets(fitxer)

if (!is.null(NOMES)) {
  falten <- setdiff(NOMES, pestanyes)
  if (length(falten) > 0) {
    stop("Aquestes pestanyes no són al fitxer: ", paste(falten, collapse = ", "),
         "\nPestanyes disponibles: ", paste(pestanyes, collapse = ", "))
  }
  pestanyes <- NOMES
}
if (!is.null(EXCLOU)) pestanyes <- setdiff(pestanyes, EXCLOU)

if (length(pestanyes) == 0) stop("No queda cap pestanya per exportar.")

# --------------------------- EXPORTACIÓ ------------------------------------

neteja <- function(x) {
  x <- gsub("[\\\\/:*?\"<>|]", "-", x)   # caràcters no vàlids en noms de fitxer
  x <- gsub("\\s+", "_", trimws(x))
  x
}

cat("\nFitxer d'entrada:", fitxer, "\n")
cat("Format de sortida:", format, "  Carpeta:", carpeta, "\n")
cat(strrep("-", 62), "\n")

generats <- character(0)

for (sh in pestanyes) {
  dades <- suppressMessages(
    as.data.frame(readxl::read_excel(fitxer, sheet = sh, guess_max = 100000),
                  stringsAsFactors = FALSE)
  )

  nom <- file.path(carpeta, paste0(prefix, neteja(sh), ".", format))

  if (format == "xlsx") {
    wb <- createWorkbook()
    addWorksheet(wb, substr(sh, 1, 31))
    writeData(wb, 1, dades,
              headerStyle = createStyle(textDecoration = "bold"))
    setColWidths(wb, 1, cols = seq_len(max(1, ncol(dades))), widths = "auto")
    freezePane(wb, 1, firstRow = TRUE)
    saveWorkbook(wb, nom, overwrite = TRUE)
  } else {
    utils::write.table(dades, nom, sep = CSV_SEP, dec = CSV_DEC,
                       row.names = FALSE, na = "", qmethod = "double",
                       fileEncoding = "UTF-8")
  }

  generats <- c(generats, nom)
  cat(sprintf("  %-22s %5d files x %2d columnes  ->  %s\n",
              sh, nrow(dades), ncol(dades), basename(nom)))
}

cat(strrep("-", 62), "\n")
cat(length(generats), "fitxers generats a", normalizePath(carpeta), "\n\n")

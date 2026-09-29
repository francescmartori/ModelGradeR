#!/usr/bin/env Rscript

# ---------------------------------------------------------------------------
# grups_workshop.R
#
# Genera els grups aleatoris de tres persones per a un workshop, a partir del
# fitxer de participants, i els exporta a un Excel amb una pestanya per grup
# classe.
#
# Ús:
#   Rscript grups_workshop.R participants.xlsx W3
#   Rscript grups_workshop.R participants.csv  W3 20260305
#
#   1r argument: fitxer de participants (.xlsx, .xls o .csv)
#   2n argument: identificador del workshop (p. ex. W3). Va al nom del fitxer
#                de sortida i als identificadors de grup.
#   3r argument (opcional): llavor aleatòria. Si no se'n dona cap, se'n deriva
#                una del nom del workshop, de manera que el mateix workshop
#                dona sempre els mateixos grups (reproduïble).
#
# El fitxer de participants ha de tenir, com a mínim, una columna amb
# l'identificador de l'estudiant (correu) i una columna amb el grup classe.
# Els noms es detecten automàticament; si no els troba, es poden fixar a mà
# més avall, a la secció CONFIGURACIÓ.
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(readxl)
  library(openxlsx)
})

# --------------------------- CONFIGURACIÓ ----------------------------------

MIDA_GRUP <- 3

# Si executes l'script des de RStudio, o si Rscript no et passa bé els
# arguments, omple aquestes tres variables i executa'l sense arguments.
# Els arguments de línia d'ordres, si n'hi ha, tenen prioritat.
FITXER_DEFECTE   <- "participants.xlsx"
WORKSHOP_DEFECTE <- "W1"
LLAVOR_DEFECTE   <- NULL   # NULL: es deriva del nom del workshop

# Deixa-ho a NULL per detectar les columnes automàticament, o posa-hi el nom
# exacte de la columna del teu fitxer, entre cometes.
COL_ID     <- "ID"   # p. ex. "user"
COL_CLASSE <- "Grups"   # p. ex. "classe"
COL_NOM    <- "Nom"   # p. ex. "nom"  (opcional)

# Candidats per a la detecció automàtica (en minúscules, sense accents)
CAND_ID     <- c("user", "usuari", "correu", "email", "e-mail", "mail", "id")
CAND_CLASSE <- c("classe", "class", "grup_classe", "grupclasse", "id_classe",
                 "idclasse", "grup", "group", "seccio", "seccion")
CAND_NOM    <- c("nom", "nombre", "name", "alumne", "estudiant", "cognoms")

# ---------------------------------------------------------------------------

args <- commandArgs(trailingOnly = TRUE)

fitxer   <- if (length(args) >= 1) args[1] else FITXER_DEFECTE
workshop <- if (length(args) >= 2) args[2] else WORKSHOP_DEFECTE

if (is.null(fitxer) || is.null(workshop) || !nzchar(fitxer) || !nzchar(workshop)) {
  cat("Ús: Rscript grups_workshop.R <participants.xlsx|csv> <workshop> [llavor]\n")
  cat("   o bé omple FITXER_DEFECTE i WORKSHOP_DEFECTE a la secció CONFIGURACIÓ.\n")
  quit(status = 1)
}

if (!file.exists(fitxer)) {
  stop("No trobo el fitxer de participants: ", fitxer,
       "\nDirectori de treball actual: ", getwd())
}

# Llavor: la donada, o una derivada del nom del workshop
if (length(args) >= 3) {
  llavor <- as.integer(args[3])
  if (is.na(llavor)) stop("La llavor ha de ser un nombre enter.")
} else if (!is.null(LLAVOR_DEFECTE)) {
  llavor <- as.integer(LLAVOR_DEFECTE)
} else {
  llavor <- sum(utf8ToInt(workshop) * seq_along(utf8ToInt(workshop))) * 7919
  llavor <- as.integer(llavor %% .Machine$integer.max)
}

# --------------------------- LECTURA ---------------------------------------

llegir_participants <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (ext %in% c("xlsx", "xls")) {
    as.data.frame(readxl::read_excel(path), stringsAsFactors = FALSE)
  } else if (ext %in% c("csv", "txt")) {
    # prova primer amb coma, després amb punt i coma
    d <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
    if (ncol(d) == 1) {
      d <- utils::read.csv2(path, stringsAsFactors = FALSE, check.names = FALSE)
    }
    d
  } else {
    stop("Format no reconegut: .", ext, ". Fes servir .xlsx, .xls o .csv")
  }
}

normalitza <- function(x) {
  x <- tolower(trimws(x))
  x <- iconv(x, to = "ASCII//TRANSLIT")
  gsub("[^a-z0-9_-]", "", x)
}

detecta_columna <- function(dades, fixada, candidats, obligatoria, etiqueta) {
  if (!is.null(fixada)) {
    if (!(fixada %in% names(dades))) {
      stop("La columna '", fixada, "' no és al fitxer. Columnes disponibles: ",
           paste(names(dades), collapse = ", "))
    }
    return(fixada)
  }
  noms <- normalitza(names(dades))
  for (cand in candidats) {
    idx <- which(noms == cand)
    if (length(idx) > 0) return(names(dades)[idx[1]])
  }
  if (obligatoria) {
    stop("No he sabut quina és la columna de ", etiqueta,
         ". Columnes del fitxer: ", paste(names(dades), collapse = ", "),
         "\nFixa-la a mà a la secció CONFIGURACIÓ de l'script.")
  }
  NULL
}

dades <- llegir_participants(fitxer)

col_id     <- detecta_columna(dades, COL_ID,     CAND_ID,     TRUE,  "l'identificador")
col_classe <- detecta_columna(dades, COL_CLASSE, CAND_CLASSE, TRUE,  "el grup classe")
col_nom    <- detecta_columna(dades, COL_NOM,    CAND_NOM,    FALSE, "el nom")

dades[[col_id]]     <- trimws(as.character(dades[[col_id]]))
dades[[col_classe]] <- trimws(as.character(dades[[col_classe]]))

# fora files buides i identificadors repetits
dades <- dades[nzchar(dades[[col_id]]) & !is.na(dades[[col_id]]), , drop = FALSE]
repetits <- dades[[col_id]][duplicated(dades[[col_id]])]
if (length(repetits) > 0) {
  warning("Identificadors repetits al fitxer, em quedo amb la primera aparició: ",
          paste(unique(repetits), collapse = ", "))
  dades <- dades[!duplicated(dades[[col_id]]), , drop = FALSE]
}

if (nrow(dades) == 0) stop("El fitxer de participants no té cap fila utilitzable.")

# --------------------------- ASSIGNACIÓ ------------------------------------

set.seed(llavor)

classes <- sort(unique(dades[[col_classe]]))
contador_grup <- 0L
assignacions <- list()

for (cl in classes) {
  bloc <- dades[dades[[col_classe]] == cl, , drop = FALSE]
  n <- nrow(bloc)

  # ordre aleatori dins de la classe
  bloc <- bloc[sample.int(n), , drop = FALSE]

  n_grups <- n %/% MIDA_GRUP
  n_comodins <- n %% MIDA_GRUP

  etiquetes <- character(n)

  if (n_grups > 0) {
    ids_grup <- contador_grup + seq_len(n_grups)
    contador_grup <- contador_grup + n_grups
    etiquetes[seq_len(n_grups * MIDA_GRUP)] <-
      sprintf("G%02d", rep(ids_grup, each = MIDA_GRUP))
  }
  if (n_comodins > 0) {
    etiquetes[(n - n_comodins + 1):n] <- "COMODI"
  }

  res <- data.frame(
    Workshop  = workshop,
    Classe    = cl,
    Grup      = etiquetes,
    stringsAsFactors = FALSE
  )
  if (!is.null(col_nom)) res$Nom <- as.character(bloc[[col_nom]])
  res$Usuari     <- bloc[[col_id]]
  res$Grup_final <- ifelse(etiquetes == "COMODI", "", etiquetes)

  # els grups primer, els comodins al final
  res <- res[order(res$Grup == "COMODI", res$Grup), , drop = FALSE]
  rownames(res) <- NULL

  assignacions[[cl]] <- res
}

total <- do.call(rbind, assignacions)
rownames(total) <- NULL

# --------------------------- SORTIDA ---------------------------------------

wb <- createWorkbook()

estil_cap <- createStyle(textDecoration = "bold", fgFill = "#1F3864",
                         fontColour = "#FFFFFF", halign = "left",
                         border = "bottom")
estil_com <- createStyle(fgFill = "#FFF2CC")

nom_pestanya <- function(x) {
  x <- gsub("[\\[\\]:*?/\\\\]", "-", x)
  substr(x, 1, 31)
}

for (cl in classes) {
  res <- assignacions[[cl]]
  sh <- nom_pestanya(cl)
  addWorksheet(wb, sh)
  writeData(wb, sh, res, headerStyle = estil_cap)
  files_com <- which(res$Grup == "COMODI") + 1
  if (length(files_com) > 0) {
    addStyle(wb, sh, estil_com, rows = files_com,
             cols = seq_len(ncol(res)), gridExpand = TRUE, stack = TRUE)
  }
  setColWidths(wb, sh, cols = seq_len(ncol(res)), widths = "auto")
  freezePane(wb, sh, firstRow = TRUE)
}

# pestanya amb tot junt, que és la que fa el join amb les notes
addWorksheet(wb, "ASSIGNACIO")
writeData(wb, "ASSIGNACIO", total, headerStyle = estil_cap)
setColWidths(wb, "ASSIGNACIO", cols = seq_len(ncol(total)), widths = "auto")
freezePane(wb, "ASSIGNACIO", firstRow = TRUE)

sortida <- sprintf("grups_%s.xlsx", workshop)
saveWorkbook(wb, sortida, overwrite = TRUE)

# --------------------------- RESUM -----------------------------------------

cat(sprintf("\nWorkshop %s   (llavor %d)\n", workshop, llavor))
cat(strrep("-", 46), "\n")
for (cl in classes) {
  res <- assignacions[[cl]]
  ng <- length(unique(res$Grup[res$Grup != "COMODI"]))
  nc <- sum(res$Grup == "COMODI")
  cat(sprintf("  %-14s %3d persones -> %2d grups + %d comodins\n",
              cl, nrow(res), ng, nc))
}
cat(strrep("-", 46), "\n")
cat(sprintf("  %-14s %3d persones -> %2d grups + %d comodins\n",
            "TOTAL", nrow(total), contador_grup,
            sum(total$Grup == "COMODI")))
cat(sprintf("\nFitxer generat: %s\n\n", sortida))

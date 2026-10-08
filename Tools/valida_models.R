#!/usr/bin/env Rscript

# ---------------------------------------------------------------------------
# valida_models.R
#
# Comprova si les mètriques que ha entregat cada alumne són compatibles amb
# el model de Gretl que ha declarat. L'objectiu no és detectar errors sinó
# prediccions brillants irrastrejables: només aixeca bandera quan el resultat
# entregat és MILLOR del que el model declarat pot donar.
#
# Els models es llegeixen de l'objecte `models` del propi RData, que és on
# els deixa la consolidació. Si el magatzem és anterior a això, cau cap a
# llegir els fitxers .model.txt d'un directori.
#
# No cal el fitxer de test. Gretl reporta la desviació típica de la regressió
# (l'error que el model comet al train); el RMSE al test ha de ser d'aquest
# ordre o una mica superior. Comparem la ràtio:
#
#       ratio = RMSE_entregat / D.T._de_la_regressió
#
#   ratio ~ 1 a 1.5   coherent
#   ratio < RATIO_MIN el resultat és millor del que el model pot donar
#   ratio > RATIO_MAX el model no s'ha aplicat bé (error, no trampa)
#
# Tres estats, i cap conseqüència automàtica: l'script fa una llista per
# revisar, mai treu la nota a ningú.
#
#   VALIDAT         ratio dins de l'interval i resposta sense transformar
#   NO VALIDAT      ratio per sota del mínim: a revisar
#   NO VALIDABLE    sense model, no es pot parsejar, o la variable dependent
#                   està transformada (log(y), y^2...) i la D.T. de la
#                   regressió ja no és comparable amb el RMSE en escala
#                   original
#
# Ús, des del directori de l'app:
#   Rscript tools/valida_models.R
#   Rscript tools/valida_models.R W02.RData AdD/processed medv
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
})

# --------------------------- CONFIGURACIÓ ----------------------------------

RDATA_DEFECTE   <- "W02.RData"
MODELS_DEFECTE  <- "AdD/processed"   # només si el RData no porta els models
RESPOSTA        <- "medv"            # variable dependent esperada

RATIO_MIN <- 0.80   # per sota: millor del que el model pot donar
RATIO_MAX <- 3.00   # per sobre: model mal aplicat

TZ        <- "Europe/Madrid"
EXCLOU    <- c("test", "francesc.martori@iqs.url.edu")
SORTIDA   <- "validacio_models.csv"

# ---------------------------------------------------------------------------

args    <- commandArgs(trailingOnly = TRUE)
rdata   <- if (length(args) >= 1) args[1] else RDATA_DEFECTE
dir_mod <- if (length(args) >= 2) args[2] else MODELS_DEFECTE
resposta<- if (length(args) >= 3) args[3] else RESPOSTA

if (!file.exists(rdata)) stop("No trobo el RData: ", rdata)

say <- function(...) cat(..., "\n", sep = "")

# --------------------------- 1. INTENTS ------------------------------------

resultats <- NULL
models    <- data.frame()
load(rdata)                       # resultats, i potser models
if (!is.data.frame(models)) models <- data.frame()

if (is.character(resultats$time)) {
  t_utc <- as.POSIXct(resultats$time, tz = "UTC",
                      tryFormats = c("%Y-%m-%dT%H:%M:%OSZ",
                                     "%Y-%m-%d %H:%M:%OS",
                                     "%Y-%m-%d %H:%M:%S"))
} else {
  t_utc <- as.POSIXct(resultats$time, tz = "UTC")
}

intents <- resultats %>%
  mutate(user  = tolower(trimws(user)),
         t_utc = t_utc,
         # Els fitxers es van anomenar amb l'hora UTC del servidor, amb
         # mil·lisegons; per aparellar n'hi ha prou amb el segon.
         clau  = paste0(user, "|", format(t_utc, "%Y%m%d-%H%M%S", tz = "UTC")),
         hora  = format(t_utc, "%d/%m %H:%M:%S", tz = TZ)) %>%
  filter(!user %in% tolower(EXCLOU))

say("Intents al magatzem: ", nrow(intents), "  (", n_distinct(intents$user),
    " alumnes)")

# --------------------------- 2. MODELS -------------------------------------

# Parseja una sortida de model de Gretl en castellà.
parseja_gretl <- function(txt) {
  g1 <- function(p) {
    m <- regmatches(txt, regexpr(p, txt, perl = TRUE))
    if (length(m) == 0) return(NA_character_)
    sub(p, "\\1", m, perl = TRUE)
  }
  dep   <- g1("Variable dependiente:\\s*(\\S+)")
  dt_r  <- g1("D\\.T\\. de la regresi[oó]n\\s+([0-9.]+)")
  dt_y  <- g1("D\\.T\\. de la vble\\. dep\\.\\s+([0-9.]+)")
  r2    <- g1("R-cuadrado\\s+([0-9.]+)")
  nobs  <- g1("observaciones\\s+[0-9]+-([0-9]+)")

  # Files de coeficients: comencen amb exactament dos espais, nom i número.
  # El signe menys de Gretl és U+2212, no un guió ASCII.
  linies <- strsplit(txt, "\n", fixed = TRUE)[[1]]
  pat <- "^  (\\S+)\\s+(−?-?[0-9.]+(?:[eE][+-]?[0-9]+)?)(\\s|$)"
  hit <- grep(pat, linies, perl = TRUE)
  noms <- sub(pat, "\\1", linies[hit], perl = TRUE)
  vals <- as.numeric(gsub("−", "-", sub(pat, "\\2", linies[hit], perl = TRUE)))

  regs <- noms[noms != "const"]

  list(dep = dep,
       regs = if (length(regs)) paste(regs, collapse = "+") else NA_character_,
       n_regs = length(regs),
       coefs = if (length(vals)) paste(round(vals, 4), collapse = "; ") else NA_character_,
       dt_reg = as.numeric(dt_r),
       dt_dep = as.numeric(dt_y),
       r2 = as.numeric(r2),
       n_obs = as.numeric(nobs))
}

te_id <- "attempt_id" %in% names(intents)

if (nrow(models) > 0 && "model_text" %in% names(models)) {
  say("Models dins del RData: ", nrow(models))
  crus <- models %>%
    transmute(clau_id = attempt_id, text = model_text)
} else {
  if (!dir.exists(dir_mod)) {
    stop("El RData no porta models i no trobo el directori: ", dir_mod)
  }
  fitxers <- list.files(dir_mod, pattern = "\\.model\\.txt$",
                        full.names = TRUE)
  say("El RData no porta models; llegits de ", dir_mod, ": ",
      length(fitxers), " fitxers")
  if (length(fitxers) == 0) {
    say("\nNo hi ha cap model enganxat. Res a validar.")
    quit(status = 0)
  }
  crus <- data.frame(
    clau_id = sub("\\.model\\.txt$", "", basename(fitxers)),
    text = vapply(fitxers, function(f)
      paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "\n"),
      character(1), USE.NAMES = FALSE),
    stringsAsFactors = FALSE
  )
}

models_p <- lapply(seq_len(nrow(crus)), function(i) {
  p <- tryCatch(parseja_gretl(crus$text[i]), error = function(e) NULL)
  if (is.null(p)) p <- list(dep = NA_character_, regs = NA_character_,
                            n_regs = 0, coefs = NA_character_,
                            dt_reg = NA_real_, dt_dep = NA_real_,
                            r2 = NA_real_, n_obs = NA_real_)
  id <- crus$clau_id[i]
  data.frame(clau_id = id,
             # Clau alternativa, per a magatzems sense attempt_id:
             # <user>-<AAAAMMDD>-<HHMMSS> a partir del nom base.
             clau_ts = paste0(tolower(sub("^(.*?)-[0-9]{8}-[0-9]{6}\\..*$", "\\1", id)),
                              "|",
                              sub("^.*?-([0-9]{8}-[0-9]{6})\\..*$", "\\1", id)),
             p, stringsAsFactors = FALSE)
}) %>% bind_rows()

# --------------------------- 3. APARELLAMENT -------------------------------

if (te_id) {
  say("Aparellament per attempt_id")
  v <- intents %>% left_join(models_p, by = c("attempt_id" = "clau_id"))
} else {
  say("Aparellament per usuari i marca de temps (magatzem sense attempt_id)")
  v <- intents %>% left_join(models_p %>% select(-clau_id),
                             by = c("clau" = "clau_ts"))
}
v <- v %>%
  mutate(
    ratio = ifelse(!is.na(dt_reg) & dt_reg > 0, RMSE / dt_reg, NA_real_),
    estat = case_when(
      is.na(dep) & is.na(dt_reg)          ~ "NO VALIDABLE: sense model",
      is.na(dep) | is.na(dt_reg)          ~ "NO VALIDABLE: no es pot parsejar",
      dep != resposta                     ~ "NO VALIDABLE: resposta transformada",
      ratio < RATIO_MIN                   ~ "NO VALIDAT: millor que el model",
      ratio > RATIO_MAX                   ~ "NO VALIDAT: model mal aplicat",
      TRUE                                ~ "VALIDAT"
    )
  )

sense <- sum(is.na(v$dep) & is.na(v$dt_reg))
if (sense > 0) say("Intents sense model aparellat: ", sense)

# --------------------------- 4. INFORME -----------------------------------

say("\n", strrep("=", 72))
say("ESTAT DELS INTENTS")
say(strrep("=", 72))
print(as.data.frame(table(v$estat)), row.names = FALSE)

say("\n", strrep("-", 72))
say("A REVISAR: el resultat és millor del que el model declarat pot donar")
say(strrep("-", 72))
sosp <- v %>% filter(grepl("millor que el model", estat)) %>%
  arrange(ratio)
if (nrow(sosp) == 0) {
  say("  cap")
} else {
  sosp %>%
    transmute(user, hora, model = regs, dt_reg = round(dt_reg, 3),
              RMSE = round(RMSE, 3), ratio = round(ratio, 3)) %>%
    as.data.frame() %>% print(row.names = FALSE)
}

say("\n", strrep("-", 72))
say("NO VALIDABLES (variable dependent transformada: revisió manual)")
say(strrep("-", 72))
nv <- v %>% filter(grepl("resposta transformada", estat))
if (nrow(nv) == 0) {
  say("  cap")
} else {
  nv %>% count(dep, regs, name = "intents") %>%
    as.data.frame() %>% print(row.names = FALSE)
}

say("\n", strrep("-", 72))
say("DISTRIBUCIÓ DE LA RÀTIO (només resposta sense transformar)")
say(strrep("-", 72))
r <- v %>% filter(dep == resposta, !is.na(ratio)) %>% pull(ratio)
if (length(r) > 0) {
  print(round(quantile(r, c(0, .05, .25, .5, .75, .95, 1), na.rm = TRUE), 3))
  say("\nCalibra RATIO_MIN mirant aquests valors: els models ben aplicats",
      " s'agrupen a prop d'1.")
}

say("\n", strrep("-", 72))
say("ESPECIFICACIONS DECLARADES (què han provat)")
say(strrep("-", 72))
v %>% filter(!is.na(regs)) %>% count(dep, regs, name = "intents") %>%
  arrange(desc(intents)) %>% as.data.frame() %>% print(row.names = FALSE)

# --------------------------- 5. SORTIDA -----------------------------------

out <- v %>%
  select(user, hora, any_of(c("attempt_id", "RMSE", "MAE", "has_evidence")),
         dep, regs, coefs, dt_reg, r2, n_obs, ratio, estat) %>%
  arrange(user, hora)

write.table(out, SORTIDA, sep = "\t", dec = ".", row.names = FALSE,
            quote = FALSE, na = "")
say("\nEscrit: ", SORTIDA, "  (", nrow(out), " files)")
say("Cap nota s'ha modificat: això és una llista per revisar.")

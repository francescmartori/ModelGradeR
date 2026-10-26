#!/usr/bin/env Rscript

# ---------------------------------------------------------------------------
# valida_models.R
#
# Una sola pregunta: el RMSE entregat encaixa amb el model de Gretl declarat?
#
# Reconstrueix el model enganxat, l'ajusta sobre el train, prediu el test i
# compara. Les variables fabricades tenen noms arbitraris (Budget2,
# cuadradoScreens, arrelbudget), i s'identifiquen per empremta digital: els
# coeficients i la desviacio tipica que reporta Gretl nomes els reprodueix
# una combinacio de transformacions.
#
# Cinc estats, i cap consequencia automatica:
#   VALIDAT               el model reconstruit dona el RMSE entregat
#   MILLOR QUE EL MODEL   l'entrega es millor del que el model pot donar
#   PITJOR QUE EL MODEL   l'entrega es pitjor; sol ser que han enganxat un
#                         model i han entregat les prediccions d'un altre
#   ENTREGA TRENCADA      el RMSE entregat es absurd; error de l'alumne
#   NO RECONSTRUIBLE      l'script no ha pogut identificar el model
#
# L'unica llista que demana atencio es MILLOR QUE EL MODEL.
#
# Us, des del directori de l'app:
#   Rscript tools/valida_models.R W04.RData W4_Train.xlsx W4_Test_Solucions.xlsx
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({ library(dplyr); library(readxl) })

# --------------------------- CONFIGURACIO ----------------------------------

RDATA_DEFECTE <- "W04.RData"
TRAIN_DEFECTE <- "W4_Train.xlsx"
TEST_DEFECTE  <- "W4_Test_Solucions.xlsx"
RESPOSTA      <- "Gross"

RENOMS <- c(MovieScore = "Ratings")   # columnes que es diuen diferent al test

TOL_COEF  <- 5e-4   # coincidencia relativa amb els coeficients de Gretl
TOL_RMSE  <- 2e-3   # diferencia relativa per considerar que encaixa
ABSURD    <- 3      # RMSE entregat per sobre d'aixo x el de predir la mitjana
MAX_FITS  <- 40000  # regressions provades com a maxim per model

# TRUE: per validar han d'encaixar el RMSE i el MAE. Amb el RMSE sol, dos
# models diferents es confonen en el 2,4% de les parelles; amb els dos
# alhora, en el 0,7%.
EXIGEIX_MAE <- TRUE

TZ      <- "Europe/Madrid"
EXCLOU  <- c("test", "francesc.martori@iqs.url.edu")
SORTIDA <- "validacio_models.csv"

TRANSF <- list(
  "x"        = function(v) v,
  "x^2"      = function(v) v^2,
  "x^3"      = function(v) v^3,
  "sqrt(x)"  = function(v) sqrt(pmax(v, 0)),
  "log(x)"   = function(v) ifelse(v > 0, log(pmax(v, 1e-300)), NA_real_),
  "log(1+x)" = function(v) log1p(pmax(v, 0)),
  "1/x"      = function(v) ifelse(v != 0, 1 / ifelse(v == 0, 1, v), NA_real_),
  "x>1"      = function(v) as.numeric(v > 1),
  "x>0"      = function(v) as.numeric(v > 0)
)

RESP_T <- list(
  list(nom = "y",        f = function(y) y,        inv = function(p) p),
  list(nom = "sqrt(y)",  f = function(y) sqrt(y),  inv = function(p) pmax(p, 0)^2),
  list(nom = "log(y)",   f = function(y) log(y),   inv = function(p) exp(p)),
  list(nom = "log(1+y)", f = function(y) log1p(y), inv = function(p) expm1(p)),
  list(nom = "y^2",      f = function(y) y^2,      inv = function(p) sqrt(pmax(p, 0)))
)

# Pistes del nom cap a la transformacio. L'ordre importa: es prova primer la
# que suggereix el nom, i nomes despres la resta.
PISTES <- list(
  c("cuadrad|quadrat|square|\\^2|(?<![0-9])2(?![0-9])|^sq_|_sq$", "x^2"),
  c("cubo|cub|\\^3|(?<![0-9])3(?![0-9])",                        "x^3"),
  c("arrel|raiz|raiç|sqrt|^rt|^r(?=[A-Z])|^r_",             "sqrt(x)"),
  c("^l_|log|^ln",                                               "log(x)"),
  c("inv|^1_|^un_",                                              "1/x"),
  c("_d$|^d_|dumm",                                              "x>1")
)

# ---------------------------------------------------------------------------

args  <- commandArgs(trailingOnly = TRUE)
rdata <- if (length(args) >= 1) args[1] else RDATA_DEFECTE
ftr   <- if (length(args) >= 2) args[2] else TRAIN_DEFECTE
fte   <- if (length(args) >= 3) args[3] else TEST_DEFECTE
for (f in c(rdata, ftr, fte))
  if (!file.exists(f)) stop("No trobo el fitxer: ", f)

say <- function(...) cat(..., "\n", sep = "")

llegeix <- function(f) {
  d <- if (grepl("\\.csv$", f, ignore.case = TRUE)) read.csv(f) else
    as.data.frame(read_excel(f))
  for (vell in names(RENOMS))
    if (vell %in% names(d)) names(d)[names(d) == vell] <- RENOMS[[vell]]
  d
}

train <- llegeix(ftr); test <- llegeix(fte)
if (!RESPOSTA %in% names(train) || !RESPOSTA %in% names(test))
  stop("Falta la columna ", RESPOSTA, " al train o al test")

VARIABLES <- names(train)[vapply(train, is.numeric, logical(1))]
VARIABLES <- setdiff(VARIABLES, c(RESPOSTA, "id"))
VARIABLES <- intersect(VARIABLES, names(test))

# Totes les columnes transformades, calculades un sol cop.
COL_TR <- list(); COL_TE <- list()
for (b in VARIABLES) for (tn in names(TRANSF)) {
  k <- paste0(b, "|", tn)
  COL_TR[[k]] <- suppressWarnings(as.numeric(TRANSF[[tn]](train[[b]])))
  COL_TE[[k]] <- suppressWarnings(as.numeric(TRANSF[[tn]](test[[b]])))
}
YT <- lapply(RESP_T, function(rt) suppressWarnings(as.numeric(rt$f(train[[RESPOSTA]]))))
names(YT) <- vapply(RESP_T, function(rt) rt$nom, character(1))

# RMSE de predir la mitjana del train: la referencia per detectar entregues
# trencades.
RMSE_NUL <- sqrt(mean((test[[RESPOSTA]] - mean(train[[RESPOSTA]]))^2))

# --------------------------- 1. INTENTS ------------------------------------

resultats <- NULL; models <- data.frame()
load(rdata)
if (!is.data.frame(models) || nrow(models) == 0)
  stop("El magatzem no porta l'objecte `models` amb els textos.")

t_utc <- if (is.character(resultats$time)) {
  as.POSIXct(resultats$time, tz = "UTC",
             tryFormats = c("%Y-%m-%dT%H:%M:%OSZ", "%Y-%m-%d %H:%M:%OS",
                            "%Y-%m-%d %H:%M:%S"))
} else as.POSIXct(resultats$time, tz = "UTC")

intents <- resultats %>%
  mutate(user = tolower(trimws(user)), t_utc = t_utc,
         hora = format(t_utc, "%d/%m %H:%M:%S", tz = TZ)) %>%
  filter(!user %in% tolower(EXCLOU))

say("Intents: ", nrow(intents), "   alumnes: ", n_distinct(intents$user),
    "   models enganxats: ", nrow(models))

# --------------------------- 2. PARSEIG ------------------------------------

# El text sol arribar amb la codificacio trencada (UTF-8 llegit com a CP1252):
# "regresion" com a "regresiA3n" i el menys U+2212 com a tres caracters. Les
# expressions toleren qualsevol cosa al lloc dels accents.
# Gretl tambe pot copiar el model en LaTeX, i tres alumnes ho van fer. Alla
# els numeros van partits en dues columnes amb & com a separador decimal
# (0&343999 vol dir 0.343999), el menys es $-$ i cada coeficient ocupa cinc
# linies. Es converteix al format de text pla abans de parsejar.
desllatexa <- function(t) {
  if (!grepl("\\begin{tabular}", t, fixed = TRUE)) return(t)
  t <- gsub("$-$", "-", t, fixed = TRUE)
  t <- gsub("\\_", "_", t, fixed = TRUE)
  t <- gsub("\\\\textrm\\{([^}]*)\\}", "\\1", t, perl = TRUE)
  t <- gsub("$", "", t, fixed = TRUE)
  t <- gsub("\\\\(vspace|hline|smallskip|medskip)\\s*(\\{[^}]*\\})?", "", t, perl = TRUE)
  t <- gsub("\\\\multicolumn\\{[^}]*\\}\\{[^}]*\\}", "", t, perl = TRUE)
  t <- gsub("--", "-", t, fixed = TRUE)

  i1 <- regexpr("\\begin{tabular}", t, fixed = TRUE)
  i2 <- regexpr("\\end{tabular}",   t, fixed = TRUE)
  if (i1 < 0 || i2 < 0 || i2 < i1) return(t)
  coef  <- substr(t, i1 + attr(i1, "match.length"), i2 - 1)
  resta <- paste0(substr(t, 1, i1 - 1),
                  substr(t, i2 + attr(i2, "match.length"), nchar(t)))

  numfix <- function(s) gsub("([0-9])&([0-9])", "\\1.\\2", s, perl = TRUE)
  files <- strsplit(numfix(coef), "\\\\", fixed = TRUE)[[1]]
  files <- gsub("\\[[0-9]+ex\\]", "", files, perl = TRUE)
  files <- gsub("&", "   ", files, fixed = TRUE)
  files <- trimws(gsub("[ \t\r\n]+", " ", files))
  files <- paste0("  ", files[nzchar(files)])

  resta <- gsub("\\\\(begin|end)\\{[^}]*\\}", "", resta, perl = TRUE)
  resta <- gsub("&", "   ", numfix(resta), fixed = TRUE)
  resta <- gsub("\\\\", "\n", resta, fixed = TRUE)
  paste0(resta, "\n", paste(files, collapse = "\n"))
}

parseja <- function(txt) {
  txt <- desllatexa(txt)
  txt <- gsub("−|âˆ’|âˆ−", "-", txt)
  txt <- gsub("\\\\", "", txt)
  g1 <- function(p) {
    m <- regmatches(txt, regexpr(p, txt, perl = TRUE))
    if (length(m) == 0) return(NA_character_)
    sub(p, "\\1", m, perl = TRUE)
  }
  linies <- strsplit(txt, "\r\n|\n|\r", perl = TRUE)[[1]]
  pat <- "^\\s{2,}(\\S+)\\s+(-?[0-9.]+(?:[eE][+-]?[0-9]+)?)(\\s|$)"
  # regexec i no regexpr: cal extreure els grups, i sub() nomes substitueix
  # la part que casa i deixa la resta de la linia enganxada.
  mm  <- regmatches(linies, regexec(pat, linies, perl = TRUE))
  hit <- which(vapply(mm, length, integer(1)) > 0)
  # Gretl escriu "observaciones 1-179 (n = 178)" quan descarta files: el que
  # val es el (n = ...), no el rang.
  nobs <- g1("\\(n\\s*=\\s*([0-9]+)\\)")
  if (is.na(nobs)) nobs <- g1("observaciones\\s+[0-9]+-([0-9]+)")
  list(dep     = g1("Variable dependiente:\\s*(\\S+)"),
       mitjana = as.numeric(g1("Media de la vble\\. dep\\.\\s+([0-9.]+)")),
       dt_reg  = as.numeric(g1("D\\.T\\. de la regresi.{0,3}n\\s+([0-9.]+)")),
       nobs    = as.numeric(nobs),
       noms    = if (length(hit)) vapply(mm[hit], function(x) x[2], character(1))
                 else character(0),
       coefs   = if (length(hit)) as.numeric(vapply(mm[hit], function(x) x[3],
                                                    character(1)))
                 else numeric(0))
}

# --------------------------- 3. CANDIDATS ----------------------------------

transf_cands <- function(nom) {
  out <- character(0)
  for (p in PISTES)
    if (grepl(p[1], nom, perl = TRUE, ignore.case = TRUE)) out <- c(out, p[2])
  out <- unique(out)
  if (length(out) == 0) out <- "x"
  c(out, setdiff(names(TRANSF), out))
}

# Variables originals compatibles amb un nom fabricat. Generos a proposit:
# un candidat erroni simplement no passara la prova dels coeficients.
base_cands <- function(nom) {
  net <- gsub("[^a-z]", "", tolower(nom))
  nut <- net
  for (p in PISTES) nut <- gsub(p[1], "", nut, perl = TRUE, ignore.case = TRUE)
  nut <- gsub("[^a-z]", "", nut)
  sc <- vapply(VARIABLES, function(v) {
    vl <- tolower(v)
    d1 <- as.numeric(adist(nut, vl)) / max(nchar(nut), nchar(vl), 1)
    d2 <- as.numeric(adist(net, vl)) / max(nchar(net), nchar(vl), 1)
    s  <- 1 - min(d1, d2)
    if (nchar(nut) >= 3 && (grepl(substr(vl, 1, 3), nut, fixed = TRUE) ||
                            grepl(substr(nut, 1, 3), vl, fixed = TRUE)))
      s <- max(s, 0.75)
    s
  }, numeric(1))
  ordre <- VARIABLES[order(sc, decreasing = TRUE)]
  bons  <- ordre[sort(sc, decreasing = TRUE) >= 0.5]
  if (length(bons) == 0) bons <- head(ordre, 3)
  c(head(bons, 4), setdiff(ordre, head(bons, 4)))
}

# --------------------------- 4. IDENTIFICACIO ------------------------------

identifica <- function(p) {
  regs <- p$noms[p$noms != "const"]
  if (length(regs) == 0 || length(p$coefs) != length(regs) + 1) return(NULL)
  gc <- p$coefs; k <- length(gc)
  if (any(!is.finite(gc))) return(NULL)   # coeficient no llegit: no s'intenta

  # La resposta, per la mitjana que reporta Gretl. Es compara sobre tot el
  # train amb tolerancia ampla: les transformacions difereixen en ordres de
  # magnitud, aixi que unes quantes files de mes o de menys no enganyen.
  cands <- RESP_T
  if (!is.na(p$mitjana)) {
    for (rt in RESP_T) {
      m <- suppressWarnings(mean(YT[[rt$nom]], na.rm = TRUE))
      if (is.finite(m) && abs(m - p$mitjana) <= abs(p$mitjana) * 0.02) {
        cands <- list(rt); break
      }
    }
  }

  # Opcions per regressor, ordenades per plausibilitat (millor base i millor
  # transformacio primer) i truncades perque el producte no passi de
  # MAX_FITS. Sense truncar, quatre regressors amb onze variables i nou
  # transformacions donen 96 milions de combinacions.
  per <- max(2L, as.integer(floor(MAX_FITS^(1 / length(regs)))))
  opc <- lapply(regs, function(r) {
    bs <- base_cands(r); ts <- transf_cands(r)
    g <- expand.grid(ib = seq_along(bs), it = seq_along(ts))
    g <- g[order(g$ib + g$it, g$ib, g$it), , drop = FALSE]
    g <- head(g, per)
    cbind(b = bs[g$ib], t = ts[g$it])
  })
  graella <- as.matrix(expand.grid(lapply(opc, function(o) seq_len(nrow(o)))))

  for (rt in cands) {
    yt <- YT[[rt$nom]]
    for (i in seq_len(nrow(graella))) {
      claus <- vapply(seq_along(regs), function(j) {
        o <- opc[[j]][graella[i, j], ]
        paste0(o[["b"]], "|", o[["t"]])
      }, character(1))
      cols <- COL_TR[claus]
      ok <- is.finite(yt)
      for (cc in cols) ok <- ok & is.finite(cc)
      nn <- sum(ok)
      if (nn < k + 2) next
      if (!is.na(p$nobs) && nn != p$nobs) next
      X <- cbind(1, do.call(cbind, lapply(cols, function(cc) cc[ok])))
      b <- tryCatch(qr.solve(X, yt[ok]), error = function(e) NULL)
      if (is.null(b) || length(b) != k) next
      if (!all(abs(b - gc) <= abs(gc) * TOL_COEF + 1e-10)) next
      e  <- yt[ok] - X %*% b
      dt <- sqrt(sum(e^2) / (nn - k))
      if (!is.na(p$dt_reg) && abs(dt - p$dt_reg) > p$dt_reg * TOL_COEF) next
      return(list(rt = rt, claus = claus, coef = as.numeric(b), n = nn))
    }
  }
  NULL
}

avalua <- function(idf) {
  Z  <- cbind(1, do.call(cbind, COL_TE[idf$claus]))
  pr <- idf$rt$inv(as.numeric(Z %*% idf$coef))
  y  <- test[[RESPOSTA]]
  ok <- is.finite(pr) & is.finite(y)
  if (sum(ok) == 0) return(c(rmse = NA_real_, mae = NA_real_))
  c(rmse = sqrt(mean((y[ok] - pr[ok])^2)), mae = mean(abs(y[ok] - pr[ok])))
}

descriu <- function(idf) {
  parts <- vapply(idf$claus, function(k) {
    p <- strsplit(k, "|", fixed = TRUE)[[1]]
    sub("x", p[1], p[2], fixed = TRUE)
  }, character(1))
  paste0(idf$rt$nom, " ~ ", paste(parts, collapse = " + "))
}

# --------------------------- 5. VALIDACIO ----------------------------------

# Control del parseig abans de res: si aixo falla, tota la resta es inutil i
# val mes veure-ho aqui que no pas rebre 264 "no reconstruible" sense motiu.
ctrl <- lapply(models$model_text, function(t)
  tryCatch(parseja(t), error = function(e) NULL))
say("\nParseig dels ", length(ctrl), " textos enganxats:")
say("  amb taula de coeficients llegida: ",
    sum(vapply(ctrl, function(p) !is.null(p) && length(p$coefs) > 0 &&
                 all(is.finite(p$coefs)), logical(1))))
say("  amb D.T. de la regressio:         ",
    sum(vapply(ctrl, function(p) !is.null(p) && !is.na(p$dt_reg), logical(1))))
say("  amb nombre d'observacions:        ",
    sum(vapply(ctrl, function(p) !is.null(p) && !is.na(p$nobs), logical(1))))
say("  sense cap model a dins:           ",
    sum(vapply(ctrl, function(p) is.null(p) || length(p$coefs) == 0, logical(1))))

say("\nReconstruint models...")
cache <- new.env(hash = TRUE, parent = emptyenv())
files <- lapply(seq_len(nrow(models)), function(i) {
  txt <- models$model_text[i]
  # Molts intents comparteixen el mateix text: cada text s'identifica un cop.
  clau <- paste0(nchar(txt), "|", substr(txt, 1, 300))
  if (exists(clau, envir = cache, inherits = FALSE)) {
    r <- get(clau, envir = cache)
  } else {
    p   <- tryCatch(parseja(txt), error = function(e) NULL)
    idf <- if (is.null(p)) NULL else tryCatch(identifica(p), error = function(e) NULL)
    r <- if (is.null(idf)) list(esp = NA_character_, rmse = NA_real_, mae = NA_real_)
         else { m <- avalua(idf)
                list(esp = descriu(idf), rmse = m[["rmse"]], mae = m[["mae"]]) }
    assign(clau, r, envir = cache)
  }
  data.frame(attempt_id = models$attempt_id[i], especificacio = r$esp,
             rmse_model = r$rmse, mae_model = r$mae, stringsAsFactors = FALSE)
}) %>% bind_rows()

v <- intents %>%
  left_join(files, by = "attempt_id") %>%
  mutate(
    desvia  = abs(rmse_model - RMSE) / RMSE,
    desv_mae = abs(mae_model - MAE) / MAE,
    estat = case_when(
      is.na(rmse_model)         ~ "NO RECONSTRUIBLE",
      RMSE > ABSURD * RMSE_NUL  ~ "ENTREGA TRENCADA",
      desvia <= TOL_RMSE &
        (!EXIGEIX_MAE | desv_mae <= TOL_RMSE) ~ "VALIDAT",
      desvia <= TOL_RMSE        ~ "NOMES ENCAIXA EL RMSE",
      RMSE < rmse_model         ~ "MILLOR QUE EL MODEL",
      TRUE                      ~ "PITJOR QUE EL MODEL"
    )
  )

# --------------------------- 6. INFORME ------------------------------------

say("\n", strrep("=", 72))
say("EL RMSE ENTREGAT ENCAIXA AMB EL MODEL DECLARAT?")
say(strrep("=", 72))
print(as.data.frame(table(v$estat)), row.names = FALSE)
say("\n  (RMSE de predir la mitjana del train: ", round(RMSE_NUL), ")")

say("\n", strrep("-", 72))
say("MILLOR QUE EL MODEL: l'unica llista que demana atencio")
say(strrep("-", 72))
m <- v %>% filter(estat == "MILLOR QUE EL MODEL") %>% arrange(desc(desvia))
if (nrow(m) == 0) say("  cap") else
  m %>% transmute(user, hora, especificacio,
                  entregat = round(RMSE, 1), del_model = round(rmse_model, 1),
                  diferencia = paste0(round(100 * desvia, 1), "%")) %>%
  as.data.frame() %>% print(row.names = FALSE)

say("\n", strrep("-", 72))
say("NOMES ENCAIXA EL RMSE: el MAE diu que les prediccions no son les mateixes")
say(strrep("-", 72))
sr <- v %>% filter(estat == "NOMES ENCAIXA EL RMSE") %>% arrange(desc(desv_mae))
if (nrow(sr) == 0) say("  cap") else
  sr %>% transmute(user, hora, especificacio,
                   dif_rmse = paste0(round(100 * desvia, 2), "%"),
                   dif_mae  = paste0(round(100 * desv_mae, 2), "%")) %>%
  as.data.frame() %>% print(row.names = FALSE)

say("\n", strrep("-", 72))
say("ENTREGA TRENCADA: prediccions absurdes, error de l'alumne")
say(strrep("-", 72))
tr <- v %>% filter(estat == "ENTREGA TRENCADA")
if (nrow(tr) == 0) say("  cap") else
  say("  ", nrow(tr), " intents de ", n_distinct(tr$user), " alumnes")

say("\n", strrep("-", 72))
say("PITJOR QUE EL MODEL: solen haver enganxat un model i entregat un altre")
say(strrep("-", 72))
pj <- v %>% filter(estat == "PITJOR QUE EL MODEL")
say("  ", nrow(pj), " intents de ", n_distinct(pj$user), " alumnes")

say("\n", strrep("-", 72))
say("ESPECIFICACIONS IDENTIFICADES")
say(strrep("-", 72))
v %>% filter(!is.na(especificacio)) %>% count(especificacio, name = "intents") %>%
  arrange(desc(intents)) %>% head(20) %>% as.data.frame() %>% print(row.names = FALSE)

# --------------------------- 7. SORTIDA ------------------------------------

out <- v %>%
  select(user, hora, any_of(c("attempt_id", "RMSE", "MAE", "has_evidence")),
         especificacio, rmse_model, mae_model, desvia, desv_mae, estat) %>%
  arrange(user, hora)
write.table(out, SORTIDA, sep = "\t", dec = ".", row.names = FALSE,
            quote = FALSE, na = "")
say("\nEscrit: ", SORTIDA, "  (", nrow(out), " files)")
say("Cap nota s'ha modificat.")
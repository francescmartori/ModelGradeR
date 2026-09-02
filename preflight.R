# preflight.R -- pre-deployment checks for the Workshop Corrector app.
#
# Run this ON THE SERVER, FROM THE APP DIRECTORY, ideally as the same
# user that runs Shiny Server (usually 'shiny'):
#
#   cd /srv/shiny-server/your-app
#   sudo -u shiny Rscript preflight.R
#
# Running it as your own user checks the code; running it as the shiny
# user additionally checks permissions, which is where production
# deployments usually break.
#
# Every check prints [OK] or [FAIL]; the script exits with a non-zero
# status if anything failed, so it can be used in deployment scripts.

fails <- 0
ok   <- function(msg) cat("[OK]   ", msg, "\n")
fail <- function(msg) { cat("[FAIL] ", msg, "\n"); fails <<- fails + 1 }

cat("== Preflight checks ==\n")
cat("R version:", R.version.string, "\n")
cat("Running as user:", Sys.info()[["user"]], "\n")
cat("Working directory:", getwd(), "\n\n")

# ----------------------------------------------------------------
# 1. Stale binary packages (built under a different R version).
#    This is the SETLENGTH / SET_GROWABLE_BIT class of error.
#    Only packages in the APP'S recursive dependency closure cause a
#    FAIL; other stale packages on the server are reported as
#    informational (they cannot break this app, but will break
#    whatever does use them).
# ----------------------------------------------------------------
cat("-- 1. Package binary compatibility (app dependency closure) --\n")
cat("Library paths (R loads the FIRST copy found, in this order):\n")
for (i in seq_along(.libPaths())) cat("  ", i, ". ", .libPaths()[i], "\n", sep = "")

deps <- c("shiny", "shinyjs", "readr", "dplyr", "tidyr", "purrr",
          "tibble", "ggplot2", "readxl", "shinyalert", "yaml",
          "ragg")   # not a declared dep, but Shiny prefers it for
                    # plot rendering when installed, pulling in
                    # systemfonts/textshaping at runtime

# All installed copies, in libPaths order; the EFFECTIVE copy of each
# package (the one R actually loads) is the first occurrence.
ip_all <- do.call(rbind, lapply(.libPaths(), function(lib) {
  ip <- installed.packages(lib.loc = lib)
  if (nrow(ip) > 0) ip else NULL
}))
eff <- ip_all[!duplicated(ip_all[, "Package"]), , drop = FALSE]

closure <- unique(c(deps, unlist(
  tools::package_dependencies(deps, db = eff, recursive = TRUE,
                              which = c("Depends", "Imports", "LinkingTo"))
)))

current_rv <- paste(R.version$major,
                    strsplit(R.version$minor, "\\.")[[1]][1], sep = ".")
# The Built field may be just "4.5.2" or the full string
# "R 4.6.1; x86_64-pc-linux-gnu; <date>; unix" depending on how the
# package metadata was written. Extract the first x.y version number.
built_mm <- vapply(eff[, "Built"], function(b) {
  v <- regmatches(b, regexpr("[0-9]+\\.[0-9]+", b))
  if (length(v) == 0) "" else v
}, character(1))

stale_eff  <- eff[built_mm != current_rv, , drop = FALSE]
stale_app  <- intersect(stale_eff[, "Package"], closure)

# Base packages ship with R itself and can NEVER legitimately be stale
# for the running R. If they appear here, .libPaths() is mixing in the
# library of ANOTHER R installation ahead of this R's own library:
# that is a configuration problem (R_LIBS_SITE / Renviron), not
# something install.packages() can fix.
is_base    <- stale_eff[, "Package"] %in%
  rownames(installed.packages(priority = "base"))
base_stale <- intersect(stale_eff[is_base, "Package"], closure)
cran_stale <- setdiff(stale_app, base_stale)

if (length(base_stale) > 0) {
  fail(paste0("BASE packages resolve to another R installation's ",
              "library: ", paste(base_stale, collapse = ", ")))
  cat("       This means .libPaths() puts a different R's library ",
      "before this R's own.\n       Fix the R_LIBS_SITE / Renviron ",
      "configuration; do NOT try to install these.\n", sep = "")
}
if (length(cran_stale) > 0) {
  fail(paste0("App dependencies whose loaded copy was built under ",
              "another R version: ", paste(cran_stale, collapse = ", ")))
  cat("       Fix: sudo Rscript -e 'install.packages(c(",
      paste0('\"', cran_stale, '\"', collapse = ", "),
      "), lib = .libPaths()[1])'\n", sep = "")
}
if (length(base_stale) == 0 && length(cran_stale) == 0) {
  ok(paste0("All ", length(closure),
            " packages in the app's dependency closure resolve to ",
            "copies built under R ", current_rv))
}

stale_other <- setdiff(stale_eff[, "Package"], closure)
if (length(stale_other) > 0) {
  cat("[INFO] Stale packages NOT used by this app (safe to ignore for ",
      "this deployment,\n       candidates for cleanup or rebuild if ",
      "another app needs them):\n       ",
      paste(stale_other, collapse = ", "), "\n", sep = "")
}

# ----------------------------------------------------------------
# 2. All app dependencies load cleanly (this catches dyn.load errors
#    at deploy time instead of at first use).
# ----------------------------------------------------------------
cat("\n-- 2. Loading all app dependencies --\n")
for (p in deps) {
  res <- tryCatch({
    suppressPackageStartupMessages(
      library(p, character.only = TRUE, quietly = TRUE)
    )
    TRUE
  }, error = function(e) conditionMessage(e))
  if (isTRUE(res)) ok(paste("library(", p, ")")) else
    fail(paste0("library(", p, "): ", res))
}

# ----------------------------------------------------------------
# 3. Headless plot rendering test, using the SAME device Shiny would
#    choose: ragg::agg_png when ragg is installed (this is what pulls
#    in systemfonts/textshaping at runtime), plain png() otherwise.
#    A base-png-only test can pass while the app still fails.
# ----------------------------------------------------------------
cat("\n-- 3. Headless ggplot rendering (Shiny's preferred device) --\n")
plot_test <- tryCatch({
  tmp <- tempfile(fileext = ".png")
  if (requireNamespace("ragg", quietly = TRUE)) {
    dev_used <- "ragg::agg_png (Shiny's preferred device)"
    ragg::agg_png(tmp, width = 600, height = 400)
  } else {
    dev_used <- "grDevices::png (ragg not installed)"
    png(tmp, width = 600, height = 400)
  }
  print(
    ggplot2::ggplot(data.frame(x = c("A", "B"), y = c(1, 2)),
                    ggplot2::aes(x, y)) +
      ggplot2::geom_boxplot() +
      ggplot2::labs(title = "preflight test")
  )
  dev.off()
  file.exists(tmp) && file.info(tmp)$size > 0
}, error = function(e) conditionMessage(e))
if (isTRUE(plot_test)) ok(paste0("ggplot rendered via ", dev_used)) else
  fail(paste0("Plot rendering failed: ", plot_test))

# ----------------------------------------------------------------
# 4. App configuration and input files: source global.R, which runs
#    config validation, consolidation, and loads participants and
#    solutions. Any config problem surfaces here with its own message.
# ----------------------------------------------------------------
cat("\n-- 4. Sourcing global.R (config validation + data loading) --\n")
glob <- tryCatch({ source("global.R"); TRUE },
                 error = function(e) conditionMessage(e))
if (isTRUE(glob)) ok("global.R sourced: config valid, input files found") else
  fail(paste0("global.R failed: ", glob))

# ----------------------------------------------------------------
# 5. Write permissions, AS THIS USER, on every path the app writes to.
#    Shiny Server usually runs as 'shiny'; app directories deployed as
#    root are the classic silent killer: the app runs, students get
#    results on screen, and no CSV is ever saved.
# ----------------------------------------------------------------
cat("\n-- 5. Write permissions on results paths --\n")
if (isTRUE(glob)) {
  check_write <- function(label, dir_path) {
    probe <- file.path(dir_path, paste0(".preflight-", Sys.getpid()))
    res <- tryCatch({
      writeLines("x", probe)
      final <- paste0(probe, ".renamed")
      stopifnot(file.rename(probe, final))   # atomic-rename path
      unlink(final)
      TRUE
    }, error = function(e) conditionMessage(e))
    if (isTRUE(res)) ok(paste0(label, " writable (incl. rename): ", dir_path))
    else {
      unlink(probe)
      fail(paste0(label, " NOT writable by user '",
                  Sys.info()[["user"]], "': ", dir_path, " (", res, ")"))
    }
  }
  check_write("results_path", config$results_path)
  check_write("processed", file.path(config$results_path, "processed"))
  # The consolidated RData is written relative to the app directory:
  check_write("app directory (RData)", dirname(normalizePath(
    config$resultats_rdata, mustWork = FALSE)))
} else {
  fail("Skipped (global.R did not load)")
}

# ----------------------------------------------------------------
# Summary
# ----------------------------------------------------------------
cat("\n== Summary ==\n")
if (fails == 0) {
  cat("All checks passed. The app should start and render plots.\n")
} else {
  cat(fails, "check(s) FAILED. Fix them before deploying.\n")
}
quit(status = ifelse(fails == 0, 0, 1))

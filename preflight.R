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
#    This is the SETLENGTH / SET_GROWABLE_BIT class of error:
#    the package loads fine or not depending on when it was compiled,
#    and some (graphics-related ones) only load at first plot.
#    Scans ALL library paths, which catches the two-site-library trap.
# ----------------------------------------------------------------
cat("-- 1. Package binary compatibility across all library paths --\n")
current_rv <- paste(R.version$major,
                    strsplit(R.version$minor, "\\.")[[1]][1], sep = ".")
stale_any <- FALSE
for (lib in .libPaths()) {
  ip <- installed.packages(lib.loc = lib)
  if (nrow(ip) == 0) next
  built <- vapply(ip[, "Built"], function(b) {
    parts <- strsplit(b, "\\.")[[1]]
    paste(parts[1], parts[2], sep = ".")
  }, character(1))
  stale <- rownames(ip)[built != current_rv]
  if (length(stale) > 0) {
    stale_any <- TRUE
    fail(paste0("Library ", lib, " has packages built under another ",
                "R version: ", paste(stale, collapse = ", ")))
    cat("       Fix: sudo Rscript -e 'update.packages(lib.loc = \"",
        lib, "\", ask = FALSE, checkBuilt = TRUE)'\n", sep = "")
  } else {
    ok(paste0("All packages in ", lib, " built under R ", current_rv))
  }
}

# ----------------------------------------------------------------
# 2. All app dependencies load cleanly (this catches dyn.load errors
#    at deploy time instead of at first use).
# ----------------------------------------------------------------
cat("\n-- 2. Loading all app dependencies --\n")
deps <- c("shiny", "shinyjs", "readr", "dplyr", "tidyr", "purrr",
          "tibble", "ggplot2", "readxl", "shinyalert", "yaml")
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
# 3. Headless plot rendering test. Shiny renders plots through a PNG
#    device, which pulls in systemfonts/textshaping/ragg lazily --
#    exactly the libraries that broke in production. This reproduces
#    that path without a browser.
# ----------------------------------------------------------------
cat("\n-- 3. Headless ggplot rendering (PNG device) --\n")
plot_test <- tryCatch({
  tmp <- tempfile(fileext = ".png")
  png(tmp, width = 600, height = 400)
  print(
    ggplot2::ggplot(data.frame(x = c("A", "B"), y = c(1, 2)),
                    ggplot2::aes(x, y)) +
      ggplot2::geom_boxplot() +
      ggplot2::labs(title = "preflight test")
  )
  dev.off()
  file.exists(tmp) && file.info(tmp)$size > 0
}, error = function(e) conditionMessage(e))
if (isTRUE(plot_test)) ok("ggplot rendered to PNG") else
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

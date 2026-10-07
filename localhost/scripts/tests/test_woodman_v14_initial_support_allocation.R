# Read-only validation of a completed bounded v14 Windows runtime fixture.
# Usage: Rscript test_woodman_v14_initial_support_allocation.R FIXTURE_DIR
# Reads the captured normalization scalars and saved scientific maps. No output
# rasters are created and no canonical run or input is changed.
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 1L)
script_arg <- commandArgs()[startsWith(commandArgs(), "--file=")][[1L]]
script <- normalizePath(sub("^--file=", "", script_arg), winslash = "/")
source(file.path(dirname(script), "..", "postprocessing_sourcing",
                 "2post_runtime_sourcing_v1.R"))
.rs_require()
run <- normalizePath(args[[1L]], winslash = "/", mustWork = TRUE)
meta <- .rs_meta(run)
stopifnot(meta$mc == 1L, meta$end - meta$start == 2L)
read_map <- function(path) as.numeric(terra::values(terra::rast(file.path(run, path))))
initial <- read_map("Temp/2_IniSt01.tif")
excluded <- !is.finite(initial)
stopifnot(any(excluded), any(is.finite(initial) & initial == 0))
sum0 <- function(x) sum(x, na.rm = TRUE)
close_total <- function(actual, expected, label) {
  tolerance <- max(1e-5, 2e-7 * abs(expected))
  if (abs(actual - expected) > tolerance)
    stop(label, ": actual=", actual, ", expected=", expected)
}
for (step in 1:3) {
  map <- function(stem) read_map(sprintf("debugging_1/%s%02d.tif", stem, step))
  available <- map("Growth")
  post <- map("Growth_less_harv")
  harvest <- map("Harvest_tot")
  request <- map("Expect_harv_tot")
  stopifnot(all(!is.finite(available[excluded])),
            all(!is.finite(post[excluded])),
            !any(harvest[excluded] > 0, na.rm = TRUE),
            !any(request[excluded] > 0, na.rm = TRUE))
  pressure <- list()
  for (channel in c("W", "V")) {
    files <- list.files(file.path(run, "Sourcing/MC001"),
      pattern = sprintf("^%s_scalars[0-9]+_%02d[.]csv$", channel, step),
      full.names = TRUE)
    stopifnot(length(files) > 0L)
    scalars <- lapply(files, .rs_scalars)
    # This fixture has eligible source cells for every positive demand origin.
    stopifnot(all(vapply(scalars, function(s)
      s[["demand"]] == 0 || s[["denominator"]] > 0, logical(1))))
    target <- sum(vapply(scalars, .rs_target, numeric(1)))
    pressure[[channel]] <- map(paste0("Proj_harv_", channel, "tot"))
    stopifnot(!any(pressure[[channel]][excluded] > 0, na.rm = TRUE))
    close_total(sum0(pressure[[channel]]), target,
                paste("Demand conservation", channel, step))
  }
  shortfall <- map("Non_harv_AGR")
  redistributed <- map("Ex_agr_harv")
  stopifnot(!any(redistributed[excluded] > 0, na.rm = TRUE))
  close_total(sum0(redistributed), sum0(shortfall),
              paste("TOF shortfall redistribution", step))
  close_total(sum0(request), sum0(pressure$W) + sum0(pressure$V),
              paste("Total requested harvest", step))
  # The existing harvest clamp uses zero for unavailable/NoData stock.
  expected_harvest <- numeric(length(initial))
  valid <- is.finite(available) & available > 0 & is.finite(request) & request > 0
  expected_harvest[valid] <- pmin(available[valid], request[valid])
  stopifnot(identical(is.na(harvest), is.na(expected_harvest)),
            all(harvest == expected_harvest))
}
cat("PASS: fixed initial support, allocation conservation and exact harvest clamp:",
    basename(run), "\n")

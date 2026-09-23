# Zero a chosen final demand step in an unrun scratch fixture; production data
# are never edited. Stage baseline/candidate children afterwards to freeze hashes.
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 2L)
root <- normalizePath(args[1], winslash = "/", mustWork = TRUE)
step <- as.integer(args[2])
stopifnot(is.finite(step), step > 0L, step <= 51L)
if (!startsWith(tolower(root), "e:/mofuss_active/ecsa_sourcing_and_speed_v1/runtime_regression/") ||
    !file.exists(file.path(root, "fixture_manifest.json")) ||
    file.exists(file.path(root, "runtime_result.json")) ||
    file.exists(file.path(root, "zero_demand_test_fixture.csv"))) {
  stop("Only a fresh unrun task fixture may be transformed")
}
demand <- file.path(root, "In/DemandScenarios")
pattern <- sprintf("^(fwuse_[WV](_clipped|_ext_fwdef)?|[WV]_origin_demand)%02d[.]csv$", step)
files <- list.files(demand, pattern = pattern, full.names = TRUE)
stopifnot(length(files) == 8L)
for (file in files) {
  d <- read.csv(file, check.names = FALSE)
  stopifnot("Value" %in% names(d))
  d$Value <- 0
  write.csv(d, file, row.names = FALSE)
}
p <- read.csv(file.path(root, "LULCC/TempTables/parameters_dinamica.csv"))
year <- as.integer(p$ParCHR[p$Var == "start_year"]) + step - 1L
for (channel in c("w", "v")) {
  file <- file.path(demand, paste0("BaU_fwch_", channel, ".csv"))
  d <- read.csv(file, check.names = FALSE)
  column <- paste0(year, "_fw_", channel)
  stopifnot(column %in% names(d))
  d[[column]] <- 0
  write.csv(d, file, row.names = FALSE)
}
write.csv(data.frame(step = step, year = year, W_demand = 0, V_demand = 0),
  file.path(root, "zero_demand_test_fixture.csv"), row.names = FALSE)
cat("Zero-demand fixture source created:", root, "year", year, "\n")

# Validate the real parameter exporter in isolated temporary folders. No
# simulations, country preparation, or canonical run outputs are touched.
scripts <- normalizePath("localhost/scripts", winslash = "/", mustWork = TRUE)
scratch <- tempfile("woodman_freeze_parameters_", tmpdir = tempdir())
dir.create(scratch, recursive = TRUE)
original_wd <- getwd()
expected_names <- c("start_year", "end_year", "monte_carlo_runs",
                    "uncapped_regrowth", "npa_ease", "woodman_luc_freeze_year")
base_parameters <- data.frame(Var = head(expected_names, 5L),
                              ParCHR = c("2000", "2050", "3", "0", "10"))
export_fixture <- function(name, freeze = NULL, duplicate = FALSE) {
  folder <- file.path(scratch, name)
  dir.create(folder)
  params <- base_parameters
  if (!is.null(freeze)) {
    params <- rbind(params, data.frame(Var = "woodman_luc_freeze_year", ParCHR = freeze))
    if (duplicate) params <- rbind(params, tail(params, 1L))
  }
  source_path <- file.path(folder, "parameters.csv")
  write.csv(params, source_path, row.names = FALSE)
  env <- new.env(parent = globalenv())
  env$countrydir <- folder
  env$parameters_file_path <- source_path
  env$webmofuss <- 0L
  previous <- getwd()
  on.exit(setwd(previous), add = TRUE)
  value <- try(sys.source(file.path(scripts, "7_parameters_dinamica_v1.R"),
                          envir = env), silent = TRUE)
  list(error = inherits(value, "try-error"),
       runtime = file.path(folder, "LULCC/TempTables/parameters_dinamica.csv"))
}
for (year in c(2000L, 2026L, 2050L)) {
  result <- export_fixture(paste0("valid_", year), as.character(year))
  stopifnot(!result$error)
  table <- read.csv(result$runtime, stringsAsFactors = FALSE)
  stopifnot(identical(table$Var, expected_names),
            identical(table$ParCHR, c(2000L, 2050L, 3L, 0L, 10L, year)))
}
legacy <- export_fixture("legacy_default")
stopifnot(!legacy$error)
legacy_table <- read.csv(legacy$runtime, stringsAsFactors = FALSE)
stopifnot(identical(legacy_table$Var, expected_names),
          identical(tail(legacy_table$ParCHR, 1L), 2050L))
for (year in c("1999", "2051", "2026.5", "NA", "", "2026x")) {
  result <- export_fixture(paste0("invalid_", match(year,
                           c("1999", "2051", "2026.5", "NA", "", "2026x"))), year)
  stopifnot(result$error, !file.exists(result$runtime))
}
duplicate <- export_fixture("duplicate", "2026", duplicate = TRUE)
stopifnot(duplicate$error, !file.exists(duplicate$runtime))
stopifnot(identical(getwd(), original_wd))
cat("WOODMAN_FREEZE_PARAMETER_EXPORT_TESTS_OK\n")

# Execute only the growth-table dispatch from rnorm_v8.R with in-memory CSV mocks.
# This catches an inactive LUC3 branch without initializing or removing MC runs.
script_argument <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
stopifnot(length(script_argument) == 1L)
test_script <- sub("^--file=", "", script_argument)
repository_root <- normalizePath(
  file.path(dirname(test_script), "..", "..", ".."),
  winslash = "/", mustWork = TRUE
)
script_path <- file.path(repository_root, "localhost", "scripts", "rnorm_v8.R")
script_lines <- readLines(script_path, warn = FALSE)
preflight_line <- grep("^if \\(LUCmap_v == 3L &&", script_lines)
deletion_line <- grep("^debugging_to_remove <- list.files\\(", script_lines)
stopifnot(length(preflight_line) == 1L, length(deletion_line) == 1L,
          preflight_line < deletion_line)
expressions <- as.list(parse(file = script_path))
is_growth_dispatch <- function(expr) {
  is.call(expr) && identical(expr[[1L]], as.name("if")) &&
    grepl("growth_parameters3_file", paste(deparse(expr), collapse = "\n"), fixed = TRUE)
}
matches <- Filter(is_growth_dispatch, expressions)
stopifnot(length(matches) == 1L)
growth_dispatch <- matches[[1L]]

exercise <- function(delimiter, available = TRUE, tof = c(0, 1, 1)) {
  env <- new.env(parent = baseenv())
  env$LUCmap_v <- 3L
  env$file.exists <- function(path) available
  env$readLines <- function(...) paste0('"Key*"', delimiter, '"TOF"')
  env$read.csv <- function(path, sep) {
    stopifnot(identical(sep, delimiter))
    data.frame(`Key*` = seq_along(tof), TOF = tof, check.names = FALSE)
  }
  written <- NULL
  env$write.csv <- function(x, file, row.names) {
    written <<- list(x = x, file = file, row.names = row.names)
  }
  result <- tryCatch(eval(growth_dispatch, envir = env), error = identity)
  list(env = env, written = written, result = result)
}

for (delimiter in c(",", ";")) {
  test <- exercise(delimiter)
  stopifnot(!inherits(test$result, "error"))
  stopifnot(nrow(test$env$data_all3) == 3L)
  stopifnot(nrow(test$env$data_FOR3) == 1L)
  stopifnot(nrow(test$env$data_TOF3) == 2L)
  stopifnot(test$env$max_tot3 == 3L)
  stopifnot(identical(test$written$file, "Temp/LULC_Categories3.csv"))
  stopifnot(identical(test$written$x$Key, 1:3))
  stopifnot(identical(test$written$row.names, FALSE))
}

missing <- exercise(",", available = FALSE)
stopifnot(inherits(missing$result, "error"))
stopifnot(grepl("requires LULCC/TempTables/growth_parameters3.csv",
              conditionMessage(missing$result), fixed = TRUE))
invalid <- exercise(",", tof = c(0, 2))
stopifnot(inherits(invalid$result, "error"))
stopifnot(grepl("TOF column containing only 0 or 1",
              conditionMessage(invalid$result), fixed = TRUE))
cat("RNORM_V8_LUC3_GROWTH_OK\n")

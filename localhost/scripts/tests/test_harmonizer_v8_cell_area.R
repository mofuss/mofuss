suppressPackageStartupMessages(library(terra))

script_argument <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
stopifnot(length(script_argument) == 1L)
test_script <- sub("^--file=", "", script_argument)
repository_root <- normalizePath(
  file.path(dirname(test_script), "..", "..", ".."),
  winslash = "/",
  mustWork = TRUE
)
script_path <- file.path(
  repository_root,
  "localhost",
  "scripts",
  "5_harmonizer_v8.R"
)
expressions <- parse(file = script_path)

load_definition <- function(name) {
  matches <- which(vapply(expressions, function(expression) {
    is.call(expression) &&
      identical(expression[[1L]], as.name("<-")) &&
      identical(as.character(expression[[2L]]), name)
  }, logical(1)))
  stopifnot(length(matches) == 1L)
  eval(expressions[[matches]], envir = .GlobalEnv)
}

load_definition("accurate_cell_area")

fixture <- tempfile("harmonizer_v8_cell_area_")
dir.create(fixture, recursive = TRUE)
on.exit(unlink(fixture, recursive = TRUE, force = TRUE), add = TRUE)

original_options <- terra::terraOptions(print = FALSE)
on.exit(
  terra::terraOptions(
    tempdir = original_options$tempdir,
    memfrac = original_options$memfrac,
    memmax = original_options$memmax,
    todisk = original_options$todisk
  ),
  add = TRUE
)
terra::terraOptions(tempdir = fixture, todisk = TRUE)

geometry <- terra::rast(
  nrows = 121,
  ncols = 87,
  xmin = 1357554.89448733,
  xmax = 1444554.89448733,
  ymin = -4117953.866106,
  ymax = -3996953.866106,
  crs = "EPSG:3395"
)
reference <- terra::cellSize(
  geometry,
  mask = FALSE,
  unit = "ha",
  transform = TRUE,
  rcx = max(terra::nrow(geometry), terra::ncol(geometry))
)
chunked <- accurate_cell_area(
  geometry,
  unit = "ha",
  max_cells_per_chunk = 500L
)

stopifnot(
  terra::compareGeom(reference, chunked, stopOnError = FALSE),
  isTRUE(all.equal(
    terra::values(reference, mat = FALSE),
    terra::values(chunked, mat = FALSE),
    tolerance = 0
  ))
)

invalid_chunk_error <- tryCatch(
  accurate_cell_area(geometry, max_cells_per_chunk = 0),
  error = identity
)
stopifnot(
  inherits(invalid_chunk_error, "error"),
  grepl("one positive number", conditionMessage(invalid_chunk_error))
)

cat("HARMONIZER_V8_CELL_AREA_OK\n")

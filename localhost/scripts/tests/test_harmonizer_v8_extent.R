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

for (name in c(
  ".demand_origin_bounds_or_stop",
  ".analysis_extent_with_demand_origins",
  ".location_ids_or_stop",
  ".remap_location_ids_to_template"
)) {
  load_definition(name)
}

fixture <- tempfile("harmonizer_v8_extent_")
dir.create(fixture, recursive = TRUE)
on.exit(unlink(fixture, recursive = TRUE, force = TRUE), add = TRUE)

write_location <- function(path, raster, cells, ids) {
  location_values <- rep(NA_integer_, terra::ncell(raster))
  location_values[cells] <- ids
  terra::values(raster) <- location_values
  terra::writeRaster(raster, path, datatype = "INT4S", overwrite = TRUE)
}

# The AOI alone aligns to (0, 4, 0, 4). The W origin is in the intentional
# one-cell preprocessing fringe and requires one additional northern row.
walking <- terra::rast(
  nrows = 5,
  ncols = 4,
  xmin = 0.2,
  xmax = 4.2,
  ymin = 0.2,
  ymax = 5.2,
  crs = "EPSG:3857"
)
vehicle <- walking
walking_path <- file.path(fixture, "locs_raster_w.tif")
vehicle_path <- file.path(fixture, "locs_raster_v.tif")
write_location(walking_path, walking, 2L, 101L)
write_location(vehicle_path, vehicle, 14L, 202L)

analysis_extent <- .analysis_extent_with_demand_origins(
  userarea_extent = terra::ext(0.1, 3.9, 0.1, 3.9),
  grid_origin = c(0, 0),
  resolution = 1,
  analysis_crs = "EPSG:3857",
  location_paths = c(W = walking_path, V = vehicle_path)
)
stopifnot(isTRUE(all.equal(
  unname(as.vector(analysis_extent)),
  c(0, 4, 0, 5)
)))

template <- terra::rast(
  analysis_extent,
  resolution = 1,
  crs = "EPSG:3857"
)
terra::values(template) <- 1
walking_data <- .location_ids_or_stop(
  walking_path,
  "walking fixture",
  include_cells = TRUE
)
walking_remap <- .remap_location_ids_to_template(
  walking_path,
  template,
  walking_data$cells,
  walking_data$ids,
  "walking fixture"
)
stopifnot(identical(
  as.numeric(terra::values(walking_remap$raster, na.rm = TRUE)),
  101
))

# Retaining the larger raster geometry while masking the AOI is essential; a
# vector crop here would discard the newly added fringe row.
userarea <- terra::vect(
  "POLYGON ((0.1 0.1, 3.9 0.1, 3.9 3.9, 0.1 3.9, 0.1 0.1))",
  crs = "EPSG:3857"
)
userarea_raster <- terra::rasterize(userarea, template, field = 1)
userarea_mask <- terra::mask(userarea_raster, userarea)
stopifnot(terra::compareGeom(
  userarea_mask,
  template,
  stopOnError = FALSE
))

# A distant origin still indicates a region/alignment error and must fail.
far <- terra::rast(
  nrows = 1,
  ncols = 1,
  xmin = 1.2,
  xmax = 2.2,
  ymin = 6.2,
  ymax = 7.2,
  crs = "EPSG:3857"
)
far_path <- file.path(fixture, "locs_raster_far.tif")
write_location(far_path, far, 1L, 303L)
far_error <- tryCatch(
  .analysis_extent_with_demand_origins(
    userarea_extent = terra::ext(0.1, 3.9, 0.1, 3.9),
    grid_origin = c(0, 0),
    resolution = 1,
    analysis_crs = "EPSG:3857",
    location_paths = c(far = far_path)
  ),
  error = identity
)
stopifnot(
  inherits(far_error, "error"),
  grepl("more than the permitted one-cell diagonal", conditionMessage(far_error))
)

cat("HARMONIZER_V8_EXTENT_OK\n")

# Regression checks for extensive harvest amounts transferred between grids.
# Run from the repository root; fixtures stay inside R's temporary directory.

stage2 <- file.path(
  getwd(), "localhost", "scripts", "postprocessing_emissions",
  "2post_emissions_bau-vs-ics_v14.R"
)
env <- new.env(parent = globalenv())
env$MOFUSS_CONFIG_ONLY <- TRUE
sys.source(stage2, envir = env)

check_projection <- function() {
  scratch <- tempfile("mofuss_harvest_projection_")
  dir.create(scratch)
  old_options <- terra::terraOptions(print = FALSE)
  on.exit({
    terra::terraOptions(
      tempdir = old_options$tempdir, datatype = old_options$datatype,
      todisk = old_options$todisk
    )
    unlink(scratch, recursive = TRUE)
  }, add = TRUE)
  terra::terraOptions(tempdir = scratch, todisk = FALSE)

  source <- terra::rast(
    nrows = 10L, ncols = 8L,
    xmin = 1300000, xmax = 1700000, ymin = -2000000, ymax = -1500000,
    crs = "EPSG:3395"
  )
  corners <- terra::project(
    rbind(c(1300000, -2000000), c(1700000, -1500000)),
    terra::crs(source), "EPSG:4326"
  )
  target <- terra::rast(
    nrows = 17L, ncols = 13L,
    xmin = corners[1L, 1L] - 0.1, xmax = corners[2L, 1L] + 0.1,
    ymin = corners[1L, 2L] - 0.1, ymax = corners[2L, 2L] + 0.1,
    crs = "EPSG:4326"
  )

  verify <- function(values, label) {
    x <- terra::setValues(source, values)
    result <- env$.v14_project_harvest(x, target, label)
    expected <- sum(values, na.rm = TRUE)
    actual <- env$.v9_global_sum(result$raster)
    stopifnot(
      terra::compareGeom(result$raster, target, stopOnError = FALSE),
      is.finite(actual),
      abs(actual - expected) <= env$.v9_tolerance(expected),
      result$positive_factor >= 0, result$negative_factor >= 0
    )
    saved <- env$.v9_write_raster(
      result$raster, file.path(scratch, paste0(label, ".tif")), TRUE
    )
    stopifnot(abs(env$.v9_global_sum(saved) - expected) <= env$.v9_tolerance(expected))
    result
  }

  positive <- verify(seq_len(80L), "positive")
  negative <- verify(-seq_len(80L), "negative")
  stopifnot(
    terra::global(positive$raster, "min", na.rm = TRUE)[1L, 1L] >= 0,
    terra::global(negative$raster, "max", na.rm = TRUE)[1L, 1L] <= 0
  )
  balanced <- c(seq_len(40L), rep(-sum(seq_len(40L)) / 40, 40L))
  invisible(verify(balanced, "zero_net"))
  near_zero <- balanced
  near_zero[[80L]] <- near_zero[[80L]] + 1e-7
  invisible(verify(near_zero, "near_zero_net"))
  sparse <- balanced
  sparse[c(1L, 2L, 79L, 80L)] <- NA_real_
  sparse[c(3L, 78L)] <- 0
  invisible(verify(sparse, "sparse_signed"))
  invisible(verify(rep(0, 80L), "all_zero"))

  unchanged <- terra::setValues(source, sparse)
  same_grid <- env$.v14_project_harvest(unchanged, source, "same_grid")
  stopifnot(identical(terra::values(same_grid$raster), terra::values(unchanged)))

  expect_failure <- function(x, template, pattern) {
    failure <- tryCatch(
      env$.v14_project_harvest(x, template, "invalid_fixture"),
      error = identity
    )
    stopifnot(inherits(failure, "error"), grepl(pattern, conditionMessage(failure)))
  }
  clipped <- terra::rast(
    nrows = 17L, ncols = 13L,
    xmin = mean(corners[, 1L]), xmax = terra::xmax(target),
    ymin = terra::ymin(target), ymax = terra::ymax(target), crs = "EPSG:4326"
  )
  expect_failure(terra::setValues(source, seq_len(80L)), clipped, "does not cover")
  expect_failure(terra::setValues(source, rep(NA_real_, 80L)), target, "no valid cells")
  expect_failure(terra::setValues(source, c(Inf, rep(1, 79L))), target, "infinite values")

  # Small-memory computers must follow the same accounting when intermediates
  # spill to files; the helper must restore the caller's datatype afterward.
  terra::terraOptions(todisk = TRUE, datatype = "FLT4S")
  invisible(verify(balanced, "file_backed_zero_net"))
  stopifnot(identical(terra::terraOptions(print = FALSE)$datatype, "FLT4S"))

  cat("Harvest projection checks passed: signs, cancellation, NA/zero cells,\n",
      "unchanged grids, clipping rejection, and file-backed precision.\n", sep = "")
}

check_projection()

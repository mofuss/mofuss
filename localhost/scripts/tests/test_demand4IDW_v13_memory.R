# Regression checks for bounded demand correction and IDW table construction.
# Run from the repository root. All generated files stay in R's temp directory.
suppressPackageStartupMessages({
  library(terra)
  library(dplyr)
  library(purrr)
})
script <- file.path(getwd(), "localhost", "scripts", "3_demand4IDW_v13.R")
helpers <- new.env(parent = globalenv())
for (expr in parse(script)) {
  if (is.call(expr) && identical(expr[[1L]], as.name("<-")) &&
      is.symbol(expr[[2L]]) && as.character(expr[[2L]]) %in% c(
        ".project_demand_stack", ".preserve_projected_stack_mass", ".build_idw_demand_table"
      )) eval(expr, envir = helpers)
}

scratch <- tempfile("demand_memory_")
dir.create(scratch)
terraOptions(tempdir = scratch, memfrac = 0.2, memmax = 2, memmin = 0, progress = 0)

# Independent reference: the former per-year coordinate/full-join pipeline.
legacy_table <- function(stack, years, channel) {
  dfs <- lapply(seq_along(years), function(k) {
    d <- as.data.frame(stack[[k]], xy = TRUE, na.rm = TRUE)
    names(d)[3L] <- paste0(years[[k]], "_fw_", channel)
    d
  })
  ids <- bind_rows(lapply(dfs, function(d) {
    tibble(x = d$x, y = d$y, ID = as.integer(row.names(d)))
  })) %>% group_by(x, y) %>% summarise(ID = first(ID), .groups = "drop")
  out <- reduce(dfs, full_join, by = c("x", "y"))
  out$centroids <- !is.na(out[[names(dfs[[1L]])[3L]]])
  out <- left_join(out, ids, by = c("x", "y")) %>%
    relocate(ID) %>% relocate(centroids, .after = last_col())
  columns <- paste0(years, "_fw_", channel)
  out[columns] <- lapply(out[columns], function(x) replace(x, is.na(x), 0))
  totals <- rowSums(out[columns])
  if (all(totals < 0.1)) out[columns] <- 0.2 else out <- out[totals >= 0.1, ]
  out <- as.data.frame(out)
  rownames(out) <- NULL
  out
}

template <- rast(nrows = 4, ncols = 6, xmin = 0, xmax = 6,
                 ymin = 0, ymax = 4, crs = "EPSG:3395")
v <- matrix(NA_real_, nrow = ncell(template), ncol = 3L)
v[c(5, 7, 12, 22), 1L] <- c(1, 0.05, 0, 0.01)
v[c(1, 6, 7, 12, 23), 2L] <- c(0.2, 0.1, 0.04, 0, 0)
v[c(2, 5, 12, 22), 3L] <- c(0.5, 2, 0, 0.02)
stack <- rast(rep(list(template), 3L))
values(stack) <- v
years <- 2000:2002
for (channel in c("w", "v")) {
  for (budget in c(0.00001, 32)) {
    actual <- helpers$.build_idw_demand_table(stack, years, channel, block_mb = budget)
    expected <- legacy_table(stack, years, channel)
    stopifnot(isTRUE(all.equal(actual, expected, tolerance = 0)))
    stopifnot(identical(actual$ID, c(5L, 1L, 6L, 2L)),
              identical(actual$centroids, c(TRUE, FALSE, FALSE, FALSE)))
  }
}
low <- stack / 1000
stopifnot(isTRUE(all.equal(
  helpers$.build_idw_demand_table(low, years, "w", block_mb = 0.00001),
  legacy_table(low, years, "w"), tolerance = 0
)))
one <- helpers$.build_idw_demand_table(stack[[1L]], years[[1L]], "w")
stopifnot(isTRUE(all.equal(one, legacy_table(stack[[1L]], years[[1L]], "w"))))

# Correction must stay on disk even when each individual layer fits in RAM.
# Include a zero-total year and NA cells; compare to the former equations.
source <- stack
values(source)[, 3L] <- replace(v[, 3L], !is.na(v[, 3L]), 0)
source_files <- file.path(scratch, paste0("source_", years, ".tif"))
for (k in seq_along(years)) {
  writeRaster(source[[k]], source_files[[k]], datatype = "FLT8S", overwrite = TRUE)
}
projected <- source * c(0.91, 1.03, 1)
corrected <- helpers$.preserve_projected_stack_mass(projected, source_files, "fixture")
stopifnot(!inMemory(corrected$raster),
          all(datatype(corrected$raster) == "FLT8S"),
          identical(names(corrected$raster), names(projected)),
          isTRUE(all.equal(values(corrected$raster), values(source), tolerance = 1e-12)),
          all(abs(corrected$audit$residual_Mg) <= 0.01))
zero_projected <- source * 0
failure <- tryCatch(
  helpers$.preserve_projected_stack_mass(zero_projected, source_files, "invalid"),
  error = identity
)
stopifnot(inherits(failure, "error"), grepl("undefined", conditionMessage(failure)))

# Computing the grid from one layer must preserve the former multi-band warp.
geographic <- source
crs(geographic) <- "EPSG:4326"
geo_files <- file.path(scratch, paste0("geographic_", years, ".tif"))
for (k in seq_along(years)) {
  writeRaster(geographic[[k]], geo_files[[k]], datatype = "FLT8S")
}
legacy_projected <- project(rast(geo_files), "EPSG:3395", method = "bilinear",
                            res = 100000, filename = file.path(scratch, "legacy_warp.tif"))
annual_projected <- helpers$.project_demand_stack(geo_files, "EPSG:3395", 100000)
stopifnot(!inMemory(annual_projected),
          compareGeom(legacy_projected, annual_projected),
          isTRUE(all.equal(values(legacy_projected), values(annual_projected), tolerance = 0)))

# A sparse full-size grid checks exact IDs above the float32 integer boundary.
large <- rast(nrows = 4097L, ncols = 4097L, xmin = 0, xmax = 4097,
              ymin = 0, ymax = 4097, crs = "EPSG:3395")
high_ids <- c(16777217L, as.integer(ncell(large)))
large_values <- rep(NA_real_, ncell(large))
large_values[high_ids] <- c(1, 2)
large <- writeRaster(setValues(large, large_values), file.path(scratch, "high_ids.tif"),
                     datatype = "FLT8S", gdal = "COMPRESS=DEFLATE")
rm(large_values)
invisible(gc())
high_table <- helpers$.build_idw_demand_table(large, 2000L, "v", block_mb = 4)
stopifnot(identical(high_table$ID, high_ids),
          identical(high_table$`2000_fw_v`, c(1, 2)))

cat("DEMAND4IDW_V13_MEMORY_OK: values, IDs, row order, NA/zero fallback, mass, file backing.\n")

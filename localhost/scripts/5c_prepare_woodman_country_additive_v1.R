# SPDX-License-Identifier: Apache-2.0
# Add Woodman LUC3 inputs to an already prepared country run.
# Dry-run by default. This script never runs the destructive country steps
# 0/1/2/5/6, never writes In or Out, and refuses existing output files.
#
# Rscript 5c_prepare_woodman_country_additive_v1.R
#   --target D:/MDG_1000m_bau1_2050_mc3_capped
#   --prepared F:/MDG_1000m_bau1_2050_mc3_capped
#   --scratch E:/MoFuSS_Active/woodman_mdg_country_prep_2026-10-04
# Add --apply only after reviewing the dry-run manifest.

suppressPackageStartupMessages(library(terra))

woodman_country_path <- function(root, ...) file.path(root, "LULCC", ...)

woodman_read_parameters <- function(path) {
  x <- read.csv(
    path, colClasses = "character", stringsAsFactors = FALSE,
    check.names = FALSE, na.strings = character()
  )
  if (!all(c("Var", "ParCHR") %in% names(x)) ||
      anyNA(x$Var) || anyDuplicated(x$Var)) {
    stop("Invalid or duplicated parameter rows: ", path)
  }
  x$ParCHR[is.na(x$ParCHR)] <- ""
  x
}

woodman_parameter_value <- function(parameters, name, default = NULL) {
  value <- parameters$ParCHR[parameters$Var == name]
  if (!length(value)) return(default)
  if (length(value) != 1L) stop("Duplicate parameter: ", name)
  trimws(value[[1L]])
}

woodman_assert_same_file <- function(path_a, path_b, label) {
  if (!file.exists(path_a) || !file.exists(path_b)) {
    stop("Missing ", label, " in target or prepared run.")
  }
  if (!identical(unname(tools::md5sum(path_a)),
                 unname(tools::md5sum(path_b)))) {
    stop(label, " differs between target and prepared runs.")
  }
}

woodman_static_manifest <- function(target_dir, prepared_dir) {
  folders <- c(
    "LULCC/TempRaster", "LULCC/TempTables",
    "LULCC/TempVector", "LULCC/TempVector_GCS"
  )
  run_local_tables <- c(
    "Country.csv", "Country.txt", "OS_type.txt", "OStype.csv",
    "Rpath.csv", "Rpath.txt", "parameters_dinamica.csv"
  )
  result <- lapply(folders, function(folder) {
    source_dir <- file.path(prepared_dir, folder)
    if (!dir.exists(source_dir)) stop("Missing prepared folder: ", source_dir)
    relative_files <- list.files(
      source_dir, recursive = TRUE, all.files = TRUE, no.. = TRUE
    )
    relative_files <- relative_files[
      file.exists(file.path(source_dir, relative_files)) &
        !dir.exists(file.path(source_dir, relative_files))
    ]
    if (identical(folder, "LULCC/TempTables")) {
      relative_files <- relative_files[
        !basename(relative_files) %in% run_local_tables &
          !grepl("^(growth_parameters3|TOFvsFOR_Categories3)[.]csv$",
                 basename(relative_files))
      ]
    }
    if (identical(folder, "LULCC/TempRaster")) {
      relative_files <- relative_files[
        !grepl(
          "^(LULCt3_c(_[0-9]{4})?|TOFvsFOR_mask3(_[0-9]{4})?|WoodmanTransition_[0-9]{4})[.]tif$",
          basename(relative_files)
        )
      ]
    }
    if (!length(relative_files)) return(NULL)
    data.frame(
      relative = file.path(folder, relative_files),
      source = file.path(source_dir, relative_files),
      target = file.path(target_dir, folder, relative_files),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, result)
}

woodman_forced_urban_parameters <- function(growth, pre_key, modis_key,
                                             modis_forced_key) {
  required <- c("Key*", "LULC", "rmax", "rmaxSD", "K", "KSD", "TOF")
  if (!setequal(names(growth), required) ||
      anyDuplicated(growth[["Key*"]]) ||
      anyNA(growth[, required])) {
    stop("Woodman growth table has missing, duplicate, or unexpected columns.")
  }
  zones <- sub("_[^_]+$", "", as.character(growth$LULC))
  urban <- endsWith(as.character(growth$LULC), "_Urban") &
    growth$TOF == 1L & is.finite(growth$K) & is.finite(growth$KSD)
  urban_zones <- zones[urban]
  if (!length(urban_zones) || anyDuplicated(urban_zones)) {
    stop("Woodman needs one native Urban TOF row per represented zone.")
  }
  urban_k <- growth$K[urban]
  urban_ksd <- growth$KSD[urban]
  positive_cv <- urban_ksd[urban_k > 0] / urban_k[urban_k > 0]
  positive_cv <- positive_cv[is.finite(positive_cv) & positive_cv >= 0]
  if (length(positive_cv)) {
    urban_cv <- stats::median(positive_cv)
  } else if (all(urban_k == 0 & urban_ksd == 0)) {
    urban_cv <- 0
  } else {
    stop("Woodman native Urban rows lack a usable TOF coefficient of variation.")
  }

  forced_keys <- terra::ifel(modis_key == modis_forced_key, pre_key, NA)
  frequency <- as.data.frame(terra::freq(forced_keys))
  if (!nrow(frequency)) {
    forced_k <- mean(urban_k)
  } else {
    if (!all(c("value", "count") %in% names(frequency))) {
      names(frequency)[(ncol(frequency) - 1L):ncol(frequency)] <-
        c("value", "count")
    }
    pixel_count <- as.numeric(frequency$count)
    source_zone <- zones[match(as.numeric(frequency$value),
                               as.numeric(growth[["Key*"]]))]
    matched_k <- urban_k[match(source_zone, urban_zones)]
    if (any(!is.finite(pixel_count)) || sum(pixel_count) <= 0) {
      stop("Invalid forced-urban footprint frequencies.")
    }
    matched <- is.finite(matched_k)
    fallback_k <- if (any(matched)) {
      stats::weighted.mean(matched_k[matched], pixel_count[matched])
    } else {
      mean(urban_k)
    }
    matched_k[!matched] <- fallback_k
    forced_k <- stats::weighted.mean(matched_k, pixel_count)
  }
  c(K = round(forced_k, 4L), KSD = round(forced_k * urban_cv, 4L))
}

woodman_align_to_template <- function(x, template, method = "near") {
  if (!inherits(x, "SpatRaster")) x <- terra::rast(x)
  if (!nzchar(terra::crs(x)) || !nzchar(terra::crs(template))) {
    stop("Cannot align rasters without declared CRSs.")
  }
  if (!terra::same.crs(x, template)) {
    out <- terra::project(x, template, method = method, threads = TRUE)
  } else if (terra::compareGeom(x, template, stopOnError = FALSE)) {
    out <- x
  } else {
    x <- terra::crop(x, terra::ext(template), snap = "out")
    out <- if (terra::compareGeom(x, template, stopOnError = FALSE)) {
      x
    } else {
      terra::resample(x, template, method = method, threads = TRUE)
    }
  }
  terra::mask(out, template)
}

prepare_woodman_country_additive <- function(
    target_dir, prepared_dir, scratch_dir, apply = FALSE,
    scripts_dir = file.path(getwd(), "localhost", "scripts")) {
  target_dir <- normalizePath(target_dir, winslash = "/", mustWork = TRUE)
  prepared_dir <- normalizePath(prepared_dir, winslash = "/", mustWork = TRUE)
  scripts_dir <- normalizePath(scripts_dir, winslash = "/", mustWork = TRUE)
  scratch_dir <- gsub("\\\\", "/", scratch_dir)
  if (identical(target_dir, prepared_dir) || !nzchar(scratch_dir)) {
    stop("Target, prepared, and scratch directories must be distinct.")
  }
  for (root in c(target_dir, prepared_dir, scripts_dir)) {
    if (identical(scratch_dir, root) ||
        startsWith(scratch_dir, paste0(root, "/"))) {
      stop("Scratch directory must be outside the run and source directories.")
    }
  }
  if (!is.logical(apply) || length(apply) != 1L || is.na(apply)) {
    stop("apply must be exactly TRUE or FALSE.")
  }

  target_parameters_path <- woodman_country_path(
    target_dir, "DownloadedDatasets", "SourceDataGlobal", "parameters.csv"
  )
  prepared_parameters_path <- woodman_country_path(
    prepared_dir, "DownloadedDatasets", "SourceDataGlobal", "parameters.csv"
  )
  target_parameters <- woodman_read_parameters(target_parameters_path)
  prepared_parameters <- woodman_read_parameters(prepared_parameters_path)
  allowed_differences <- c(
    "LULCt3map", "LULCt3map_dataset", "LULCt3map_name",
    "LULCt3map_yr", "woodman_series_dir"
  )
  old <- prepared_parameters[
    !prepared_parameters$Var %in% allowed_differences, c("Var", "ParCHR")
  ]
  new <- target_parameters[
    !target_parameters$Var %in% allowed_differences, c("Var", "ParCHR")
  ]
  old <- old[order(old$Var), , drop = FALSE]
  new <- new[order(new$Var), , drop = FALSE]
  rownames(old) <- NULL
  rownames(new) <- NULL
  if (!identical(old, new)) {
    stop("Prepared and target parameters differ beyond the Woodman LUC3 rows.")
  }
  required_values <- c(
    LULCt1map = "YES", LULCt3map = "YES",
    LULCt3map_dataset = "woodman",
    LULCt3map_name = "woodman_luc_pcs.tif",
    LULCt3map_yr = "2000", start_year = "2000"
  )
  for (name in names(required_values)) {
    if (!identical(tolower(woodman_parameter_value(
      target_parameters, name, ""
    )), tolower(required_values[[name]]))) {
      stop("Unexpected target parameter: ", name)
    }
  }
  end_year <- suppressWarnings(as.integer(
    woodman_parameter_value(target_parameters, "end_year")
  ))
  if (is.na(end_year) || end_year < 2000L || end_year > 2050L) {
    stop("Woodman end_year must be between 2000 and 2050.")
  }
  years <- 2000:end_year

  global_raster <- function(root, name) woodman_country_path(
    root, "DownloadedDatasets", "SourceDataGlobal", "InRaster", name
  )
  global_table <- function(root, name) woodman_country_path(
    root, "DownloadedDatasets", "SourceDataGlobal", "InTables", name
  )
  pre_path <- global_raster(target_dir, "pre2000_v1_woodman_luc_pcs.tif")
  modis_path <- global_raster(target_dir, "modis_lc_type1_pcs.tif")
  zone_path <- global_raster(target_dir, "woodman_zone_pcs.tif")
  crosswalk_path <- global_table(target_dir, "woodman_key_crosswalk.csv")
  growth_source_path <- global_table(
    target_dir, "growth_parameters_v3_woodman.csv"
  )
  annual_source_paths <- global_raster(
    target_dir, sprintf("woodman_luc_%d_pcs.tif", years)
  )
  required_inputs <- c(
    pre_path, modis_path, zone_path, crosswalk_path,
    growth_source_path, annual_source_paths
  )
  missing <- required_inputs[!file.exists(required_inputs)]
  if (length(missing)) stop("Missing target input: ", missing[[1L]])

  for (name in c("modis_lc_type1_pcs.tif", "DTEM_pcs_masked.tif",
                 "datamask_pcs.tif")) {
    woodman_assert_same_file(
      global_raster(target_dir, name), global_raster(prepared_dir, name),
      name
    )
  }
  for (name in c(
    "IDW_C++_fw_v01.tif", "IDW_C++_fw_w01.tif",
    "fricc_v.tif", "fricc_w.tif"
  )) {
    woodman_assert_same_file(
      file.path(target_dir, "In", name),
      file.path(prepared_dir, "In", name), name
    )
  }
  idw_paths <- list.files(
    file.path(target_dir, "In"), pattern = "^IDW_C[+][+]_fw_.*[.]tif$",
    full.names = TRUE
  )
  if (!length(idw_paths)) stop("Target run has no preserved IDW rasters.")
  idw_hash_before <- tools::md5sum(idw_paths)

  template_path <- woodman_country_path(
    prepared_dir, "TempRaster", "mask_c.tif"
  )
  template <- terra::rast(template_path)
  for (path in c(
    file.path(target_dir, "In", "IDW_C++_fw_v01.tif"),
    file.path(target_dir, "In", "fricc_v.tif")
  )) {
    if (!terra::compareGeom(template, terra::rast(path),
                            stopOnError = FALSE)) {
      stop("Prepared analysis mask and target grid differ: ", path)
    }
  }
  pre_key <- terra::rast(pre_path)
  modis_key <- terra::rast(modis_path)
  if (!terra::compareGeom(pre_key, modis_key, stopOnError = FALSE)) {
    stop("Woodman pre2000 and MODIS forced-urban maps use different grids.")
  }
  growth <- read.csv(growth_source_path, check.names = FALSE)
  crosswalk <- read.csv(crosswalk_path, check.names = FALSE)
  key_values <- as.integer(growth[["Key*"]])
  if (!all(c("Key", "IDorig", "luc_code") %in% names(crosswalk)) ||
      anyNA(key_values) || anyDuplicated(key_values) ||
      !setequal(key_values, as.integer(crosswalk$Key))) {
    stop("Woodman growth table and crosswalk keys do not match.")
  }
  key_limits <- terra::minmax(pre_key)
  if (key_limits[1L, 1L] < min(key_values) ||
      key_limits[2L, 1L] > max(key_values)) {
    stop("Woodman pre2000 raster contains keys outside the growth table.")
  }
  forced_key <- max(key_values) + 1L

  prepared_modis_growth <- read.csv(
    global_table(prepared_dir, "growth_parameters_v3_modis.csv"),
    check.names = FALSE
  )
  prepared_growth1 <- read.csv(
    woodman_country_path(prepared_dir, "TempTables",
                         "growth_parameters1.csv"),
    check.names = FALSE
  )
  forced_modis <- prepared_growth1[
    prepared_growth1$LULC == "Urban_Forced", , drop = FALSE
  ]
  if (nrow(forced_modis) != 1L ||
      forced_modis[["Key*"]][[1L]] !=
        max(prepared_modis_growth[["Key*"]]) + 1L) {
    stop("Prepared MODIS forced key is not the unique appended source key.")
  }
  modis_forced_key <- as.integer(forced_modis[["Key*"]][[1L]])

  static_manifest <- woodman_static_manifest(target_dir, prepared_dir)
  existing_static <- file.exists(static_manifest$target)
  if (any(existing_static)) {
    for (i in which(existing_static)) {
      woodman_assert_same_file(
        static_manifest$source[[i]], static_manifest$target[[i]],
        static_manifest$relative[[i]]
      )
    }
  }
  static_to_copy <- static_manifest[!existing_static, , drop = FALSE]
  global_baseline_path <- global_raster(target_dir, "woodman_luc_pcs.tif")
  growth3_path <- woodman_country_path(
    target_dir, "TempTables", "growth_parameters3.csv"
  )
  global_growth3_path <- global_table(
    target_dir, "growth_parameters3.csv"
  )
  categories3_path <- woodman_country_path(
    target_dir, "TempTables", "TOFvsFOR_Categories3.csv"
  )
  parameters_dinamica_path <- woodman_country_path(
    target_dir, "TempTables", "parameters_dinamica.csv"
  )
  base_key_path <- woodman_country_path(
    target_dir, "TempRaster", "LULCt3_c.tif"
  )
  base_tof_path <- woodman_country_path(
    target_dir, "TempRaster", "TOFvsFOR_mask3.tif"
  )
  annual_output_paths <- unlist(lapply(years, function(year) {
    woodman_country_path(
      target_dir, "TempRaster",
      c(
        sprintf("LULCt3_c_%d.tif", year),
        sprintf("TOFvsFOR_mask3_%d.tif", year),
        sprintf("WoodmanTransition_%d.tif", year)
      )
    )
  }), use.names = FALSE)
  new_outputs <- c(
    global_baseline_path, growth3_path, global_growth3_path, categories3_path,
    parameters_dinamica_path, base_key_path, base_tof_path,
    annual_output_paths
  )
  collision <- new_outputs[file.exists(new_outputs)]
  if (length(collision)) {
    stop("Refusing to overwrite existing Woodman output: ", collision[[1L]])
  }
  message(
    if (apply) "APPLY" else "DRY RUN",
    ": add ", nrow(static_to_copy), " prepared static files, ",
    length(new_outputs), " Woodman/runtime files; years ",
    years[[1L]], "-", tail(years, 1L),
    "; forced keys MODIS=", modis_forced_key,
    ", Woodman=", forced_key,
    "; preserved IDWs=", length(idw_paths), "."
  )
  if (!apply) {
    return(invisible(list(
      static_to_copy = static_to_copy$relative,
      new_outputs = new_outputs,
      idw_paths = idw_paths
    )))
  }

  dir.create(scratch_dir, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(scratch_dir) || file.access(scratch_dir, 2) != 0L) {
    stop("Scratch directory is not writable: ", scratch_dir)
  }
  terra::terraOptions(tempdir = scratch_dir, memfrac = 0.5)
  forced <- woodman_forced_urban_parameters(
    growth, pre_key, modis_key, modis_forced_key
  )
  growth3 <- rbind(
    growth,
    data.frame(
      "Key*" = forced_key, LULC = "Urban_Forced",
      rmax = 0, rmaxSD = 0, K = forced[["K"]], KSD = forced[["KSD"]],
      TOF = 1L, check.names = FALSE
    )
  )
  baseline <- terra::ifel(
    is.na(modis_key), pre_key,
    terra::ifel(modis_key == modis_forced_key, forced_key, pre_key)
  )
  terra::writeRaster(
    baseline, global_baseline_path, datatype = "INT2S", overwrite = FALSE,
    wopt = list(gdal = c("COMPRESS=LZW"))
  )
  rm(baseline, pre_key, modis_key)
  invisible(gc())

  for (i in seq_len(nrow(static_to_copy))) {
    source <- static_to_copy$source[[i]]
    target <- static_to_copy$target[[i]]
    dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
    if (file.exists(target) ||
        !isTRUE(file.copy(source, target, overwrite = FALSE,
                          copy.mode = TRUE))) {
      stop("Could not add prepared static file: ", target)
    }
    woodman_assert_same_file(source, target, static_to_copy$relative[[i]])
  }
  write.csv(growth3, growth3_path, row.names = FALSE, quote = FALSE)
  write.csv(growth3, global_growth3_path, row.names = FALSE, quote = FALSE)
  woodman_assert_same_file(
    growth3_path, global_growth3_path, "Woodman growth_parameters3.csv"
  )
  country_key <- woodman_align_to_template(
    terra::rast(global_baseline_path), template, method = "near"
  )
  terra::writeRaster(
    country_key, base_key_path, datatype = "INT2S", overwrite = FALSE,
    wopt = list(gdal = c("COMPRESS=LZW"))
  )
  tof_matrix <- as.matrix(growth3[, c("Key*", "TOF")])
  country_tof <- terra::classify(
    country_key, tof_matrix, right = NA, others = NA
  )
  terra::writeRaster(
    country_tof, base_tof_path, datatype = "INT2S", overwrite = FALSE,
    wopt = list(gdal = c("COMPRESS=LZW"))
  )
  write.csv(
    data.frame(Key = as.integer(growth3[["Key*"]]),
               x = as.integer(growth3$TOF)),
    categories3_path, row.names = FALSE
  )
  rm(country_key, country_tof)
  invisible(gc())

  countrydir <- target_dir
  country_name <- "Global"
  country_parameters <- target_parameters
  userarea_r <- template
  align_raster_to_template <- woodman_align_to_template
  woodman_no_overwrite <- TRUE
  source(
    file.path(scripts_dir, "5b_harmonizer_woodman_multitemp_v1.R"),
    local = TRUE
  )
  webmofuss <- 1L
  parameters_file_path <- target_parameters_path
  prior_wd <- getwd()
  on.exit(setwd(prior_wd), add = TRUE)
  source(
    file.path(scripts_dir, "7_parameters_dinamica_v1.R"),
    local = TRUE
  )
  incomplete <- new_outputs[!file.exists(new_outputs)]
  if (length(incomplete)) stop("Missing prepared output: ", incomplete[[1L]])
  if (!identical(unname(idw_hash_before),
                 unname(tools::md5sum(idw_paths)))) {
    stop("A preserved IDW raster changed during additive preparation.")
  }
  installed <- c(static_to_copy$target, new_outputs)
  manifest <- data.frame(
    kind = c(rep("prepared_static", nrow(static_to_copy)),
             rep("woodman_runtime", length(new_outputs))),
    path = installed,
    bytes = file.info(installed)$size,
    md5 = unname(tools::md5sum(installed)),
    stringsAsFactors = FALSE
  )
  manifest_path <- tempfile(
    pattern = "woodman_country_additive_manifest_",
    tmpdir = scratch_dir, fileext = ".csv"
  )
  write.csv(manifest, manifest_path, row.names = FALSE)
  message(
    "Additive Woodman country preparation completed; IDWs unchanged. ",
    "Manifest: ", manifest_path
  )
  invisible(list(
    static_added = static_to_copy$relative,
    outputs = new_outputs,
    idw_paths = idw_paths,
    manifest_path = manifest_path
  ))
}

woodman_parse_cli <- function(args) {
  apply <- "--apply" %in% args
  args <- args[args != "--apply"]
  if (length(args) != 6L ||
      !setequal(args[c(TRUE, FALSE)], c("--target", "--prepared", "--scratch"))) {
    stop(
      "Usage: Rscript 5c_prepare_woodman_country_additive_v1.R ",
      "--target PATH --prepared PATH --scratch PATH [--apply]"
    )
  }
  parsed <- stats::setNames(args[c(FALSE, TRUE)], args[c(TRUE, FALSE)])
  list(
    target_dir = parsed[["--target"]],
    prepared_dir = parsed[["--prepared"]],
    scratch_dir = parsed[["--scratch"]],
    apply = apply
  )
}

if (sys.nframe() == 0L) {
  file_argument <- commandArgs(FALSE)
  file_argument <- sub("^--file=", "", file_argument[
    grepl("^--file=", file_argument)
  ][[1L]])
  scripts_dir <- dirname(normalizePath(file_argument, mustWork = TRUE))
  options <- woodman_parse_cli(commandArgs(trailingOnly = TRUE))
  do.call(
    prepare_woodman_country_additive,
    c(options, list(scripts_dir = scripts_dir))
  )
}

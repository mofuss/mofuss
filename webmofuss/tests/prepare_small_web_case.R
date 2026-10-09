# Prepare a disposable, real-data Kenya case from the supplied server backup.
# This is a validation fixture, not a replacement production entrypoint.
#
# First use --stage-only with a fresh --output directory. Before --run, start
# R with TMPDIR/TEMP/TMP pointing inside that fixture, then --resume-staging.
# Optional: --backup F:/webmofuss_speedup_sandbox --cores 2 --stage-only
# --resume-staging reuses verified copies only before any preprocessing starts.
# --resume-preprocessing reruns preparation in a verified disposable fixture;
# it refuses evidence of simulation/results and preserves prior stage status.
# --resume-after-demand starts at harmonization after verified demand/growth
# completion. Set TMPDIR/TEMP/TMP below this fixture before starting R so its
# process tempdir() and the legacy vector-write helpers stay isolated.
# Output contains case/, demand/, admin_regions/, rTemp/ and preparation_audit/.
# The original preprocessing calculations are sourced unchanged. The only
# process adaptations replace Windows executable discovery with explicit paths.
# No 0_main.R, notifications, IDW, simulation, or reporting entrypoint is run.

main <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  option <- function(name, default = NULL) {
    i <- which(args == name)
    if (!length(i)) return(default)
    if (length(i) != 1L || i == length(args)) stop("Invalid option: ", name)
    args[[i + 1L]]
  }
  backup <- option("--backup", "F:/webmofuss_speedup_sandbox")
  output <- option("--output")
  cores <- as.integer(option("--cores", "2"))
  run <- "--run" %in% args
  stage_only <- "--stage-only" %in% args
  resume_staging <- "--resume-staging" %in% args
  resume_after_demand <- "--resume-after-demand" %in% args
  resume_preprocessing <- "--resume-preprocessing" %in% args || resume_after_demand
  resume <- resume_staging || resume_preprocessing
  if (is.null(output)) stop("An explicit --output directory is required.")
  if (run == stage_only) stop("Choose exactly one of --run or --stage-only.")
  if (resume_staging && resume_preprocessing) stop("Choose only one resume mode.")
  if (resume_after_demand && "--resume-preprocessing" %in% args) stop("Choose only one preprocessing resume mode.")
  if (resume_preprocessing && !run) stop("--resume-preprocessing requires --run.")
  if (is.na(cores) || cores < 1L || cores > 8L) stop("--cores must be 1 through 8.")
  if (.Platform$OS.type != "windows") stop("This fixture targets the supplied Windows installation.")
  norm <- function(x, must = FALSE) normalizePath(x, winslash = "/", mustWork = must)
  backup <- norm(backup, TRUE)
  output <- gsub("\\\\", "/", output)
  if (!grepl("^E:/MoFuSS_Active/[^/]+/.+", output, ignore.case = TRUE) ||
      grepl("(^|/)\\.\\.(/|$)", output)) {
    stop("Output must be a named task subdirectory below E:/MoFuSS_Active.")
  }
  if (file.exists(output) && !resume) stop("Output already exists; choose a new empty task directory or explicitly resume.")
  if (resume && !dir.exists(output)) stop("Resume requires an existing fixture directory.")
  if (run) {
    private_temp <- tolower(norm(tempdir(), TRUE))
    intended_output <- tolower(sub("/+$", "", output))
    if (!(identical(private_temp, intended_output) ||
          startsWith(private_temp, paste0(intended_output, "/")))) {
      stop(paste0(
        "R tempdir() is outside the fixture: ", tempdir(), ". ",
        "Set TMPDIR, TEMP and TMP to an existing private directory below --output BEFORE starting R. ",
        "For a new fixture, run --stage-only first, then restart R with those variables and --run --resume-staging."
      ))
    }
  }
  # Existing ancestors must not redirect writes through a junction/symlink.
  ancestor <- dirname(output)
  while (!dir.exists(ancestor)) ancestor <- dirname(ancestor)
  if (!startsWith(tolower(norm(ancestor, TRUE)), "e:/mofuss_active")) {
    stop("Output ancestor resolves outside E:/MoFuSS_Active.")
  }
  github <- file.path(backup, "mofuss/mofuss")
  shared <- file.path(backup, "mofuss")
  source_global <- file.path(shared, "LULCC/DownloadedDatasets/SourceDataGlobal")
  source_scripts <- file.path(github, "localhost/scripts")
  script_names <- c(
    "0_set_directories_and_region_v3.R", "2_copy_files_v1.R",
    "2c_demand_tables_v5.R", "3_demand4IDW_v8.R",
    "4_produce_growth_and_stock_csv_v1.R", "5_harmonizer_v5.R",
    "6a_scenarios.R", "6d_parameters_dinamica_v1.R"
  )
  scripts <- file.path(source_scripts, script_names)
  generator_name <- "2b_oneschema_fix_v5.R"
  generator_source <- file.path(source_scripts, generator_name)
  required_rasters <- c(
    "A_MgDM_mofuss.tif", "k_mofuss.tif", "m_mofuss.tif",
    "ctrees_2000_agb_MgDM_ha.tif", "daac_agb_2010_pcs.tif",
    "esa_agb_2007_pcs.tif", "pre2001_v1_modis_lc_type1_pcs.tif",
    "DTEM_pcs.tif", "treecover2000_pcs.tif", "gain_pcs.tif",
    "lossyear_pcs.tif", "npa_pcs.tif", "hydrorivers7_pcs.tif",
    "hydrolakes_pcs.tif", "grip5_pcs.tif", "borders_pcs.tif"
  )
  required <- c(scripts, generator_source, file.path(github, "parameters/parameters.csv"),
                file.path(source_global, "InRaster", required_rasters),
                file.path(shared, "demand/demand_in/wp_global1000m_gcs.tif"),
                file.path(shared, "demand/demand_in/mofuss_regions0.gpkg"),
                file.path(shared, "admin_regions/gadm_410-levels.gpkg"),
                file.path(shared, "admin_regions/ecoregions2017.gpkg"))
  missing <- required[!file.exists(required)]
  if (length(missing)) stop("Missing original inputs (none will be fabricated):\n", paste(missing, collapse = "\n"))
  exe <- option("--dinamica", "C:/Program Files/Dinamica EGO/DinamicaConsole.exe")
  r_exe <- file.path(R.home("bin"), "R.exe")
  if (!file.exists(r_exe)) stop("Cannot locate current R.exe: ", r_exe)
  if (run && !file.exists(exe)) stop("Dinamica executable does not exist: ", exe)
  # Check every explicitly loaded package before staging large inputs.
  packages <- c("sf", "terra", "readr")
  if (run) {
    lines <- unlist(lapply(c(scripts, generator_source), readLines, warn = FALSE))
    lines <- lines[!grepl("^\\s*#", lines)]
    matches <- regmatches(lines, gregexpr("library\\([A-Za-z][A-Za-z0-9.]*\\)", lines))
    packages <- unique(c(packages, gsub("^library\\(|\\)$", "", unlist(matches))))
  }
  options(rgl.useNULL = TRUE)
  absent <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(absent)) stop("Required R packages unavailable: ", paste(absent, collapse = ", "))

  if (!resume) dir.create(output, recursive = TRUE)
  output <- norm(output, TRUE)
  if (!startsWith(tolower(output), "e:/mofuss_active/")) stop("Output resolves outside E:/MoFuSS_Active.")
  country <- file.path(output, "case")
  demand <- file.path(output, "demand")
  admin <- file.path(output, "admin_regions")
  scratch <- file.path(output, "rTemp")
  audit <- file.path(output, "preparation_audit")
  engine_temp <- file.path(output, "engine_temp")
  local_scratch <- file.path(output, "r_local_scratch")
  staged_global <- file.path(country, "LULCC/DownloadedDatasets/SourceDataGlobal")
  inside <- function(path) {
    # Resolve the existing parent of not-yet-existing paths, including wildcards.
    path <- gsub("\\\\", "/", path)
    for (p in path) {
      if (!grepl("^[A-Za-z]:/|^/", p)) p <- file.path(getwd(), p)
      if (grepl("(^|/)\\.\\.(/|$)", p)) stop("Parent traversal rejected: ", p)
      p <- sub("[?*].*$", "", p)
      while (!file.exists(p)) p <- dirname(p)
      resolved <- tolower(norm(p, TRUE))
      if (!(identical(resolved, tolower(output)) ||
            startsWith(resolved, paste0(tolower(output), "/")))) {
        stop("Write/delete outside fixture rejected: ", p)
      }
    }
    invisible(TRUE)
  }
  copy_tree <- function(from, to) {
    inside(to)
    dir.create(to, recursive = TRUE, showWarnings = FALSE)
    entries <- list.files(from, all.files = TRUE, no.. = TRUE, full.names = TRUE)
    if (!length(entries) || !all(file.copy(entries, to, recursive = TRUE, copy.mode = FALSE))) {
      stop("Could not stage all files from ", from)
    }
  }
  inventory <- function(root) {
    if (!dir.exists(root)) stop("Missing staged/source directory: ", root)
    relative <- list.files(root, recursive = TRUE, all.files = TRUE,
                           include.dirs = FALSE, no.. = TRUE)
    relative <- sort(relative, method = "radix")
    data.frame(path = relative, bytes = unname(file.info(file.path(root, relative))$size),
               stringsAsFactors = FALSE)
  }
  prepare_aoi <- function() {
    bbox <- sf::st_bbox(c(xmin = 35.9, ymin = -0.45, xmax = 36.2, ymax = -0.15), crs = sf::st_crs(4326))
    # The harmonizer supplies ID/NAME_0 itself. Keeping those fields in KML
    # would create duplicate bind_cols names; Name is only a display label.
    aoi <- sf::st_sf(Name = "Nakuru_validation", geometry = sf::st_as_sfc(bbox))
    kml <- file.path(staged_global, "InVector_GCS/small_nakuru.kml")
    inside(kml)
    if (file.exists(kml)) {
      old_aoi <- sf::st_transform(sf::st_read(kml, quiet = TRUE), 4326)
      if (nrow(old_aoi) != 1L ||
          !isTRUE(all.equal(as.numeric(sf::st_bbox(old_aoi)), as.numeric(bbox), tolerance = 1e-10)) ||
          !isTRUE(sf::st_equals(sf::st_zm(old_aoi), aoi, sparse = FALSE)[1, 1])) {
        stop("Previously staged AOI geometry differs; resume refused.")
      }
    }
    sf::st_write(aoi, kml, driver = "KML", delete_dsn = file.exists(kml), quiet = TRUE)
    writeLines(c("Fixture-only KML field adapter: Name plus original rectangle geometry.",
                 "ID/NAME_0 omitted because 5_harmonizer_v5.R supplies them itself.",
                 "Existing geometry verified equal before regenerating the staged KML.",
                 "Bounds remain 35.9..36.2 E, -0.45..-0.15 N; no source backup changes."),
               file.path(audit, "aoi_layout_adapter.txt"))
    aoi
  }
  if (resume) {
    # This resume assumes these are the untouched copies made by this harness;
    # names and sizes check completeness, not arbitrary same-size corruption.
    marker <- file.path(audit, "fixture_scope.txt")
    hashes <- file.path(audit, "preprocessing_source_md5.csv")
    if (!file.exists(marker) || !file.exists(hashes)) stop("No complete fixture identity marker; resume refused.")
    if (!paste("Backup:", backup) %in% readLines(marker, warn = FALSE)) stop("Fixture backup path differs; resume refused.")
    if (resume_staging && length(list.files(audit, pattern = "\\.status\\.txt$", recursive = TRUE))) {
      stop("Preprocessing has already started; --resume-staging is not permitted.")
    }
    recorded <- read.csv(hashes, colClasses = "character", check.names = FALSE)
    staged_scripts <- file.path(audit, "original_scripts", script_names)
    if (!identical(recorded$path, scripts) ||
        !identical(recorded$md5, unname(tools::md5sum(scripts))) ||
        !identical(recorded$md5, unname(tools::md5sum(staged_scripts)))) {
      stop("Original preprocessing scripts differ from recorded hashes; resume refused.")
    }
  }
  if (resume_staging) {
    pairs <- list(
      c(file.path(shared, "demand/demand_in"), file.path(demand, "demand_in")),
      c(file.path(shared, "admin_regions"), admin),
      c(source_scripts, file.path(audit, "original_scripts"))
    )
    for (pair in pairs) {
      inside(pair[2])
      copied_inventory <- inventory(pair[2])
      if (identical(pair[2], file.path(demand, "demand_in"))) {
        # This known fixture-derived VRT is regenerated from the original TIFF
        # below, so it is not part of the untouched source-copy inventory.
        copied_inventory <- copied_inventory[copied_inventory$path != "population_2020.vrt", , drop = FALSE]
        rownames(copied_inventory) <- NULL
      }
      if (!identical(inventory(pair[1]), copied_inventory)) {
        stop("Copied file names/sizes differ from source; resume refused: ", pair[2])
      }
    }
    message("Verified pre-preprocessing fixture; reusing complete demand/admin/script copies.")
  }
  if (resume_preprocessing) {
    status_files <- list.files(audit, pattern = "\\.status\\.txt$", full.names = TRUE)
    if (!length(status_files)) stop("No previous preparation attempt found; use --resume-staging.")
    simulation_markers <- file.path(country, c("Temp/k_all.csv", "Temp/rmax_all.csv", "Temp/i_st_all.csv"))
    results <- unlist(lapply(file.path(country, c("Out", "Debugging", "Summary_Report")),
                            list.files, recursive = TRUE, all.files = TRUE, no.. = TRUE))
    if (any(file.exists(simulation_markers)) || length(results)) {
      stop("Simulation/results evidence exists; preprocessing cleanup is not permitted.")
    }
    if (!identical(unname(tools::md5sum(generator_source)),
                   unname(tools::md5sum(file.path(audit, "original_scripts", generator_name))))) {
      stop("Staged demand generator differs from original source; resume refused.")
    }
    needed_staged <- c(file.path(staged_global, "InRaster", required_rasters),
                       file.path(staged_global, "parameters.csv"),
                       file.path(staged_global, "InVector_GCS/small_nakuru.kml"),
                       file.path(demand, "demand_in/wp_global1000m_gcs.tif"),
                       file.path(demand, "demand_in/mofuss_regions0.gpkg"))
    if (!all(file.exists(needed_staged))) stop("Completed raw staging is required before preprocessing resume.")
    if (resume_after_demand) {
      # tempdir() is fixed during R initialization; setting Sys.setenv here is
      # too late for the harmonizer's safe_st_write scratch directory.
      inside(tempdir())
      completed_names <- c("3_demand4IDW_v8.R", "4_produce_growth_and_stock_csv_v1.R")
      for (name in completed_names) {
        status_path <- file.path(audit, paste0(name, ".status.txt"))
        if (!file.exists(status_path) ||
            !grepl("^COMPLETE ", readLines(status_path, n = 1L, warn = FALSE))) {
          stop("Demand/growth checkpoint incomplete: ", name)
        }
      }
      checkpoint_inputs <- c(
        file.path(demand, "to_idw", c("locs_raster_w.tif", "locs_raster_v.tif", "BaU_fwch_w.csv", "BaU_fwch_v.csv")),
        file.path(demand, "pop_out/WorldPop_rururbR_2020.tif"),
        file.path(country, "In/DemandScenarios", c("BaU_fwch_w.csv", "BaU_fwch_v.csv")),
        file.path(country, "LULCC/TempTables", c("annos.txt", "Country.txt", "Country.csv", "Rpath.csv", "OStype.csv", "growth_parameters1.csv")),
        file.path(staged_global, "InRaster/modis_lc_type1_pcs.tif"),
        file.path(staged_global, "InTables/growth_parameters1.csv")
      )
      if (!all(file.exists(checkpoint_inputs)) || any(file.info(checkpoint_inputs)$size <= 0)) {
        stop("Required completed demand/growth products missing: ",
             paste(checkpoint_inputs[!file.exists(checkpoint_inputs) | is.na(file.info(checkpoint_inputs)$size) |
                                       file.info(checkpoint_inputs)$size <= 0], collapse = ", "))
      }
      checkpoint_manifest <- tempfile(pattern = "demand_checkpoint_", tmpdir = audit, fileext = ".csv")
      write.csv(data.frame(path = checkpoint_inputs, bytes = file.info(checkpoint_inputs)$size,
                           md5 = unname(tools::md5sum(checkpoint_inputs))),
                 checkpoint_manifest, row.names = FALSE)
    }
    snapshot <- tempfile(pattern = "prior_stage_status_", tmpdir = audit, fileext = ".txt")
    inside(snapshot)
    writeLines(c(paste("Preparation resume:", Sys.time()),
                 "Original eight script hashes and source backup identity verified.",
                 "No Monte Carlo matrices or dynamics outputs found; preparation cleanup is permitted.",
                 "Staged inputs are reused; no large-data recopy or raw raster recrop.",
                 unlist(lapply(status_files, function(p) c(basename(p), readLines(p, warn = FALSE))))), snapshot)
    message(if (resume_after_demand) "Verified demand/growth checkpoint; resuming at harmonization."
            else "Verified disposable fixture; restarting preprocessing with prior status preserved.")
  }
  for (p in c(country, demand, admin, scratch, audit, engine_temp, local_scratch,
              file.path(staged_global, "InRaster"),
              file.path(staged_global, "InVector_GCS"))) {
    inside(p)
    dir.create(p, recursive = TRUE, showWarnings = FALSE)
  }
  # Whole population and national boundaries are deliberately retained. The
  # demand code normalizes population to national totals BEFORE selecting AOI
  # locations; cropping these inputs to Nakuru would alter real demand.
  if (!resume) {
    message("Staging complete demand input and administrative datasets...")
    copy_tree(file.path(shared, "demand/demand_in"), file.path(demand, "demand_in"))
    copy_tree(file.path(shared, "admin_regions"), admin)
    copy_tree(source_scripts, file.path(audit, "original_scripts"))
  } else if (resume_staging) {
    writeLines(c(paste("Staging resumed:", Sys.time()),
                 "Backup path and original preprocessing MD5 hashes match the fixture marker.",
                 "Recursive source/copy relative file names and byte sizes match for demand/admin/scripts.",
                 "No preprocessing stage status exists; copied datasets are assumed untouched.",
                 "This completeness check does not detect arbitrary same-size data corruption.",
                 "Only small raster crops are regenerated; complete datasets are not recopied."),
               file.path(audit, "staging_resume.txt"))
  }
  if (!resume_preprocessing) {
  write.csv(data.frame(path = scripts, md5 = unname(tools::md5sum(scripts))),
            file.path(audit, "preprocessing_source_md5.csv"), row.names = FALSE)
  writeLines(c(paste("Backup:", backup), paste("R:", R.version.string),
               paste("Dinamica executable:", exe), paste("Processors:", cores),
               "Kenya/Nakuru AOI: 35.9..36.2 E, -0.45..-0.15 N; 1 km target resolution.",
               "2000 through 2050; 2 Monte Carlo runs. Reporting is not executed.",
               "Native raster crops retain a two-cell halo; wider landscape context is not represented.",
               "Population/national demand normalization inputs are copied complete.",
               "The two Windows discovery batches are replaced with explicit executable paths.",
               "No changes to numerical preprocessing formulas or supplied EGOML models."),
             file.path(audit, "fixture_scope.txt"))
  aoi <- prepare_aoi()
  terra::terraOptions(tempdir = scratch)
  raster_manifest <- list()
  for (name in required_rasters) {
    original <- file.path(source_global, "InRaster", name)
    src <- terra::rast(original)
    if (terra::nlyr(src) != 1L || !nzchar(terra::crs(src))) stop("Unexpected raster schema: ", original)
    projected_aoi <- terra::project(terra::vect(aoi), terra::crs(src))
    bounds <- as.vector(terra::ext(projected_aoi))
    spacing <- terra::res(src)
    bounds <- bounds + c(-2 * spacing[1], 2 * spacing[1], -2 * spacing[2], 2 * spacing[2])
    destination <- file.path(staged_global, "InRaster", name)
    clipped <- terra::crop(src, terra::ext(bounds), snap = "out")
    n_valid <- terra::global(!is.na(clipped), "sum", na.rm = TRUE)[1, 1]
    sparse_features <- c("borders_pcs.tif", "npa_pcs.tif", "hydrorivers7_pcs.tif",
                         "hydrolakes_pcs.tif", "grip5_pcs.tif", "gain_pcs.tif", "lossyear_pcs.tif")
    if (!is.finite(n_valid) || (n_valid == 0 && !name %in% sparse_features)) {
      stop("AOI has no valid observations in required continuous/land-cover input: ", original)
    }
    if (n_valid == 0) message("Preserving empty sparse-feature crop without changing NoData: ", name)
    type <- terra::datatype(src)
    if (length(type) != 1L || !nzchar(type)) stop("No native storage datatype for ", original)
    terra::writeRaster(clipped, destination, datatype = type,
                       gdal = c("COMPRESS=LZW"), overwrite = resume_staging)
    raster_manifest[[name]] <- data.frame(
      source = original, staged = destination, source_bytes = file.info(original)$size,
      source_mtime = as.character(file.info(original)$mtime), datatype = type,
      nrow = terra::nrow(clipped), ncol = terra::ncol(clipped), valid_cells = n_valid,
      xmin = terra::xmin(clipped), xmax = terra::xmax(clipped),
      ymin = terra::ymin(clipped), ymax = terra::ymax(clipped)
    )
  }
  write.csv(do.call(rbind, raster_manifest), file.path(audit, "raster_staging.csv"), row.names = FALSE)
  params <- read.csv(file.path(github, "parameters/parameters.csv"),
                     colClasses = "character", check.names = FALSE, na.strings = "")
  overrides <- c(GEE_country = "KEN", GEE_scale = "1000", byregion = "Country",
                 region2BprocessedCtry = "Kenya", region2BprocessedCtry_iso = "KEN",
                 aoi_poly = "1", aoi_poly_file = "small_nakuru.kml", subcountry = "0",
                 start_year = "2000", end_year = "2050", monte_carlo_runs = "2", mapscale = "1000",
                 idw_debug = "NO", friction = "R")
  for (key in names(overrides)) {
    i <- which(params$Var == key)
    if (length(i) != 1L) stop("Expected exactly one template parameter: ", key)
    params$ParCHR[i] <- overrides[[key]]
  }
  parameter_file <- file.path(staged_global, "parameters.csv")
  write.csv(params, parameter_file, row.names = FALSE, na = "")
  write.csv(data.frame(Var = names(overrides), ParCHR = unname(overrides)),
            file.path(audit, "parameter_overrides.csv"), row.names = FALSE)
  } else {
    parameter_file <- file.path(staged_global, "parameters.csv")
    params <- read.csv(parameter_file, colClasses = "character", check.names = FALSE)
    expected <- c(region2BprocessedCtry_iso = "KEN", aoi_poly = "1",
                  aoi_poly_file = "small_nakuru.kml", GEE_scale = "1000", mapscale = "1000",
                  start_year = "2000", end_year = "2050", monte_carlo_runs = "2",
                  scenario_ver = "BaU1_v2", idw_debug = "NO", friction = "R")
    for (key in names(expected)) {
      observed <- params$ParCHR[params$Var == key]
      if (!identical(observed, unname(expected[key]))) stop("Fixture parameter changed: ", key)
    }
    prepare_aoi()
  }
  # The supplied population TIFF calls its band GlobalWorldPop, while the
  # original demand code explicitly selects a column ending in pop_2020.
  # A single-band VRT is a metadata-only alias; it still reads the complete,
  # unchanged TIFF. No crop, resampling, conversion, or population scaling.
  population_tif <- file.path(demand, "demand_in/wp_global1000m_gcs.tif")
  population_vrt <- file.path(demand, "demand_in/population_2020.vrt")
  inside(population_vrt)
  population <- terra::rast(population_tif)
  if (terra::nlyr(population) != 1L) stop("Population metadata adapter requires exactly one original band.")
  sf::gdal_utils("translate", source = population_tif, destination = population_vrt,
                 options = c("-of", "VRT", "-b", "1"), quiet = TRUE)
  vrt_lines <- readLines(population_vrt, warn = FALSE)
  band_start <- grep("<VRTRasterBand\\b", vrt_lines, perl = TRUE)
  band_end <- grep("</VRTRasterBand>", vrt_lines, fixed = TRUE)
  if (length(band_start) != 1L || length(band_end) != 1L || band_end <= band_start) {
    stop("Unexpected generated population VRT structure.")
  }
  descriptions <- grep("<Description>.*</Description>", vrt_lines)
  descriptions <- descriptions[descriptions > band_start & descriptions < band_end]
  if (length(descriptions) > 1L) stop("Unexpected multiple population band descriptions.")
  if (length(descriptions)) vrt_lines[descriptions] <- "    <Description>pop_2020</Description>"
  else vrt_lines <- append(vrt_lines, "    <Description>pop_2020</Description>", after = band_start)
  writeLines(vrt_lines, population_vrt)
  aliased_population <- terra::rast(population_vrt)
  if (!identical(names(aliased_population), "pop_2020") ||
      !terra::compareGeom(population, aliased_population, stopOnError = FALSE)) {
    stop("Population VRT did not preserve geometry and provide the expected band name.")
  }
  check_cells <- unique(as.integer(round(seq(1, terra::ncell(population), length.out = 257))))
  original_values <- terra::extract(population, check_cells)[[1]]
  aliased_values <- terra::extract(aliased_population, check_cells)[[1]]
  if (!identical(original_values, aliased_values)) stop("Population VRT sample values differ from the original TIFF.")
  params <- read.csv(parameter_file, colClasses = "character", check.names = FALSE)
  pop_index <- which(params$Var == "pop_map_name")
  if (length(pop_index) != 1L || !params$ParCHR[pop_index] %in%
      c("wp_global1000m_gcs.tif", "population_2020.vrt")) stop("Unexpected population input parameter.")
  params$ParCHR[pop_index] <- "population_2020.vrt"
  write.csv(params, parameter_file, row.names = FALSE, na = "")
  writeLines(c(paste("Original TIFF:", population_tif),
               paste("Original band name:", names(population)),
               paste("Fixture VRT:", population_vrt), "Alias band name: pop_2020",
               "GDAL VRT references original band 1, with only band Description changed afterward.",
               "Complete national population retained; original TIFF is unchanged.",
               "Geometry comparison and 257 deterministic sample cells matched exactly.",
               "Fixture parameter pop_map_name is population_2020.vrt."),
             file.path(audit, "population_band_alias.txt"))
  if (stage_only) {
    message("Staging complete; preprocessing has NOT run: ", output)
    return(invisible(output))
  }
  # Original harmonizer helpers honor this variable before tempdir(). Override
  # an inherited location for this process only, and retain the deletion guard.
  inside(tempdir())
  inside(local_scratch)
  previous_local_scratch <- Sys.getenv("MOFUSS_LOCAL_SCRATCH", unset = NA_character_)
  on.exit({
    if (is.na(previous_local_scratch)) Sys.unsetenv("MOFUSS_LOCAL_SCRATCH")
    else Sys.setenv(MOFUSS_LOCAL_SCRATCH = previous_local_scratch)
  }, add = TRUE)
  Sys.setenv(MOFUSS_LOCAL_SCRATCH = norm(local_scratch, TRUE))
  inside(Sys.getenv("MOFUSS_LOCAL_SCRATCH"))
  writeLines(c(paste("R startup tempdir:", norm(tempdir(), TRUE)),
               paste("MOFUSS_LOCAL_SCRATCH:", Sys.getenv("MOFUSS_LOCAL_SCRATCH")),
               "Both locations verified inside this fixture before sourcing any original stage.",
               "MOFUSS_LOCAL_SCRATCH override is process-local and restored on exit."),
             file.path(audit, "private_r_scratch.txt"))

  # Isolate script variables and keep source/input paths explicitly separated.
  e <- new.env(parent = globalenv())
  e$webmofuss <- 1L
  e$githubdir <- github
  e$countrydir <- country
  e$demanddir <- demand
  e$admindir <- admin
  e$emissionsdir <- file.path(output, "emissions")
  e$rTempdir <- scratch
  e$parameters_file_path <- parameter_file
  e$parameters_file <- "parameters.csv"
  e$scriptsmofuss <- paste0(source_scripts, "/")
  e$delimiter <- ","
  if (resume_after_demand) {
    # Restore stable directory identity only. Stage 5 reconstructs AOI vectors
    # and settings from disk, and stage 6a reads annual demand tables itself.
    # Never serialize/restore live terra pointers or stale R temp rasters.
    saved_country <- readLines(file.path(country, "LULCC/TempTables/Country.txt"), warn = FALSE)
    if (!identical(saved_country, "Global")) stop("Unexpected staged country directory identity.")
    e$country_name <- saved_country
    e$country <- saved_country
    e$os <- Sys.info()[["sysname"]]
  }
  # Guard the inherited scripts' unqualified destructive operations. All their
  # numerical outputs are rooted in countrydir/demanddir/admindir/rTempdir.
  e$unlink <- function(x, recursive = FALSE, force = FALSE, expand = TRUE) {
    inside(x)
    base::unlink(x, recursive = recursive, force = force, expand = expand)
  }
  e$file.remove <- function(...) {
    paths <- c(...)
    inside(paths)
    base::file.remove(paths)
  }
  e$file.copy <- function(from, to, ...) {
    inside(to)
    base::file.copy(from, to, ...)
  }
  e$file.rename <- function(from, to) {
    inside(from)
    inside(to)
    base::file.rename(from, to)
  }
  e$setwd <- function(dir) {
    inside(dir)
    base::setwd(dir)
  }
  e$shell <- function(cmd, ...) {
    expected <- file.path(country, "LULCC/RpathOSsystem2.bat")
    if (!identical(norm(cmd, TRUE), norm(expected, TRUE))) stop("Unexpected shell call blocked: ", cmd)
    writeLines(paste0('"', norm(r_exe, TRUE), '"'), file.path(country, "LULCC/TempTables/Rpath.txt"))
    writeLines("64", file.path(country, "LULCC/TempTables/OS_type.txt"))
    invisible(0L)
  }
  model_order <- c("1_Matrix_gain", "1_Matrix_loss", "2_Distance_calc",
                   "3_Ranges_gain", "3_Ranges_loss", "4_Weights_gain", "4_Weights_loss",
                   "5_Correlation_gain", "5_Correlation_loss", "6_Probability_gain",
                   "6_Probability_loss", "7_Simulation_gain", "7_Simulation_loss",
                   "8_Validation_gain", "8_Validation_loss")
  e$system <- function(command, ...) {
    expected <- file.path(country, "LULCC/lucdynamics_luc1/LULCC_blackbox_scripts2.bat")
    if (!identical(norm(command, TRUE), norm(expected, TRUE))) stop("Unexpected system call blocked: ", command)
    # Dinamica 2.4 compiles temporary DLLs; a private TEMP prevents collisions
    # with unrelated user runs. Change only this R process/its children.
    previous_temp <- Sys.getenv(c("TEMP", "TMP"), unset = NA_character_)
    on.exit({
      for (key in names(previous_temp)) {
        if (is.na(previous_temp[[key]])) Sys.unsetenv(key)
        else do.call(Sys.setenv, setNames(list(previous_temp[[key]]), key))
      }
    }, add = TRUE)
    Sys.setenv(TEMP = engine_temp, TMP = engine_temp)
    model_dir <- dirname(expected)
    for (name in model_order) {
      model <- file.path(model_dir, paste0(name, "_win241.egoml"))
      if (!file.exists(model)) stop("Missing original calibration model: ", model)
      logfile <- file.path(audit, paste0(name, ".log"))
      message("Native LULCC preprocessing: ", name, " (", cores, " processors)")
      result <- base::system2(exe, c("-processors", cores, "-log-level", "4", shQuote(model)),
                              stdout = logfile, stderr = logfile, wait = TRUE)
      if (!identical(as.integer(result), 0L)) stop("LULCC preprocessing failed: ", name, "; see ", logfile)
    }
    invisible(0L)
  }
  e$system2 <- function(...) stop("Unexpected system2 call blocked.")
  e$download.file <- function(...) stop("Network download blocked in fixture preparation.")
  # The supplied demand script sets optimizeD=0. Fail closed if its optional
  # keep() branch becomes active, rather than allowing it to discard guards.
  e$keep <- function(...) stop("Unexpected variable-cleanup branch blocked; optimizeD must remain 0.")
  suppressPackageStartupMessages(library(readr))
  oldwd <- getwd()
  on.exit(base::setwd(oldwd), add = TRUE)
  ensure_bau_demand <- function() {
    destination <- file.path(demand, "demand_in/demand_bau1_v2.csv")
    if (file.exists(destination)) return(invisible(NULL))
    bridge <- file.path(staged_global, "demand/demand_in")
    inside(bridge)
    dir.create(bridge, recursive = TRUE, showWarnings = FALSE)
    inputs <- c("A_LMIC_Estimates_2050_popmedian_original.xlsx", "demand_parameters.csv")
    from <- file.path(demand, "demand_in", inputs)
    if (!all(file.exists(from)) || !all(file.copy(from, bridge, overwrite = TRUE))) {
      stop("Supplied WHO workbook/demand parameters missing; cannot build real BaU demand.")
    }
    original <- file.path(audit, "original_scripts", generator_name)
    if (!identical(unname(tools::md5sum(original)), unname(tools::md5sum(generator_source)))) {
      stop("Demand constructor differs from original backup.")
    }
    write.csv(data.frame(path = generator_source, md5 = unname(tools::md5sum(original))),
              file.path(audit, "demand_generator_md5.csv"), row.names = FALSE)
    status <- file.path(audit, paste0(generator_name, ".status.txt"))
    writeLines(paste("START", Sys.time()), status)
    message("Constructing missing BaU table with supplied original WHO demand constructor.")
    source(original, local = e, chdir = FALSE, echo = FALSE)
    generated <- file.path(bridge, "demand_bau1_v2.csv")
    if (!file.exists(generated) || !file.copy(generated, destination, overwrite = FALSE)) {
      stop("Original demand constructor did not provide the expected BaU table.")
    }
    writeLines(c(paste("COMPLETE", Sys.time()),
                 "Original numerical constructor unchanged; nested input/output layout bridged by file copies."), status)
    invisible(NULL)
  }
  selected_stages <- if (resume_after_demand) which(seq_along(scripts) >= match("5_harmonizer_v5.R", script_names))
                     else seq_along(scripts)
  for (i in selected_stages) {
    base::setwd(country)
    if (identical(script_names[i], "2c_demand_tables_v5.R")) ensure_bau_demand()
    message("Preprocessing ", i, "/", length(scripts), ": ", script_names[i])
    original <- file.path(audit, "original_scripts", script_names[i])
    status <- file.path(audit, paste0(script_names[i], ".status.txt"))
    writeLines(paste("START", Sys.time()), status)
    source(original, local = e, chdir = FALSE, echo = FALSE)
    writeLines(paste("COMPLETE", Sys.time()), status)
  }
  writeLines(c("The original preprocessing stages completed.",
               "IDW generation, frozen Monte Carlo draws, simulation and reports remain separate steps.",
               "This fixture is NOT a proof that a server model replacement is compatible."),
             file.path(audit, "PREPROCESSING_COMPLETE.txt"))
  message("Preprocessing completed: ", country)
  invisible(country)
}

main()

# MoFuSS
# Version 4
# Date: Aug 2026
# EGOML dependency bundle: V8

# 2dolist

# Internal parameters

# Load libraries ----
library(msm)
library(raster)
library(tidyverse)
library(readxl)
library(readr)
library(tibble)

# A zero standard deviation represents a fixed parameter. msm::rtnorm() is not
# defined for sd = 0, so return the fixed value instead of generating NaNs.
draw_truncated_or_fixed <- function(
  n, mean_value, sd_value, lower = -Inf, upper = Inf
) {
  if (!is.finite(mean_value) || !is.finite(sd_value) || sd_value < 0) {
    stop("Monte Carlo means/SDs must be finite and SDs non-negative.")
  }
  if (sd_value == 0) {
    return(rep(min(max(mean_value, lower), upper), n))
  }
  msm::rtnorm(
    n, mean = mean_value, sd = sd_value, lower = lower, upper = upper
  )
}

mc_batch_ready_filename <- "mc_batch_ready.csv"

# Publish one immutable description of the complete Monte Carlo input batch.
# The file is created beside the tables with an atomic rename, so a concurrent
# CCTS process can observe either no ready batch or the complete ready batch,
# never a partially written manifest.
write_mc_batch_ready <- function(
  temp_dir, mc_runs, start_year, end_year, scenario_ver, byregion,
  geography, uncapped_regrowth, luc_version, agb_version
) {
  category_name <- sprintf("LULC_Categories%d.csv", luc_version)
  batch_files <- c(
    "i_st_all.csv", "k_all.csv", "rmax_all.csv",
    "Harvest_pixels_V.csv", "Harvest_pixels_W.csv",
    "Prune_factor_V.csv", "Prune_factor_W.csv",
    category_name
  )
  paths <- file.path(temp_dir, batch_files)
  missing <- batch_files[!file.exists(paths)]
  if (length(missing)) {
    stop(
      "Cannot publish the MC batch-ready manifest; missing file(s): ",
      paste(missing, collapse = ", ")
    )
  }

  for (name in batch_files[seq_len(7L)]) {
    path <- file.path(temp_dir, name)
    tab <- read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
    keys <- suppressWarnings(as.integer(tab[[1L]]))
    if (nrow(tab) != mc_runs || ncol(tab) < 2L ||
        !identical(keys, seq_len(mc_runs))) {
      stop(
        "Cannot publish the MC batch-ready manifest; invalid run keys/shape in ",
        path, "."
      )
    }
    numeric_values <- lapply(
      tab[-1L], function(x) suppressWarnings(as.numeric(x))
    )
    if (any(!vapply(
      numeric_values,
      function(x) length(x) == mc_runs && all(is.finite(x)),
      logical(1)
    ))) {
      stop(
        "Cannot publish the MC batch-ready manifest; non-numeric or ",
        "non-finite values in ", path, "."
      )
    }
  }

  info <- file.info(paths)
  hashes <- unname(tools::md5sum(paths))
  if (anyNA(info$size) || any(info$size <= 0) ||
      anyNA(hashes) || any(!grepl("^[0-9a-f]{32}$", hashes))) {
    stop("Cannot publish the MC batch-ready manifest; file metadata/hash failure.")
  }

  created_utc <- format(Sys.time(), tz = "UTC", "%Y-%m-%dT%H:%M:%SZ")
  batch_id <- sprintf(
    "%s-pid%d", format(Sys.time(), tz = "UTC", "%Y%m%dT%H%M%SZ"),
    Sys.getpid()
  )
  manifest <- data.frame(
    schema_version = 1L,
    status = "ready",
    batch_id = batch_id,
    created_utc = created_utc,
    generated_by = "rnorm_v8.R",
    script_bundle = "V8",
    scenario_dir = normalizePath(getwd(), winslash = "/", mustWork = TRUE),
    scenario_ver = scenario_ver,
    byregion = byregion,
    geography = geography,
    start_year = start_year,
    end_year = end_year,
    monte_carlo_runs = mc_runs,
    uncapped_regrowth = uncapped_regrowth,
    lulc_version = luc_version,
    agb_version = agb_version,
    file = batch_files,
    file_size_bytes = as.numeric(info$size),
    md5 = hashes,
    stringsAsFactors = FALSE
  )

  target <- file.path(temp_dir, mc_batch_ready_filename)
  temporary <- tempfile(
    pattern = ".mc_batch_ready_", tmpdir = temp_dir, fileext = ".csv"
  )
  on.exit(if (file.exists(temporary)) unlink(temporary, force = TRUE), add = TRUE)
  write.csv(manifest, temporary, row.names = FALSE)
  reread <- read.csv(temporary, check.names = FALSE, stringsAsFactors = FALSE)
  if (nrow(reread) != length(batch_files) ||
      !identical(as.character(reread$file), batch_files) ||
      !identical(tolower(as.character(reread$md5)), hashes)) {
    stop("MC batch-ready manifest verification failed before publication.")
  }
  if (file.exists(target)) {
    stop(
      "Refusing to replace an existing ready manifest in place: ", target,
      ". A normal rnorm_v8 run must initialize a new Temp directory first."
    )
  }
  if (!file.rename(temporary, target)) {
    stop("Cannot atomically publish ", target, ".")
  }
  cat(sprintf("[OK] Current MC batch ready: %s (%s)\n", batch_id, target))
  invisible(manifest)
}

# Read in the arguments listed at the command line in Dinamica EGO'S "Run external process"
args=(commandArgs(TRUE))

# "args" is now a list of character vectors.
# First check to see if arguments are passed.
# Then cycle through each element of the list and evaluate the expressions.
if(length(args)==0){
  print("No arguments supplied by DINAMICA.")
  ## Supply default values here (to be used when running the script through R directly)
  MC=30 # MonteCarlo runs
  IT=2010 # Initial year
  K_MC=1
  TOF_MC=1
  Ini_st_MC=1
  Ini_st.factor.percentage=75
  COVER_MAP=1
  rmax_MC=1
  DEF_FW=1
  IL=48 # Iteration length in week - each year = 48 weeks
  # STdyn=20 # Simulation length set by dinamica, but cycles in the repeat functor is STdyn+1 as 2 cycles are needed for 1 year: 1jan->31dec
  Harv.Pix.W=25400
  Prune.W=1
  Harv.Pix.V=25400
  Prune.V=1
  Harv.Pix_MC=10000
  Prune_MC=1
  # Subset_locs=0
  Harvestable_W="Not supplied" 
  Harvestable_V="Not supplied"
  Histograms.per.Fig_FOR=50
  Histograms.per.Fig_TOF=50
  AGBmap=1
  OSType=64
  BaUvsICS="BaU"
  LUCmap_v = 1 
  AGBmap_v = 1
  CTrees = 1
  DryRun = 0
  
}else{
  for(i in 1:length(args)){
    eval(parse(text=args[[i]]))
  }
}

dinamica_stdyn <- if (exists("STdyn", inherits = FALSE)) {
  suppressWarnings(as.integer(STdyn))
} else {
  NA_integer_
}

# Fail before removing any prior result if this is not a complete MoFuSS
# scenario root or if Dinamica supplied an inconsistent run configuration.
required_root_inputs <- c(
  "LULCC/TempTables/Country.csv",
  "LULCC/TempTables/parameters_dinamica.csv",
  "LULCC/TempRaster/Mask_c.tif"
)
missing_root_inputs <- required_root_inputs[!file.exists(required_root_inputs)]
if (length(missing_root_inputs)) {
  stop(
    "Refusing to initialize Monte Carlo outputs outside a valid MoFuSS ",
    "scenario root. Missing: ", paste(missing_root_inputs, collapse = ", ")
  )
}

MC <- suppressWarnings(as.integer(MC))
IT <- suppressWarnings(as.integer(IT))
IL <- suppressWarnings(as.numeric(IL))
if (length(MC) != 1L || is.na(MC) || MC < 1L) {
  stop("MC must be one positive integer.")
}
if (length(IT) != 1L || is.na(IT)) {
  stop("IT must be one integer model-start year.")
}
if (length(IL) != 1L || !is.finite(IL) || IL <= 0) {
  stop("IL must be one positive iteration length in weeks.")
}

country_table <- read.csv(
  "LULCC/TempTables/Country.csv",
  check.names = FALSE,
  stringsAsFactors = FALSE
)
if (!all(c("Key.", "Country") %in% names(country_table))) {
  stop("Country.csv must contain Key. and Country columns.")
}
country_rows <- country_table[as.character(country_table$Key.) == "1", , drop = FALSE]
if (nrow(country_rows) != 1L) {
  stop("Country.csv must contain exactly one Key.=1 row with a Country value.")
}
country_name <- trimws(as.character(country_rows$Country[[1]]))
if (!nzchar(country_name)) stop("Country.csv contains an empty Country value.")

parameters_directory <- file.path(
  getwd(), "LULCC", "DownloadedDatasets", paste0("SourceData", country_name)
)
parameters_name <- list.files(
  path = parameters_directory,
  pattern = "^parameters.*\\.csv$",
  full.names = TRUE
)
if (length(parameters_name) != 1L) {
  stop(
    "Expected exactly one parameters*.csv in ", parameters_directory,
    "; found ", length(parameters_name), "."
  )
}
first_line <- readLines(parameters_name, n = 1L, warn = FALSE)
sep <- if (grepl(";", first_line, fixed = TRUE)) ";" else ","
preflight_parameters <- read.csv(
  parameters_name,
  sep = sep,
  check.names = FALSE,
  stringsAsFactors = FALSE
)
if (!all(c("Var", "ParCHR") %in% names(preflight_parameters))) {
  stop("The parameter table must contain Var and ParCHR columns.")
}
parameter_value <- function(key) {
  value <- preflight_parameters$ParCHR[preflight_parameters$Var == key]
  if (length(value) != 1L || is.na(value) || !nzchar(trimws(value))) {
    stop("Expected exactly one non-empty parameter value for: ", key)
  }
  trimws(as.character(value))
}
configured_start <- suppressWarnings(as.integer(parameter_value("start_year")))
configured_end <- suppressWarnings(as.integer(parameter_value("end_year")))
configured_mc <- suppressWarnings(as.integer(parameter_value("monte_carlo_runs")))
if (anyNA(c(configured_start, configured_end, configured_mc))) {
  stop("start_year, end_year, and monte_carlo_runs must be integers.")
}
if (IT != configured_start) {
  stop("Dinamica IT does not match parameters.csv start_year.")
}
if (MC != configured_mc) {
  stop("Dinamica MC does not match parameters.csv monte_carlo_runs.")
}
if (!is.na(dinamica_stdyn) && dinamica_stdyn != configured_end - configured_start) {
  stop("Dinamica STdyn does not match end_year - start_year in parameters.csv.")
}
if (!(configured_end - configured_start) %in% c(20L, 30L, 35L, 40L, 50L)) {
  stop("Supported simulation lengths are 20, 30, 35, 40, or 50 years.")
}

DryRun <- if (exists("DryRun", inherits = FALSE)) {
  suppressWarnings(as.integer(DryRun))
} else {
  0L
}
if (length(DryRun) != 1L || is.na(DryRun) || !DryRun %in% c(0L, 1L)) {
  stop("DryRun must be 0 or 1.")
}

publish_current_mc_batch <- function() {
  byregion_value <- parameter_value("byregion")
  geography_key <- if (tolower(byregion_value) == "regional") {
    "region2BprocessedReg"
  } else if (tolower(byregion_value) == "country") {
    "region2BprocessedCtry_iso"
  } else {
    stop("Unsupported byregion value while publishing the MC batch: ", byregion_value)
  }
  luc_version <- suppressWarnings(as.integer(LUCmap_v))
  agb_version <- suppressWarnings(as.integer(AGBmap_v))
  uncapped_value <- suppressWarnings(as.integer(parameter_value("uncapped_regrowth")))
  if (anyNA(c(luc_version, agb_version, uncapped_value))) {
    stop("LUCmap_v, AGBmap_v, and uncapped_regrowth must be integers.")
  }
  write_mc_batch_ready(
    temp_dir = "Temp",
    mc_runs = MC,
    start_year = configured_start,
    end_year = configured_end,
    scenario_ver = parameter_value("scenario_ver"),
    byregion = byregion_value,
    geography = parameter_value(geography_key),
    uncapped_regrowth = uncapped_value,
    luc_version = luc_version,
    agb_version = agb_version
  )
}

PublishExistingBatch <- if (exists("PublishExistingBatch", inherits = FALSE)) {
  suppressWarnings(as.integer(PublishExistingBatch))
} else {
  0L
}
if (length(PublishExistingBatch) != 1L || is.na(PublishExistingBatch) ||
    !PublishExistingBatch %in% c(0L, 1L)) {
  stop("PublishExistingBatch must be 0 or 1.")
}
if (PublishExistingBatch == 1L) {
  scenario_ver <- parameter_value("scenario_ver")
  if (!grepl("^bau", scenario_ver, ignore.case = TRUE)) {
    stop("PublishExistingBatch is only valid in a completed BAU scenario.")
  }
  end_code <- configured_end - configured_start + 1L
  incomplete <- integer()
  for (id in seq_len(MC)) {
    run_dir <- paste0("debugging_", id)
    files <- if (dir.exists(run_dir)) list.files(run_dir) else character()
    pattern <- sprintf("^Growth_less_harv0*%d(?:\\.[^.]+)?$", end_code)
    if (!any(grepl(pattern, files, ignore.case = TRUE, perl = TRUE))) {
      incomplete <- c(incomplete, id)
    }
  }
  if (length(incomplete)) {
    stop(
      "Cannot adopt an existing BAU MC batch until every dynamic run is complete; ",
      "missing endpoint run(s): ", paste(incomplete, collapse = ", ")
    )
  }
  publish_current_mc_batch()
  cat("[OK] Existing completed BAU batch adopted; no simulation outputs changed.\n")
  quit(save = "no", status = 0L, runLast = FALSE)
}
if (DryRun == 1L) {
  cat(
    sprintf(
      "[DRY-RUN] rnorm_v8 preflight passed: %d MC runs, %d-%d; no outputs removed or created.\n",
      MC, configured_start, configured_end
    )
  )
  quit(save = "no", status = 0L, runLast = FALSE)
}

# Destructive initialization starts only after the complete preflight above.
debugging_to_remove <- list.files(
  path = ".", pattern = "^debugging_[0-9]+$", full.names = TRUE,
  recursive = FALSE
)
invisible(lapply(debugging_to_remove, unlink, recursive = TRUE, force = TRUE))

unlink("Debugging", recursive = TRUE, force = TRUE)
unlink("Temp", recursive = TRUE, force = TRUE)
dir.create("Debugging", showWarnings = FALSE)
dir.create("Temp", showWarnings = FALSE)

unlink("Summary_Report//Mofuss_Summary_Report.pdf", force = TRUE)
unlink("LaTeX//Mofuss_Summary_Report.pdf", force = TRUE)
unlink("Mofuss_Summary_Report.pdf", force = TRUE)
unlink("LaTeX//InputPara.csv", force = TRUE)
unlink("LaTeX//NRBTable.csv", force = TRUE)
unlink("LaTeX//fNRBTable.csv", force = TRUE)
unlink("LaTeX//SumTable.csv", force = TRUE)

unlink("Out//*", recursive = TRUE,force=TRUE)
unlink("HTML_animation//*", recursive = TRUE,force=TRUE)	
dir.create("Out", recursive = TRUE, showWarnings = FALSE)
dir.create("HTML_animation", recursive = TRUE, showWarnings = FALSE)
OutDir<-"Out"

for (i in 1:MC) {
  print(i)
  unlink(paste0("debugging_",i), recursive = TRUE,force=TRUE)
}
Sys.sleep(15)
for (i in 1:MC) {
  print(i)
  dir.create(paste0("debugging_",i))
}


# Read the CSV file with the detected separator
country_parameters <- read_delim(parameters_name, delim = sep)

# Print the tibble (up to 30 rows)
print(as_tibble(country_parameters), n = 30)

# if (webmofuss == 1) {
#   # Read parameters table in webmofuss
#   country_parameters <- read_csv(parameters_file_path)
# } else if(webmofuss == 0) {
#   # Read parameters table (recognizing the delimiter)
#   detect_delimiter <- function(file_path) {
#     # Read the first line of the file
#     first_line <- readLines(file_path, n = 1)
#     # Check if the first line contains ',' or ';'
#     if (grepl(";", first_line)) {
#       return(";")
#     } else {
#       return(",")
#     }
#   }
#   # Detect the delimiter
#   delimiter <- detect_delimiter(parameters_file_path)
#   # Read the CSV file with the detected delimiter
#   country_parameters <- read_delim(parameters_file_path, delim = delimiter)
#   print(tibble::as_tibble(country_parameters), n=100)
# }

# country_parameters %>%
#   dplyr::filter(Var == "LULCt1map") %>%
#   pull(ParCHR) -> LULCt1map
# 
# country_parameters %>%
#   dplyr::filter(Var == "LULCt2map") %>%
#   pull(ParCHR) -> LULCt2map
# 
# country_parameters %>%
#   dplyr::filter(Var == "LULCt3map") %>%
#   pull(ParCHR) -> LULCt3map
# 
# country_parameters %>%
#   dplyr::filter(Var == "AGB1map") %>%
#   pull(ParCHR) -> AGB1map
# 
# country_parameters %>%
#   dplyr::filter(Var == "AGB2map") %>%
#   pull(ParCHR) -> AGB2map
# 
# country_parameters %>%
#   dplyr::filter(Var == "AGB3map") %>%
#   pull(ParCHR) -> AGB3map

country_parameters %>%
  dplyr::filter(Var == "end_year") %>%
  pull(ParCHR) %>%
  as.integer(.) -> end_year
STdyn <- end_year - IT

if (LUCmap_v == 1) {
  
  if (file.exists("LULCC/TempTables/growth_parameters1.csv") == TRUE) {
    # Check the first line of the file to determine the delimiter
    first_linegp1 <- readLines("LULCC/TempTables/growth_parameters1.csv", n = 1)
    # Determine the delimiter based on the first line
    delimitergp1 <- ifelse(grepl(";", first_linegp1), ";", ",")
    # Read the CSV file with the appropriate delimiter
    data_all1 <- read.csv("LULCC/TempTables/growth_parameters1.csv", sep = delimitergp1)
    # %>%
    #   mutate(
    #     `Key*` = as.numeric(`Key*`),
    #     rmax = as.numeric(rmax),
    #     rmaxSD = as.numeric(rmaxSD),
    #     K = as.numeric(K),
    #     KSD = as.numeric(KSD),
    #     TOF = as.numeric(TOF)
    #   )
    data_FOR1<-subset(data_all1, data_all1$TOF==0)
    data_TOF1<-subset(data_all1, data_all1$TOF==1)
    #Adjusts for raster resolution - May2023
    data_TOF1r <- data_TOF1
    rasters_res<-xres(raster("LULCC/TempRaster//Mask_c.tif"))
    max_tot1<-nrow(data_all1)
    
    LULC_Categories1<-as.data.frame(data_all1[ ,1])
    colnames(LULC_Categories1)<-("x")
    LULC_Categories1=data.frame(Key=c(1:max_tot1),LULC_Categories1)
    write.csv(LULC_Categories1,"Temp/LULC_Categories1.csv",row.names = FALSE)
  }
  
  # dataTOFvsFOR_1 <- read.csv ("LULCC/TempTables/TOFvsFOR_Categories1.csv")
  # write.csv(dataTOFvsFOR_1,"LULCC/TempTables/TOFvsFOR_Categories.csv",row.names = FALSE)
  
  # TOFvsFOR_mask1 <- raster("LULCC/TempRaster/TOFvsFOR_mask1.tif")
  # writeRaster(TOFvsFOR_mask1, "LULCC/TempRaster/TOFvsFOR_mask.tif", datatype="INT2S", overwrite=TRUE)
  
} else if (LUCmap_v == 2) {
  
  if (file.exists("LULCC/TempTables/growth_parameters2.csv") == TRUE) {
    # Check the first line of the file to determine the delimiter
    first_linegp2 <- readLines("LULCC/TempTables/growth_parameters2.csv", n = 1)
    # Determine the delimiter based on the first line
    delimitergp2 <- ifelse(grepl(";", first_linegp2), ";", ",")
    # Read the CSV file with the appropriate delimiter
    data_all2 <- read.csv("LULCC/TempTables/growth_parameters2.csv", sep = delimitergp2)
    # %>%
    #   mutate(
    #     `Key*` = as.numeric(`Key*`),
    #     rmax = as.numeric(rmax),
    #     rmaxSD = as.numeric(rmaxSD),
    #     K = as.numeric(K),
    #     KSD = as.numeric(KSD),
    #     TOF = as.numeric(TOF)
    #   )
    
    data_FOR2<-subset(data_all2, data_all2$TOF==0)
    data_TOF2<-subset(data_all2, data_all2$TOF==1)
    #Adjusts for raster resolution - May2023
    data_TOF2r <- data_TOF2
    rasters_res<-xres(raster("LULCC/TempRaster//Mask_c.tif"))
    max_tot2<-nrow(data_all2)
    
    LULC_Categories2<-as.data.frame(data_all2[ ,1])
    colnames(LULC_Categories2)<-("x")
    LULC_Categories2=data.frame(Key=c(1:max_tot2),LULC_Categories2)
    write.csv(LULC_Categories2,"Temp/LULC_Categories2.csv",row.names = FALSE)
  }
  
  # dataTOFvsFOR_2 <- read.csv ("LULCC/TempTables/TOFvsFOR_Categories2.csv")
  # write.csv(dataTOFvsFOR_2,"LULCC/TempTables/TOFvsFOR_Categories.csv",row.names = FALSE)
  
  # TOFvsFOR_mask2 <- raster("LULCC/TempRaster/TOFvsFOR_mask2.tif")
  # writeRaster(TOFvsFOR_mask2, "LULCC/TempRaster/TOFvsFOR_mask.tif", datatype="INT2S", overwrite=TRUE)
  
} else if (LUCmap_v == 3) {
  
  # if (file.exists("LULCC/TempTables/growth_parameters3.csv") == TRUE) {
  #   data_semicolon<-read.csv("LULCC/TempTables/growth_parameters3.csv", sep=";", header=T)
  #   data_comma<-read.csv("LULCC/TempTables/growth_parameters3.csv", sep=",", header=T)
  #   if (is.null(data_semicolon$TOF[1])) { 
  #     data_all3<-data_comma
  #   } else {
  #     data_all3<-data_semicolon
  #   }
  #   data_FOR3<-subset(data_all3, data_all3$TOF==0)
  #   data_TOF3<-subset(data_all3, data_all3$TOF==1)
  #   max_tot3<-nrow(data_all3)
  #   
  #   LULC_Categories3<-as.data.frame(data_all3[ ,1])
  #   colnames(LULC_Categories3)<-("x")
  #   LULC_Categories3=data.frame(Key=c(1:max_tot3),LULC_Categories3)
  #   write.csv(LULC_Categories3,"Temp/LULC_Categories3.csv",row.names = FALSE)
  # }
  
  # dataTOFvsFOR_3 <- read.csv ("LULCC/TempTables/TOFvsFOR_Categories3.csv")
  # write.csv(dataTOFvsFOR_3,"LULCC/TempTables/TOFvsFOR_Categories.csv",row.names = FALSE)
  # 
  # TOFvsFOR_mask3 <- raster("LULCC/TempRaster/TOFvsFOR_mask3.tif")
  # writeRaster(TOFvsFOR_mask3, "LULCC/TempRaster/TOFvsFOR_mask.tif", datatype="INT2S", overwrite=TRUE)
  
}

# # Rename AGB maps following the version being used
# if (AGBmap_v == 1){
#   agb_c1 <- raster("LULCC/TempRaster/agb_c1.tif")
#   writeRaster(agb_c1, "LULCC/TempRaster/agb_c.tif", datatype="INT4S", overwrite=TRUE)
#   
# } else if (AGBmap_v == 2){
#   agb_c2 <- raster("LULCC/TempRaster/agb_c2.tif")
#   writeRaster(agb_c2, "LULCC/TempRaster/agb_c.tif", datatype="INT4S", overwrite=TRUE)
#   
# } else if (AGBmap_v == 3){
#   agb_c3<- raster("LULCC/TempRaster/agb_c3.tif")
#   writeRaster(agb_c3, "LULCC/TempRaster/agb_c.tif", datatype="INT4S", overwrite=TRUE)
#   
# }

if (OSType == 64) {
  ffmpeg_path<-file.path(getwd(),"ffmpeg64/bin/ffmpeg.exe")
} else {
  ffmpeg_path<-file.path(getwd(),"ffmpeg32/bin/ffmpeg.exe")
}

ST = (48/IL*STdyn)+1

print(MC)
print(IT)
print(K_MC)
print(TOF_MC)
print(Ini_st_MC)
print(Ini_st.factor.percentage)
Ini_st.factor = Ini_st.factor.percentage/100
print(COVER_MAP)
print(rmax_MC)
print (DEF_FW)
print(IL)
print(STdyn)
print(ST)
print(Harv.Pix.W)
print(Prune.W)
print(Harv.Pix.V)
print(Prune.V)
print(Harv.Pix_MC)
print(Prune_MC)
print(ffmpeg_path)
# print(Subset_locs)
# print(MaxAGB)
print(AGBmap)

if (K_MC == 1) {
  K_MC_yesno = "Yes"
} else {
  K_MC_yesno = "No"
}

if (TOF_MC == 1) {
  TOF_MC_yesno = "Yes"
} else {
  TOF_MC_yesno = "No"
}

if (Ini_st_MC == 1) {
  Ini_st_MC_yesno = "Yes"
} else {
  Ini_st_MC_yesno = "No"
}

if (COVER_MAP == 1) {
  COVER_MAP_yesno = "Yes"
  Ini_st_MC = 0
  Ini_st_MC_yesno = "Not applicable"
  Ini_st.factor.percentage = "Tree cover as a % of K"
} else {
  COVER_MAP_yesno = "No"
  Ini_st.factor.percentage = paste(Ini_st.factor.percentage,"% of K",sep="")
}

if (rmax_MC == 1) {
  rmax_MC_yesno = "Yes"
} else {
  rmax_MC_yesno = "No"
}

if (DEF_FW == 1) {
  DEF_FW_yesno = "Yes"
} else {
  DEF_FW_yesno = "No"
}

if (AGBmap == 1) {
  AGBmap_yesno = "Yes"
  COVER_MAP_yesno = "Not applicable"
  Ini_st_MC_yesno = "Not applicable"
  Ini_st.factor.percentage = "Using AGB map"
} else {
  AGBmap_yesno = "No"
}

if (OSType == 32) {
  res1000<-100
  res600<-100
  res300<-100
} else {
  res1000<-1000
  res600<-600
  res300<-300
}


# FORESTS ----
# Rename depending on LUCmap

if (LUCmap_v == 1) {
  data_all <- data_all1
  data_FOR <- data_FOR1
  data_TOF <- data_TOF1
  max_tot <- max_tot1
  LULC_Categories <- LULC_Categories1
} else if (LUCmap_v == 2) {
  data_all <- data_all2
  data_FOR <- data_FOR2
  data_TOF <- data_TOF2
  max_tot <- max_tot2
  LULC_Categories <- LULC_Categories2
} else if (LUCmap_v == 3) {
  data_all <- data_all3
  data_FOR <- data_FOR3
  data_TOF <- data_TOF3
  max_tot <- max_tot3
  LULC_Categories <- LULC_Categories3
}

max_FOR<-nrow(data_FOR)

## Histograms 4 forests ----

if (max_FOR!=0) {
  histograms_per_figure_FOR<-Histograms.per.Fig_FOR
  tpaso_FOR<-histograms_per_figure_FOR-1
  height_figure_FOR<-75*histograms_per_figure_FOR/5
  for(j in 1:(ceiling(max_FOR/histograms_per_figure_FOR))) { 
    if (j==1) {
      min5_FOR<-1
      max5_FOR<-j+tpaso_FOR
    } else {
      min5_FOR<-max5_FOR+1
      max5_FOR<-(tpaso_FOR*j)+j
    }
    if (max5_FOR>max_FOR) {
      max5_FOR<-max_FOR
    } else { 
      max5_FOR<-max5_FOR
    }
    
    # rmax values ----
    
    if (CTrees == 0) {
      tiff(filename=paste(OutDir,"//Histogram_rmax",j,".tif",sep=""),width=290,height=height_figure_FOR,units="mm",res=res1000,bg="white",
           compression=c("lzw"),type=c("windows"),pointsize=12,family="",restoreConsole=TRUE)
      n<-layout(matrix(1:histograms_per_figure_FOR, (histograms_per_figure_FOR/5),5, byrow=TRUE))
      par(oma=c(0,0,5,0))  # top has 5 lines of space
      # Set margins to accommodate larger subtitles
      par(mar = c(5, 4, 6, 2))  # bottom, left, top, right
    }
    
    for(i in min5_FOR:max5_FOR) {
      if (data_FOR[i,3]==0) {
        textmsg<-"WARNING ERROR IN SUPPLY PARAMETERS csv DATASET"
        write.csv(textmsg,paste(OutDir,"//",textmsg,".csv",sep=""),row.names = FALSE)
      } else {
        lulc1_name<-data_FOR[i,2]
        r1max<-data_FOR[i,3]
        r1maxsd<-data_FOR[i,4]*rmax_MC
        LULC_ID_FOR<-data_FOR[i,1]
        r1 <- draw_truncated_or_fixed(
          MC, mean_value = r1max, sd_value = r1maxsd,
          lower = r1max / 10
        )
        r1[1]<-r1max 
        
        # Adjust the font size for the histogram titles
        cex_main1 <- 0.3 + (15 / max(nchar(lulc1_name), 15))
        
        hist((r1*100),nclass=15,xlab="rmax",ylab="Frequency",main=lulc1_name,sub=expression("% yr"^{-1}*""),col="grey",cex.main = cex_main1)
        
        r1<-as.data.frame(r1)
        colnames(r1)<-paste("LULC_rmax",LULC_ID_FOR,sep="")
        r1=data.frame(Key=c(1:MC),r1) 
        write.csv(r1,paste("Temp//rmax",LULC_ID_FOR,".csv",sep=""),row.names = FALSE)
      }
    }
    
    if (CTrees == 0) {
      Main_Title<-paste("Parameters set by user (Forests and Woodlands):
			StartUp year =",IT,"    Sim. length =",ST-1,"yr     MC =",MC,"runs    rmax w/MC?",rmax_MC_yesno,"   Carrying capacity (K) w/MC?",K_MC_yesno,"  Trees Outside Forests (TOF) w/MC?",TOF_MC_yesno," \n Initial Stock =",Ini_st.factor.percentage,"     Initial Stock w/MC =",Ini_st_MC_yesno,"     \n Iteration length =",IL,"weeks (",IL*0.25,"months )     Tree cover map provided?",COVER_MAP_yesno,"     AGB map provided?",AGBmap_yesno,"     Accounting for fuelwood from deforestation?",DEF_FW_yesno) 
      title(main=Main_Title,line=NA,outer=TRUE,adj=0.5,
            cex.main = 1,   font.main= 1, col.main= "blue")
      dev.off()
    }
    
    #K values ----
    
    if (CTrees == 0) {
      tiff(filename=paste(OutDir,"//Histogram_K",j,".tif",sep=""),width=290,height=height_figure_FOR,units="mm",res=res1000,bg="white",
           compression=c("lzw"),type=c("windows"),pointsize=12,family="",restoreConsole=TRUE)
      n<-layout(matrix(1:histograms_per_figure_FOR, (histograms_per_figure_FOR/5),5, byrow=TRUE))
      par(oma=c(0,0,5,0))  # top has 5 lines of space
      # Set margins to accommodate larger subtitles
      par(mar = c(5, 4, 6, 2))  # bottom, left, top, right
    }
    
    for(i in min5_FOR:max5_FOR) {
      if (data_FOR[i,3]==0) {
        textmsg<-"WARNING ERROR IN SUPPLY PARAMETERS csv DATASET"
        write.csv(textmsg,paste(OutDir,"//",textmsg,".csv",sep=""),row.names = FALSE)
      } else {
        lulc2_name<-data_FOR[i,2]
        k1max<-data_FOR[i,5]
        k1maxsd<-data_FOR[i,6]*K_MC
        LULC_ID_FOR<-data_FOR[i,1]
        k1 <- draw_truncated_or_fixed(
          MC, mean_value = k1max, sd_value = k1maxsd, lower = 0
        )
        k1[1]<-k1max
        
        # Adjust the font size for the histogram titles
        cex_main2 <- 0.3 + (15 / max(nchar(lulc2_name), 15))
        
        hist(k1,nclass=15,xlab="K",ylab="Frequency",main=lulc2_name,sub=expression("tDM ha"^{-1}*""),col="grey",cex.main = cex_main2)
        k1<-as.data.frame(k1)
        colnames(k1)<-paste("LULC_K",LULC_ID_FOR,sep="")
        k1=data.frame(Key=c(1:MC),k1) 
        write.csv(k1,paste("Temp//k",LULC_ID_FOR,".csv",sep=""),row.names = FALSE)
      }
    }
    
    if (CTrees == 0) {
      Main_Title<-paste("Parameters set by user (Forests and Woodlands):
			StartUp year =",IT,"    Sim. length =",ST-1,"yr     MC =",MC,"runs    rmax w/MC?",rmax_MC_yesno,"   Carrying capacity (K) w/MC?",K_MC_yesno,"  Trees Outside Forests (TOF) w/MC?",TOF_MC_yesno," \n Initial Stock =",Ini_st.factor.percentage,"     Initial Stock w/MC =",Ini_st_MC_yesno,"     \n Iteration length =",IL,"weeks (",IL*0.25,"months )     Tree cover map provided?",COVER_MAP_yesno,"     AGB map provided?",AGBmap_yesno,"     Accounting for fuelwood from deforestation?",DEF_FW_yesno) 
      title(main=Main_Title,line=NA,outer=TRUE,adj=0.5,
            cex.main = 1,   font.main= 1, col.main= "blue")
      dev.off()
    }
    
    # Initial Stock values----
    
    if (CTrees == 0) {
      tiff(filename=paste(OutDir,"//Histogram_ini_stock",j,".tif",sep=""),width=290,height=height_figure_FOR,units="mm",
           res=res1000,bg="white",compression=c("lzw"),type=c("windows"),pointsize=12,family="",restoreConsole=TRUE)
      n<-layout(matrix(1:histograms_per_figure_FOR, (histograms_per_figure_FOR/5),5, byrow=TRUE))
      par(oma=c(0,0,5,0))  # top has 5 lines of space
      # Set margins to accommodate larger subtitles
      par(mar = c(5, 4, 6, 2))  # bottom, left, top, right
    }
    
    for(i in min5_FOR:max5_FOR) {
      if (data_FOR[i,3]==0) {
        textmsg<-"WARNING ERROR IN SUPPLY PARAMETERS csv DATASET"
        write.csv(textmsg,paste(OutDir,"//",textmsg,".csv",sep=""),row.names = FALSE)
      } else {
        lulc3_name<-data_FOR[i,2]
        inst1<-data_FOR[i,5]*Ini_st.factor
        inst1sd<-data_FOR[i,6]*Ini_st.factor*Ini_st_MC
        LULC_ID_FOR<-data_FOR[i,1]
        st1 <- draw_truncated_or_fixed(
          MC, mean_value = inst1, sd_value = inst1sd, lower = 0
        )
        st1[1]<-inst1
        
        # Adjust the font size for the histogram titles
        cex_main3 <- 0.3 + (15 / max(nchar(lulc3_name), 15))
        
        hist(st1,nclass=15,xlab="Initial Stock",ylab="Frequency",main=lulc3_name,sub=expression("tDM ha"^{-1}*""),col="grey", cex.main = cex_main3)
        st1<-as.data.frame(st1)
        colnames(st1)<-paste("LULC_IniSt",LULC_ID_FOR,sep="")
        st1=data.frame(Key=c(1:MC),st1) 
        write.csv(st1,paste("Temp//i_st",LULC_ID_FOR,".csv",sep=""),row.names = FALSE)
      }
    }
    
    if (CTrees == 0) {
      Main_Title<-paste("Parameters set by user (Forests and Woodlands):
			StartUp year =",IT,"    Sim. length =",ST-1,"yr     MC =",MC,"runs    rmax w/MC?",rmax_MC_yesno,"   Carrying capacity (K) w/MC?",K_MC_yesno,"  Trees Outside Forests (TOF) w/MC?",TOF_MC_yesno," \n Initial Stock =",Ini_st.factor.percentage,"     Initial Stock w/MC =",Ini_st_MC_yesno,"     \n Iteration length =",IL,"weeks (",IL*0.25,"months )     Tree cover map provided?",COVER_MAP_yesno,"     AGB map provided?",AGBmap_yesno,"     Accounting for fuelwood from deforestation?",DEF_FW_yesno) 
      title(main=Main_Title,line=NA,outer=TRUE,adj=0.5,
            cex.main = 1,   font.main= 1, col.main= "blue")
      dev.off()
    }
    
  } # j hasta ceiling(max_FOR/5)
  
} else {
  textmsg<-"No Forest or Woodland class provided"
  write.csv(textmsg,paste(OutDir,"//",textmsg,".csv",sep=""),row.names = FALSE)
}


#TREES OUTSIDE FORESTS (TOF)----

max_TOF<-nrow(data_TOF)

if (max_TOF!=0) {
  histograms_per_figure_TOF<-Histograms.per.Fig_TOF
  tpaso_TOF<-histograms_per_figure_TOF-1
  height_figure_TOF<-75*histograms_per_figure_TOF/5
  for(j in 1:(ceiling(max_TOF/histograms_per_figure_TOF))) { 
    if (j==1) {
      min5_TOF<-1
      max5_TOF<-j+tpaso_TOF
    } else {
      min5_TOF<-max5_TOF+1
      max5_TOF<-(tpaso_TOF*j)+j
    }
    if (max5_TOF>max_TOF) {
      max5_TOF<-max_TOF
    } else { 
      max5_TOF<-max5_TOF
    }
    
    tiff(filename=paste(OutDir,"//Histogram_TOF",j,".tif",sep=""),width=290,height=height_figure_TOF,units="mm",res=res1000,bg="white",
         compression=c("lzw"),type=c("windows"),pointsize=12,family="",restoreConsole=TRUE)
    n<-layout(matrix(1:histograms_per_figure_TOF, (histograms_per_figure_TOF/5),5, byrow=TRUE))
    par(oma=c(0,0,5,0))  # top has 5 lines of space
    # Set margins to accommodate larger subtitles
    par(mar = c(5, 4, 6, 2))  # bottom, left, top, right
    
    for(i in min5_TOF:max5_TOF) {
      lulc4_name<-data_TOF[i,2]
      kTOFmax<-data_TOF[i,5]
      kTOFmaxsd<-data_TOF[i,6]*TOF_MC
      LULC_ID_TOF<-data_TOF[i,1]
      kTOF <- draw_truncated_or_fixed(
        MC, mean_value = kTOFmax, sd_value = kTOFmaxsd, lower = 0
      )
      kTOF[1]<-kTOFmax
      
      # Adjust the font size for the histogram titles
      cex_main4 <- 0.3 + (15 / max(nchar(lulc4_name), 15))
      
      hist(kTOF,nclass=15,xlab="Available fuelwood from pruning",ylab="Frequency",main=lulc4_name,sub=expression("tDM ha"^{-1}*" yr"^{-1}*""),col="grey", cex.main = cex_main4)
      kTOF<-as.data.frame(kTOF)
      colnames(kTOF)<-paste("LULC_TOF",LULC_ID_TOF,sep="")
      kTOF=data.frame(Key=c(1:MC),kTOF)  
      write.csv(kTOF,paste("Temp//i_st",LULC_ID_TOF,".csv",sep=""),row.names = FALSE)
      write.csv(kTOF,paste("Temp//k",LULC_ID_TOF,".csv",sep=""),row.names = FALSE)
      write.csv(kTOF,paste("Temp//rmax",LULC_ID_TOF,".csv",sep=""),row.names = FALSE)
      print(i)
    }
    
    Main_Title<-paste("Parameters set by user (Trees Outside Forests (TOF)):
			StartUp year =",IT,"    Sim. length =",ST-1,"yr     MC =",MC,"runs    rmax w/MC?",rmax_MC_yesno,"   Carrying capacity (K) w/MC?",K_MC_yesno,"  Trees Outside Forests (TOF) w/MC?",TOF_MC_yesno," \n Initial Stock =",Ini_st.factor.percentage,"     Initial Stock w/MC =",Ini_st_MC_yesno,"     \n Iteration length =",IL,"weeks (",IL*0.25,"months )     Tree cover map provided?",COVER_MAP_yesno,"     AGB map provided?",AGBmap_yesno,"     Accounting for fuelwood from deforestation?",DEF_FW_yesno) 
    title(main=Main_Title,line=NA,outer=TRUE,adj=0.5,
          cex.main = 1,   font.main= 1, col.main= "blue")
    
    dev.off()
    
  } # j hasta ceiling(max_TOF/5)
  
} else {
  textmsg<-"No Trees Outside Forest class provided"
  write.csv(textmsg,paste(OutDir,"//",textmsg,".csv",sep=""),row.names = FALSE)
}

#TABLES TO BE READ BY DINAMICA ----

k_files1dig <- dir("Temp", pattern = glob2rx("k?.csv"), full.names = TRUE)
k_files2dig <- dir("Temp", pattern = glob2rx("k??.csv"), full.names = TRUE)
k_files3dig <- dir("Temp", pattern = glob2rx("k???.csv"), full.names = TRUE)
k_files<-c(k_files1dig,k_files2dig,k_files3dig)
k_tables <- lapply(k_files, read.csv)

K_all_1 <- do.call(cbind, k_tables)
# Adjust K for pixel resolution (separating FOR from TOF); starting from "by hectare" data
rasters_res <- xres(raster("LULCC/TempRaster//Mask_c.tif"))
res_factor_to_ha <-(rasters_res*rasters_res)/(100*100)
namesFOR <- names(K_all_1)[grep("LULC_K", names(K_all_1))]
K_all_1[,c(namesFOR)]<-K_all_1[,c(namesFOR)]*res_factor_to_ha
namesTOR <- names(K_all_1)[grep("LULC_TOF", names(K_all_1))]
K_all_1[,c(namesTOR)]<-K_all_1[,c(namesTOR)]*res_factor_to_ha 
K_all_2 <- K_all_1[ , grepl( "LULC" , names( K_all_1 ) ) ]
K_all_3 <- data.frame(Key=c(1:MC),K_all_2) %>%
  replace(is.na(.), 0.11111)
write.csv(K_all_3,"Temp//k_all.csv",row.names = FALSE)

rmax_files1dig <- dir("Temp", pattern = glob2rx("rmax?.csv"), full.names = TRUE)
rmax_files2dig <- dir("Temp", pattern = glob2rx("rmax??.csv"), full.names = TRUE)
rmax_files3dig <- dir("Temp", pattern = glob2rx("rmax???.csv"), full.names = TRUE)
rmax_files<-c(rmax_files1dig,rmax_files2dig,rmax_files3dig)
rmax_tables <- lapply(rmax_files, read.csv)

rmax_all_1<-do.call(cbind, rmax_tables)
rmax_all_2<-rmax_all_1[ , grepl( "LULC" , names( rmax_all_1 ) ) ]
#Lo que hay que hacer es multiplicar por resolucion aquellas columnas cuyo nombre tene TOF y listo
rmax_all_3ha=data.frame(Key=c(1:MC),rmax_all_2) 
rmax_all_3 <- rmax_all_3ha %>% 
  mutate_at(vars(matches("LULC_TOF")), function(x){res_factor_to_ha*x}) %>%
  replace(is.na(.), 0.11111)
write.csv(rmax_all_3,"Temp//rmax_all.csv",row.names = FALSE)

inist_files1dig <- dir("Temp", pattern = glob2rx("i_st?.csv"), full.names = TRUE)
inist_files2dig <- dir("Temp", pattern = glob2rx("i_st??.csv"), full.names = TRUE)
inist_files3dig <- dir("Temp", pattern = glob2rx("i_st???.csv"), full.names = TRUE)
inist_files<-c(inist_files1dig,inist_files2dig,inist_files3dig)
inist_tables <- lapply(inist_files, read.csv)

inist_all_1 <- do.call(cbind, inist_tables)
###Adjust Initial Stock for pixel resolution (separating FOR from TOF); starting from "by hectare" data
namesFOR_st <- names(inist_all_1)[grep("LULC_IniSt", names(inist_all_1))]
inist_all_1[,c(namesFOR_st)]<-inist_all_1[,c(namesFOR_st)]*res_factor_to_ha
namesTOR_st <- names(inist_all_1)[grep("LULC_TOF", names(inist_all_1))]
inist_all_1[,c(namesTOR_st)]<-inist_all_1[,c(namesTOR_st)]*res_factor_to_ha
inist_all_2 <- inist_all_1[ , grepl( "LULC" , names( inist_all_1 ) ) ]
inist_all_3 <- data.frame(Key=c(1:MC),inist_all_2) %>%
  replace(is.na(.), 0.11111)
write.csv(inist_all_3,"Temp//i_st_all.csv",row.names = FALSE)

# HARVESTED PIXELS MC----
if (LUCmap_v == 1) {
  LULCMap<-raster("LULCC/TempRaster/LULCt1_c.tif")
} else if (LUCmap_v == 2) {
  LULCMap<-raster("LULCC/TempRaster/LULCt2_c.tif")
} else if (LUCmap_v == 3) {
  LULCMap<-raster("LULCC/TempRaster/LULCt3_c.tif")
}

# raster::freq() returns pixel counts. Keep these totals in pixels because
# Harv.Pix.W/Harv.Pix.V and the Patcher change counts are also pixel counts.
# Area conversions are presentation-only and must never be used as MC bounds.
LULCMap_hist <- freq(
  LULCMap,
  digits = 0,
  value = NULL,
  useNA = "ifany",
  merge = FALSE,
  progress = "window"
)

df1 <- as.data.frame(LULCMap_hist)
tot_pixels <- df1[complete.cases(df1), ]
sum(tot_pixels$count)

df2_TOF <- as.data.frame(data_TOF[, 1])
colnames(df2_TOF) <- "value"
df3_TOF <- merge(df1, df2_TOF)
df4_TOF <- df3_TOF[complete.cases(df3_TOF), ]
TOF_total_pixels <- sum(df4_TOF$count)

df2_FOR <- as.data.frame(data_FOR[, 1])
colnames(df2_FOR) <- "value"
df3_FOR <- merge(df1, df2_FOR)
df4_FOR <- df3_FOR[complete.cases(df3_FOR), ]
FOR_total_pixels <- sum(df4_FOR$count)

Harvestable_total_W <- TOF_total_pixels + FOR_total_pixels
Harvestable_total_V <- FOR_total_pixels
# Vehicle harvesting remains forest-only, matching the established model.

Harvestable_area_W_ha <- Harvestable_total_W * res_factor_to_ha
Harvestable_area_V_ha <- Harvestable_total_V * res_factor_to_ha

clamp_pixel_request <- function(requested, available, label) {
  if (!is.finite(requested) || !is.finite(available) || available < 0) {
    stop(label, " harvested-pixel request/availability must be finite and non-negative.")
  }
  min(max(round(requested), 0), floor(available))
}

draw_harvest_pixels <- function(requested, available, relative_sd, n, label) {
  requested <- clamp_pixel_request(requested, available, label)
  sd_pixels <- requested * relative_sd
  draws <- draw_truncated_or_fixed(
    n = n,
    mean_value = requested,
    sd_value = sd_pixels,
    lower = if (requested > 0) requested / 10 else 0,
    upper = floor(available)
  )
  draws <- round(pmin(pmax(draws, 0), floor(available)), digits = 0)
  draws[1] <- requested
  draws
}

tiff(
  filename = paste(OutDir, "//Harvested_pixels.tif", sep = ""),
  width = 300,
  height = 175,
  units = "mm",
  res = res1000,
  bg = "white",
  compression = c("lzw"),
  type = c("windows"),
  pointsize = 12,
  family = "",
  restoreConsole = TRUE
)
n <- layout(matrix(1:2, 1, 2))
par(oma = c(0, 0, 5, 0))

# Harvest Pixels Walking #####
Harv.Pix.W <- clamp_pixel_request(
  Harv.Pix.W, Harvestable_total_W, "Walking"
)
HP1 <- draw_harvest_pixels(
  requested = Harv.Pix.W,
  available = Harvestable_total_W,
  relative_sd = Harv.Pix_MC,
  n = MC,
  label = "Walking"
)
hist(
  HP1,
  nclass = 15,
  xlab = paste(
    "1 pixel = ", res_factor_to_ha, " ha (",
    res_factor_to_ha / 100, " km2)", sep = ""
  ),
  ylab = "Frequency",
  main = "Harvested pixels per allocation event (walking)",
  sub = paste(
    "Available: ", Harvestable_total_W, " pixels; ",
    Harvestable_area_W_ha, " ha (",
    Harvestable_area_W_ha / 100, " km2)", sep = ""
  ),
  col = "grey"
)
write.csv(data.frame(Value = HP1), "Temp//Harvest_pixels_W.csv")

# Harvest Pixels Vehicle #####
Harv.Pix.V <- clamp_pixel_request(
  Harv.Pix.V, Harvestable_total_V, "Vehicle"
)
HP2 <- draw_harvest_pixels(
  requested = Harv.Pix.V,
  available = Harvestable_total_V,
  relative_sd = Harv.Pix_MC,
  n = MC,
  label = "Vehicle"
)
hist(
  HP2,
  nclass = 15,
  xlab = paste(
    "1 pixel = ", res_factor_to_ha, " ha (",
    res_factor_to_ha / 100, " km2)", sep = ""
  ),
  ylab = "Frequency",
  main = "Harvested pixels per allocation event (vehicle)",
  sub = paste(
    "Available: ", Harvestable_total_V, " pixels; ",
    Harvestable_area_V_ha, " ha (",
    Harvestable_area_V_ha / 100, " km2)", sep = ""
  ),
  col = "grey"
)
write.csv(data.frame(Value = HP2), "Temp//Harvest_pixels_V.csv")


Main_Title<-paste("Parameters set by user:
			StartUp year =",IT,"    Sim. length =",ST-1,"yr     MC =",MC,"runs    rmax w/MC?",rmax_MC_yesno, "   Carrying capacity (K) w/MC?",K_MC_yesno,
                  " \n Trees Outside Forests (TOF) w/MC?",TOF_MC_yesno,"   Initial Stock =",Ini_st.factor.percentage,"     Initial Stock w/MC =",Ini_st_MC_yesno,"   Iteration length =",IL,"weeks (",IL*0.25,"months )",
                  " \n Tree cover map provided?",COVER_MAP_yesno,"   AGB map provided?",AGBmap_yesno,"   Accounting for fuelwood from deforestation?",DEF_FW_yesno) 
title(main=Main_Title,line=NA,outer=TRUE,adj=0.5,
      cex.main = 1,   font.main= 1, col.main= "blue")
dev.off()


# PRUNE FACTORS MC ----

tiff(filename=paste(OutDir,"//Prune_factors.tif",sep=""),width=300,height=175,units="mm",res=res1000,bg="white",compression=c("lzw"),type=c("windows"),pointsize=12,family="",restoreConsole=TRUE)
n<-layout(matrix(1:2, 1,2))
par(oma=c(0,0,5,0))  # top has 5 lines of space

##### Prune Factor Walking #####

Prune.W
Prune.Wsd<-Prune.W*1*Prune_MC

PF1<-round(rtnorm(MC,mean=Prune.W,Prune.Wsd,lower=0.9), digits=0)
PF1[1]<-Prune.W
hist((PF1),nclass=15,xlab="",ylab="Frequency",main="Prune Factor (walking)",sub=expression(""),col="grey")
PF1<-as.data.frame(PF1)
colnames(PF1)<-"Value"
write.csv(PF1,"Temp//Prune_factor_W.csv")

##### Prune Factor Vehicle #####

Prune.V
Prune.Vsd<-Prune.V*1*Prune_MC

PF2<-round(rtnorm(MC,mean=Prune.V,Prune.Vsd,lower=0.9), digits=0)
PF2[1]<-Prune.V
hist((PF2),nclass=15,xlab="",ylab="Frequency",main="Prune Factor (vehicle)",sub=expression(""),col="grey")
PF2<-as.data.frame(PF2)
colnames(PF2)<-"Value"
write.csv(PF2,"Temp//Prune_factor_V.csv")

Main_Title<-paste("Parameters set by user:
			StartUp year =",IT,"    Sim. length =",ST-1,"yr     MC =",MC,"runs    rmax w/MC?",rmax_MC_yesno, "   Carrying capacity (K) w/MC?",K_MC_yesno,
                  " \n Trees Outside Forests (TOF) w/MC?",TOF_MC_yesno,"   Initial Stock =",Ini_st.factor.percentage,"     Initial Stock w/MC =",Ini_st_MC_yesno,"   Iteration length =",IL,"weeks (",IL*0.25,"months )",
                  " \n Tree cover map provided?",COVER_MAP_yesno,"   AGB map provided?",AGBmap_yesno,"   Accounting for fuelwood from deforestation?",DEF_FW_yesno) 
title(main=Main_Title,line=NA,outer=TRUE,adj=0.5,
      cex.main = 1,   font.main= 1, col.main= "blue")
dev.off()


###############################
### Max AGB value for graphs and animation showing last MC run
###############################

maxAGB_all<-read.csv("Temp//k_all.csv")
MaxAGB<-(max(maxAGB_all, na.rm=TRUE))
write.csv(MaxAGB,"Temp//MaxAGB.csv")
MaxAGB_lastMC<-(max(maxAGB_all[MC, ], na.rm=TRUE))
write.csv(MaxAGB_lastMC,"Temp//MaxAGB_lastMC.csv")
MaxAGB_firstMC<-(max(maxAGB_all[1, ], na.rm=TRUE))
write.csv(MaxAGB_firstMC,"Temp//MaxAGB_firstMC.csv")


# END ----

publish_current_mc_batch()

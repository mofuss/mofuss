# Build a two-country regional regression source from an already isolated LSO
# fixture. These arbitrary AAA/BBB countries are test geometry, not evidence.
suppressPackageStartupMessages(library(terra))
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L) stop("Supply one previously staged fixture directory.")
root <- normalizePath(args[[1L]], winslash = "/", mustWork = TRUE)
if (!grepl("^E:/MoFuSS_Active/ecsa_sourcing_and_speed_v1/runtime_regression/", root) ||
    !file.exists(file.path(root, "fixture_manifest.json")) ||
    file.exists(file.path(root, "runtime_result.json"))) {
  stop("Only an unrun, isolated regression fixture may be transformed.")
}
if (file.exists(file.path(root, "synthetic_split_country_fixture.csv"))) {
  stop("Synthetic fixture already exists; create a fresh named fixture.")
}
mask <- rast(file.path(root, "LULCC/TempRaster/mask_c.tif"))
cells <- values(mask, mat = FALSE)
column <- rep(seq_len(ncol(mask)), times = nrow(mask))
zone_values <- ifelse(!is.na(cells), ifelse(column <= floor(ncol(mask) / 2), 1L, 2L), NA_integer_)
zones <- setValues(rast(mask), zone_values)
zone_path <- file.path(root, "LULCC/TempRaster/synthetic_country_id.tif")
writeRaster(zones, zone_path, datatype = "INT2S", overwrite = FALSE)
countries <- as.polygons(zones, dissolve = TRUE, values = TRUE, na.rm = TRUE)
names(countries) <- "CountryID"
countries$GID_0 <- c("AAA", "BBB")[countries$CountryID]
countries$NAME_0 <- c("Synthetic West", "Synthetic East")[countries$CountryID]
countries$mofuss_reg <- "SYN_PAIR"
countries$mofuss_reg_name <- "Synthetic two-country test"
countries$region_code <- "SYN_PAIR"
vector_path <- file.path(root, "LULCC/DownloadedDatasets/SourceDataGlobal/InVector/extent_mask.gpkg")
writeVector(countries, vector_path, overwrite = TRUE)

demand_root <- file.path(root, "In/DemandScenarios")
for (channel in c("W", "V")) {
  index_path <- file.path(demand_root, paste0(channel, "_origin_component_index.csv"))
  original_index <- read.csv(index_path, check.names = FALSE)
  stopifnot(nrow(original_index) == 1L)
  shares <- if (channel == "W") c(0.95, 0.05) else c(0.35, 0.65)
  paths <- list.files(demand_root, paste0("^", channel, "_origin_demand[0-9]+[.]csv$"), full.names = TRUE)
  for (path in paths) {
    original <- read.csv(path, check.names = FALSE)
    total <- sum(original$Value)
    write.csv(data.frame(Key = 1:2, Value = total * shares), path, row.names = FALSE)
  }
  index <- original_index[rep(1L, 2L), , drop = FALSE]
  index$ComponentIndex <- 1:2
  index$DemandISO3 <- c("AAA", "BBB")
  index$JobID <- paste0(channel, "_", c("AAA", "BBB"), "_ORIGIN")
  index$DirectionRule <- if (channel == "W") "origin_country_demand_domestic_sources_only" else "origin_country_demand_bilateral_sources_tiers_ABC"
  index$AllowedSourceISO3 <- if (channel == "W") c("AAA", "BBB") else rep("AAA;BBB", 2L)
  index$FirstYearDemandTons <- original_index$FirstYearDemandTons * shares
  index$LastYearDemandTons <- original_index$LastYearDemandTons * shares
  write.csv(index, index_path, row.names = FALSE)
  pressure_root <- file.path(root, "In", paste0(channel, "_origin_components"))
  originals <- list.files(pressure_root, "001_[0-9]+[.]tif$", full.names = TRUE)
  stopifnot(length(originals) == 6L)
  for (path in originals) {
    original <- rast(path)
    old_values <- values(original, mat = FALSE)
    period <- sub(".*001_([0-9]+)[.]tif$", "\\1", basename(path))
    first <- if (channel == "W") {
      ifelse(zone_values == 1L, old_values, 0)
    } else {
      old_values * ifelse(zone_values == 1L, 3, 0.3)
    }
    second <- if (channel == "W") {
      ifelse(zone_values == 2L, old_values, 0)
    } else {
      old_values * ifelse(zone_values == 1L, 0.5, 3)
    }
    # Dinamica 2.4's GDAL reader requires a finite NoData marker for floats;
    # terra's default NaN marker can trigger a native assertion. Zero outside
    # each W domestic domain also preserves the legacy additive accumulator.
    writeRaster(setValues(rast(original), first), path, datatype = "FLT4S", NAflag = -2147483648, overwrite = TRUE)
    second_path <- sub("001_", "002_", path, fixed = TRUE)
    writeRaster(setValues(rast(original), second), second_path, datatype = "FLT4S", NAflag = -2147483648, overwrite = FALSE)
    # Keep the compatibility aggregate consistent with the two raw components.
    summed <- ifelse(is.na(first), second, ifelse(is.na(second), first, first + second))
    aggregate_path <- file.path(root, "In", paste0("IDW_C++_fw_", tolower(channel), period, ".tif"))
    writeRaster(setValues(rast(original), summed), aggregate_path, datatype = "FLT4S", NAflag = -2147483648, overwrite = TRUE)
  }
}
parameters_path <- file.path(root, "LULCC/DownloadedDatasets/SourceDataGlobal/parameters.csv")
parameters <- read.csv(parameters_path, check.names = FALSE)
parameters$ParCHR[parameters$Var == "byregion"] <- "Regional"
parameters$ParCHR[parameters$Var == "region2BprocessedReg"] <- "SYN_PAIR"
write.csv(parameters, parameters_path, row.names = FALSE)
write.csv(data.frame(
  CountryID = 1:2, ISO3 = c("AAA", "BBB"),
  W_demand_share = c(0.95, 0.05), V_demand_share = c(0.35, 0.65),
  W_sources = c("AAA", "BBB"), V_sources = "AAA;BBB",
  purpose = "Synthetic regression only; arbitrary west/east split of original LSO raster"
), file.path(root, "synthetic_split_country_fixture.csv"), row.names = FALSE)
cat("Synthetic split-country runtime source prepared:", root, "\n")
cat("Do not run this modified source fixture directly: stage old/new children from it to record fresh input hashes.\n")

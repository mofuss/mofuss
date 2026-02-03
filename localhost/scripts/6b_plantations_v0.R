# MoFuSS
# Version 1
# Date: Jan 2026

# 2dolist ----
# lulc_new, paste0(countrydir,"/LULCC/TempRaster/LULCt1_c_plant.tif"), # Dinamica must read this!

# Internal parameters ----
check_projection_24b <- 0
# plant_wd <- "/mnt/mofuss_ssd/plantations_fao/eucalyptus/"
plant_wd <- "C:/Users/aghil/Documents/MoFuSS_FAO_localhost/plantations_fao/eucalyptus/"

tiles <- c(
  "in/ZMB_productivity_continuous_V_AGB_MAI-noexcl-1.tif",
  "in/ZMB_productivity_continuous_V_AGB_MAI-noexcl-2.tif",
  "in/ZMB_productivity_continuous_V_AGB_MAI-noexcl-3.tif",
  "in/ZMB_productivity_continuous_V_AGB_MAI-noexcl-4.tif"
)

out_merged   <- "temp/ZMB_productivity_merged_24b.tif"

T1   <- 2027 # Plantations establishment
Tend <- 2050 # end of simulations
Tr = 9 # “We restrict rotations to ≥Tr years to reflect minimum stem size and establishment requirements for charcoal plantations.”
efchratio <- 6 # This includes all losses from standing AGB to packed charcoal in the truck.
rhov=0.52; BEFv=1.30
max_dist_m <- 12000          # <-- set your threshold in meters
road_classes_to_use <- c(2)  # <-- choose among 1:5, 2 main highways 
min_pix <- 1000 # --- 3) Remove patches smaller than N pixels (e.g., 10) ---
Kv = 2
KSDv = 2

####
opt_rotation <- 12 # You need to adjust this after  lines 172 - 180, pick the year with highest production potential
####

# Load packages ----
library(conflicted)

library(terra)
library(dplyr)
library(stringr)
library(ggplot2)
library(readr)

##
source(paste0(githubdir,"/localhost/scripts/2a_scen_gen_v4.R"),
       echo = TRUE)

setwd(plant_wd)

# 1) Merge tiles into one (assumes tiles are adjacent or minimally overlapping) ----
if (!file.exists(paste0(plant_wd,"temp/ZMB_productivity_merged_24b.tif"))) {
  cat("Merging original tifs: ", paste0(plant_wd,"temp/ZMB_productivity_merged_24b.tif"), "\n")
  
  # Read as SpatRasterCollection so merge/mosaic is efficient
  rc <- sprc(tiles)
  
  # If tiles do NOT overlap -> merge is best
  # If they DO overlap and you want a rule -> use mosaic(fun=...) instead
  r_merged <- merge(rc)
  
  # Write merged (optional but recommended to avoid keeping huge object in memory)
  writeRaster(r_merged, out_merged, overwrite=TRUE, wopt=list(gdal="COMPRESS=LZW"))
  
}

# Reload from disk for a clean pipeline
r_merged <- rast(out_merged)

# Sanity check
stopifnot(nlyr(r_merged) >= 1)

# 2) Eligible plantation mask ----
lulc <- rast(paste0(countrydir,"/LULCC/TempRaster/LULCt1_c_original.tif"))  # already cropped to Zambia, 1 km, World Mercator
npa <- rast(paste0(countrydir,"/LULCC/TempRaster/npa_c_original.tif"))
roads <- rast(paste0(countrydir,"/LULCC/TempRaster/roads_c_original.tif"))

if (!file.exists(paste0(plant_wd,"temp/r_merged_1km_merc.tif"))) {
  cat("Projecting merged file: ", paste0(plant_wd,"temp/ZMB_productivity_merged_24b.tif"), "\n")
  
  r_merged_1km <- project(
    r_merged, lulc, method = "bilinear",
    filename = "temp/r_merged_1km_merc.tif",
    overwrite = TRUE,
    wopt = list(gdal = "COMPRESS=LZW", datatype = "FLT4S")
  )
  plot(r_merged_1km[[1]])
} else {
  r_merged_1km <- rast(paste0(plant_wd,"temp/r_merged_1km_merc.tif"))
  plot(r_merged_1km[[1]])
}

## Check that projection and sampling dont alter too much the aggregated results!
### Predict AGB at year t from Chapman–Richards params a,b,c
agb_pred_year <- function(r, t, a_name="a", b_name="b", c_name="c",
                          rho=rhov, BEF=BEFv) {
  
  # sanity check that a,b,c exist
  if (!all(c(a_name,b_name,c_name) %in% names(r))) {
    stop("Missing one of a,b,c in r. Available names:\n", paste(names(r), collapse=", "))
  }
  
  a  <- r[[a_name]]
  b  <- r[[b_name]]
  c_ <- r[[c_name]]
  
  Vt   <- a * (1 - exp(-b * t))^c_
  AGBt <- Vt * (rho * BEF)
  
  names(AGBt) <- paste0("AGB_pred_t", t)
  AGBt
}

if (check_projection_24b == 1){

agb25_pred <- agb_pred_year(r_merged, 25)
agb25_pred_1km <- agb_pred_year(r_merged_1km, 25)

# writeRaster(agb25_pred, "temp/AGB_pred_t25.tif", overwrite=TRUE)
# writeRaster(agb25_pred_1km, "temp/AGB_pred_t25_1km.tif", overwrite=TRUE)

# AGB in Mg/ha -> total Mg by multiplying by cell area (ha), then sum
total_Mg <- function(agb_mg_ha) {
  a_ha <- cellSize(agb_mg_ha, unit = "ha")
  global(agb_mg_ha * a_ha, "sum", na.rm = TRUE)
}

# Your two rasters:
# agb25_pred      : ~100m, lon/lat
# agb25_pred_1km  : 1km, mercator (from r_merged_1km)

tot_100m <- total_Mg(agb25_pred)
tot_1km  <- total_Mg(agb25_pred_1km)

tot_100m
tot_1km

pct_diff <- 100 * (tot_1km - tot_100m) / tot_100m
pct_diff

}

# Build the eligible plantation mask from LULC classes ONLY first ----
gp <- read.csv(paste0(countrydir,"/LULCC/SourceData/InTables/growth_parameters1.csv"), stringsAsFactors = FALSE)
tof <- read.csv(paste0(countrydir,"/LULCC/TempTables/TOFvsFOR_Categories1.csv"), stringsAsFactors = FALSE)
tail(gp)
tail(tof)

# Make sure Keys are numeric
gp$Key  <- as.integer(gp$Key)
tof$Key <- as.integer(tof$Key)

max_gp  <- max(gp$Key, na.rm = TRUE)
max_tof <- max(tof$Key, na.rm = TRUE)

if (max_tof == max_gp + 1) {

  message("Removing extra TOF row with Key = ", max_tof)

  tof <- tof[tof$Key != max_tof, ]

} else {

  message("No TOF row removed. max(gp$Key) = ", max_gp,
          ", max(tof$Key) = ", max_tof)

}
tail(gp)
tail(tof)

# LULC KEYWORDS ---- 
lulc_keywords <- c("Barren", "Shrublands")
lulc_pattern  <- paste(lulc_keywords, collapse = "|")
# MANUAL CLASSES ----
manual_classes <- c(39)  # optional e.g. 23

lulc_classes_keep <- gp %>%
  dplyr::filter(str_detect(LULC, regex(lulc_pattern, ignore_case = TRUE))) %>%
  dplyr::pull(`Key.`) %>%
  base::unique() %>%
  base::union(manual_classes) %>%
  base::sort()

eligible_mask_1km <- ifel(lulc %in% lulc_classes_keep, 1, NA)
plot(lulc)
names(eligible_mask_1km) <- "eligible_mask_1km"
lulc_classes_keep

# lulc and r_merged_1km should match, but let's be safe
compareGeom(lulc, r_merged_1km[[1]], stopOnError = FALSE)

r_lulc_eligible_1km <- mask(r_merged_1km, eligible_mask_1km)
plot(r_lulc_eligible_1km[[1]])

# --- 1) Remove pixels overlapping protected areas (where npa is non-NA) ---
# Keep eligible pixels only where npa is NA
r1 <- mask(r_lulc_eligible_1km, npa, maskvalues = NA, inverse = TRUE)
plot(npa)
# Explanation: inverse=TRUE keeps cells where npa is NA; masks where npa is not NA
plot(r_lulc_eligible_1km[[1]])
plot(r1[[1]])

# --- 2) Remove pixels farther than X meters from selected road classes ---
# Create a binary raster of "selected roads"
roads_sel <- roads %in% road_classes_to_use

# Convert FALSE to NA so distance() treats only TRUE cells as sources
roads_src <- ifel(roads_sel, 1, NA)

# Distance-to-selected-roads in meters (because World Mercator units are meters)
d2road <- distance(roads_src)

# Keep only pixels within threshold
r2 <- mask(r1, d2road, maskvalues = (max_dist_m), inverse = FALSE)
# maskvalues here works differently depending on terra version; safest is explicit:
r2 <- ifel(d2road <= max_dist_m, r1, NA)
plot(r2[[1]])

# --- 3) Remove patches smaller than N pixels (e.g., 10) ---
elig_bin <- ifel(!is.na(r2), 1, NA)

patch_id <- patches(elig_bin, directions = 8)

# Count pixels per patch
f <- freq(patch_id)

small_ids <- f$value[f$count <= min_pix]

good_patch <- ifel(patch_id %in% small_ids, NA, 1)

r3 <- mask(r2, good_patch, maskvalues = NA)
plot(r2[[1]])
plot(r3[[1]])

# Result
r_lulc_eligible_1km_constrained <- r3
names(r_lulc_eligible_1km_constrained) <- names(r_lulc_eligible_1km)
plot(r_lulc_eligible_1km_constrained[[1]])

writeRaster(r_lulc_eligible_1km, "temp/r_merged_1km_LULCeligible.tif",
             overwrite=TRUE, wopt=list(gdal="COMPRESS=LZW"), datatype="FLT4S")
writeRaster(r_lulc_eligible_1km_constrained, "temp/r_lulc_eligible_1km_constrained.tif",
            overwrite=TRUE, wopt=list(gdal="COMPRESS=LZW"), datatype="FLT4S")

# 2) Build a productivity score (choose a rotation age)
H <- Tend - T1 + 1   # inclusive window = 24 yrs
Ts <- 2:H

total_charcoal_onecut <- function(r, T) {
  
  agbT <- agb_pred_year(r, T)   # Mg/ha
  
  area_ha <- cellSize(agbT, unit="ha")
  tot_AGB <- global(agbT * area_ha, "sum", na.rm = TRUE)[1,1]
  
  tot_char <- tot_AGB / efchratio
  
  c(T=T,
    total_AGB_Mg = tot_AGB,
    total_charcoal_Mg = tot_char)
}

tab <- do.call(rbind, lapply(Ts, function(T)
  total_charcoal_onecut(r_lulc_eligible_1km_constrained, T)
))
tab <- as.data.frame(tab)

# ---- add harvest-count + period totals ----
tab$n_harvests_2027_2050 <- floor(H / tab$T)

tab$total_charcoal_2027_2050 <- tab$total_charcoal_Mg *
  tab$n_harvests_2027_2050

tab

tabTr <- tab %>% dplyr::filter(T >= Tr)
tabTr

# 3) Pick the year with highest production potential ----
ggplot(tabTr, aes(x = T, y = total_charcoal_2027_2050)) +
  geom_line() +
  geom_point(size = 2) +
  labs(
    x = "Rotation age T (years)",
    y = "Total charcoal 2027–2050 (Mg)",
    title = paste0("Charcoal production vs rotation age (T ≥ ",Tr,")")
  ) +
  theme_minimal()

# readline("Check Rotation Cycle — press [enter] to continue")
# browser(text = "Check Rotation Cycle")

ggplot(tabTr, aes(x = T, y = n_harvests_2027_2050)) +
  geom_step() +
  geom_point(size = 2) +
  labs(
    x = "Rotation age T (years)",
    y = "Number of harvests between 2027–2050",
    title = "Discrete harvest opportunities drive dents"
  ) +
  theme_minimal()

# Even annual production ----
# This will build a year-by-year series from 2027–2050, with zeros until you start harvesting (e.g., 2035), then constant.

charcoal_even_supply <- function(r, T,
                                 start_year = T1,
                                 end_year   = Tend,
                                 start_harvest_year = T1 + Tr, 
                                 conv = efchratio) {
  
  # One full harvest at age T (Mg charcoal)
  agbT <- agb_pred_year(r, T)                 # Mg/ha
  area_ha <- cellSize(agbT, unit="ha")
  char_onecut <- global((agbT * area_ha) / conv, "sum", na.rm=TRUE)[1,1]
  
  # Regulated annual flow (Mg/yr)
  char_annual <- char_onecut / T
  
  years <- start_year:end_year
  
  data.frame(
    year = years,
    T = T,
    annual_charcoal_Mg = ifelse(years < start_harvest_year, 0, char_annual),
    char_onecut_Mg = char_onecut,
    char_annual_steady_Mg = char_annual
  )
}

supply_opt <- charcoal_even_supply(r_lulc_eligible_1km_constrained, T=opt_rotation,
                                 start_harvest_year = (T1 + Tr))
supply_opt

tail(gp)
tail(tof)
# --- gp: add final row ---
gp_fix <- gp %>%
  bind_rows(
    tibble(
      Key.   = max(gp$Key., na.rm = TRUE) + 1,
      LULC  = "Plantations_Forced",
      rmax  = 0,
      rmaxSD= 0,
      K     = Kv,
      KSD   = KSDv,
      TOF   = 1
    )
  )

# --- tof: add final row ---
tof_fix <- tof %>%
  bind_rows(
    tibble(
      Key = max(tof$Key, na.rm = TRUE) + 1,
      x   = 1
    )
  )

gp_fix$Key.  <- as.integer(gp_fix$Key.)
tof_fix$Key <- as.integer(tof_fix$Key)
tail(gp_fix)
tail(tof_fix)

r_lulc_eligible_1km_constrained

# dd to LULC and run Mofuss R scrpts for zambia plant!!!
new_class_value <- max(tof$Key, na.rm = TRUE) + 1

# Output
out_lulc_edited <- "temp/LULCt1_c_plant.tif"
 
# Mask
mask <- !is.na(r_lulc_eligible_1km_constrained) & (r_lulc_eligible_1km_constrained != 0)
mask <- mask[[1]]
plot(mask)
 
# Apply update: only where mask is TRUE set to 717, else keep original LULC
lulc_new <- ifel(mask, new_class_value, lulc)

# Write output (use integer datatype)
writeRaster(
  lulc_new, out_lulc_edited,
  overwrite = TRUE,
  datatype = "INT2U",
  wopt = list(gdal = "COMPRESS=LZW")
)

# Affect demand table just as if it were demand tunning 1 ----
demand_4plant <- read.csv(
  paste0(countrydir, "/LULCC/DownloadedDatasets/SourceDataGlobal/demand/demand_in/cons_fuels_years_BAU_Lusaka-NotLusaka.csv"),
  stringsAsFactors = FALSE
)
unique(demand_4plant$area)

# wood-equivalent supply by year
sup <- supply_opt %>%
  dplyr::transmute(year = as.integer(year), sub = annual_charcoal_Mg * efchratio)

if (any(duplicated(sup$year))) stop("sup has duplicate years")

demand_4plant$year <- as.integer(demand_4plant$year)

# 1) Update ONLY Lusaka / Urban / charcoal (by year), keep >= 1
u_new <- demand_4plant %>%
  dplyr::filter(country=="Lusaka", area=="Urban", fuel=="charcoal") %>%
  dplyr::left_join(sup, by="year") %>%
  dplyr::mutate(fuel_tons3 = as.integer(pmax(1, round(fuel_tons3 - dplyr::coalesce(sub, 0))))) %>%
  dplyr::select(-sub)

# 2) Put those rows back; everything else stays exactly the same
demand_adj <- demand_4plant %>%
  dplyr::anti_join(u_new, by=c("iso3","country","region","area","fuel","year")) %>%
  dplyr::bind_rows(u_new)

# 3) Rebuild Overall for Lusaka + charcoal as Rural + Urban
overall_new <- demand_adj %>%
  dplyr::filter(country=="Lusaka", fuel=="charcoal", area %in% c("Rural","Urban")) %>%
  dplyr::group_by(iso3, country, region, fuel, year) %>%
  dplyr::summarise(
    area = "Overall",
    dplyr::across(dplyr::where(is.numeric), ~ sum(.x, na.rm=TRUE)),
    .groups = "drop"
  )

# 4) Replace old Overall rows with rebuilt ones
demand_adj <- demand_adj %>%
  dplyr::filter(!(country=="Lusaka" & fuel=="charcoal" & area=="Overall")) %>%
  dplyr::bind_rows(overall_new)

readr::write_csv(
  demand_adj,
  paste0(countrydir, "/LULCC/DownloadedDatasets/SourceDataGlobal/demand/demand_in/cons_fuels_years_BAU_Lusaka-NotLusaka.csv")
)

# Seek 25%
check_lusaka_2010_2050 <- function(demand_4plant, demand_adj) {
  
  o <- demand_4plant %>%
    dplyr::filter(country=="Lusaka", area=="Urban", fuel=="charcoal", year %in% 2010:2050) %>%
    dplyr::summarise(total = sum(fuel_tons3, na.rm = TRUE)) %>%
    dplyr::pull(total)
  
  a <- demand_adj %>%
    dplyr::filter(country=="Lusaka", area=="Urban", fuel=="charcoal", year %in% 2010:2050) %>%
    dplyr::summarise(total = sum(fuel_tons3, na.rm = TRUE)) %>%
    dplyr::pull(total)
  
  ratio <- a / o
  reduction <- 100 * (1 - ratio)
  
  cat("Original (sum 2010–2050):", round(o), "\n")
  cat("Adjusted (sum 2010–2050):", round(a), "\n")
  cat("Reduction:", round(reduction, 1), "%\n")
  
  if (reduction >= 20 && reduction <= 30) cat("✅ Within target range (20–30%)\n")
  else cat("❌ Outside target range\n")
  
  invisible(reduction)
}

# Check the % of reduction here before going forward
check_lusaka_2010_2050(demand_4plant, demand_adj)

source(paste0(githubdir,"/localhost/scripts/2b_demand_tables_v5.R"),
       echo = TRUE)

source(paste0(githubdir,"/localhost/scripts/3_demand4IDW_v7.R"),
       echo = TRUE)

source(paste0(githubdir,"/localhost/scripts/5_harmonizer_v4.R"),
       echo = TRUE)

source(paste0(githubdir,"/localhost/scripts/6_scenarios.R"),
       echo = TRUE)

# Write output (use integer datatype) into mofuss working folder
writeRaster(
  lulc_new, paste0(countrydir,"/LULCC/TempRaster/LULCt1_c_plant.tif"), # Dinamica must read this!
  overwrite = TRUE,
  datatype = "INT2U",
  wopt = list(gdal = "COMPRESS=LZW")
)

write_csv(
  gp_fix,
  paste0(countrydir, "/LULCC/TempTables/growth_parameters1.csv")
)

write_csv(
  tof_fix,
  paste0(countrydir, "/LULCC/TempTables/TOFvsFOR_Categories1.csv")
)

check_lusaka_2010_2050(demand_4plant, demand_adj)

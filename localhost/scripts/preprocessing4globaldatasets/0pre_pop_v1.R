# MoFuSS
# Version 4
# Date: Ago 2025

# 2dolist
# Check Terra's handling of VRT, sometimes it doesn't work and need to be updated
# Need to restart R and windows before running this script

# Internal parameters
# cpu = "nrb" #"alien" # vs nrb
# if (cpu == "alien") {
#   gdrivedir <- "G:/My Drive/webpages/2024_MoFuSSGlobal_Datasets/"
# } else {
#   gdrivedir <- "G:/Mi Unidad/webpages/2024_MoFuSSGlobal_Datasets/"
# }

# Define all folders based on node ----
# Detect OS and node name
os <- Sys.info()["sysname"]
node_name <- Sys.info()[["nodename"]]
cat(os,node_name)

if (os == "Windows" & node_name == "WINLANASE") {
  #ADD node
  #demanddir <- "F:/demand"
  #admindir <- "F:/admin_regions"
  #emissionsdir <- "F:/emissions"
  #rTempdir <- "F:/rTemp"
  #globalsouth_mofuss_bindingfolder <- "G:/My Drive/webpages/2024_MoFuSSGlobal_Datasets/mofussDS_v2/globalsouth_mofuss_bindingfolder"
  gdrivedir <- "G:/My Drive/webpages/2025_MoFuSSGlobal_Datasets/"
  
} else if (os == "Windows" & node_name == "ASUSLAP"){
  #ADD node
  # demanddir <- "C:/Users/aghil/Documents/MoFuSS_FAO_localhost/demand"
  # admindir <- "C:/Users/aghil/Documents/MoFuSS_FAO_localhost/admin_regions"
  # emissionsdir <- "C:/Users/aghil/Documents/MoFuSS_FAO_localhost/emissions"
  # rTempdir <- "C:/Users/aghil/Documents/MoFuSS_FAO_localhost/rTemp"
  gdrivedir <- "G:/Mi Unidad/webpages/2025_MoFuSSGlobal_Datasets/"
  
} else if (os == "Windows" & node_name == "EDITORIALCIGA"){
  #ADD node
  demanddir <- "E:/demand"
  admindir <- "E:/admin_regions"
  emissionsdir <- "E:/emissions"
  rTempdir <- "E:/rTemp"
  
} else if (os == "Linux" & node_name == "linux-c3"){
  #ADD node
  demanddir <- "/home/mofuss/demand"
  admindir <- "/home/mofuss/admin_regions"
  emissionsdir <- "/home/mofuss/emissions"
  rTempdir <- "/home/mofuss/rTemp"
  
} else if (os == "Windows" & node_name == "NRBV1"){
  #ADD node
  demanddir <- "D:/demand"
  admindir <- "D:/admin_regions"
  emissionsdir <- "D:/emissions"
  rTempdir <- "D:/rTemp"
  
}

# Load packages ----
library(terra)
terraOptions(steps = 55)
terraOptions(progress=0)
library(tidyterra)
library(tidyverse)
library(sf)
library(mapview)
library(tictoc)
library(mapview)
library(readxl)
library(hacksaw)

setwd(countrydir)
getwd()
country_name

# Read parameters table ----
if (webmofuss == 1) {
  # Read parameters table in webmofuss
  country_parameters <- read_csv(parameters_file_path)
} else if(webmofuss == 0) {
  # Read parameters table (recognizing the delimiter)
  detect_delimiter <- function(file_path) {
    # Read the first line of the file
    first_line <- readLines(file_path, n = 1)
    # Check if the first line contains ',' or ';'
    if (grepl(";", first_line)) {
      return(";")
    } else {
      return(",")
    }
  }
  # Detect the delimiter
  delimiter <- detect_delimiter(parameters_file_path)
  # Read the CSV file with the detected delimiter
  country_parameters <- read_delim(parameters_file_path, delim = delimiter)
  print(tibble::as_tibble(country_parameters), n=100)
}

country_parameters %>%
  dplyr::filter(Var == "proj_gcs") %>%
  pull(ParCHR) -> proj_gcs

country_parameters %>%
  dplyr::filter(Var == "epsg_gcs") %>%
  pull(ParCHR) %>%
  as.integer(.) -> epsg_gcs

country_parameters %>%
  dplyr::filter(Var == "proj_pcs") %>%
  pull(ParCHR) -> proj_pcs

country_parameters %>%
  dplyr::filter(Var == "epsg_pcs") %>%
  pull(ParCHR) %>%
  as.integer(.) -> epsg_pcs

country_parameters %>%
  dplyr::filter(Var == "proj_authority") %>%
  pull(ParCHR) -> proj_authority

setwd(gdrivedir)

# HSRL 1km ----
## Global ----
tic("Global HRSL")
GlobalHRSL30moriginal <- list.files(path = "population_in/HRSL/population_af_2018-10-01_Global/",
                                  pattern = "population.*\\.tif$|general.*\\.tif$", full.names = TRUE)
GlobalHRSL30moriginalVRT <- vrt(GlobalHRSL30moriginal, "HRSL_VRT/GlobalHSRL.vrt", overwrite=TRUE)

GlobalHRSL <- rast(GlobalHRSL30moriginalVRT)
res(GlobalHRSL)
res(GlobalHRSL) <- 0.0083333333 # 0.01 double check... (0.009?) 0.01 = 1.11 km (2 decimals, km accuracy)
GlobalHRSL <- terra::resample(GlobalHRSL30moriginalVRT, GlobalHRSL, "sum")
terra::writeRaster(GlobalHRSL,
                   "population_in/HRSL/outputmaps/hsrl_global1000m_gcs.tif",
                   filetype = "GTiff", overwrite = TRUE)
toc()

# Project geotiff to world mercator: # https://epsg.io/3395 World Mercator = EPSG:3395
# Project geotiff to Mollweide: # https://epsg.io/54009 Mollweide = ESRI:54009
# If TRUE the GDAL-warp algorithm is used. 
# Otherwise a slower internal algorithm is used that may be more accurate if there is much variation in the cell sizes of the output raster. 
# Only the near and bilinear algorithms are available for the internal algorithm
popvrt_global <- rast("population_in/HRSL/outputmaps/hsrl_global1000m_gcs.tif")
popvrt_global_p <- popvrt_global %>% terra::project(paste0(proj_authority,":",epsg_pcs),
                                                    method = "bilinear", 
                                                    gdal = FALSE,
                                                    res = 1000)
terra::writeRaster(popvrt_global_p,
                   "population_in/HRSL/outputmaps/hsrl_global1000m_pcs.tif",
                   filetype = "GTiff", overwrite = TRUE)

popglo_gcs <- popvrt_global %>%
  terra::global(., 'sum', na.rm=TRUE) %>%
  pull(sum)
popglo_wm <- popvrt_global_p %>%
  terra::global(., 'sum', na.rm=TRUE) %>%
  pull(sum)
popglo_gcs/popglo_wm

### Copy 2 MoFuSS ----
copy2mofussfiles_hsrl <- c("hsrl_global1000m_gcs.tif", "hsrl_global1000m_pcs.tif")
for (f in copy2mofussfiles) {
  file.copy(from=f, 
            to=paste0(demanddir,"/demand_in/"),  
            overwrite = TRUE, recursive = TRUE, copy.mode = TRUE)
}

# HSRL 100m ----
## Nepal ---- 
# NEED 2B FIXED
tic("Nepal local HRSL")
NepalHRSL30moriginal <- list.files(path = "population_in/HRSL/population_af_2018-10-01_Asia/",
                                   pattern = "population_npl.*\\.tif$", full.names = TRUE)
NepalHRSL30moriginalVRT <- vrt(NepalHRSL30moriginal, "HRSL_VRT/GlobalHSRL.vrt", overwrite=TRUE)

NepalHRSL <- rast(NepalHRSL30moriginalVRT)
res(NepalHRSL)
res(NepalHRSL) <- 0.00083333333 #100m approx
NepalHRSL <- terra::resample(NepalHRSL30moriginalVRT, NepalHRSL, "sum")
terra::writeRaster(NepalHRSL,
                   "population_in/HRSL/100m/popvrt_nepal.tif",
                   filetype = "GTiff", overwrite = TRUE)

gcs_pop <- NepalHRSL %>%
  terra::global(., 'sum', na.rm=TRUE) %>%
  pull(sum)

toc()

tic("Nepal projection")
# Project geotiff to world mercator: # https://epsg.io/3395 World Mercator
popvrt_nepal <- rast("population_in/HRSL/100m/popvrt_nepal.tif")
popvrt_nepal_p <- popvrt_nepal %>% terra::project("EPSG:3395",
                                                  method = "bilinear", # benchmark
                                                  gdal = FALSE, # quite sensitive to GDAL-warp algorithm
                                                  res = 100)
terra::writeRaster(popvrt_nepal_p,
                   "population_in/HRSL/100m/popvrt_nepal_p.tif",
                   filetype = "GTiff", overwrite = TRUE)
wm_pop <- popvrt_nepal_p %>%
  terra::global(., 'sum', na.rm=TRUE) %>%
  pull(sum)
47828048
wm_pop


popvrt_nepal_padj <- (gcs_pop/wm_pop)*popvrt_nepal_p
wm_pop_adj <- popvrt_nepal_padj %>%
  terra::global(., 'sum', na.rm=TRUE) %>%
  pull(sum)
terra::writeRaster(popvrt_nepal_padj,
                   "population_in/HRSL/100m/popvrt_nepal_padj.tif",
                   filetype = "GTiff", overwrite = TRUE)
toc()

## Rwanda ----
# NEED 2B FIXED
tic("Rwanda local HRSL")

globaladmin <- vect("admin_regions/regions_adm0/mofuss_regions0.gpkg")
rwandaadmin <- globaladmin %>% dplyr::filter(GID_0 == "RWA")

RwandaHRSL30moriginal <- list.files(path = "population_in/HRSL/population_af_2018-10-01_SSA/",
                                    pattern = "population_AF20_2018-10-01.*\\.tif$", full.names = TRUE)
RwandaHRSL30moriginalVRT <- vrt(RwandaHRSL30moriginal, "HRSL_VRT/GlobalHSRL.vrt", overwrite=TRUE)

RwandaHRSL <- rast(RwandaHRSL30moriginal)
res(RwandaHRSL)
res(RwandaHRSL) <- 0.00083333333 #100m approx
RwandaHRSL <- terra::resample(RwandaHRSL30moriginalVRT, RwandaHRSL, "sum")
RwandaHRSL_c <- crop(RwandaHRSL, ext(rwandaadmin) + .01)
RwandaHRSL_m <- mask(RwandaHRSL_c, rwandaadmin)
plot(RwandaHRSL_m)
lines(rwandaadmin)
terra::writeRaster(RwandaHRSL_m,
                   "population_in/HRSL/100m/popvrt_rwanda.tif",
                   filetype = "GTiff", overwrite = TRUE)

gcs_pop <- RwandaHRSL_m %>%
  terra::global(., 'sum', na.rm=TRUE) %>%
  pull(sum)

toc()

tic("Rwanda projection")
# Project geotiff to world mercator: # https://epsg.io/3395 World Mercator
popvrt_rwanda <- rast("population_in/HRSL/100m/popvrt_rwanda.tif")
popvrt_rwanda_p <- popvrt_rwanda %>% terra::project("EPSG:3395",
                                                    method = "bilinear", # benchmark
                                                    gdal = FALSE, # super sensitive to GDAL-warp algorithm
                                                    res = 100)
terra::writeRaster(popvrt_rwanda_p,
                   "population_in/HRSL/100m/popvrt_rwanda_p.tif",
                   filetype = "GTiff", overwrite = TRUE)
wm_pop <- popvrt_rwanda_p %>%
  terra::global(., 'sum', na.rm=TRUE) %>%
  pull(sum) # 12.53 millions
wm_pop

popvrt_rwanda_padj <- (gcs_pop/wm_pop)*popvrt_rwanda_p
wm_pop_adj <- popvrt_rwanda_padj %>%
  terra::global(., 'sum', na.rm=TRUE) %>%
  pull(sum)
terra::writeRaster(popvrt_rwanda_padj,
                   "population_in/HRSL/100m/popvrt_rwanda_padj.tif",
                   filetype = "GTiff", overwrite = TRUE)
toc()



# WORLDPOP 1km ----
## Global ----
tic("Global WorldPop")
GlobalWorldPop100moriginal <- list.files(path = "population_in/WORLDPOP/Global/",
                                    pattern = "2020_UNadj_constrained.*\\.tif$", full.names = TRUE)
GlobalWorldPop100moriginalVRT <- vrt(GlobalWorldPop100moriginal, "HRSL_VRT/GlobalWorldPop.vrt", overwrite=TRUE)

GlobalWorldPop <- rast(GlobalWorldPop100moriginalVRT)
res(GlobalWorldPop)
res(GlobalWorldPop) <- 0.0083333333 # 0.0089831528 from original GEE
GlobalWorldPop <- terra::resample(GlobalWorldPop100moriginalVRT, GlobalWorldPop, "sum")
terra::writeRaster(GlobalWorldPop,
                   "population_in/WORLDPOP/outputmaps/wp_global1000m_gcs.tif",
                   filetype = "GTiff", overwrite = TRUE)
toc()
popvrt_global_wp <- rast("population_in/WORLDPOP/outputmaps/wp_global1000m_gcs.tif")
popvrt_global__wp_p <- popvrt_global_wp %>% terra::project(paste0(proj_authority,":",epsg_pcs),
                                                    method = "bilinear", 
                                                    gdal = FALSE,
                                                    res = 1000)
terra::writeRaster(popvrt_global__wp_p,
                   "population_in/WORLDPOP/outputmaps/wp_global1000m_pcs.tif",
                   filetype = "GTiff", overwrite = TRUE)

popglo_gcs_wp <- popvrt_global_wp %>%
  terra::global(., 'sum', na.rm=TRUE) %>%
  pull(sum)
popglo_wm_wp <- popvrt_global__wp_p %>%
  terra::global(., 'sum', na.rm=TRUE) %>%
  pull(sum)
popglo_gcs_wp/popglo_wm_wp

### Copy 2 MoFuSS ----
copy2mofussfiles_wp <- c("population_in/WORLDPOP/outputmaps/wp_global1000m_gcs.tif",
                         "population_in/WORLDPOP/outputmaps/wp_global1000m_pcs.tif")
for (f in copy2mofussfiles_wp) {
  file.copy(from=f, 
            to=paste0(demanddir,"/demand_in/"),  
            overwrite = TRUE, recursive = TRUE, copy.mode = TRUE)
}

## Kenya ----
tic("Kenya WorldPop")
KenyaWorldPop100moriginal <- list.files(path = "population_in/WORLDPOP/Global/",
                                         pattern = "ken_ppp_2020_UNadj_constrained.*\\.tif$", full.names = TRUE)
KenyaWorldPop100moriginalVRT <- vrt(KenyaWorldPop100moriginal, "WP_VRT/KenyaGlobalWorldPop.vrt", overwrite=TRUE)

KenyaGlobalWorldPop <- rast(KenyaWorldPop100moriginalVRT)
res(KenyaGlobalWorldPop)
res(KenyaGlobalWorldPop) <- 0.0083333333 # 0.0089831528 from original GEE
KenyaGlobalWorldPop <- terra::resample(KenyaWorldPop100moriginalVRT, KenyaGlobalWorldPop, "sum")
terra::writeRaster(KenyaGlobalWorldPop,
                   "population_in/WORLDPOP/outputmaps/wp_kenya1000m_gcs.tif",
                   filetype = "GTiff", overwrite = TRUE)
toc()
popvrt_kenyaglobal_wp <- rast("population_in/WORLDPOP/outputmaps/wp_kenya1000m_gcs.tif")
popvrt_kenyaglobal__wp_p <- popvrt_kenyaglobal_wp %>% terra::project(paste0(proj_authority,":",epsg_pcs),
                                                           method = "bilinear", 
                                                           gdal = FALSE,
                                                           res = 1000)
terra::writeRaster(popvrt_kenyaglobal__wp_p,
                   "population_in/WORLDPOP/outputmaps/wp_kenya1000m_pcs.tif",
                   filetype = "GTiff", overwrite = TRUE)

popkenglo_gcs_wp <- popvrt_kenyaglobal_wp %>%
  terra::global(., 'sum', na.rm=TRUE) %>%
  pull(sum)
popkenglo_wm_wp <- popvrt_kenyaglobal__wp_p %>%
  terra::global(., 'sum', na.rm=TRUE) %>%
  pull(sum)
popkenglo_gcs_wp/popkenglo_wm_wp

### Copy 2 MoFuSS ----
copy2mofussfiles_wp <- c("population_in/WORLDPOP/outputmaps/wp_kenya1000m_gcs.tif",
                         "population_in/WORLDPOP/outputmaps/wp_kenya1000m_pcs.tif")
for (fk in copy2mofussfiles_wp) {
  file.copy(from=fk, 
            to=paste0(demanddir,"/demand_in/"),  
            overwrite = TRUE, recursive = TRUE, copy.mode = TRUE)
}

## Uganda ----
tic("Uganda WorldPop")
UgandaWorldPop100moriginal <- list.files(path = "population_in/WORLDPOP/Global/",
                                        pattern = "uga_ppp_2020_UNadj_constrained.*\\.tif$", full.names = TRUE)
UgandaWorldPop100moriginalVRT <- vrt(UgandaWorldPop100moriginal, "WP_VRT/UgandaGlobalWorldPop.vrt", overwrite=TRUE)

UgandaGlobalWorldPop <- rast(UgandaWorldPop100moriginalVRT)
res(UgandaGlobalWorldPop)
res(UgandaGlobalWorldPop) <- 0.0083333333 # 0.0089831528 from original GEE
UgandaGlobalWorldPop <- terra::resample(UgandaWorldPop100moriginalVRT, UgandaGlobalWorldPop, "sum")
terra::writeRaster(UgandaGlobalWorldPop,
                   "population_in/WORLDPOP/outputmaps/wp_rwanda1000m_gcs.tif",
                   filetype = "GTiff", overwrite = TRUE)
toc()
popvrt_ugandaglobal_wp <- rast("population_in/WORLDPOP/outputmaps/wp_uganda1000m_gcs.tif")
popvrt_ugandaglobal__wp_p <- popvrt_ugandaglobal_wp %>% terra::project(paste0(proj_authority,":",epsg_pcs),
                                                                     method = "bilinear", 
                                                                     gdal = FALSE,
                                                                     res = 1000)
terra::writeRaster(popvrt_ugandaglobal__wp_p,
                   "population_in/WORLDPOP/outputmaps/wp_uganda1000m_pcs.tif",
                   filetype = "GTiff", overwrite = TRUE)

popugaglo_gcs_wp <- popvrt_ugandaglobal_wp %>%
  terra::global(., 'sum', na.rm=TRUE) %>%
  pull(sum)
popugaglo_wm_wp <- popvrt_ugandaglobal__wp_p %>%
  terra::global(., 'sum', na.rm=TRUE) %>%
  pull(sum)
popugaglo_gcs_wp/popugaglo_wm_wp

### Copy 2 MoFuSS ----
copy2mofussfiles_wp <- c("population_in/WORLDPOP/outputmaps/wp_uganda1000m_gcs.tif",
                         "population_in/WORLDPOP/outputmaps/wp_uganda1000m_pcs.tif")
for (fu in copy2mofussfiles_wp) {
  file.copy(from=fu, 
            to=paste0(demanddir,"/demand_in/"),  
            overwrite = TRUE, recursive = TRUE, copy.mode = TRUE)
}

# WORLDPOP 100m ----
## Nepal ----
tic("Nepal WorldPop")
NepalWorldPop100moriginal <- list.files(path = "population_in/WORLDPOP/Asia/",
                                         pattern = "npl_ppp_2020_UNadj_constrained.*\\.tif$", full.names = TRUE)
NepalWorldPop100moriginalVRT <- vrt(NepalWorldPop100moriginal, "WP_VRT/NepalWorldPop.vrt", overwrite=TRUE)

NepalWorldPop <- rast(NepalWorldPop100moriginalVRT)
res(NepalWorldPop)
res(NepalWorldPop) <- 0.00083333333 #100m approx
NepalWorldPop <- terra::resample(NepalWorldPop100moriginalVRT, NepalWorldPop, "sum")
terra::writeRaster(NepalWorldPop,
                   "population_in/WORLDPOP/outputmaps/100m/wp_nepal100m_gcs.tif",
                   filetype = "GTiff", overwrite = TRUE)
toc()
popvrt_nepal_wp <- rast("population_in/WORLDPOP/outputmaps/100m/wp_nepal100m_gcs.tif")
popvrt_nepal__wp_p <- popvrt_nepal_wp %>% terra::project(paste0(proj_authority,":",epsg_pcs), # Try another projection!
                                                           method = "bilinear", 
                                                           gdal = FALSE,
                                                           res = 100)
terra::writeRaster(popvrt_nepal__wp_p,
                   "population_in/WORLDPOP/outputmaps/100m/wp_nepal100m_pcs.tif",
                   filetype = "GTiff", overwrite = TRUE)

popnpl_gcs_wp <- popvrt_nepal_wp %>%
  terra::global(., 'sum', na.rm=TRUE) %>%
  pull(sum)
popnpl_wm_wp <- popvrt_nepal__wp_p %>%
  terra::global(., 'sum', na.rm=TRUE) %>%
  pull(sum)
popnpl_gcs_wp/popnpl_wm_wp #See if new projection make these two values more even

### Copy 2 MoFuSS ----
copy2mofussfiles_wp <- c("population_in/WORLDPOP/outputmaps/100m/wp_nepal100m_gcs.tif",
                         "population_in/WORLDPOP/outputmaps/100m/wp_nepal100m_pcs.tif")
for (f in copy2mofussfiles_wp) {
  file.copy(from=f, 
            to=paste0(demanddir,"/demand_in/"),  
            overwrite = TRUE, recursive = TRUE, copy.mode = TRUE)
}

## Malawi ----
tic("Malawi WorldPop")
MalawiWorldPop100moriginal <- list.files(path = "population_in/WORLDPOP/Africa/",
                                        pattern = "mwi_ppp_2020_UNadj_constrained.*\\.tif$", full.names = TRUE)
MalawiWorldPop100moriginalVRT <- vrt(MalawiWorldPop100moriginal, "WP_VRT/MalawiWorldPop.vrt", overwrite=TRUE)

MalawiWorldPop <- rast(MalawiWorldPop100moriginalVRT)
res(MalawiWorldPop)
res(MalawiWorldPop) <- 0.00083333333 #100m approx
MalawiWorldPop <- terra::resample(MalawiWorldPop100moriginalVRT, MalawiWorldPop, "sum")
terra::writeRaster(MalawiWorldPop,
                   "population_in/WORLDPOP/outputmaps/100m/wp_malawi100m_gcs.tif",
                   filetype = "GTiff", overwrite = TRUE)
toc()
popvrt_malawi_wp <- rast("population_in/WORLDPOP/outputmaps/100m/wp_malawi100m_gcs.tif")
popvrt_malawi__wp_p <- popvrt_malawi_wp %>% terra::project(paste0(proj_authority,":",epsg_pcs), # Try another projection!
                                                         method = "bilinear", 
                                                         gdal = FALSE,
                                                         res = 100)
terra::writeRaster(popvrt_malawi__wp_p,
                   "population_in/WORLDPOP/outputmaps/100m/wp_malawi100m_pcs.tif",
                   filetype = "GTiff", overwrite = TRUE)

popmwi_gcs_wp <- popvrt_malawi_wp %>%
  terra::global(., 'sum', na.rm=TRUE) %>%
  pull(sum)
popmwi_wm_wp <- popvrt_malawi__wp_p %>%
  terra::global(., 'sum', na.rm=TRUE) %>%
  pull(sum)
popmwi_gcs_wp/popmwi_wm_wp #See if new projection make these two values more even

### Copy 2 MoFuSS ----
copy2mofussfiles_wp <- c("population_in/WORLDPOP/outputmaps/100m/wp_malawi100m_gcs.tif",
                         "population_in/WORLDPOP/outputmaps/100m/wp_malawi100m_pcs.tif")
for (f in copy2mofussfiles_wp) {
  file.copy(from=f, 
            to=paste0(demanddir,"/demand_in/"),  
            overwrite = TRUE, recursive = TRUE, copy.mode = TRUE)
}

## Honduras ----
tic("Honduras WorldPop")
HondurasWorldPop100moriginal <- list.files(path = "population_in/WORLDPOP/Americas/",
                                         pattern = "hnd_ppp_2020_UNadj_constrained.*\\.tif$", full.names = TRUE)
HondurasWorldPop100moriginalVRT <- vrt(HondurasWorldPop100moriginal, "WP_VRT/HondurasWorldPop.vrt", overwrite=TRUE)

HondurasWorldPop <- rast(HondurasWorldPop100moriginalVRT)
res(HondurasWorldPop)
res(HondurasWorldPop) <- 0.00083333333 #100m approx
HondurasWorldPop <- terra::resample(HondurasWorldPop100moriginalVRT, HondurasWorldPop, "sum")
terra::writeRaster(HondurasWorldPop,
                   "population_in/WORLDPOP/outputmaps/100m/wp_honduras100m_gcs.tif",
                   filetype = "GTiff", overwrite = TRUE)
toc()
popvrt_honduras_wp <- rast("population_in/WORLDPOP/outputmaps/100m/wp_honduras100m_gcs.tif")
popvrt_honduras__wp_p <- popvrt_honduras_wp %>% terra::project(paste0(proj_authority,":",epsg_pcs), # Try another projection!
                                                           method = "bilinear", 
                                                           gdal = FALSE,
                                                           res = 100)
terra::writeRaster(popvrt_honduras__wp_p,
                   "population_in/WORLDPOP/outputmaps/100m/wp_honduras100m_pcs.tif",
                   filetype = "GTiff", overwrite = TRUE)

pophnd_gcs_wp <- popvrt_honduras_wp %>%
  terra::global(., 'sum', na.rm=TRUE) %>%
  pull(sum)
pophnd_wm_wp <- popvrt_honduras__wp_p %>%
  terra::global(., 'sum', na.rm=TRUE) %>%
  pull(sum)
pophnd_gcs_wp/pophnd_wm_wp # See if new projection make these two values more even

### Copy 2 MoFuSS ----
copy2mofussfiles_wp <- c("population_in/WORLDPOP/outputmaps/100m/wp_honduras100m_gcs.tif",
                         "population_in/WORLDPOP/outputmaps/100m/wp_honduras100m_pcs.tif")
for (f in copy2mofussfiles_wp) {
  file.copy(from=f, 
            to=paste0(demanddir,"/demand_in/"),  
            overwrite = TRUE, recursive = TRUE, copy.mode = TRUE)
}

## Papua New Guinea ----
tic("PNG WorldPop")
PNGWorldPop100moriginal <- list.files(path = "population_in/WORLDPOP/Oceania/",
                                        pattern = "png_ppp_2020_UNadj_constrained.*\\.tif$", full.names = TRUE)
PNGWorldPop100moriginalVRT <- vrt(PNGWorldPop100moriginal, "WP_VRT/PNGWorldPop.vrt", overwrite=TRUE)

PNGWorldPop <- rast(PNGWorldPop100moriginalVRT)
res(PNGWorldPop)
res(PNGWorldPop) <- 0.00083333333 #100m approx
PNGWorldPop <- terra::resample(PNGWorldPop100moriginalVRT, PNGWorldPop, "sum")
terra::writeRaster(PNGWorldPop,
                   "population_in/WORLDPOP/outputmaps/100m/wp_png100m_gcs.tif",
                   filetype = "GTiff", overwrite = TRUE)
toc()
popvrt_png_wp <- rast("population_in/WORLDPOP/outputmaps/100m/wp_png100m_gcs.tif")
popvrt_png__wp_p <- popvrt_png_wp %>% terra::project(paste0(proj_authority,":",epsg_pcs), # Try another projection!
                                                         method = "bilinear", 
                                                         gdal = FALSE,
                                                         res = 100)
terra::writeRaster(popvrt_png__wp_p,
                   "population_in/WORLDPOP/outputmaps/100m/wp_png100m_pcs.tif",
                   filetype = "GTiff", overwrite = TRUE)

poppng_gcs_wp <- popvrt_png_wp %>%
  terra::global(., 'sum', na.rm=TRUE) %>%
  pull(sum)
poppng_wm_wp <- popvrt_png__wp_p %>%
  terra::global(., 'sum', na.rm=TRUE) %>%
  pull(sum)
poppng_gcs_wp/poppng_wm_wp #See if new projection make these two values more even

# Adjust projected population to match gcs - eventually country census data
popvrt_png__wp_p_adj <- popvrt_png__wp_p*(poppng_gcs_wp/poppng_wm_wp)
poppng_wm_wp_adj <- popvrt_png__wp_p_adj %>%
  terra::global(., 'sum', na.rm=TRUE) %>%
  pull(sum)
terra::writeRaster(popvrt_png__wp_p_adj,
                   "population_in/WORLDPOP/outputmaps/100m/wp_png100m_pcs.tif",
                   filetype = "GTiff", overwrite = TRUE)

### Copy 2 MoFuSS ----
copy2mofussfiles_wp <- c("population_in/WORLDPOP/outputmaps/100m/wp_png100m_gcs.tif",
                         "population_in/WORLDPOP/outputmaps/100m/wp_png100m_pcs.tif")
for (f in copy2mofussfiles_wp) {
  file.copy(from=f, 
            to=paste0(demanddir,"/demand_in/"),  
            overwrite = TRUE, recursive = TRUE, copy.mode = TRUE)
}

## Zambia ----
tic("Zambia WorldPop")
ZambiaWorldPop100moriginal <- list.files(path = "population_in/WORLDPOP/Africa/",
                                      pattern = "zmb_ppp_2020_UNadj_constrained.*\\.tif$", full.names = TRUE)
ZambiaWorldPop100moriginalVRT <- vrt(ZambiaWorldPop100moriginal, "WP_VRT/ZambiaWorldPop.vrt", overwrite=TRUE)

ZambiaWorldPop <- rast(ZambiaWorldPop100moriginalVRT)
res(ZambiaWorldPop)
res(ZambiaWorldPop) <- 0.00083333333 #100m approx
ZambiaWorldPop <- terra::resample(ZambiaWorldPop100moriginalVRT, ZambiaWorldPop, "sum")
terra::writeRaster(ZambiaWorldPop,
                   "population_in/WORLDPOP/outputmaps/100m/wp_zambia100m_gcs.tif",
                   filetype = "GTiff", overwrite = TRUE)
toc()
popvrt_zambia_wp <- rast("population_in/WORLDPOP/outputmaps/100m/wp_zambia100m_gcs.tif")
popvrt_zambia__wp_p <- popvrt_zambia_wp %>% terra::project(paste0(proj_authority,":",epsg_pcs), # Try another projection!
                                                     method = "bilinear", 
                                                     gdal = FALSE,
                                                     res = 100)
terra::writeRaster(popvrt_zambia__wp_p,
                   "population_in/WORLDPOP/outputmaps/100m/wp_zambia100m_pcs.tif",
                   filetype = "GTiff", overwrite = TRUE)

popzambia_gcs_wp <- popvrt_zambia_wp %>%
  terra::global(., 'sum', na.rm=TRUE) %>%
  pull(sum)
popzambia_wm_wp <- popvrt_zambia__wp_p %>%
  terra::global(., 'sum', na.rm=TRUE) %>%
  pull(sum)
popzambia_gcs_wp/popzambia_wm_wp #See if new projection make these two values more even

# Adjust projected population to match gcs - eventually country census data
popvrt_zambia__wp_p_adj <- popvrt_zambia__wp_p*(popzambia_gcs_wp/popzambia_wm_wp)
popzambia_wm_wp_adj <- popvrt_zambia__wp_p_adj %>%
  terra::global(., 'sum', na.rm=TRUE) %>%
  pull(sum)
terra::writeRaster(popvrt_zambia__wp_p_adj,
                   "population_in/WORLDPOP/outputmaps/100m/wp_zambia100m_pcs.tif",
                   filetype = "GTiff", overwrite = TRUE)

### Copy 2 MoFuSS ----
copy2mofussfiles_wp <- c("population_in/WORLDPOP/outputmaps/100m/wp_zambia100m_gcs.tif",
                         "population_in/WORLDPOP/outputmaps/100m/wp_zambia100m_pcs.tif")
for (f in copy2mofussfiles_wp) {
  file.copy(from=f, 
            to=paste0(demanddir,"/demand_in/"),  
            overwrite = TRUE, recursive = TRUE, copy.mode = TRUE)
}


## Madagascar ----
tic("Madagascar WorldPop")
MadagascarWorldPop100moriginal <- list.files(path = "population_in/WORLDPOP/Africa/",
                                         pattern = "mdg_ppp_2020_UNadj_constrained.*\\.tif$", full.names = TRUE)
MadagascarWorldPop100moriginalVRT <- vrt(MadagascarWorldPop100moriginal, "WP_VRT/MadagascarWorldPop.vrt", overwrite=TRUE)

MadagascarWorldPop <- rast(MadagascarWorldPop100moriginalVRT)
res(MadagascarWorldPop)
res(MadagascarWorldPop) <- 0.00083333333 #100m approx
MadagascarWorldPop <- terra::resample(MadagascarWorldPop100moriginalVRT, MadagascarWorldPop, "sum")
terra::writeRaster(MadagascarWorldPop,
                   "population_in/WORLDPOP/outputmaps/100m/wp_madagascar100m_gcs.tif",
                   filetype = "GTiff", overwrite = TRUE)
toc()
popvrt_madagascar_wp <- rast("population_in/WORLDPOP/outputmaps/100m/wp_madagascar100m_gcs.tif")
popvrt_madagascar__wp_p <- popvrt_madagascar_wp %>% terra::project(paste0(proj_authority,":",epsg_pcs), # Try another projection!
                                                           method = "bilinear", 
                                                           gdal = FALSE,
                                                           res = 100)
terra::writeRaster(popvrt_madagascar__wp_p,
                   "population_in/WORLDPOP/outputmaps/100m/wp_madagascar100m_pcs.tif",
                   filetype = "GTiff", overwrite = TRUE)

popmadagascar_gcs_wp <- popvrt_madagascar_wp %>%
  terra::global(., 'sum', na.rm=TRUE) %>%
  pull(sum)
popmadagascar_wm_wp <- popvrt_madagascar__wp_p %>%
  terra::global(., 'sum', na.rm=TRUE) %>%
  pull(sum)
popmadagascar_gcs_wp/popmadagascar_wm_wp #See if new projection make these two values more even

# Adjust projected population to match gcs - eventually country census data
popvrt_madagascar__wp_p_adj <- popvrt_madagascar__wp_p*(popmadagascar_gcs_wp/popmadagascar_wm_wp)
popmadagascar_wm_wp_adj <- popvrt_madagascar__wp_p_adj %>%
  terra::global(., 'sum', na.rm=TRUE) %>%
  pull(sum)
terra::writeRaster(popvrt_madagascar__wp_p_adj,
                   "population_in/WORLDPOP/outputmaps/100m/wp_madagascar100m_pcs.tif",
                   filetype = "GTiff", overwrite = TRUE)

### Copy 2 MoFuSS ----
copy2mofussfiles_wp <- c("population_in/WORLDPOP/outputmaps/100m/wp_madagascar100m_gcs.tif",
                         "population_in/WORLDPOP/outputmaps/100m/wp_madagascar100m_pcs.tif")
for (f in copy2mofussfiles_wp) {
  file.copy(from=f, 
            to=paste0(demanddir,"/demand_in/"),  
            overwrite = TRUE, recursive = TRUE, copy.mode = TRUE)
}

## Mozambique ----
tic("Mozambique WorldPop")
MozambiqueWorldPop100moriginal <- list.files(path = "population_in/WORLDPOP/Africa/",
                                             pattern = "moz_ppp_2020_UNadj_constrained.*\\.tif$", full.names = TRUE)
MozambiqueWorldPop100moriginalVRT <- vrt(MozambiqueWorldPop100moriginal, "WP_VRT/MozambiqueWorldPop.vrt", overwrite=TRUE)

MozambiqueWorldPop <- rast(MozambiqueWorldPop100moriginalVRT)
res(MozambiqueWorldPop)
res(MozambiqueWorldPop) <- 0.00083333333 #100m approx
MozambiqueWorldPop <- terra::resample(MozambiqueWorldPop100moriginalVRT, MozambiqueWorldPop, "sum")
terra::writeRaster(MozambiqueWorldPop,
                   "population_in/WORLDPOP/outputmaps/100m/wp_mozambique100m_gcs.tif",
                   filetype = "GTiff", overwrite = TRUE)
toc()
popvrt_mozambique_wp <- rast("population_in/WORLDPOP/outputmaps/100m/wp_mozambique100m_gcs.tif")
popvrt_mozambique__wp_p <- popvrt_mozambique_wp %>% terra::project(paste0(proj_authority,":",epsg_pcs), # Try another projection!
                                                                   method = "bilinear", 
                                                                   gdal = FALSE,
                                                                   res = 100)
terra::writeRaster(popvrt_mozambique__wp_p,
                   "population_in/WORLDPOP/outputmaps/100m/wp_mozambique100m_pcs.tif",
                   filetype = "GTiff", overwrite = TRUE)

popmozambique_gcs_wp <- popvrt_mozambique_wp %>%
  terra::global(., 'sum', na.rm=TRUE) %>%
  pull(sum)
popmozambique_wm_wp <- popvrt_mozambique__wp_p %>%
  terra::global(., 'sum', na.rm=TRUE) %>%
  pull(sum)
popmozambique_gcs_wp/popmozambique_wm_wp #See if new projection make these two values more even

# Adjust projected population to match gcs - eventually country census data
popvrt_mozambique__wp_p_adj <- popvrt_mozambique__wp_p*(popmozambique_gcs_wp/popmozambique_wm_wp)
popmozambique_wm_wp_adj <- popvrt_mozambique__wp_p_adj %>%
  terra::global(., 'sum', na.rm=TRUE) %>%
  pull(sum)
terra::writeRaster(popvrt_mozambique__wp_p_adj,
                   "population_in/WORLDPOP/outputmaps/100m/wp_mozambique100m_pcs.tif",
                   filetype = "GTiff", overwrite = TRUE)

### Copy 2 MoFuSS ----
copy2mofussfiles_wp <- c("population_in/WORLDPOP/outputmaps/100m/wp_mozambique100m_gcs.tif",
                         "population_in/WORLDPOP/outputmaps/100m/wp_mozambique100m_pcs.tif")
for (f in copy2mofussfiles_wp) {
  file.copy(from=f, 
            to=paste0(demanddir,"/demand_in/"),  
            overwrite = TRUE, recursive = TRUE, copy.mode = TRUE)
}

## Uganda ----
tic("Uganda WorldPop")
UgandaWorldPop100moriginal <- list.files(path = "population_in/WORLDPOP/Africa/",
                                             pattern = "uga_ppp_2020_UNadj_constrained.*\\.tif$", full.names = TRUE)
UgandaWorldPop100moriginalVRT <- vrt(UgandaWorldPop100moriginal, "WP_VRT/UgandaWorldPop.vrt", overwrite=TRUE)

UgandaWorldPop <- rast(UgandaWorldPop100moriginalVRT)
res(UgandaWorldPop)
res(UgandaWorldPop) <- 0.00083333333 #100m approx
UgandaWorldPop <- terra::resample(UgandaWorldPop100moriginalVRT, UgandaWorldPop, "sum")
terra::writeRaster(UgandaWorldPop,
                   "population_in/WORLDPOP/outputmaps/100m/wp_uganda100m_gcs.tif",
                   filetype = "GTiff", overwrite = TRUE)
toc()
popvrt_uganda_wp <- rast("population_in/WORLDPOP/outputmaps/100m/wp_uganda100m_gcs.tif")
popvrt_uganda__wp_p <- popvrt_uganda_wp %>% terra::project(paste0(proj_authority,":",epsg_pcs), # Try another projection!
                                                                   method = "bilinear", 
                                                                   gdal = FALSE,
                                                                   res = 100)
terra::writeRaster(popvrt_uganda__wp_p,
                   "population_in/WORLDPOP/outputmaps/100m/wp_uganda100m_pcs.tif",
                   filetype = "GTiff", overwrite = TRUE)

popuganda_gcs_wp <- popvrt_uganda_wp %>%
  terra::global(., 'sum', na.rm=TRUE) %>%
  pull(sum)
popuganda_wm_wp <- popvrt_uganda__wp_p %>%
  terra::global(., 'sum', na.rm=TRUE) %>%
  pull(sum)
popuganda_gcs_wp/popuganda_wm_wp #See if new projection make these two values more even

# Adjust projected population to match gcs - eventually country census data
popvrt_uganda__wp_p_adj <- popvrt_uganda__wp_p*(popuganda_gcs_wp/popuganda_wm_wp)
popuganda_wm_wp_adj <- popvrt_uganda__wp_p_adj %>%
  terra::global(., 'sum', na.rm=TRUE) %>%
  pull(sum)
terra::writeRaster(popvrt_uganda__wp_p_adj,
                   "population_in/WORLDPOP/outputmaps/100m/wp_uganda100m_pcs.tif",
                   filetype = "GTiff", overwrite = TRUE)

### Copy 2 MoFuSS ----
copy2mofussfiles_wp <- c("population_in/WORLDPOP/outputmaps/100m/wp_uganda100m_gcs.tif",
                         "population_in/WORLDPOP/outputmaps/100m/wp_uganda100m_pcs.tif")
for (f in copy2mofussfiles_wp) {
  file.copy(from=f, 
            to=paste0(demanddir,"/demand_in/"),  
            overwrite = TRUE, recursive = TRUE, copy.mode = TRUE)
}

## Rwanda ----
tic("Rwanda WorldPop")
RwandaWorldPop100moriginal <- list.files(path = "population_in/WORLDPOP/Africa/",
                                         pattern = "rwa_ppp_2020_UNadj_constrained.*\\.tif$", full.names = TRUE)
RwandaWorldPop100moriginalVRT <- vrt(RwandaWorldPop100moriginal, "WP_VRT/RwandaWorldPop.vrt", overwrite=TRUE)

RwandaWorldPop <- rast(RwandaWorldPop100moriginalVRT)
res(RwandaWorldPop)
res(RwandaWorldPop) <- 0.00083333333 #100m approx
RwandaWorldPop <- terra::resample(RwandaWorldPop100moriginalVRT, RwandaWorldPop, "sum")
terra::writeRaster(RwandaWorldPop,
                   "population_in/WORLDPOP/outputmaps/100m/wp_rwanda100m_gcs.tif",
                   filetype = "GTiff", overwrite = TRUE)
toc()
popvrt_rwanda_wp <- rast("population_in/WORLDPOP/outputmaps/100m/wp_rwanda100m_gcs.tif")
popvrt_rwanda__wp_p <- popvrt_rwanda_wp %>% terra::project(paste0(proj_authority,":",epsg_pcs), # Try another projection!
                                                           method = "bilinear", 
                                                           gdal = FALSE,
                                                           res = 100)
terra::writeRaster(popvrt_rwanda__wp_p,
                   "population_in/WORLDPOP/outputmaps/100m/wp_rwanda100m_pcs.tif",
                   filetype = "GTiff", overwrite = TRUE)

poprwanda_gcs_wp <- popvrt_rwanda_wp %>%
  terra::global(., 'sum', na.rm=TRUE) %>%
  pull(sum)
poprwanda_wm_wp <- popvrt_rwanda__wp_p %>%
  terra::global(., 'sum', na.rm=TRUE) %>%
  pull(sum)
poprwanda_gcs_wp/poprwanda_wm_wp #See if new projection make these two values more even

# Adjust projected population to match gcs - eventually country census data
popvrt_rwanda__wp_p_adj <- popvrt_rwanda__wp_p*(poprwanda_gcs_wp/poprwanda_wm_wp)
poprwanda_wm_wp_adj <- popvrt_rwanda__wp_p_adj %>%
  terra::global(., 'sum', na.rm=TRUE) %>%
  pull(sum)
terra::writeRaster(popvrt_rwanda__wp_p_adj,
                   "population_in/WORLDPOP/outputmaps/100m/wp_rwanda100m_pcs.tif",
                   filetype = "GTiff", overwrite = TRUE)

### Copy 2 MoFuSS ----
copy2mofussfiles_wp <- c("population_in/WORLDPOP/outputmaps/100m/wp_rwanda100m_gcs.tif",
                         "population_in/WORLDPOP/outputmaps/100m/wp_rwanda100m_pcs.tif")
for (f in copy2mofussfiles_wp) {
  file.copy(from=f, 
            to=paste0(demanddir,"/demand_in/"),  
            overwrite = TRUE, recursive = TRUE, copy.mode = TRUE)
}

## DRC ----
tic("DRC WorldPop")
DRCWorldPop100moriginal <- list.files(path = "population_in/WORLDPOP/Africa/",
                                         pattern = "cod_ppp_2020_UNadj_constrained.*\\.tif$", full.names = TRUE)
DRCWorldPop100moriginalVRT <- vrt(DRCWorldPop100moriginal, "WP_VRT/DRCWorldPop.vrt", overwrite=TRUE)

DRCWorldPop <- terra::rast(DRCWorldPop100moriginalVRT)
res(DRCWorldPop)
res(DRCWorldPop) <- 0.00083333333 #100m approx
DRCWorldPop <- terra::resample(DRCWorldPop100moriginalVRT, DRCWorldPop, "sum")
terra::writeRaster(DRCWorldPop,
                   "population_in/WORLDPOP/outputmaps/100m/wp_drc100m_gcs.tif",
                   filetype = "GTiff", overwrite = TRUE)
toc()
popvrt_drc_wp <- rast("population_in/WORLDPOP/outputmaps/100m/wp_drc100m_gcs.tif")
popvrt_drc__wp_p <- popvrt_drc_wp %>% terra::project(paste0(proj_authority,":",epsg_pcs), # Try another projection!
                                                           method = "bilinear", 
                                                           gdal = FALSE,
                                                           res = 100)
terra::writeRaster(popvrt_drc__wp_p,
                   "population_in/WORLDPOP/outputmaps/100m/wp_drc100m_pcs.tif",
                   filetype = "GTiff", overwrite = TRUE)

popdrc_gcs_wp <- popvrt_drc_wp %>%
  terra::global(., 'sum', na.rm=TRUE) %>%
  pull(sum)
popdrc_wm_wp <- popvrt_drc__wp_p %>%
  terra::global(., 'sum', na.rm=TRUE) %>%
  pull(sum)
popdrc_gcs_wp/popdrc_wm_wp #See if new projection make these two values more even

# Adjust projected population to match gcs - eventually country census data
popvrt_drc__wp_p_adj <- popvrt_drc__wp_p*(popdrc_gcs_wp/popdrc_wm_wp)
popdrc_wm_wp_adj <- popvrt_drc__wp_p_adj %>%
  terra::global(., 'sum', na.rm=TRUE) %>%
  pull(sum)
terra::writeRaster(popvrt_drc__wp_p_adj,
                   "population_in/WORLDPOP/outputmaps/100m/wp_drc100m_pcs.tif",
                   filetype = "GTiff", overwrite = TRUE)

### Copy 2 MoFuSS ----
copy2mofussfiles_wp <- c("population_in/WORLDPOP/outputmaps/100m/wp_drc100m_gcs.tif",
                         "population_in/WORLDPOP/outputmaps/100m/wp_drc100m_pcs.tif")
for (f in copy2mofussfiles_wp) {
  file.copy(from=f, 
            to=paste0(demanddir,"/demand_in/"),  
            overwrite = TRUE, recursive = TRUE, copy.mode = TRUE)
}

## El Salvador ----
tic("El Salvador WorldPop")
SLVWorldPop100moriginal <- list.files(path = "population_in/WORLDPOP/Americas/",
                                      pattern = "slv_ppp_2020_UNadj_constrained.*\\.tif$", full.names = TRUE)
SLVWorldPop100moriginalVRT <- vrt(SLVWorldPop100moriginal, "WP_VRT/SLVWorldPop.vrt", overwrite=TRUE)

SLVWorldPop <- terra::rast(SLVWorldPop100moriginalVRT)
res(SLVWorldPop)
res(SLVWorldPop) <- 0.00083333333 #100m approx
SLVWorldPop <- terra::resample(SLVWorldPop100moriginalVRT, SLVWorldPop, "sum")
terra::writeRaster(SLVWorldPop,
                   "population_in/WORLDPOP/outputmaps/100m/wp_slv100m_gcs.tif",
                   filetype = "GTiff", overwrite = TRUE)
toc()
popvrt_slv_wp <- rast("population_in/WORLDPOP/outputmaps/100m/wp_slv100m_gcs.tif")
popvrt_slv__wp_p <- popvrt_slv_wp %>% terra::project(paste0(proj_authority,":",epsg_pcs), # Try another projection!
                                                     method = "bilinear", 
                                                     gdal = FALSE,
                                                     res = 100)
terra::writeRaster(popvrt_slv__wp_p,
                   "population_in/WORLDPOP/outputmaps/100m/wp_slv100m_pcs.tif",
                   filetype = "GTiff", overwrite = TRUE)

popslv_gcs_wp <- popvrt_slv_wp %>%
  terra::global(., 'sum', na.rm=TRUE) %>%
  pull(sum)
popslv_wm_wp <- popvrt_slv__wp_p %>%
  terra::global(., 'sum', na.rm=TRUE) %>%
  pull(sum)
popslv_gcs_wp/popslv_wm_wp #See if new projection make these two values more even

# Adjust projected population to match gcs - eventually country census data
popvrt_slv__wp_p_adj <- popvrt_slv__wp_p*(popslv_gcs_wp/popslv_wm_wp)
popslv_wm_wp_adj <- popvrt_slv__wp_p_adj %>%
  terra::global(., 'sum', na.rm=TRUE) %>%
  pull(sum)
terra::writeRaster(popvrt_slv__wp_p_adj,
                   "population_in/WORLDPOP/outputmaps/100m/wp_slv100m_pcs.tif",
                   filetype = "GTiff", overwrite = TRUE)

### Copy 2 MoFuSS ----
copy2mofussfiles_wp <- c("population_in/WORLDPOP/outputmaps/100m/wp_slv100m_gcs.tif",
                         "population_in/WORLDPOP/outputmaps/100m/wp_slv100m_pcs.tif")
for (f in copy2mofussfiles_wp) {
  file.copy(from=f, 
            to=paste0(demanddir,"/demand_in/"),  
            overwrite = TRUE, recursive = TRUE, copy.mode = TRUE)
}

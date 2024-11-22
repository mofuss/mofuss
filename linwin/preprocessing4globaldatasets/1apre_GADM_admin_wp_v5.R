# MoFuSS
# Version 3
# Date: Mar 2024
# This script load the GADM datasets for levels 0, 1 and 2 and selects those countries
# with complete info in both the WHO and HRSL population maps. For Nepal use 3

# 2dolist

# Internal parameters
run_ms = "Yes" # Run ms_simplfy?
newadminlevel = 3 # Use 3, 4, or 5 depending on the desired admin level.
# Any different value will bypass this and keep the original adm0, adm1 and adm2.

# Load packages ----
library(sf)
#library(tictoc)
#library(mapview)
library(tidyverse)
library(readxl)
library(hacksaw)
library(rmapshaper)
library(svDialogs)

setwd(countrydir)
getwd()
country_name

# Read parameters table ----
country_parameters <- read_excel(paste0("LULCC/DownloadedDatasets/SourceData",country_name,"/",parameters_file))
print(tibble::as_tibble(country_parameters), n=100)

country_parameters %>%
  dplyr::filter(Var == "epsg_gcs") %>%
  pull(ParCHR) %>%
  as.integer(.) -> epsg_gcs

country_parameters %>%
  dplyr::filter(Var == "epsg_pcs") %>%
  pull(ParCHR) %>%
  as.integer(.) -> epsg_pcs

country_parameters %>%
  dplyr::filter(Var == "proj_authority") %>%
  pull(ParCHR) -> proj_authority

if (exists("demanddir") == FALSE) {
  choose_directory1 = function(caption = "Choose the directory where demand_in files are") {
    if(.Platform$OS.type == "unix")  {
      setwd(tk_choose.dir("/home/mofuss/Documents", caption = caption))
    } else {
      setwd(choose.dir("/home/mofuss/Documents", caption = caption)) # Elegir bien esta carpeta de inicio
    }
  }
  choose_directory1()
  demanddir <- getwd()
}

if (exists("admindir") == FALSE) {
  choose_directory1 = function(caption = "Choose the directory where admin_regions files are") {
    if(.Platform$OS.type == "unix")  {
      setwd(tk_choose.dir("/home/mofuss/Documents", caption = caption))
    } else {
      setwd(choose.dir("/home/mofuss/Documents", caption = caption)) # Elegir bien esta carpeta de inicio
    }
  }
  choose_directory1()
  admindir <- getwd()
}

setwd(admindir)

unlink("regions_adm0/", recursive=TRUE)
unlink("regions_adm1/", recursive=TRUE)
unlink("regions_adm2/", recursive=TRUE)
unlink("regions_adm0_p/", recursive=TRUE)
unlink("regions_adm1_p/", recursive=TRUE)
unlink("regions_adm2_P/", recursive=TRUE)
unlink("InVector/", recursive=TRUE)

if (!dir.exists("regions_adm0")) {dir.create("regions_adm0")}
if (!dir.exists("regions_adm1")) {dir.create("regions_adm1")} 
if (!dir.exists("regions_adm2")) {dir.create("regions_adm2")} 
if (!dir.exists("regions_adm0_p")) {dir.create("regions_adm0_p")}
if (!dir.exists("regions_adm1_p")) {dir.create("regions_adm1_p")} 
if (!dir.exists("regions_adm2_p")) {dir.create("regions_adm2_p")} 
if (!dir.exists("InVector")) {dir.create("InVector")} 


# Read global GADM for admin 0, 1 and 2 and resolve the 9 disputed territories for the three cases
recodedisputed <- function(adm_lyr){
  adm_lyr_recoded <- adm_lyr %>% 
    mutate(GID_0 = recode(GID_0, 
                          "Z01" = "IND",  # Jammu and Kashmir, Indian admin_regionsistered Kashmir
                          "Z02" = "CHN",  # Shaksgam Valley, The Trans-Karakoram Tract https://en.wikipedia.org/wiki/Trans-Karakoram_Tract
                          "Z03" = "CHN",  # Aksai Chin https://en.wikipedia.org/wiki/Aksai_Chin
                          "Z04" = "IND",  # Kaurik https://en.wikipedia.org/wiki/Kaurik
                          "Z05" = "IND",  # Lapthal https://en.wikipedia.org/wiki/Lapthal
                          "Z06" = "PAK",  # Azad Kashmir, Pakistan admin_regionsistered Kashmir
                          "Z07" = "IND",  # Arunachal Pradesh
                          "Z08" = "CHN",  # Back to China after GADM layer. Pa-li-chia-ssu https://geographic.org/geographic_names/name.php?uni=-2924886&fid=2690&c=india
                          "Z09" = "IND")) #, # Sang, looks like India admin_regions, in Himachal Pradesh. 
           # NAME_0 = recode(NAME_0,
           #                 "Jammu and Kashmir" = "India",
           #                 "Shaksgam Valley" = "China",
           #                 "Aksai Chin" = "China",
           #                 "Kaurik" = "India",
           #                 "Lapthal" = "India",
           #                 "Azad Kashmir" = "Pakistan",
           #                 "Arunachal Pradesh" = "India",
           #                 "Pa-li-chia-ssu" = "India",
           #                 "Sang" = "India"))
  return(adm_lyr_recoded)
}

gadm_adm0_sel <- st_read("gadm_410-levels.gpkg", layer = "ADM_0") %>%
  dplyr::rename(NAME_0 = "COUNTRY") %>%
  dplyr::select(GID_0,NAME_0) %>%
  recodedisputed()
head(gadm_adm0_sel)

gadm_adm0_sel_db <- gadm_adm0_sel %>% st_drop_geometry()
gadm_adm0_sel_db

gadm_adm1_sel <- st_read("gadm_410-levels.gpkg", layer = "ADM_1") %>%
  dplyr::rename(NAME_0 = "COUNTRY") %>%
  dplyr::select(GID_0,NAME_0,GID_1,NAME_1) %>%
  recodedisputed()
gadm_adm1_sel$NAME_1 <- gsub("[,/.()+]", "", gadm_adm1_sel$NAME_1)
head(gadm_adm1_sel)

gadm_adm1_sel_db <- gadm_adm1_sel %>% st_drop_geometry()
gadm_adm1_sel_db

gadm_adm2_sel <- st_read("gadm_410-levels.gpkg", layer = "ADM_2") %>%
  dplyr::rename(NAME_0 = "COUNTRY") %>%
  dplyr::select(GID_0,NAME_0,GID_1,NAME_1,GID_2,NAME_2) %>%
  recodedisputed()
gadm_adm2_sel$NAME_1 <- gsub("[,/.()+]", "", gadm_adm2_sel$NAME_1)
gadm_adm2_sel$NAME_2 <- gsub("[,/.()+]", "", gadm_adm2_sel$NAME_2)
head(gadm_adm2_sel)

gadm_adm2_sel_db <- gadm_adm2_sel %>% st_drop_geometry()
gadm_adm2_sel_db

unique(gadm_adm2_sel$GID_0)

# If higher admin levels are available (3 or 4) for country based analysis at 100m in principle,
# then this can be used to replace "gadm_adm2_sel" with "gadm_adm3_sel" or "gadm_adm4_sel" depending on each case.
if (newadminlevel == 3){
  
  sf::sf_use_s2(FALSE)
  
  gadm_adm3_sel <- st_read("gadm_410-levels.gpkg", layer = "ADM_3") %>%
    dplyr::rename(NAME_0 = "COUNTRY") %>%
    dplyr::select(GID_0,NAME_0,GID_1,NAME_1,GID_2,NAME_2,GID_3,NAME_3) %>%
    recodedisputed()
  gadm_adm3_sel_Csub <- unique(gadm_adm3_sel$GID_0)
  
  gadm_adm2_sel_Ssub <- gadm_adm2_sel %>% 
    filter(!(GID_0 %in% gadm_adm3_sel_Csub))
  
  # Identify the columns to keep (example, adjust as needed)
  common_columns <- intersect(names(gadm_adm2_sel_Ssub), names(gadm_adm3_sel))
  unique_sf1_columns <- setdiff(names(gadm_adm2_sel_Ssub), common_columns)
  unique_sf2_columns <- setdiff(names(gadm_adm3_sel), common_columns)
  
  # Add missing columns to sf1
  for(col in unique_sf2_columns) {
    gadm_adm2_sel_Ssub[[col]] <- NA
  }
  
  # Add missing columns to sf2
  for(col in unique_sf1_columns) {
    gadm_adm3_sel[[col]] <- NA
  }
  
  # Ensure the order of columns matches
  gadm_adm2_sel_Ssub <- gadm_adm2_sel_Ssub[, c(common_columns, unique_sf1_columns, unique_sf2_columns)]
  gadm_adm3_sel <- gadm_adm3_sel[, c(common_columns, unique_sf2_columns, unique_sf1_columns)]
  
  # Combine the data
  gadm_adm3_sel_merged <- rbind(gadm_adm2_sel_Ssub, gadm_adm3_sel)
  
  gadm_adm2_sel <- gadm_adm3_sel_merged %>%
    mutate(GID_2 = if_else(!is.na(GID_3), GID_3, GID_2),
           NAME_2 = if_else(!is.na(NAME_3), NAME_3, NAME_2)) %>%
    dplyr::select(GID_0,NAME_0,GID_1,NAME_1,GID_2,NAME_2)
  
} else if (newadminlevel == 4){
  
  sf::sf_use_s2(FALSE)
  
  gadm_adm4_sel <- st_read("gadm_410-levels.gpkg", layer = "ADM_4") %>%
    dplyr::rename(NAME_0 = "COUNTRY") %>%
    dplyr::select(GID_0,NAME_0,GID_1,NAME_1,GID_2,NAME_2,GID_3,NAME_3,GID_4,NAME_4) %>%
    recodedisputed()
  gadm_adm4_sel_Csub <- unique(gadm_adm4_sel$GID_0)
  
  gadm_adm2_sel_Ssub <- gadm_adm2_sel %>% 
    filter(!(GID_0 %in% gadm_adm4_sel_Csub))
  
  # Identify the columns to keep (example, adjust as needed)
  common_columns <- intersect(names(gadm_adm2_sel_Ssub), names(gadm_adm4_sel))
  unique_sf1_columns <- setdiff(names(gadm_adm2_sel_Ssub), common_columns)
  unique_sf2_columns <- setdiff(names(gadm_adm4_sel), common_columns)
  
  # Add missing columns to sf1
  for(col in unique_sf2_columns) {
    gadm_adm2_sel_Ssub[[col]] <- NA
  }
  
  # Add missing columns to sf2
  for(col in unique_sf1_columns) {
    gadm_adm4_sel[[col]] <- NA
  }
  
  # Ensure the order of columns matches
  gadm_adm2_sel_Ssub <- gadm_adm2_sel_Ssub[, c(common_columns, unique_sf1_columns, unique_sf2_columns)]
  gadm_adm4_sel <- gadm_adm4_sel[, c(common_columns, unique_sf2_columns, unique_sf1_columns)]
  
  # Combine the data
  gadm_adm4_sel_merged <- rbind(gadm_adm2_sel_Ssub, gadm_adm4_sel)
  
  gadm_adm2_sel <- gadm_adm4_sel_merged %>%
    mutate(GID_2 = if_else(!is.na(GID_4), GID_4, GID_2),
           NAME_2 = if_else(!is.na(NAME_4), NAME_4, NAME_2)) %>%
    dplyr::select(GID_0,NAME_0,GID_1,NAME_1,GID_2,NAME_2)
  
} else if (newadminlevel == 5){ # OJO TEMRINAR NIVEL 5
  
  sf::sf_use_s2(FALSE)
  
  gadm_adm5_sel <- st_read("gadm_410-levels.gpkg", layer = "ADM_5") %>%
    dplyr::rename(NAME_0 = "COUNTRY") %>%
    dplyr::select(GID_0,NAME_0,GID_1,NAME_1,GID_2,NAME_2,GID_3,NAME_3,GID_4,NAME_4,GID_5,NAME_5) %>%
    recodedisputed()
  gadm_adm5_sel_Csub <- unique(gadm_adm5_sel$GID_0)
  
  gadm_adm2_sel_Ssub <- gadm_adm2_sel %>% 
    filter(!(GID_0 %in% gadm_adm5_sel_Csub))
  
  # Identify the columns to keep (example, adjust as needed)
  common_columns <- intersect(names(gadm_adm2_sel_Ssub), names(gadm_adm5_sel))
  unique_sf1_columns <- setdiff(names(gadm_adm2_sel_Ssub), common_columns)
  unique_sf2_columns <- setdiff(names(gadm_adm5_sel), common_columns)
  
  # Add missing columns to sf1
  for(col in unique_sf2_columns) {
    gadm_adm2_sel_Ssub[[col]] <- NA
  }
  
  # Add missing columns to sf2
  for(col in unique_sf1_columns) {
    gadm_adm5_sel[[col]] <- NA
  }
  
  # Ensure the order of columns matches
  gadm_adm2_sel_Ssub <- gadm_adm2_sel_Ssub[, c(common_columns, unique_sf1_columns, unique_sf2_columns)]
  gadm_adm5_sel <- gadm_adm5_sel[, c(common_columns, unique_sf2_columns, unique_sf1_columns)]
  
  # Combine the data
  gadm_adm5_sel_merged <- rbind(gadm_adm2_sel_Ssub, gadm_adm5_sel)
  
  gadm_adm2_sel <- gadm_adm5_sel_merged %>%
    mutate(GID_2 = if_else(!is.na(GID_5), GID_5, GID_2),
           NAME_2 = if_else(!is.na(NAME_5), NAME_5, NAME_2)) %>%
    dplyr::select(GID_0,NAME_0,GID_1,NAME_1,GID_2,NAME_2)
  
}

# Countries from GADM, before WHO or Facebook filter
gadm_adm0_sel_db <- gadm_adm0_sel %>% st_drop_geometry()
gadm_adm0_sel_db

# Read the WHO dataset in order to filter the GADM dataset by WHO availability (regions)
setwd(demanddir)
whodb <- read_excel("demand_in/A_LMIC_Estimates_2050_popmedian.xlsx")
head(whodb)
whodb_sel_u <- whodb %>% dplyr::select(iso3,country,region) %>% 
  unique()
whodb_sel_u
regions_u <- unique(whodb_sel_u$region)
regions_u
print(tibble::as_tibble(whodb_sel_u), n=100) ####

region1 <- whodb_sel_u %>% dplyr::filter(region %in% regions_u[1]) # %>%
# filter(iso3 != "IRN", # Iran missing from Facebook Population maps
#        iso3 != "AFG") # Afganistan missing from Facebook Population maps
region2 <- whodb_sel_u %>% dplyr::filter(region %in% regions_u[2])
regionNorAfr <- whodb_sel_u %>% dplyr::filter(region %in% regions_u[3]) %>%
filter(iso3 != "AZE", # Middle East or near...
       iso3 != "GEO", # Middle East or near...
       iso3 != "IRQ", # Middle East or near...
       iso3 != "JOR", # Middle East or near...
       iso3 != "SYR", # Middle East or near...
       iso3 != "TUR", # Middle East or near...
       iso3 != "YEM", # Middle East or near...
       iso3 != "ARM", # Middle East or near...
       iso3 != "SDN") # Sudan missing from Facebook Population maps
regionEastEuro <- whodb_sel_u %>% dplyr::filter(region %in% regions_u[3]) %>%
  filter(iso3 != "EGY", # North Africa...
    iso3 != "DZA", # North Africa...
    iso3 != "MAR", # North Africa...
    iso3 != "TUN", # North Africa...
    iso3 != "YEM", # Yemen rendered isolated from the rest but is not a country of interest, it can be added as a new region
    iso3 != "SDN") # Sudan moved to SSA by hand
regionSSA <- whodb_sel_u %>% dplyr::filter(region %in% regions_u[4]) %>% # Sudan added manually from NorthAfrica to SSA
  # filter(iso3 != "CPV", # Cabo Verde missing from Facebook Population maps
  #        iso3 != "SSD", # South Sudan missing from Facebook Population maps
  #        iso3 != "SOM", # Somalia missing from Facebook Population maps
  filter(iso3 != "LSO") # Lesotho doesn't have subnational admin and was buggy - fixed?

regionLATAM <- whodb_sel_u %>% dplyr::filter(region %in% regions_u[5]) %>%
# filter(iso3 != "GUY", # Guyana missing from Facebook Population maps
#        iso3 != "JAM", # Jamaica missing from Facebook Population mapss
filter(iso3 != "BLZ") # Belize doesn't have subnational admin and was buggy - fixed?
region6 <- whodb_sel_u %>% dplyr::filter(region %in% regions_u[6])  %>%
   filter(iso3 != "PRK")  # North Korea is isolated from region
  #        iso3 != "MMR", # Myanmar missing from Facebook Population maps
  #        iso3 != "AFG", # Afganistan missing from Facebook Population maps
  #        iso3 != "CHN") # China missing from Facebook Population maps
region7 <- whodb_sel_u %>% dplyr::filter(region %in% regions_u[7])

R1_adm0 <- gadm_adm0_sel %>% dplyr::filter(GID_0 %in% region1$iso3)
R2_adm0 <- gadm_adm0_sel %>% dplyr::filter(GID_0 %in% region2$iso3)
NorAfr_adm0 <- gadm_adm0_sel %>% dplyr::filter(GID_0 %in% regionNorAfr$iso3)
NorEastEuro_adm0 <- gadm_adm0_sel %>% dplyr::filter(GID_0 %in% regionEastEuro$iso3)
SSA_adm0 <- gadm_adm0_sel %>% dplyr::filter(GID_0 %in% regionSSA$iso3)
LATAM_adm0 <- gadm_adm0_sel %>% dplyr::filter(GID_0 %in% regionLATAM$iso3)
R6_adm0 <- gadm_adm0_sel %>% dplyr::filter(GID_0 %in% region6$iso3)
OCEANIA_adm0 <- gadm_adm0_sel %>% dplyr::filter(GID_0 %in% region7$iso3)

ASIA_adm0 <- rbind(R1_adm0,R6_adm0,NorEastEuro_adm0) %>% 
  ms_dissolve(field = "GID_0", copy_fields = c("NAME_0")) %>% 
  dplyr::select(GID_0, NAME_0)
ASIA_adm0_db <- ASIA_adm0 %>% st_drop_geometry()

setwd(admindir)

# Sub-Saharan Africa ----
subregionsSSA <- read_excel("subregionsSSA_v4.xlsx")
subregionsSSA
unique(subregionsSSA$Subregion)
sort(unique(subregionsSSA$NAME_0))
sort(unique(SSA_adm0$NAME_0))

SSA_adm0_subregions <- SSA_adm0 %>% 
  right_join(subregionsSSA, by = "GID_0") %>%
  dplyr::select (-NAME_0.y) %>% 
  dplyr::rename(NAME_0 = NAME_0.x) %>%
  filter(!is.na(NAME_0))
SSA_adm0_subregions
SSA_adm0_subregions %>% st_drop_geometry()

# Build regions
SSA_adm0_eastern <- SSA_adm0_subregions %>% dplyr::filter(Subregion %in% "Eastern Africa") %>% mutate(ID = 1:nrow(.))
SSA_adm0_eastern$mofuss_reg <- "SSA_adm0_eastern"
#SSA_adm0_southeastern <- SSA_adm0_subregions %>% dplyr::filter(Subregion %in% "Southeastern Africa") %>% mutate(ID = 1:nrow(.))
#SSA_adm0_southeastern$mofuss_reg <- "SSA_adm0_southeastern"
SSA_adm0_southern <- SSA_adm0_subregions %>% dplyr::filter(Subregion %in% "Southern Africa") %>% mutate(ID = 1:nrow(.))
SSA_adm0_southern$mofuss_reg <- "SSA_adm0_southern"
SSA_adm0_western <- SSA_adm0_subregions %>% dplyr::filter(Subregion %in% "Western Africa") %>% mutate(ID = 1:nrow(.))
SSA_adm0_western$mofuss_reg <- "SSA_adm0_western"
SSA_adm0_madagascar <- SSA_adm0_subregions %>% dplyr::filter(Subregion %in% "Madagascar") %>% mutate(ID = 1:nrow(.))
SSA_adm0_madagascar$mofuss_reg <- "SSA_adm0_madagascar"
SSA_adm0_stp <- SSA_adm0_subregions %>% dplyr::filter(Subregion %in% "São Tomé and Príncipe") %>% mutate(ID = 1:nrow(.))
SSA_adm0_stp$mofuss_reg <- "SSA_adm0_stp"
SSA_adm0_comoros <- SSA_adm0_subregions %>% dplyr::filter(Subregion %in% "Comoros") %>% mutate(ID = 1:nrow(.))
SSA_adm0_comoros$mofuss_reg <- "SSA_adm0_comoros"
SSA_adm0_mauritius <- SSA_adm0_subregions %>% dplyr::filter(Subregion %in% "Mauritius") %>% mutate(ID = 1:nrow(.))
SSA_adm0_mauritius$mofuss_reg <- "SSA_adm0_mauritius"
SSA_adm0_central <- SSA_adm0_subregions %>% dplyr::filter(Subregion %in% "Central Africa") %>% mutate(ID = 1:nrow(.))
SSA_adm0_central$mofuss_reg <- "SSA_adm0_central"
SSA_adm0_northcentral <- SSA_adm0_subregions %>% dplyr::filter(Subregion %in% "Northcentral Africa") %>% mutate(ID = 1:nrow(.))
SSA_adm0_northcentral$mofuss_reg <- "SSA_adm0_northcentral"
SSA_adm0_tanzania <- SSA_adm0_subregions %>% dplyr::filter(Subregion %in% "Tanzania") %>% mutate(ID = 1:nrow(.))
SSA_adm0_tanzania$mofuss_reg <- "SSA_adm0_tanzania"
SSA_adm0_kenya <- SSA_adm0_subregions %>% dplyr::filter(Subregion %in% "Kenya") %>% mutate(ID = 1:nrow(.))
SSA_adm0_kenya$mofuss_reg <- "SSA_adm0_kenya"
SSA_adm0_angola <- SSA_adm0_subregions %>% dplyr::filter(Subregion %in% "Angola") %>% mutate(ID = 1:nrow(.))
SSA_adm0_angola$mofuss_reg <- "SSA_adm0_angola"
SSA_adm0_uganda <- SSA_adm0_subregions %>% dplyr::filter(Subregion %in% "Uganda") %>% mutate(ID = 1:nrow(.))
SSA_adm0_uganda$mofuss_reg <- "SSA_adm0_uganda"
SSA_adm0_mali <- SSA_adm0_subregions %>% dplyr::filter(Subregion %in% "Mali") %>% mutate(ID = 1:nrow(.))
SSA_adm0_mali$mofuss_reg <- "SSA_adm0_mali"
SSA_adm0_chad <- SSA_adm0_subregions %>% dplyr::filter(Subregion %in% "Chad") %>% mutate(ID = 1:nrow(.))
SSA_adm0_chad$mofuss_reg <- "SSA_adm0_chad"
SSA_adm0_niger <- SSA_adm0_subregions %>% dplyr::filter(Subregion %in% "Niger") %>% mutate(ID = 1:nrow(.))
SSA_adm0_niger$mofuss_reg <- "SSA_adm0_niger"
SSA_adm0_caboverde <- SSA_adm0_subregions %>% dplyr::filter(Subregion %in% "Cabo Verde") %>% mutate(ID = 1:nrow(.))
SSA_adm0_caboverde$mofuss_reg <- "SSA_adm0_caboverde"
SSA_adm0_westcentral <- SSA_adm0_subregions %>% dplyr::filter(Subregion %in% "Westcentral Africa") %>% mutate(ID = 1:nrow(.))
SSA_adm0_westcentral$mofuss_reg <- "SSA_adm0_westcentral"
SSA_adm0_westsouthern <- SSA_adm0_subregions %>% dplyr::filter(Subregion %in% "Westsouthern Africa") %>% mutate(ID = 1:nrow(.))
SSA_adm0_westsouthern$mofuss_reg <- "SSA_adm0_westsouthern"
SSA_adm0_mauritania <- SSA_adm0_subregions %>% dplyr::filter(Subregion %in% "Mauritania") %>% mutate(ID = 1:nrow(.))
SSA_adm0_mauritania$mofuss_reg <- "SSA_adm0_mauritania"
SSA_adm0_benin <- SSA_adm0_subregions %>% dplyr::filter(Subregion %in% "Benin") %>% mutate(ID = 1:nrow(.))
SSA_adm0_benin$mofuss_reg <- "SSA_adm0_benin"
SSA_adm0_burkinafaso <- SSA_adm0_subregions %>% dplyr::filter(Subregion %in% "Burkina Faso") %>% mutate(ID = 1:nrow(.))
SSA_adm0_burkinafaso$mofuss_reg <- "SSA_adm0_burkinafaso"
SSA_adm0_cdivoire <- SSA_adm0_subregions %>% dplyr::filter(Subregion %in% "Côte d'Ivoire") %>% mutate(ID = 1:nrow(.))
SSA_adm0_cdivoire$mofuss_reg <- "SSA_adm0_cdivoire"
SSA_adm0_ghana <- SSA_adm0_subregions %>% dplyr::filter(Subregion %in% "Ghana") %>% mutate(ID = 1:nrow(.))
SSA_adm0_ghana$mofuss_reg <- "SSA_adm0_ghana"
SSA_adm0_senegambia <- SSA_adm0_subregions %>% dplyr::filter(Subregion %in% "Senegambia") %>% mutate(ID = 1:nrow(.))
SSA_adm0_senegambia$mofuss_reg <- "SSA_adm0_senegambia"
SSA_adm0_togo <- SSA_adm0_subregions %>% dplyr::filter(Subregion %in% "Togo") %>% mutate(ID = 1:nrow(.))
SSA_adm0_togo$mofuss_reg <- "SSA_adm0_togo"
# SSA_adm0_malawi <- SSA_adm0_subregions %>% dplyr::filter(Subregion %in% "Malawi") %>% mutate(ID = 1:nrow(.))
# SSA_adm0_malawi$mofuss_reg <- "SSA_adm0_malawi"
# SSA_adm0_mozambique <- SSA_adm0_subregions %>% dplyr::filter(Subregion %in% "Mozambique") %>% mutate(ID = 1:nrow(.))
# SSA_adm0_mozambique$mofuss_reg <- "SSA_adm0_mozambique"
SSA_adm0_malambique <- SSA_adm0_subregions %>% dplyr::filter(Subregion %in% "Malambique") %>% mutate(ID = 1:nrow(.))
SSA_adm0_malambique$mofuss_reg <- "SSA_adm0_malambique"
SSA_adm0_zambia <- SSA_adm0_subregions %>% dplyr::filter(Subregion %in% "Zambia") %>% mutate(ID = 1:nrow(.))
SSA_adm0_zambia$mofuss_reg <- "SSA_adm0_zambia"
SSA_adm0_zimbabwe <- SSA_adm0_subregions %>% dplyr::filter(Subregion %in% "Zimbabwe") %>% mutate(ID = 1:nrow(.))
SSA_adm0_zimbabwe$mofuss_reg <- "SSA_adm0_zimbabwe"

# Saves each region separatedly for level 0
st_write(SSA_adm0_eastern, "regions_adm0/SSA_adm0_eastern.gpkg", delete_layer = TRUE)
#st_write(SSA_adm0_southeastern, "regions_adm0/SSA_adm0_southeastern.gpkg", delete_layer = TRUE)
st_write(SSA_adm0_southern, "regions_adm0/SSA_adm0_southern.gpkg", delete_layer = TRUE)
st_write(SSA_adm0_western, "regions_adm0/SSA_adm0_western.gpkg", delete_layer = TRUE)
st_write(SSA_adm0_madagascar, "regions_adm0/SSA_adm0_madagascar.gpkg", delete_layer = TRUE)
st_write(SSA_adm0_stp, "regions_adm0/SSA_adm0_stp.gpkg", delete_layer = TRUE)
st_write(SSA_adm0_comoros, "regions_adm0/SSA_adm0_comoros.gpkg", delete_layer = TRUE)
st_write(SSA_adm0_mauritius, "regions_adm0/SSA_adm0_mauritius.gpkg", delete_layer = TRUE)
st_write(SSA_adm0_central, "regions_adm0/SSA_adm0_central.gpkg", delete_layer = TRUE)
st_write(SSA_adm0_northcentral, "regions_adm0/SSA_adm0_northcentral.gpkg", delete_layer = TRUE)
st_write(SSA_adm0_tanzania, "regions_adm0/SSA_adm0_tanzania.gpkg", delete_layer = TRUE)
st_write(SSA_adm0_kenya, "regions_adm0/SSA_adm0_kenya.gpkg", delete_layer = TRUE)
st_write(SSA_adm0_angola, "regions_adm0/SSA_adm0_angola.gpkg", delete_layer = TRUE)
st_write(SSA_adm0_uganda, "regions_adm0/SSA_adm0_uganda.gpkg", delete_layer = TRUE)
st_write(SSA_adm0_mali, "regions_adm0/SSA_adm0_mali.gpkg", delete_layer = TRUE)
st_write(SSA_adm0_chad, "regions_adm0/SSA_adm0_chad.gpkg", delete_layer = TRUE)
st_write(SSA_adm0_niger, "regions_adm0/SSA_adm0_niger.gpkg", delete_layer = TRUE)
st_write(SSA_adm0_caboverde, "regions_adm0/SSA_adm0_caboverde.gpkg", delete_layer = TRUE)
st_write(SSA_adm0_westcentral, "regions_adm0/SSA_adm0_westcentral.gpkg", delete_layer = TRUE)
st_write(SSA_adm0_westsouthern, "regions_adm0/SSA_adm0_westsouthern.gpkg", delete_layer = TRUE)
st_write(SSA_adm0_mauritania, "regions_adm0/SSA_adm0_mauritania.gpkg", delete_layer = TRUE)
st_write(SSA_adm0_benin, "regions_adm0/SSA_adm0_benin.gpkg", delete_layer = TRUE)
st_write(SSA_adm0_burkinafaso, "regions_adm0/SSA_adm0_burkinafaso.gpkg", delete_layer = TRUE)
st_write(SSA_adm0_cdivoire, "regions_adm0/SSA_adm0_cdivoire.gpkg", delete_layer = TRUE)
st_write(SSA_adm0_ghana, "regions_adm0/SSA_adm0_ghana.gpkg", delete_layer = TRUE)
st_write(SSA_adm0_senegambia, "regions_adm0/SSA_adm0_senegambia.gpkg", delete_layer = TRUE)
st_write(SSA_adm0_togo, "regions_adm0/SSA_adm0_togo.gpkg", delete_layer = TRUE)
st_write(SSA_adm0_malambique, "regions_adm0/SSA_adm0_malambique.gpkg", delete_layer = TRUE)
#st_write(SSA_adm0_mozambique, "regions_adm0/SSA_adm0_mozambique.gpkg", delete_layer = TRUE)
st_write(SSA_adm0_zambia, "regions_adm0/SSA_adm0_zambia.gpkg", delete_layer = TRUE)
st_write(SSA_adm0_zimbabwe, "regions_adm0/SSA_adm0_zimbabwe.gpkg", delete_layer = TRUE)

SSA_adm0_eastern %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm0_p/SSA_adm0_eastern_p.gpkg", delete_layer = TRUE)
# SSA_adm0_southeastern %>%
#  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
#  st_write("regions_adm0_p/SSA_adm0_southeastern_p.gpkg", delete_layer = TRUE)
SSA_adm0_southern %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm0_p/SSA_adm0_southern_p.gpkg", delete_layer = TRUE)
SSA_adm0_western %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm0_p/SSA_adm0_western_p.gpkg", delete_layer = TRUE)
SSA_adm0_madagascar %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm0_p/SSA_adm0_madagascar_p.gpkg", delete_layer = TRUE)
SSA_adm0_stp %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm0_p/SSA_adm0_stp_p.gpkg", delete_layer = TRUE)
SSA_adm0_comoros %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm0_p/SSA_adm0_comoros_p.gpkg", delete_layer = TRUE)
SSA_adm0_mauritius %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm0_p/SSA_adm0_mauritius_p.gpkg", delete_layer = TRUE)
SSA_adm0_central %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm0_p/SSA_adm0_central_p.gpkg", delete_layer = TRUE)
SSA_adm0_northcentral %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm0_p/SSA_adm0_northcentral_p.gpkg", delete_layer = TRUE)
SSA_adm0_tanzania %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm0_p/SSA_adm0_tanzania_p.gpkg", delete_layer = TRUE)
SSA_adm0_kenya %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm0_p/SSA_adm0_kenya_p.gpkg", delete_layer = TRUE)
SSA_adm0_angola %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm0_p/SSA_adm0_angola_p.gpkg", delete_layer = TRUE)
SSA_adm0_uganda %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm0_p/SSA_adm0_uganda_p.gpkg", delete_layer = TRUE)
SSA_adm0_mali %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm0_p/SSA_adm0_mali_p.gpkg", delete_layer = TRUE)
SSA_adm0_chad %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm0_p/SSA_adm0_chad_p.gpkg", delete_layer = TRUE)
SSA_adm0_niger %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm0_p/SSA_adm0_niger_p.gpkg", delete_layer = TRUE)
SSA_adm0_caboverde %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm0_p/SSA_adm0_caboverde_p.gpkg", delete_layer = TRUE)
SSA_adm0_westcentral %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm0_p/SSA_adm0_westcentral_p.gpkg", delete_layer = TRUE)
SSA_adm0_westsouthern %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm0_p/SSA_adm0_westsouthern_p.gpkg", delete_layer = TRUE)
SSA_adm0_mauritania %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm0_p/SSA_adm0_mauritania_p.gpkg", delete_layer = TRUE)
SSA_adm0_benin %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm0_p/SSA_adm0_benin_p.gpkg", delete_layer = TRUE)
SSA_adm0_burkinafaso %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm0_p/SSA_adm0_burkinafaso_p.gpkg", delete_layer = TRUE)
SSA_adm0_cdivoire %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm0_p/SSA_adm0_cdivoire_p.gpkg", delete_layer = TRUE)
SSA_adm0_ghana %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm0_p/SSA_adm0_ghana_p.gpkg", delete_layer = TRUE)
SSA_adm0_senegambia %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm0_p/SSA_adm0_senegambia_p.gpkg", delete_layer = TRUE)
SSA_adm0_togo %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm0_p/SSA_adm0_togo_p.gpkg", delete_layer = TRUE)
SSA_adm0_malambique %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm0_p/SSA_adm0_malambique_p.gpkg", delete_layer = TRUE)
# SSA_adm0_mozambique %>%
#   st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
#   st_write("regions_adm0_p/SSA_adm0_mozambique_p.gpkg", delete_layer = TRUE)
SSA_adm0_zambia %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm0_p/SSA_adm0_zambia_p.gpkg", delete_layer = TRUE)
SSA_adm0_zimbabwe %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm0_p/SSA_adm0_zimbabwe_p.gpkg", delete_layer = TRUE)

# Sub admin_regions #1
SSA_adm1_eastern <- gadm_adm1_sel %>% dplyr::filter(GID_0 %in% SSA_adm0_eastern$GID_0) %>% mutate(ID = 1:nrow(.))
# SSA_adm1_southeastern <- gadm_adm1_sel %>% dplyr::filter(GID_0 %in% SSA_adm0_southeastern$GID_0) %>% mutate(ID = 1:nrow(.))
SSA_adm1_southern <- gadm_adm1_sel %>% dplyr::filter(GID_0 %in% SSA_adm0_southern$GID_0) %>% mutate(ID = 1:nrow(.))
SSA_adm1_western <- gadm_adm1_sel %>% dplyr::filter(GID_0 %in% SSA_adm0_western$GID_0) %>% mutate(ID = 1:nrow(.))
SSA_adm1_madagascar <- gadm_adm1_sel %>% dplyr::filter(GID_0 %in% SSA_adm0_madagascar$GID_0) %>% mutate(ID = 1:nrow(.))
SSA_adm1_stp <- gadm_adm1_sel %>% dplyr::filter(GID_0 %in% SSA_adm0_stp$GID_0) %>% mutate(ID = 1:nrow(.))
SSA_adm1_comoros <- gadm_adm1_sel %>% dplyr::filter(GID_0 %in% SSA_adm0_comoros$GID_0) %>% mutate(ID = 1:nrow(.))
SSA_adm1_mauritius <- gadm_adm1_sel %>% dplyr::filter(GID_0 %in% SSA_adm0_mauritius$GID_0) %>% mutate(ID = 1:nrow(.))
SSA_adm1_central <- gadm_adm1_sel %>% dplyr::filter(GID_0 %in% SSA_adm0_central$GID_0) %>% mutate(ID = 1:nrow(.))
SSA_adm1_northcentral <- gadm_adm1_sel %>% dplyr::filter(GID_0 %in% SSA_adm0_northcentral$GID_0) %>% mutate(ID = 1:nrow(.))
SSA_adm1_tanzania <- gadm_adm1_sel %>% dplyr::filter(GID_0 %in% SSA_adm0_tanzania$GID_0) %>% mutate(ID = 1:nrow(.))
SSA_adm1_kenya <- gadm_adm1_sel %>% dplyr::filter(GID_0 %in% SSA_adm0_kenya$GID_0) %>% mutate(ID = 1:nrow(.))
SSA_adm1_angola <- gadm_adm1_sel %>% dplyr::filter(GID_0 %in% SSA_adm0_angola$GID_0) %>% mutate(ID = 1:nrow(.))
SSA_adm1_uganda <- gadm_adm1_sel %>% dplyr::filter(GID_0 %in% SSA_adm0_uganda$GID_0) %>% mutate(ID = 1:nrow(.))
SSA_adm1_mali <- gadm_adm1_sel %>% dplyr::filter(GID_0 %in% SSA_adm0_mali$GID_0) %>% mutate(ID = 1:nrow(.))
SSA_adm1_chad <- gadm_adm1_sel %>% dplyr::filter(GID_0 %in% SSA_adm0_chad$GID_0) %>% mutate(ID = 1:nrow(.))
SSA_adm1_niger <- gadm_adm1_sel %>% dplyr::filter(GID_0 %in% SSA_adm0_niger$GID_0) %>% mutate(ID = 1:nrow(.))
SSA_adm1_caboverde <- gadm_adm1_sel %>% dplyr::filter(GID_0 %in% SSA_adm0_caboverde$GID_0) %>% mutate(ID = 1:nrow(.))
SSA_adm1_westcentral <- gadm_adm1_sel %>% dplyr::filter(GID_0 %in% SSA_adm0_westcentral$GID_0) %>% mutate(ID = 1:nrow(.))
SSA_adm1_westsouthern <- gadm_adm1_sel %>% dplyr::filter(GID_0 %in% SSA_adm0_westsouthern$GID_0) %>% mutate(ID = 1:nrow(.))
SSA_adm1_mauritania <- gadm_adm1_sel %>% dplyr::filter(GID_0 %in% SSA_adm0_mauritania$GID_0) %>% mutate(ID = 1:nrow(.))
SSA_adm1_benin <- gadm_adm1_sel %>% dplyr::filter(GID_0 %in% SSA_adm0_benin$GID_0) %>% mutate(ID = 1:nrow(.))
SSA_adm1_burkinafaso <- gadm_adm1_sel %>% dplyr::filter(GID_0 %in% SSA_adm0_burkinafaso$GID_0) %>% mutate(ID = 1:nrow(.))
SSA_adm1_cdivoire <- gadm_adm1_sel %>% dplyr::filter(GID_0 %in% SSA_adm0_cdivoire$GID_0) %>% mutate(ID = 1:nrow(.))
SSA_adm1_ghana <- gadm_adm1_sel %>% dplyr::filter(GID_0 %in% SSA_adm0_ghana$GID_0) %>% mutate(ID = 1:nrow(.))
SSA_adm1_senegambia <- gadm_adm1_sel %>% dplyr::filter(GID_0 %in% SSA_adm0_senegambia$GID_0) %>% mutate(ID = 1:nrow(.))
SSA_adm1_togo <- gadm_adm1_sel %>% dplyr::filter(GID_0 %in% SSA_adm0_togo$GID_0) %>% mutate(ID = 1:nrow(.))
SSA_adm1_malambique <- gadm_adm1_sel %>% dplyr::filter(GID_0 %in% SSA_adm0_malambique$GID_0) %>% mutate(ID = 1:nrow(.))
#SSA_adm1_mozambique <- gadm_adm1_sel %>% dplyr::filter(GID_0 %in% SSA_adm0_mozambique$GID_0) %>% mutate(ID = 1:nrow(.))
SSA_adm1_zambia <- gadm_adm1_sel %>% dplyr::filter(GID_0 %in% SSA_adm0_zambia$GID_0) %>% mutate(ID = 1:nrow(.))
SSA_adm1_zimbabwe <- gadm_adm1_sel %>% dplyr::filter(GID_0 %in% SSA_adm0_zimbabwe$GID_0) %>% mutate(ID = 1:nrow(.))

st_write(SSA_adm1_eastern, "regions_adm1/SSA_adm1_eastern.gpkg", delete_layer = TRUE)
# st_write(SSA_adm1_southeastern, "regions_adm1/SSA_adm1_southeastern.gpkg", delete_layer = TRUE)
st_write(SSA_adm1_southern, "regions_adm1/SSA_adm1_southern.gpkg", delete_layer = TRUE)
st_write(SSA_adm1_western, "regions_adm1/SSA_adm1_western.gpkg", delete_layer = TRUE)
st_write(SSA_adm1_madagascar, "regions_adm1/SSA_adm1_madagascar.gpkg", delete_layer = TRUE)
st_write(SSA_adm1_stp, "regions_adm1/SSA_adm1_stp.gpkg", delete_layer = TRUE)
st_write(SSA_adm1_comoros, "regions_adm1/SSA_adm1_comoros.gpkg", delete_layer = TRUE)
st_write(SSA_adm1_mauritius, "regions_adm1/SSA_adm1_mauritius.gpkg", delete_layer = TRUE)
st_write(SSA_adm1_central, "regions_adm1/SSA_adm1_central.gpkg", delete_layer = TRUE)
st_write(SSA_adm1_northcentral, "regions_adm1/SSA_adm1_northcentral.gpkg", delete_layer = TRUE)
st_write(SSA_adm1_tanzania, "regions_adm1/SSA_adm1_tanzania.gpkg", delete_layer = TRUE)
st_write(SSA_adm1_kenya, "regions_adm1/SSA_adm1_kenya.gpkg", delete_layer = TRUE)
st_write(SSA_adm1_angola, "regions_adm1/SSA_adm1_angola.gpkg", delete_layer = TRUE)
st_write(SSA_adm1_uganda, "regions_adm1/SSA_adm1_uganda.gpkg", delete_layer = TRUE)
st_write(SSA_adm1_mali, "regions_adm1/SSA_adm1_mali.gpkg", delete_layer = TRUE)
st_write(SSA_adm1_chad, "regions_adm1/SSA_adm1_chad.gpkg", delete_layer = TRUE)
st_write(SSA_adm1_niger, "regions_adm1/SSA_adm1_niger.gpkg", delete_layer = TRUE)
st_write(SSA_adm1_caboverde, "regions_adm1/SSA_adm1_caboverde.gpkg", delete_layer = TRUE)
st_write(SSA_adm1_westcentral, "regions_adm1/SSA_adm1_westcentral.gpkg", delete_layer = TRUE)
st_write(SSA_adm1_westsouthern, "regions_adm1/SSA_adm1_westsouthern.gpkg", delete_layer = TRUE)
st_write(SSA_adm1_mauritania, "regions_adm1/SSA_adm1_mauritania.gpkg", delete_layer = TRUE)
st_write(SSA_adm1_benin, "regions_adm1/SSA_adm1_benin.gpkg", delete_layer = TRUE)
st_write(SSA_adm1_burkinafaso, "regions_adm1/SSA_adm1_burkinafaso.gpkg", delete_layer = TRUE)
st_write(SSA_adm1_cdivoire, "regions_adm1/SSA_adm1_cdivoire.gpkg", delete_layer = TRUE)
st_write(SSA_adm1_ghana, "regions_adm1/SSA_adm1_ghana.gpkg", delete_layer = TRUE)
st_write(SSA_adm1_senegambia, "regions_adm1/SSA_adm1_senegambia.gpkg", delete_layer = TRUE)
st_write(SSA_adm1_togo, "regions_adm1/SSA_adm1_togo.gpkg", delete_layer = TRUE)
st_write(SSA_adm1_malambique, "regions_adm1/SSA_adm1_malambique.gpkg", delete_layer = TRUE)
#st_write(SSA_adm1_mozambique, "regions_adm1/SSA_adm1_mozambique.gpkg", delete_layer = TRUE)
st_write(SSA_adm1_zambia, "regions_adm1/SSA_adm1_zambia.gpkg", delete_layer = TRUE)
st_write(SSA_adm1_zimbabwe, "regions_adm1/SSA_adm1_zimbabwe.gpkg", delete_layer = TRUE)

SSA_adm1_eastern %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm1_p/SSA_adm1_eastern_p.gpkg", delete_layer = TRUE)
# SSA_adm1_southeastern %>%
# st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
# st_write("regions_adm1_p/SSA_adm1_southeastern_p.gpkg", delete_layer = TRUE)
SSA_adm1_southern %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm1_p/SSA_adm1_southern_p.gpkg", delete_layer = TRUE)
SSA_adm1_western %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm1_p/SSA_adm1_western_p.gpkg", delete_layer = TRUE)
SSA_adm1_madagascar %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm1_p/SSA_adm1_madagascar_p.gpkg", delete_layer = TRUE)
SSA_adm1_stp %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm1_p/SSA_adm1_stp_p.gpkg", delete_layer = TRUE)
SSA_adm1_comoros %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm1_p/SSA_adm1_comoros_p.gpkg", delete_layer = TRUE)
SSA_adm1_mauritius %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm1_p/SSA_adm1_mauritius_p.gpkg", delete_layer = TRUE)
SSA_adm1_central %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm1_p/SSA_adm1_central_p.gpkg", delete_layer = TRUE)
SSA_adm1_northcentral %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm1_p/SSA_adm1_northcentral_p.gpkg", delete_layer = TRUE)
SSA_adm1_tanzania %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm1_p/SSA_adm1_tanzania_p.gpkg", delete_layer = TRUE)
SSA_adm1_kenya %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm1_p/SSA_adm1_kenya_p.gpkg", delete_layer = TRUE)
SSA_adm1_angola %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm1_p/SSA_adm1_angola_p.gpkg", delete_layer = TRUE)
SSA_adm1_uganda %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm1_p/SSA_adm1_uganda_p.gpkg", delete_layer = TRUE)
SSA_adm1_mali %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm1_p/SSA_adm1_mali_p.gpkg", delete_layer = TRUE)
SSA_adm1_chad %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm1_p/SSA_adm1_chad_p.gpkg", delete_layer = TRUE)
SSA_adm1_niger %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm1_p/SSA_adm1_niger_p.gpkg", delete_layer = TRUE)
SSA_adm1_caboverde %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm1_p/SSA_adm1_caboverde_p.gpkg", delete_layer = TRUE)
SSA_adm1_westcentral %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm1_p/SSA_adm1_westcentral_p.gpkg", delete_layer = TRUE)
SSA_adm1_westsouthern %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm1_p/SSA_adm1_westsouthern_p.gpkg", delete_layer = TRUE)
SSA_adm1_mauritania %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm1_p/SSA_adm1_mauritania_p.gpkg", delete_layer = TRUE)
SSA_adm1_benin %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm1_p/SSA_adm1_benin_p.gpkg", delete_layer = TRUE)
SSA_adm1_burkinafaso %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm1_p/SSA_adm1_burkinafaso_p.gpkg", delete_layer = TRUE)
SSA_adm1_cdivoire %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm1_p/SSA_adm1_cdivoire_p.gpkg", delete_layer = TRUE)
SSA_adm1_ghana %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm1_p/SSA_adm1_ghana_p.gpkg", delete_layer = TRUE)
SSA_adm1_senegambia %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm1_p/SSA_adm1_senegambia_p.gpkg", delete_layer = TRUE)
SSA_adm1_togo %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm1_p/SSA_adm1_togo_p.gpkg", delete_layer = TRUE)
SSA_adm1_malambique %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm1_p/SSA_adm1_malambique_p.gpkg", delete_layer = TRUE)
# SSA_adm1_mozambique %>%
#   st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
#   st_write("regions_adm1_p/SSA_adm1_mozambique_p.gpkg", delete_layer = TRUE)
SSA_adm1_zambia %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm1_p/SSA_adm1_zambia_p.gpkg", delete_layer = TRUE)
SSA_adm1_zimbabwe %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm1_p/SSA_adm1_zimbabwe_p.gpkg", delete_layer = TRUE)


# Sub admin_regions #2
SSA_adm2_eastern <- gadm_adm2_sel %>% dplyr::filter(GID_0 %in% SSA_adm0_eastern$GID_0) %>% mutate(ID = 1:nrow(.))
# SSA_adm2_southeastern <- gadm_adm2_sel %>% dplyr::filter(GID_0 %in% SSA_adm0_southeastern$GID_0) %>% mutate(ID = 1:nrow(.))
SSA_adm2_southern <- gadm_adm2_sel %>% dplyr::filter(GID_0 %in% SSA_adm0_southern$GID_0) %>% mutate(ID = 1:nrow(.))
SSA_adm2_western <- gadm_adm2_sel %>% dplyr::filter(GID_0 %in% SSA_adm0_western$GID_0) %>% mutate(ID = 1:nrow(.))
SSA_adm2_madagascar <- gadm_adm2_sel %>% dplyr::filter(GID_0 %in% SSA_adm0_madagascar$GID_0) %>% mutate(ID = 1:nrow(.))
SSA_adm2_stp <- gadm_adm2_sel %>% dplyr::filter(GID_0 %in% SSA_adm0_stp$GID_0) %>% mutate(ID = 1:nrow(.))
# SSA_adm2_comoros <- gadm_adm2_sel %>% dplyr::filter(GID_0 %in% SSA_adm0_comoros$GID_0) %>% mutate(ID = 1:nrow(.))
# SSA_adm2_mauritius <- gadm_adm2_sel %>% dplyr::filter(GID_0 %in% SSA_adm0_mauritius$GID_0) %>% mutate(ID = 1:nrow(.))
SSA_adm2_comoros <- SSA_adm1_comoros %>% # Saving ADM1 for Comoros cause no ADM2 available
  dplyr::mutate(GID_2 = GID_1,
                NAME_2 = NAME_1)
SSA_adm2_mauritius <- SSA_adm1_mauritius %>% # Saving ADM1 for Mauritius cause no ADM2 available
  dplyr::mutate(GID_2 = GID_1,
                NAME_2 = NAME_1)
SSA_adm2_central <- gadm_adm2_sel %>% dplyr::filter(GID_0 %in% SSA_adm0_central$GID_0) %>% mutate(ID = 1:nrow(.))
SSA_adm2_northcentral <- gadm_adm2_sel %>% dplyr::filter(GID_0 %in% SSA_adm0_northcentral$GID_0) %>% mutate(ID = 1:nrow(.))
SSA_adm2_tanzania <- gadm_adm2_sel %>% dplyr::filter(GID_0 %in% SSA_adm0_tanzania$GID_0) %>% mutate(ID = 1:nrow(.))
SSA_adm2_kenya <- gadm_adm2_sel %>% dplyr::filter(GID_0 %in% SSA_adm0_kenya$GID_0) %>% mutate(ID = 1:nrow(.))
SSA_adm2_angola <- gadm_adm2_sel %>% dplyr::filter(GID_0 %in% SSA_adm0_angola$GID_0) %>% mutate(ID = 1:nrow(.))
SSA_adm2_uganda <- gadm_adm2_sel %>% dplyr::filter(GID_0 %in% SSA_adm0_uganda$GID_0) %>% mutate(ID = 1:nrow(.))
SSA_adm2_mali <- gadm_adm2_sel %>% dplyr::filter(GID_0 %in% SSA_adm0_mali$GID_0) %>% mutate(ID = 1:nrow(.))
SSA_adm2_chad <- gadm_adm2_sel %>% dplyr::filter(GID_0 %in% SSA_adm0_chad$GID_0) %>% mutate(ID = 1:nrow(.))
SSA_adm2_niger <- gadm_adm2_sel %>% dplyr::filter(GID_0 %in% SSA_adm0_niger$GID_0) %>% mutate(ID = 1:nrow(.))
# SSA_adm2_caboverde <- gadm_adm2_sel %>% dplyr::filter(GID_0 %in% SSA_adm0_caboverde$GID_0) %>% mutate(ID = 1:nrow(.))
SSA_adm2_caboverde <- SSA_adm1_caboverde %>% # Saving ADM1 for Mauritius cause no ADM2 available
  dplyr::mutate(GID_2 = GID_1,
                NAME_2 = NAME_1)
SSA_adm2_westcentral <- gadm_adm2_sel %>% dplyr::filter(GID_0 %in% SSA_adm0_westcentral$GID_0) %>% mutate(ID = 1:nrow(.))
SSA_adm2_westsouthern <- gadm_adm2_sel %>% dplyr::filter(GID_0 %in% SSA_adm0_westsouthern$GID_0) %>% mutate(ID = 1:nrow(.))
SSA_adm2_mauritania <- gadm_adm2_sel %>% dplyr::filter(GID_0 %in% SSA_adm0_mauritania$GID_0) %>% mutate(ID = 1:nrow(.))
SSA_adm2_benin <- gadm_adm2_sel %>% dplyr::filter(GID_0 %in% SSA_adm0_benin$GID_0) %>% mutate(ID = 1:nrow(.))
SSA_adm2_burkinafaso <- gadm_adm2_sel %>% dplyr::filter(GID_0 %in% SSA_adm0_burkinafaso$GID_0) %>% mutate(ID = 1:nrow(.))
SSA_adm2_cdivoire <- gadm_adm2_sel %>% dplyr::filter(GID_0 %in% SSA_adm0_cdivoire$GID_0) %>% mutate(ID = 1:nrow(.))
SSA_adm2_ghana <- gadm_adm2_sel %>% dplyr::filter(GID_0 %in% SSA_adm0_ghana$GID_0) %>% mutate(ID = 1:nrow(.))
SSA_adm2_senegambia <- gadm_adm2_sel %>% dplyr::filter(GID_0 %in% SSA_adm0_senegambia$GID_0) %>% mutate(ID = 1:nrow(.))
SSA_adm2_togo <- gadm_adm2_sel %>% dplyr::filter(GID_0 %in% SSA_adm0_togo$GID_0) %>% mutate(ID = 1:nrow(.))
SSA_adm2_malambique <- gadm_adm2_sel %>% dplyr::filter(GID_0 %in% SSA_adm0_malambique$GID_0) %>% mutate(ID = 1:nrow(.))
#SSA_adm2_mozambique <- gadm_adm2_sel %>% dplyr::filter(GID_0 %in% SSA_adm0_mozambique$GID_0) %>% mutate(ID = 1:nrow(.))
SSA_adm2_zambia <- gadm_adm2_sel %>% dplyr::filter(GID_0 %in% SSA_adm0_zambia$GID_0) %>% mutate(ID = 1:nrow(.))
SSA_adm2_zimbabwe <- gadm_adm2_sel %>% dplyr::filter(GID_0 %in% SSA_adm0_zimbabwe$GID_0) %>% mutate(ID = 1:nrow(.))

st_write(SSA_adm2_eastern, "regions_adm2/SSA_adm2_eastern.gpkg", delete_layer = TRUE)
# st_write(SSA_adm2_southeastern, "regions_adm2/SSA_adm2_southeastern.gpkg", delete_layer = TRUE)
st_write(SSA_adm2_southern, "regions_adm2/SSA_adm2_southern.gpkg", delete_layer = TRUE)
st_write(SSA_adm2_western, "regions_adm2/SSA_adm2_western.gpkg", delete_layer = TRUE)
st_write(SSA_adm2_madagascar, "regions_adm2/SSA_adm2_madagascar.gpkg", delete_layer = TRUE)
st_write(SSA_adm2_stp, "regions_adm2/SSA_adm2_stp.gpkg", delete_layer = TRUE)
st_write(SSA_adm2_comoros, "regions_adm2/SSA_adm2_comoros.gpkg", delete_layer = TRUE)
st_write(SSA_adm2_mauritius, "regions_adm2/SSA_adm2_mauritius.gpkg", delete_layer = TRUE)
st_write(SSA_adm2_central, "regions_adm2/SSA_adm2_central.gpkg", delete_layer = TRUE)
st_write(SSA_adm2_northcentral, "regions_adm2/SSA_adm2_northcentral.gpkg", delete_layer = TRUE)
st_write(SSA_adm2_tanzania, "regions_adm2/SSA_adm2_tanzania.gpkg", delete_layer = TRUE)
st_write(SSA_adm2_kenya, "regions_adm2/SSA_adm2_kenya.gpkg", delete_layer = TRUE)
st_write(SSA_adm2_angola, "regions_adm2/SSA_adm2_angola.gpkg", delete_layer = TRUE)
st_write(SSA_adm2_uganda, "regions_adm2/SSA_adm2_uganda.gpkg", delete_layer = TRUE)
st_write(SSA_adm2_mali, "regions_adm2/SSA_adm2_mali.gpkg", delete_layer = TRUE)
st_write(SSA_adm2_chad, "regions_adm2/SSA_adm2_chad.gpkg", delete_layer = TRUE)
st_write(SSA_adm2_niger, "regions_adm2/SSA_adm2_niger.gpkg", delete_layer = TRUE)
st_write(SSA_adm2_caboverde, "regions_adm2/SSA_adm2_caboverde.gpkg", delete_layer = TRUE)
st_write(SSA_adm2_westcentral, "regions_adm2/SSA_adm2_westcentral.gpkg", delete_layer = TRUE)
st_write(SSA_adm2_westsouthern, "regions_adm2/SSA_adm2_westsouthern.gpkg", delete_layer = TRUE)
st_write(SSA_adm2_mauritania, "regions_adm2/SSA_adm2_mauritania.gpkg", delete_layer = TRUE)
st_write(SSA_adm2_benin, "regions_adm2/SSA_adm2_benin.gpkg", delete_layer = TRUE)
st_write(SSA_adm2_burkinafaso, "regions_adm2/SSA_adm2_burkinafaso.gpkg", delete_layer = TRUE)
st_write(SSA_adm2_cdivoire, "regions_adm2/SSA_adm2_cdivoire.gpkg", delete_layer = TRUE)
st_write(SSA_adm2_ghana, "regions_adm2/SSA_adm2_ghana.gpkg", delete_layer = TRUE)
st_write(SSA_adm2_senegambia, "regions_adm2/SSA_adm2_senegambia.gpkg", delete_layer = TRUE)
st_write(SSA_adm2_togo, "regions_adm2/SSA_adm2_togo.gpkg", delete_layer = TRUE)
st_write(SSA_adm2_malambique, "regions_adm2/SSA_adm2_malambique.gpkg", delete_layer = TRUE)
#st_write(SSA_adm2_mozambique, "regions_adm2/SSA_adm2_mozambique.gpkg", delete_layer = TRUE)
st_write(SSA_adm2_zambia, "regions_adm2/SSA_adm2_zambia.gpkg", delete_layer = TRUE)
st_write(SSA_adm2_zimbabwe, "regions_adm2/SSA_adm2_zimbabwe.gpkg", delete_layer = TRUE)

SSA_adm2_eastern %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm2_p/SSA_adm2_eastern_p.gpkg", delete_layer = TRUE)
# SSA_adm2_southeastern %>%
#   st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
#   st_write("regions_adm2_p/SSA_adm2_southeastern_p.gpkg", delete_layer = TRUE)
SSA_adm2_southern %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm2_p/SSA_adm2_southern_p.gpkg", delete_layer = TRUE)
SSA_adm2_western %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm2_p/SSA_adm2_western_p.gpkg", delete_layer = TRUE)
SSA_adm2_madagascar %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm2_p/SSA_adm2_madagascar_p.gpkg", delete_layer = TRUE)
SSA_adm2_stp %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm2_p/SSA_adm2_stp_p.gpkg", delete_layer = TRUE)
SSA_adm2_comoros %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm2_p/SSA_adm2_comoros_p.gpkg", delete_layer = TRUE)
SSA_adm2_mauritius %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm2_p/SSA_adm2_mauritius_p.gpkg", delete_layer = TRUE)
SSA_adm2_central %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm2_p/SSA_adm2_central_p.gpkg", delete_layer = TRUE)
SSA_adm2_northcentral %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm2_p/SSA_adm2_northcentral_p.gpkg", delete_layer = TRUE)
SSA_adm2_tanzania %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm2_p/SSA_adm2_tanzania_p.gpkg", delete_layer = TRUE)
SSA_adm2_kenya %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm2_p/SSA_adm2_kenya_p.gpkg", delete_layer = TRUE)
SSA_adm2_angola %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm2_p/SSA_adm2_angola_p.gpkg", delete_layer = TRUE)
SSA_adm2_uganda %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm2_p/SSA_adm2_uganda_p.gpkg", delete_layer = TRUE)
SSA_adm2_mali %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm2_p/SSA_adm2_mali_p.gpkg", delete_layer = TRUE)
SSA_adm2_chad %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm2_p/SSA_adm2_chad_p.gpkg", delete_layer = TRUE)
SSA_adm2_niger %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm2_p/SSA_adm2_niger_p.gpkg", delete_layer = TRUE)
SSA_adm2_caboverde %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm2_p/SSA_adm2_caboverde_p.gpkg", delete_layer = TRUE)
SSA_adm2_westcentral %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm2_p/SSA_adm2_westcentral_p.gpkg", delete_layer = TRUE)
SSA_adm2_westsouthern %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm2_p/SSA_adm2_westsouthern_p.gpkg", delete_layer = TRUE)
SSA_adm2_mauritania %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm2_p/SSA_adm2_mauritania_p.gpkg", delete_layer = TRUE)
SSA_adm2_benin %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm2_p/SSA_adm2_benin_p.gpkg", delete_layer = TRUE)
SSA_adm2_burkinafaso %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm2_p/SSA_adm2_burkinafaso_p.gpkg", delete_layer = TRUE)
SSA_adm2_cdivoire %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm2_p/SSA_adm2_cdivoire_p.gpkg", delete_layer = TRUE)
SSA_adm2_ghana %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm2_p/SSA_adm2_ghana_p.gpkg", delete_layer = TRUE)
SSA_adm2_senegambia %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm2_p/SSA_adm2_senegambia_p.gpkg", delete_layer = TRUE)
SSA_adm2_togo %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm2_p/SSA_adm2_togo_p.gpkg", delete_layer = TRUE)
SSA_adm2_malambique %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm2_p/SSA_adm2_malambique_p.gpkg", delete_layer = TRUE)
# SSA_adm2_mozambique %>%
#   st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
#   st_write("regions_adm2_p/SSA_adm2_mozambique_p.gpkg", delete_layer = TRUE)
SSA_adm2_zambia %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm2_p/SSA_adm2_zambia_p.gpkg", delete_layer = TRUE)
SSA_adm2_zimbabwe %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm2_p/SSA_adm2_zimbabwe_p.gpkg", delete_layer = TRUE)

# Americas ----
subregionsLATAM <- read_excel("subregionsLATAM_v3.xlsx")
subregionsLATAM
unique(subregionsLATAM$Subregion)

LATAM_adm0_subregions <- LATAM_adm0 %>% 
  right_join(subregionsLATAM, by = "GID_0") %>%
  dplyr::select (-NAME_0.y) %>% 
  dplyr::rename(NAME_0 = NAME_0.x) %>%
  filter(!is.na(NAME_0))
LATAM_adm0_subregions %>% st_drop_geometry()
LATAM_adm0_subregions
unique(subregionsLATAM$Subregion)
# mapview(LATAM_adm0)
unique(LATAM_adm0$NAME_0)

# Build regions
LATAM_adm0_southern <- LATAM_adm0_subregions %>% dplyr::filter(Subregion %in% "Southern LATAM") %>% mutate(ID = 1:nrow(.))
LATAM_adm0_southern$mofuss_reg <- "LATAM_adm0_southern"
LATAM_adm0_CA <- LATAM_adm0_subregions %>% dplyr::filter(Subregion %in% "Central America") %>% mutate(ID = 1:nrow(.))
LATAM_adm0_CA$mofuss_reg <- "LATAM_adm0_CA"
LATAM_adm0_espanhola <- LATAM_adm0_subregions %>% dplyr::filter(Subregion %in% "Espanhola") %>% mutate(ID = 1:nrow(.))
LATAM_adm0_espanhola$mofuss_reg <- "LATAM_adm0_espanhola"
LATAM_adm0_jamaica <- LATAM_adm0_subregions %>% dplyr::filter(Subregion %in% "Jamaica") %>% mutate(ID = 1:nrow(.))
LATAM_adm0_jamaica$mofuss_reg <- "LATAM_adm0_jamaica"
LATAM_adm0_western <- LATAM_adm0_subregions %>% dplyr::filter(Subregion %in% "Western LATAM") %>% mutate(ID = 1:nrow(.))
LATAM_adm0_western$mofuss_reg <- "LATAM_adm0_western"
LATAM_adm0_brazil <- LATAM_adm0_subregions %>% dplyr::filter(Subregion %in% "Brazil") %>% mutate(ID = 1:nrow(.))
LATAM_adm0_brazil$mofuss_reg <- "LATAM_adm0_brazil"
LATAM_adm0_mexico <- LATAM_adm0_subregions %>% dplyr::filter(Subregion %in% "Mexico") %>% mutate(ID = 1:nrow(.))
LATAM_adm0_mexico$mofuss_reg <- "LATAM_adm0_mexico"
LATAM_adm0_bolivia <- LATAM_adm0_subregions %>% dplyr::filter(Subregion %in% "Bolivia") %>% mutate(ID = 1:nrow(.))
LATAM_adm0_bolivia$mofuss_reg <- "LATAM_adm0_bolivia"
LATAM_adm0_colombia <- LATAM_adm0_subregions %>% dplyr::filter(Subregion %in% "Colombia") %>% mutate(ID = 1:nrow(.))
LATAM_adm0_colombia$mofuss_reg <- "LATAM_adm0_colombia"
LATAM_adm0_guyana <- LATAM_adm0_subregions %>% dplyr::filter(Subregion %in% "Guyana") %>% mutate(ID = 1:nrow(.))
LATAM_adm0_guyana$mofuss_reg <- "LATAM_adm0_guyana"
LATAM_adm0_salvador <- LATAM_adm0_subregions %>% dplyr::filter(Subregion %in% "El Salvador") %>% mutate(ID = 1:nrow(.))
LATAM_adm0_salvador$mofuss_reg <- "LATAM_adm0_salvador"

# Saves each region separatedly for level 0
st_write(LATAM_adm0_southern, "regions_adm0/LATAM_adm0_southern.gpkg", delete_layer = TRUE)
st_write(LATAM_adm0_CA, "regions_adm0/LATAM_adm0_CA.gpkg", delete_layer = TRUE)
st_write(LATAM_adm0_espanhola, "regions_adm0/LATAM_adm0_espanhola.gpkg", delete_layer = TRUE)
st_write(LATAM_adm0_jamaica, "regions_adm0/LATAM_adm0_jamaica.gpkg", delete_layer = TRUE)
st_write(LATAM_adm0_western, "regions_adm0/LATAM_adm0_western.gpkg", delete_layer = TRUE)
# st_write(LATAM_adm0_northern, "regions_adm0/LATAM_adm0_northern.gpkg", delete_layer = TRUE)
st_write(LATAM_adm0_brazil, "regions_adm0/LATAM_adm0_brazil.gpkg", delete_layer = TRUE)
st_write(LATAM_adm0_mexico, "regions_adm0/LATAM_adm0_mexico.gpkg", delete_layer = TRUE)
st_write(LATAM_adm0_bolivia, "regions_adm0/LATAM_adm0_bolivia.gpkg", delete_layer = TRUE)
st_write(LATAM_adm0_colombia, "regions_adm0/LATAM_adm0_colombia.gpkg", delete_layer = TRUE)
st_write(LATAM_adm0_guyana, "regions_adm0/LATAM_adm0_guyana.gpkg", delete_layer = TRUE)
st_write(LATAM_adm0_salvador, "regions_adm0/LATAM_adm0_salvador.gpkg", delete_layer = TRUE)

LATAM_adm0_southern %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm0_p/LATAM_adm0_southern_p.gpkg", delete_layer = TRUE)
LATAM_adm0_CA %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm0_p/LATAM_adm0_CA_p.gpkg", delete_layer = TRUE)
LATAM_adm0_espanhola %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm0_p/LATAM_adm0_espanhola_p.gpkg", delete_layer = TRUE)
LATAM_adm0_jamaica %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm0_p/LATAM_adm0_jamaica_p.gpkg", delete_layer = TRUE)
LATAM_adm0_western %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm0_p/LATAM_adm0_western_p.gpkg", delete_layer = TRUE)
# LATAM_adm0_northern %>%
#   st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
#   st_write("regions_adm0_p/LATAM_adm0_northern_p.gpkg", delete_layer = TRUE)
LATAM_adm0_brazil %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm0_p/LATAM_adm0_brazil_p.gpkg", delete_layer = TRUE)
LATAM_adm0_mexico %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm0_p/LATAM_adm0_mexico_p.gpkg", delete_layer = TRUE)
LATAM_adm0_bolivia %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm0_p/LATAM_adm0_bolivia_p.gpkg", delete_layer = TRUE)
LATAM_adm0_colombia %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm0_p/LATAM_adm0_colombia_p.gpkg", delete_layer = TRUE)
LATAM_adm0_guyana %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm0_p/LATAM_adm0_guyana_p.gpkg", delete_layer = TRUE)
LATAM_adm0_salvador %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm0_p/LATAM_adm0_salvador_p.gpkg", delete_layer = TRUE)

  
# # Sub admin_regions #1
LATAM_adm1_southern <- gadm_adm1_sel %>% dplyr::filter(GID_0 %in% LATAM_adm0_southern$GID_0) %>% mutate(ID = 1:nrow(.))
LATAM_adm1_CA <- gadm_adm1_sel %>% dplyr::filter(GID_0 %in% LATAM_adm0_CA$GID_0) %>% mutate(ID = 1:nrow(.))
LATAM_adm1_espanhola <- gadm_adm1_sel %>% dplyr::filter(GID_0 %in% LATAM_adm0_espanhola$GID_0) %>% mutate(ID = 1:nrow(.))
LATAM_adm1_jamaica <- gadm_adm1_sel %>% dplyr::filter(GID_0 %in% LATAM_adm0_jamaica$GID_0) %>% mutate(ID = 1:nrow(.))
LATAM_adm1_western <- gadm_adm1_sel %>% dplyr::filter(GID_0 %in% LATAM_adm0_western$GID_0) %>% mutate(ID = 1:nrow(.))
# LATAM_adm1_northern <- gadm_adm1_sel %>% dplyr::filter(GID_0 %in% LATAM_adm0_northern$GID_0) %>% mutate(ID = 1:nrow(.))
LATAM_adm1_brazil <- gadm_adm1_sel %>% dplyr::filter(GID_0 %in% LATAM_adm0_brazil$GID_0) %>% mutate(ID = 1:nrow(.))
LATAM_adm1_mexico <- gadm_adm1_sel %>% dplyr::filter(GID_0 %in% LATAM_adm0_mexico$GID_0) %>% mutate(ID = 1:nrow(.))
LATAM_adm1_bolivia <- gadm_adm1_sel %>% dplyr::filter(GID_0 %in% LATAM_adm0_bolivia$GID_0) %>% mutate(ID = 1:nrow(.))
LATAM_adm1_colombia <- gadm_adm1_sel %>% dplyr::filter(GID_0 %in% LATAM_adm0_colombia$GID_0) %>% mutate(ID = 1:nrow(.))
LATAM_adm1_guyana <- gadm_adm1_sel %>% dplyr::filter(GID_0 %in% LATAM_adm0_guyana$GID_0) %>% mutate(ID = 1:nrow(.))
LATAM_adm1_salvador <- gadm_adm1_sel %>% dplyr::filter(GID_0 %in% LATAM_adm0_salvador$GID_0) %>% mutate(ID = 1:nrow(.))

# Saves each region separatedly for level 1
st_write(LATAM_adm1_southern, "regions_adm1/LATAM_adm1_southern.gpkg", delete_layer = TRUE)
st_write(LATAM_adm1_CA, "regions_adm1/LATAM_adm1_CA.gpkg", delete_layer = TRUE)
st_write(LATAM_adm1_espanhola, "regions_adm1/LATAM_adm1_espanhola.gpkg", delete_layer = TRUE)
st_write(LATAM_adm1_jamaica, "regions_adm1/LATAM_adm1_jamaica.gpkg", delete_layer = TRUE)
st_write(LATAM_adm1_western, "regions_adm1/LATAM_adm1_western.gpkg", delete_layer = TRUE)
# st_write(LATAM_adm1_northern, "regions_adm1/LATAM_adm1_northern.gpkg", delete_layer = TRUE)
st_write(LATAM_adm1_brazil, "regions_adm1/LATAM_adm1_brazil.gpkg", delete_layer = TRUE)
st_write(LATAM_adm1_mexico, "regions_adm1/LATAM_adm1_mexico.gpkg", delete_layer = TRUE)
st_write(LATAM_adm1_bolivia, "regions_adm1/LATAM_adm1_bolivia.gpkg", delete_layer = TRUE)
st_write(LATAM_adm1_colombia, "regions_adm1/LATAM_adm1_colombia.gpkg", delete_layer = TRUE)
st_write(LATAM_adm1_guyana, "regions_adm1/LATAM_adm1_guyana.gpkg", delete_layer = TRUE)
st_write(LATAM_adm1_salvador, "regions_adm1/LATAM_adm1_salvador.gpkg", delete_layer = TRUE)

LATAM_adm1_southern %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm1_p/LATAM_adm1_southern_p.gpkg", delete_layer = TRUE)
LATAM_adm1_CA %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm1_p/LATAM_adm1_CA_p.gpkg", delete_layer = TRUE)
LATAM_adm1_espanhola %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm1_p/LATAM_adm1_espanhola_p.gpkg", delete_layer = TRUE)
LATAM_adm1_jamaica %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm1_p/LATAM_adm1_jamaica_p.gpkg", delete_layer = TRUE)
LATAM_adm1_western %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm1_p/LATAM_adm1_western_p.gpkg", delete_layer = TRUE)
# LATAM_adm1_northern %>%
#   st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
#   st_write("regions_adm1_p/LATAM_adm1_northern_p.gpkg", delete_layer = TRUE)
LATAM_adm1_brazil %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm1_p/LATAM_adm1_brazil_p.gpkg", delete_layer = TRUE)
LATAM_adm1_mexico %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm1_p/LATAM_adm1_mexico_p.gpkg", delete_layer = TRUE)
LATAM_adm1_bolivia %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm1_p/LATAM_adm1_bolivia_p.gpkg", delete_layer = TRUE)
LATAM_adm1_colombia %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm1_p/LATAM_adm1_colombia_p.gpkg", delete_layer = TRUE)
LATAM_adm1_guyana %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm1_p/LATAM_adm1_guyana_p.gpkg", delete_layer = TRUE)
LATAM_adm1_salvador %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm1_p/LATAM_adm1_salvador_p.gpkg", delete_layer = TRUE)

# Sub admin_regions #2
LATAM_adm2_southern <- gadm_adm2_sel %>% dplyr::filter(GID_0 %in% LATAM_adm0_southern$GID_0) %>% mutate(ID = 1:nrow(.)) 
LATAM_adm2_CA <- gadm_adm2_sel %>% dplyr::filter(GID_0 %in% LATAM_adm0_CA$GID_0) %>% mutate(ID = 1:nrow(.))
LATAM_adm2_espanhola <- gadm_adm2_sel %>% dplyr::filter(GID_0 %in% LATAM_adm0_espanhola$GID_0) %>% mutate(ID = 1:nrow(.))
# LATAM_adm2_jamaica <- gadm_adm2_sel %>% dplyr::filter(GID_0 %in% LATAM_adm0_jamaica$GID_0) %>% mutate(ID = 1:nrow(.))
LATAM_adm2_jamaica <- LATAM_adm1_jamaica %>% # Saving ADM1 for Jamaica! cause no ADM2 available
  dplyr::mutate(GID_2 = GID_1,
                NAME_2 = NAME_1)
LATAM_adm2_western <- gadm_adm2_sel %>% dplyr::filter(GID_0 %in% LATAM_adm0_western$GID_0) %>% mutate(ID = 1:nrow(.))
# LATAM_adm2_northern <- gadm_adm2_sel %>% dplyr::filter(GID_0 %in% LATAM_adm0_northern$GID_0) %>% mutate(ID = 1:nrow(.))
LATAM_adm2_brazil <- gadm_adm2_sel %>% dplyr::filter(GID_0 %in% LATAM_adm0_brazil$GID_0) %>% mutate(ID = 1:nrow(.))
LATAM_adm2_mexico <- gadm_adm2_sel %>% dplyr::filter(GID_0 %in% LATAM_adm0_mexico$GID_0) %>% mutate(ID = 1:nrow(.))
LATAM_adm2_bolivia <- gadm_adm2_sel %>% dplyr::filter(GID_0 %in% LATAM_adm0_bolivia$GID_0) %>% mutate(ID = 1:nrow(.))
LATAM_adm2_colombia <- gadm_adm2_sel %>% dplyr::filter(GID_0 %in% LATAM_adm0_colombia$GID_0) %>% mutate(ID = 1:nrow(.))
LATAM_adm2_guyana <- gadm_adm2_sel %>% dplyr::filter(GID_0 %in% LATAM_adm0_guyana$GID_0) %>% mutate(ID = 1:nrow(.))
LATAM_adm2_salvador <- gadm_adm2_sel %>% dplyr::filter(GID_0 %in% LATAM_adm0_salvador$GID_0) %>% mutate(ID = 1:nrow(.))

# Saves each region separatedly for level 2
st_write(LATAM_adm2_southern, "regions_adm2/LATAM_adm2_southern.gpkg", delete_layer = TRUE)
st_write(LATAM_adm2_CA, "regions_adm2/LATAM_adm2_CA.gpkg", delete_layer = TRUE)
st_write(LATAM_adm2_espanhola, "regions_adm2/LATAM_adm2_espanhola.gpkg", delete_layer = TRUE)
st_write(LATAM_adm2_jamaica, "regions_adm2/LATAM_adm2_jamaica.gpkg", delete_layer = TRUE) 
st_write(LATAM_adm2_western, "regions_adm2/LATAM_adm2_western.gpkg", delete_layer = TRUE)
# st_write(LATAM_adm2_northern, "regions_adm2/LATAM_adm2_northern.gpkg", delete_layer = TRUE)
st_write(LATAM_adm2_brazil, "regions_adm2/LATAM_adm2_brazil.gpkg", delete_layer = TRUE)
st_write(LATAM_adm2_mexico, "regions_adm2/LATAM_adm2_mexico.gpkg", delete_layer = TRUE)
st_write(LATAM_adm2_bolivia, "regions_adm2/LATAM_adm2_bolivia.gpkg", delete_layer = TRUE)
st_write(LATAM_adm2_colombia, "regions_adm2/LATAM_adm2_colombia.gpkg", delete_layer = TRUE)
st_write(LATAM_adm2_guyana, "regions_adm2/LATAM_adm2_guyana.gpkg", delete_layer = TRUE)
st_write(LATAM_adm2_salvador, "regions_adm2/LATAM_adm2_salvador.gpkg", delete_layer = TRUE)

LATAM_adm2_southern %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm2_p/LATAM_adm2_southern_p.gpkg", delete_layer = TRUE)
LATAM_adm2_CA %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm2_p/LATAM_adm2_CA_p.gpkg", delete_layer = TRUE)
LATAM_adm2_espanhola %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm2_p/LATAM_adm2_espanhola_p.gpkg", delete_layer = TRUE)
LATAM_adm2_jamaica %>% # Saving ADM1 for Jamaica! cause no ADM2 available
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm2_p/LATAM_adm2_jamaica_p.gpkg", delete_layer = TRUE) 
LATAM_adm2_western %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm2_p/LATAM_adm2_western_p.gpkg", delete_layer = TRUE)
# LATAM_adm2_northern %>%
#   st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
#   st_write("regions_adm2_p/LATAM_adm2_northern_p.gpkg", delete_layer = TRUE)
LATAM_adm2_brazil %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm2_p/LATAM_adm2_brazil_p.gpkg", delete_layer = TRUE)
LATAM_adm2_mexico %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm2_p/LATAM_adm2_mexico_p.gpkg", delete_layer = TRUE)
LATAM_adm2_bolivia %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm2_p/LATAM_adm2_bolivia_p.gpkg", delete_layer = TRUE)
LATAM_adm2_colombia %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm2_p/LATAM_adm2_colombia_p.gpkg", delete_layer = TRUE)
LATAM_adm2_guyana %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm2_p/LATAM_adm2_guyana_p.gpkg", delete_layer = TRUE)
LATAM_adm2_salvador %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm2_p/LATAM_adm2_salvador_p.gpkg", delete_layer = TRUE)

# Asia ----

subregionsASIA <- read_excel("subregionsASIA_v5.xlsx")
subregionsASIA
unique(subregionsASIA$Subregion)
unique(subregionsASIA$NAME_0)
unique(ASIA_adm0$NAME_0)

ASIA_adm0_subregions <- ASIA_adm0 %>% 
  right_join(subregionsASIA, by = "GID_0") %>%
  dplyr::select (-NAME_0.y) %>% 
  dplyr::rename(NAME_0 = NAME_0.x) %>%
  filter(!is.na(NAME_0))
ASIA_adm0_subregions %>% st_drop_geometry()
ASIA_adm0 %>% st_drop_geometry()

ASIA_adm0_subregions
unique(subregionsASIA$Subregion)
unique(ASIA_adm0$NAME_0)

# Build regions
ASIA_adm0_central <- ASIA_adm0_subregions %>% dplyr::filter(Subregion %in% "Central Asia") %>% mutate(ID = 1:nrow(.))
ASIA_adm0_central$mofuss_reg <- "ASIA_adm0_central"
ASIA_adm0_india <- ASIA_adm0_subregions %>% dplyr::filter(Subregion %in% "India") %>% mutate(ID = 1:nrow(.))
ASIA_adm0_india$mofuss_reg <- "ASIA_adm0_india"
ASIA_adm0_seasia <- ASIA_adm0_subregions %>% dplyr::filter(Subregion %in% "SEAsia") %>% mutate(ID = 1:nrow(.))
ASIA_adm0_seasia$mofuss_reg <- "ASIA_adm0_seasia"
ASIA_adm0_china <- ASIA_adm0_subregions %>% dplyr::filter(Subregion %in% "China") %>% mutate(ID = 1:nrow(.))
ASIA_adm0_china$mofuss_reg <- "ASIA_adm0_china"
ASIA_adm0_mongolia <- ASIA_adm0_subregions %>% dplyr::filter(Subregion %in% "Mongolia") %>% mutate(ID = 1:nrow(.))
ASIA_adm0_mongolia$mofuss_reg <- "ASIA_adm0_mongolia"
ASIA_adm0_middleeast <- ASIA_adm0_subregions %>% dplyr::filter(Subregion %in% "Middle East") %>% mutate(ID = 1:nrow(.))
ASIA_adm0_middleeast$mofuss_reg <- "ASIA_adm0_middleeast"
ASIA_adm0_srilanka <- ASIA_adm0_subregions %>% dplyr::filter(Subregion %in% "Sri Lanka") %>% mutate(ID = 1:nrow(.))
ASIA_adm0_srilanka$mofuss_reg <- "ASIA_adm0_srilanka"
ASIA_adm0_indonesia <- ASIA_adm0_subregions %>% dplyr::filter(Subregion %in% "Indonesia") %>% mutate(ID = 1:nrow(.))
ASIA_adm0_indonesia$mofuss_reg <- "ASIA_adm0_indonesia"
ASIA_adm0_malaysia <- ASIA_adm0_subregions %>% dplyr::filter(Subregion %in% "Malaysia") %>% mutate(ID = 1:nrow(.))
ASIA_adm0_malaysia$mofuss_reg <- "ASIA_adm0_malaysia"
ASIA_adm0_philippines <- ASIA_adm0_subregions %>% dplyr::filter(Subregion %in% "Philippines") %>% mutate(ID = 1:nrow(.))
ASIA_adm0_philippines$mofuss_reg <- "ASIA_adm0_philippines"
ASIA_adm0_timorleste <- ASIA_adm0_subregions %>% dplyr::filter(Subregion %in% "Timor-Leste") %>% mutate(ID = 1:nrow(.))
ASIA_adm0_timorleste$mofuss_reg <- "ASIA_adm0_timorleste"
ASIA_adm0_bangladesh <- ASIA_adm0_subregions %>% dplyr::filter(Subregion %in% "Bangladesh") %>% mutate(ID = 1:nrow(.))
ASIA_adm0_bangladesh$mofuss_reg <- "ASIA_adm0_bangladesh"
ASIA_adm0_bhutan <- ASIA_adm0_subregions %>% dplyr::filter(Subregion %in% "Bhutan") %>% mutate(ID = 1:nrow(.))
ASIA_adm0_bhutan$mofuss_reg <- "ASIA_adm0_bhutan"
ASIA_adm0_nepal <- ASIA_adm0_subregions %>% dplyr::filter(Subregion %in% "Nepal") %>% mutate(ID = 1:nrow(.))
ASIA_adm0_nepal$mofuss_reg <- "ASIA_adm0_nepal"
ASIA_adm0_myanmar <- ASIA_adm0_subregions %>% dplyr::filter(Subregion %in% "Myanmar") %>% mutate(ID = 1:nrow(.))
ASIA_adm0_myanmar$mofuss_reg <- "ASIA_adm0_myanmar"
ASIA_adm0_pakistan <- ASIA_adm0_subregions %>% dplyr::filter(Subregion %in% "Pakistan") %>% mutate(ID = 1:nrow(.))
ASIA_adm0_pakistan$mofuss_reg <- "ASIA_adm0_pakistan"

# Saves each region separatedly for level 0
st_write(ASIA_adm0_central, "regions_adm0/ASIA_adm0_central.gpkg", delete_layer = TRUE)
st_write(ASIA_adm0_india, "regions_adm0/ASIA_adm0_india.gpkg", delete_layer = TRUE)
st_write(ASIA_adm0_seasia, "regions_adm0/ASIA_adm0_seasia.gpkg", delete_layer = TRUE)
st_write(ASIA_adm0_china, "regions_adm0/ASIA_adm0_china.gpkg", delete_layer = TRUE)
st_write(ASIA_adm0_mongolia, "regions_adm0/ASIA_adm0_mongolia.gpkg", delete_layer = TRUE)
st_write(ASIA_adm0_middleeast, "regions_adm0/ASIA_adm0_middleeast.gpkg", delete_layer = TRUE)
st_write(ASIA_adm0_srilanka, "regions_adm0/ASIA_adm0_srilanka.gpkg", delete_layer = TRUE)
st_write(ASIA_adm0_indonesia, "regions_adm0/ASIA_adm0_indonesia.gpkg", delete_layer = TRUE)
st_write(ASIA_adm0_malaysia, "regions_adm0/ASIA_adm0_malaysia.gpkg", delete_layer = TRUE)
st_write(ASIA_adm0_philippines, "regions_adm0/ASIA_adm0_philippines.gpkg", delete_layer = TRUE)
st_write(ASIA_adm0_timorleste, "regions_adm0/ASIA_adm0_timorleste.gpkg", delete_layer = TRUE)
st_write(ASIA_adm0_bangladesh, "regions_adm0/ASIA_adm0_bangladesh.gpkg", delete_layer = TRUE)
st_write(ASIA_adm0_bhutan, "regions_adm0/ASIA_adm0_bhutan.gpkg", delete_layer = TRUE)
st_write(ASIA_adm0_nepal, "regions_adm0/ASIA_adm0_nepal.gpkg", delete_layer = TRUE)
st_write(ASIA_adm0_myanmar, "regions_adm0/ASIA_adm0_myanmar.gpkg", delete_layer = TRUE)
st_write(ASIA_adm0_pakistan, "regions_adm0/ASIA_adm0_pakistan.gpkg", delete_layer = TRUE)

unique(ASIA_adm0_seasia$GID_0)

ASIA_adm0_central %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm0_p/ASIA_adm0_central_p.gpkg", delete_layer = TRUE)
ASIA_adm0_india %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm0_p/ASIA_adm0_india_p.gpkg", delete_layer = TRUE)
ASIA_adm0_seasia %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm0_p/ASIA_adm0_seasia_p.gpkg", delete_layer = TRUE)
ASIA_adm0_china %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm0_p/ASIA_adm0_china_p.gpkg", delete_layer = TRUE)
ASIA_adm0_mongolia %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm0_p/ASIA_adm0_mongolia_p.gpkg", delete_layer = TRUE)
ASIA_adm0_middleeast %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm0_p/ASIA_adm0_middleeast_p.gpkg", delete_layer = TRUE)
ASIA_adm0_srilanka %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm0_p/ASIA_adm0_srilanka_p.gpkg", delete_layer = TRUE)
ASIA_adm0_indonesia %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm0_p/ASIA_adm0_indonesia_p.gpkg", delete_layer = TRUE)
ASIA_adm0_malaysia %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm0_p/ASIA_adm0_malaysia_p.gpkg", delete_layer = TRUE)
ASIA_adm0_philippines %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm0_p/ASIA_adm0_philippines_p.gpkg", delete_layer = TRUE)
ASIA_adm0_timorleste %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm0_p/ASIA_adm0_timorleste_p.gpkg", delete_layer = TRUE)
ASIA_adm0_bangladesh %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm0_p/ASIA_adm0_bangladesh_p.gpkg", delete_layer = TRUE)
ASIA_adm0_bhutan %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm0_p/ASIA_adm0_bhutan_p.gpkg", delete_layer = TRUE)
ASIA_adm0_nepal %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm0_p/ASIA_adm0_nepal_p.gpkg", delete_layer = TRUE)
ASIA_adm0_myanmar %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm0_p/ASIA_adm0_myanmar_p.gpkg", delete_layer = TRUE)
ASIA_adm0_pakistan %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm0_p/ASIA_adm0_pakistan_p.gpkg", delete_layer = TRUE)

# # Sub admin_regions #1
ASIA_adm1_central <- gadm_adm1_sel %>% dplyr::filter(GID_0 %in% ASIA_adm0_central$GID_0) %>% mutate(ID = 1:nrow(.))
ASIA_adm1_india <- gadm_adm1_sel %>% dplyr::filter(GID_0 %in% ASIA_adm0_india$GID_0) %>% mutate(ID = 1:nrow(.))
ASIA_adm1_seasia <- gadm_adm1_sel %>% dplyr::filter(GID_0 %in% ASIA_adm0_seasia$GID_0) %>% mutate(ID = 1:nrow(.))
ASIA_adm1_china <- gadm_adm1_sel %>% dplyr::filter(GID_0 %in% ASIA_adm0_china$GID_0) %>% mutate(ID = 1:nrow(.))
ASIA_adm1_mongolia <- gadm_adm1_sel %>% dplyr::filter(GID_0 %in% ASIA_adm0_mongolia$GID_0) %>% mutate(ID = 1:nrow(.))
ASIA_adm1_middleeast <- gadm_adm1_sel %>% dplyr::filter(GID_0 %in% ASIA_adm0_middleeast$GID_0) %>% mutate(ID = 1:nrow(.))
ASIA_adm1_srilanka <- gadm_adm1_sel %>% dplyr::filter(GID_0 %in% ASIA_adm0_srilanka$GID_0) %>% mutate(ID = 1:nrow(.))
ASIA_adm1_indonesia <- gadm_adm1_sel %>% dplyr::filter(GID_0 %in% ASIA_adm0_indonesia$GID_0) %>% mutate(ID = 1:nrow(.))
ASIA_adm1_malaysia <- gadm_adm1_sel %>% dplyr::filter(GID_0 %in% ASIA_adm0_malaysia$GID_0) %>% mutate(ID = 1:nrow(.))
ASIA_adm1_philippines <- gadm_adm1_sel %>% dplyr::filter(GID_0 %in% ASIA_adm0_philippines$GID_0) %>% mutate(ID = 1:nrow(.))
ASIA_adm1_timorleste <- gadm_adm1_sel %>% dplyr::filter(GID_0 %in% ASIA_adm0_timorleste$GID_0) %>% mutate(ID = 1:nrow(.))
ASIA_adm1_bangladesh <- gadm_adm1_sel %>% dplyr::filter(GID_0 %in% ASIA_adm0_bangladesh$GID_0) %>% mutate(ID = 1:nrow(.))
ASIA_adm1_bhutan <- gadm_adm1_sel %>% dplyr::filter(GID_0 %in% ASIA_adm0_bhutan$GID_0) %>% mutate(ID = 1:nrow(.))
ASIA_adm1_nepal <- gadm_adm1_sel %>% dplyr::filter(GID_0 %in% ASIA_adm0_nepal$GID_0) %>% mutate(ID = 1:nrow(.))
ASIA_adm1_myanmar <- gadm_adm1_sel %>% dplyr::filter(GID_0 %in% ASIA_adm0_myanmar$GID_0) %>% mutate(ID = 1:nrow(.))
ASIA_adm1_pakistan <- gadm_adm1_sel %>% dplyr::filter(GID_0 %in% ASIA_adm0_pakistan$GID_0) %>% mutate(ID = 1:nrow(.))

# Saves each region separatedly for level 1
st_write(ASIA_adm1_central, "regions_adm1/ASIA_adm1_central.gpkg", delete_layer = TRUE)
st_write(ASIA_adm1_india, "regions_adm1/ASIA_adm1_india.gpkg", delete_layer = TRUE)
st_write(ASIA_adm1_seasia, "regions_adm1/ASIA_adm1_seasia.gpkg", delete_layer = TRUE)
st_write(ASIA_adm1_china, "regions_adm1/ASIA_adm1_china.gpkg", delete_layer = TRUE)
st_write(ASIA_adm1_mongolia, "regions_adm1/ASIA_adm1_mongolia.gpkg", delete_layer = TRUE)
st_write(ASIA_adm1_middleeast, "regions_adm1/ASIA_adm1_middleeast.gpkg", delete_layer = TRUE)
st_write(ASIA_adm1_srilanka, "regions_adm1/ASIA_adm1_srilanka.gpkg", delete_layer = TRUE)
st_write(ASIA_adm1_indonesia, "regions_adm1/ASIA_adm1_indonesia.gpkg", delete_layer = TRUE)
st_write(ASIA_adm1_malaysia, "regions_adm1/ASIA_adm1_malaysia.gpkg", delete_layer = TRUE)
st_write(ASIA_adm1_philippines, "regions_adm1/ASIA_adm1_philippines.gpkg", delete_layer = TRUE)
st_write(ASIA_adm1_timorleste, "regions_adm1/ASIA_adm1_timorleste.gpkg", delete_layer = TRUE)
st_write(ASIA_adm1_bangladesh, "regions_adm1/ASIA_adm1_bangladesh.gpkg", delete_layer = TRUE)
st_write(ASIA_adm1_bhutan, "regions_adm1/ASIA_adm1_bhutan.gpkg", delete_layer = TRUE)
st_write(ASIA_adm1_nepal, "regions_adm1/ASIA_adm1_nepal.gpkg", delete_layer = TRUE)
st_write(ASIA_adm1_myanmar, "regions_adm1/ASIA_adm1_myanmar.gpkg", delete_layer = TRUE)
st_write(ASIA_adm1_pakistan, "regions_adm1/ASIA_adm1_pakistan.gpkg", delete_layer = TRUE)

ASIA_adm1_central %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm1_p/ASIA_adm1_central_p.gpkg", delete_layer = TRUE)
ASIA_adm1_india %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm1_p/ASIA_adm1_india_p.gpkg", delete_layer = TRUE)
ASIA_adm1_seasia %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm1_p/ASIA_adm1_seasia_p.gpkg", delete_layer = TRUE)
ASIA_adm1_china %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm1_p/ASIA_adm1_china_p.gpkg", delete_layer = TRUE)
ASIA_adm1_mongolia %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm1_p/ASIA_adm1_mongolia_p.gpkg", delete_layer = TRUE)
ASIA_adm1_middleeast %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm1_p/ASIA_adm1_middleeast_p.gpkg", delete_layer = TRUE)
ASIA_adm1_srilanka %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm1_p/ASIA_adm1_srilanka_p.gpkg", delete_layer = TRUE)
ASIA_adm1_indonesia %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm1_p/ASIA_adm1_indonesia_p.gpkg", delete_layer = TRUE)
ASIA_adm1_malaysia %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm1_p/ASIA_adm1_malaysia_p.gpkg", delete_layer = TRUE)
ASIA_adm1_philippines %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm1_p/ASIA_adm1_philippines_p.gpkg", delete_layer = TRUE)
ASIA_adm1_timorleste %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm1_p/ASIA_adm1_timorleste_p.gpkg", delete_layer = TRUE)
ASIA_adm1_bangladesh %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm1_p/ASIA_adm1_bangladesh_p.gpkg", delete_layer = TRUE)
ASIA_adm1_bhutan %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm1_p/ASIA_adm1_bhutan_p.gpkg", delete_layer = TRUE)
ASIA_adm1_nepal %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm1_p/ASIA_adm1_nepal_p.gpkg", delete_layer = TRUE)
ASIA_adm1_myanmar %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm1_p/ASIA_adm1_myanmar_p.gpkg", delete_layer = TRUE)
ASIA_adm1_pakistan %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm1_p/ASIA_adm1_pakistan_p.gpkg", delete_layer = TRUE)

# # Sub admin_regions #2
ASIA_adm2_central <- gadm_adm2_sel %>% dplyr::filter(GID_0 %in% ASIA_adm0_central$GID_0) %>% mutate(ID = 1:nrow(.))
ASIA_adm2_india <- gadm_adm2_sel %>% dplyr::filter(GID_0 %in% ASIA_adm0_india$GID_0) %>% mutate(ID = 1:nrow(.))
ASIA_adm2_seasia <- gadm_adm2_sel %>% dplyr::filter(GID_0 %in% ASIA_adm0_seasia$GID_0) %>% mutate(ID = 1:nrow(.))
ASIA_adm2_china <- gadm_adm2_sel %>% dplyr::filter(GID_0 %in% ASIA_adm0_china$GID_0) %>% mutate(ID = 1:nrow(.))
ASIA_adm2_mongolia <- gadm_adm2_sel %>% dplyr::filter(GID_0 %in% ASIA_adm0_mongolia$GID_0) %>% mutate(ID = 1:nrow(.))
ASIA_adm2_middleeast <- gadm_adm2_sel %>% dplyr::filter(GID_0 %in% ASIA_adm0_middleeast$GID_0) %>% mutate(ID = 1:nrow(.))
ASIA_adm2_srilanka <- gadm_adm2_sel %>% dplyr::filter(GID_0 %in% ASIA_adm0_srilanka$GID_0) %>% mutate(ID = 1:nrow(.))
ASIA_adm2_indonesia <- gadm_adm2_sel %>% dplyr::filter(GID_0 %in% ASIA_adm0_indonesia$GID_0) %>% mutate(ID = 1:nrow(.))
ASIA_adm2_malaysia <- gadm_adm2_sel %>% dplyr::filter(GID_0 %in% ASIA_adm0_malaysia$GID_0) %>% mutate(ID = 1:nrow(.))
ASIA_adm2_philippines <- gadm_adm2_sel %>% dplyr::filter(GID_0 %in% ASIA_adm0_philippines$GID_0) %>% mutate(ID = 1:nrow(.))
ASIA_adm2_timorleste <- gadm_adm2_sel %>% dplyr::filter(GID_0 %in% ASIA_adm0_timorleste$GID_0) %>% mutate(ID = 1:nrow(.))
ASIA_adm2_bangladesh <- gadm_adm2_sel %>% dplyr::filter(GID_0 %in% ASIA_adm0_bangladesh$GID_0) %>% mutate(ID = 1:nrow(.))
ASIA_adm2_bhutan <- gadm_adm2_sel %>% dplyr::filter(GID_0 %in% ASIA_adm0_bhutan$GID_0) %>% mutate(ID = 1:nrow(.))
ASIA_adm2_nepal <- gadm_adm2_sel %>% dplyr::filter(GID_0 %in% ASIA_adm0_nepal$GID_0) %>% mutate(ID = 1:nrow(.))
ASIA_adm2_myanmar <- gadm_adm2_sel %>% dplyr::filter(GID_0 %in% ASIA_adm0_myanmar$GID_0) %>% mutate(ID = 1:nrow(.))
ASIA_adm2_pakistan <- gadm_adm2_sel %>% dplyr::filter(GID_0 %in% ASIA_adm0_pakistan$GID_0) %>% mutate(ID = 1:nrow(.))

# Saves each region separatedly for level 2
st_write(ASIA_adm2_central, "regions_adm2/ASIA_adm2_central.gpkg", delete_layer = TRUE)
st_write(ASIA_adm2_india, "regions_adm2/ASIA_adm2_india.gpkg", delete_layer = TRUE)
st_write(ASIA_adm2_seasia, "regions_adm2/ASIA_adm2_seasia.gpkg", delete_layer = TRUE)
st_write(ASIA_adm2_china, "regions_adm2/ASIA_adm2_china.gpkg", delete_layer = TRUE)
st_write(ASIA_adm2_mongolia, "regions_adm2/ASIA_adm2_mongolia.gpkg", delete_layer = TRUE)
st_write(ASIA_adm2_middleeast, "regions_adm2/ASIA_adm2_middleeast.gpkg", delete_layer = TRUE)
st_write(ASIA_adm2_srilanka, "regions_adm2/ASIA_adm2_srilanka.gpkg", delete_layer = TRUE)
st_write(ASIA_adm2_indonesia, "regions_adm2/ASIA_adm2_indonesia.gpkg", delete_layer = TRUE)
st_write(ASIA_adm2_malaysia, "regions_adm2/ASIA_adm2_malaysia.gpkg", delete_layer = TRUE)
st_write(ASIA_adm2_philippines, "regions_adm2/ASIA_adm2_philippines.gpkg", delete_layer = TRUE)
st_write(ASIA_adm2_timorleste, "regions_adm2/ASIA_adm2_timorleste.gpkg", delete_layer = TRUE)
st_write(ASIA_adm2_bangladesh, "regions_adm2/ASIA_adm2_bangladesh.gpkg", delete_layer = TRUE)
st_write(ASIA_adm2_bhutan, "regions_adm2/ASIA_adm2_bhutan.gpkg", delete_layer = TRUE)
st_write(ASIA_adm2_nepal, "regions_adm2/ASIA_adm2_nepal.gpkg", delete_layer = TRUE)
st_write(ASIA_adm2_myanmar, "regions_adm2/ASIA_adm2_myanmar.gpkg", delete_layer = TRUE)
st_write(ASIA_adm2_pakistan, "regions_adm2/ASIA_adm2_pakistan.gpkg", delete_layer = TRUE)

ASIA_adm2_central %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm2_p/ASIA_adm2_central_p.gpkg", delete_layer = TRUE)
ASIA_adm2_india %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm2_p/ASIA_adm2_india_p.gpkg", delete_layer = TRUE)
ASIA_adm2_seasia %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm2_p/ASIA_adm2_seasia_p.gpkg", delete_layer = TRUE)
ASIA_adm2_china %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm2_p/ASIA_adm2_china_p.gpkg", delete_layer = TRUE)
ASIA_adm2_mongolia %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm2_p/ASIA_adm2_mongolia_p.gpkg", delete_layer = TRUE)
ASIA_adm2_middleeast %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm2_p/ASIA_adm2_middleeast_p.gpkg", delete_layer = TRUE)
ASIA_adm2_srilanka %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm2_p/ASIA_adm2_srilanka_p.gpkg", delete_layer = TRUE)
ASIA_adm2_indonesia %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm2_p/ASIA_adm2_indonesia_p.gpkg", delete_layer = TRUE)
ASIA_adm2_malaysia %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm2_p/ASIA_adm2_malaysia_p.gpkg", delete_layer = TRUE)
ASIA_adm2_philippines %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm2_p/ASIA_adm2_philippines_p.gpkg", delete_layer = TRUE)
ASIA_adm2_timorleste %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm2_p/ASIA_adm2_timorleste_p.gpkg", delete_layer = TRUE)
ASIA_adm2_bangladesh %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm2_p/ASIA_adm2_bangladesh_p.gpkg", delete_layer = TRUE)
ASIA_adm2_bhutan %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm2_p/ASIA_adm2_bhutan_p.gpkg", delete_layer = TRUE)
ASIA_adm2_nepal %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm2_p/ASIA_adm2_nepal_p.gpkg", delete_layer = TRUE)
ASIA_adm2_myanmar %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm2_p/ASIA_adm2_myanmar_p.gpkg", delete_layer = TRUE)
ASIA_adm2_pakistan %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm2_p/ASIA_adm2_pakistan_p.gpkg", delete_layer = TRUE)

# Oceania----

subregionsOCEANIA <- read_excel("subregionsOCEANIA.xlsx")
subregionsOCEANIA
unique(subregionsOCEANIA$Subregion)
unique(subregionsOCEANIA$NAME_0)
unique(OCEANIA_adm0$NAME_0)

OCEANIA_adm0_subregions <- OCEANIA_adm0 %>% 
  right_join(subregionsOCEANIA, by = "GID_0") %>%
  dplyr::select (-NAME_0.y) %>% 
  dplyr::rename(NAME_0 = NAME_0.x) %>%
  filter(!is.na(NAME_0))
OCEANIA_adm0_subregions %>% st_drop_geometry()
OCEANIA_adm0 %>% st_drop_geometry()

OCEANIA_adm0_subregions
unique(subregionsOCEANIA$Subregion)
unique(OCEANIA_adm0$NAME_0)

# Build regions
OCEANIA_adm0_papuanewguinea <- OCEANIA_adm0_subregions %>% dplyr::filter(Subregion %in% "Papua New Guinea") %>% mutate(ID = 1:nrow(.))
OCEANIA_adm0_papuanewguinea$mofuss_reg <- "OCEANIA_adm0_papuanewguinea"

# Saves each region separatedly for level 0
st_write(OCEANIA_adm0_papuanewguinea, "regions_adm0/OCEANIA_adm0_papuanewguinea.gpkg", delete_layer = TRUE)

OCEANIA_adm0_papuanewguinea %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm0_p/OCEANIA_adm0_papuanewguinea_p.gpkg", delete_layer = TRUE)

# # Sub admin_regions #1
OCEANIA_adm1_papuanewguinea <- gadm_adm1_sel %>% dplyr::filter(GID_0 %in% OCEANIA_adm0_papuanewguinea$GID_0) %>% mutate(ID = 1:nrow(.))

# Saves each region separatedly for level 1
st_write(OCEANIA_adm1_papuanewguinea, "regions_adm1/OCEANIA_adm1_papuanewguinea.gpkg", delete_layer = TRUE)

OCEANIA_adm1_papuanewguinea %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm1_p/OCEANIA_adm1_papuanewguinea_p.gpkg", delete_layer = TRUE)

# # Sub admin_regions #2
OCEANIA_adm2_papuanewguinea <- gadm_adm2_sel %>% dplyr::filter(GID_0 %in% OCEANIA_adm0_papuanewguinea$GID_0) %>% mutate(ID = 1:nrow(.))

# Saves each region separatedly for level 2
st_write(OCEANIA_adm2_papuanewguinea, "regions_adm2/OCEANIA_adm2_papuanewguinea.gpkg", delete_layer = TRUE)

OCEANIA_adm2_papuanewguinea %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm2_p/OCEANIA_adm2_papuanewguinea_p.gpkg", delete_layer = TRUE)

# North Africa ----
subregionsNorAfr <- read_excel("subregionsNorAfri_v3.xlsx")
subregionsNorAfr
unique(subregionsNorAfr$Subregion)
unique(NorAfr_adm0$NAME_0)

NorAfr_adm0_subregions <- NorAfr_adm0 %>% 
  right_join(subregionsNorAfr, by = "GID_0") %>%
  dplyr::select (-NAME_0.y) %>% 
  dplyr::rename(NAME_0 = NAME_0.x) %>%
  filter(!is.na(NAME_0))
NorAfr_adm0_subregions %>% st_drop_geometry()
NorAfr_adm0 %>% st_drop_geometry()

# Build regions
NorAfr_adm0_west <- NorAfr_adm0_subregions %>% dplyr::filter(Subregion %in% "Northern Africa") %>% mutate(ID = 1:nrow(.))
NorAfr_adm0_west$mofuss_reg <- "NorAfr_adm0_west"

# Saves each region separatedly for level 0
st_write(NorAfr_adm0_west, "regions_adm0/NorAfr_adm0_west.gpkg", delete_layer = TRUE)

NorAfr_adm0_west %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm0_p/NorAfr_adm0_west_p.gpkg", delete_layer = TRUE)

# Sub admin_regions #1
NorAfr_adm1_west <- gadm_adm1_sel %>% dplyr::filter(GID_0 %in% NorAfr_adm0_west$GID_0) %>% mutate(ID = 1:nrow(.))

st_write(NorAfr_adm1_west, "regions_adm1/NorAfr_adm1_west.gpkg", delete_layer = TRUE)

NorAfr_adm1_west %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm1_p/NorAfr_adm1_west_p.gpkg", delete_layer = TRUE)

# Sub admin_regions #2
NorAfr_adm2_west <- gadm_adm2_sel %>% dplyr::filter(GID_0 %in% NorAfr_adm0_west$GID_0) %>% mutate(ID = 1:nrow(.))

st_write(NorAfr_adm2_west, "regions_adm2/NorAfr_adm2_west.gpkg", delete_layer = TRUE)

NorAfr_adm2_west %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm2_p/NorAfr_adm2_west_p.gpkg", delete_layer = TRUE)

# Load all vector layers for each admin_regions level ----
regions.list0 <- list.files(path = "regions_adm0",
                            pattern="*.gpkg", full.names = TRUE)
mofuss_regions0 <- lapply(regions.list0, st_read)
mofuss_regions0 <- do.call("rbind", mofuss_regions0)
# mofuss_regions0 <- mofuss_regions00 %>%
#   select (ID)
st_write(mofuss_regions0, "regions_adm0/mofuss_regions0.gpkg", delete_layer = TRUE)
st_write(mofuss_regions0, "regions_adm0/mofuss_regions0.shp", delete_layer = TRUE)

unique(mofuss_regions0$GID_0)

# Load all vector layers for each admin_regions level
regions.list1 <- list.files(path = "regions_adm1",
                           pattern="*.gpkg", full.names = TRUE)
mofuss_regions1 <- lapply(regions.list1, st_read)
mofuss_regions1 <- do.call("rbind", mofuss_regions1)
mofuss_regions1 %>%
  dplyr::select (ID)

st_write(mofuss_regions1, "regions_adm1/mofuss_regions1.gpkg", delete_layer = TRUE)
st_write(mofuss_regions1, "regions_adm1/mofuss_regions1.shp", delete_layer = TRUE)

# Load all vector layers for each admin_regions level
regions.list2 <- list.files(path = "regions_adm2",
                            pattern="*.gpkg", full.names = TRUE)
mofuss_regions2 <- lapply(regions.list2, st_read)
mofuss_regions2 <- do.call("rbind", mofuss_regions2)
st_write(mofuss_regions2, "regions_adm2/mofuss_regions2.gpkg", delete_layer = TRUE)
st_write(mofuss_regions2, "regions_adm2/mofuss_regions2.shp", delete_layer = TRUE)


# Simplifed polygons for the websever ----

if (run_ms == "Yes"){

library(sf)
library(tictoc)
library(mapview)
library(tidyverse)
library(readxl)
library(hacksaw)
library(rmapshaper)
  
# Using the system mapshaper - COULD NOT INSTALL IT IN WIN10!!!
check_sys_mapshaper()
system("mapshaper --version")
# https://github.com/mbloch/mapshaper
# https://github.com/ateucher/rmapshaper#using-the-system-mapshaper
# https://nodejs.org/en

adm0_regtest <- st_read("regions_adm0/mofuss_regions0.gpkg")
adm1_regtest <- st_read("regions_adm1/mofuss_regions1.gpkg")
adm2_regtest <- st_read("regions_adm2/mofuss_regions2.gpkg")


# https://epsg.io/paste0(proj_authority,":",epsg_pcs) World Mercator
# https://epsg.io/1078-method Equal Earth
# EqualEarth? <- sf::st_transform(adm0_regtest, "+proj=eqearth")

mofuss_regions0_simp <- adm0_regtest %>%
  # mutate(fNRB = sample(0:100, n(), replace = TRUE),
  #        fNRBsd = sample(0:50, n(), replace = TRUE),
  #        NRB = sample(0:3300, n(), replace = TRUE),
  #        NRBsd = sample(0:1100, n(), replace = TRUE)) %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  ms_simplify(sys = TRUE) %>%
  st_transform(epsg_gcs)
st_write(mofuss_regions0_simp, "regions_adm0/mofuss_regions0_simp.shp", delete_layer = TRUE)

mofuss_regions1_simp <- adm1_regtest %>%
  # mutate(fNRB = sample(0:100, n(), replace = TRUE),
  #        fNRBsd = sample(0:50, n(), replace = TRUE),
  #        NRB = sample(0:330, n(), replace = TRUE),
  #        NRBsd = sample(0:110, n(), replace = TRUE)) %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  ms_simplify(sys = TRUE, sys_mem = 16) %>%
  st_transform(epsg_gcs)
st_write(mofuss_regions1_simp, "regions_adm1/mofuss_regions1_simp.shp", delete_layer = TRUE)

# with st_simplify & ms_simplify
mofuss_regions2_simp <- adm2_regtest %>%
  # mutate(fNRB = sample(0:100, n(), replace = TRUE),
  #        fNRBsd = sample(0:50, n(), replace = TRUE),
  #        NRB = sample(0:33, n(), replace = TRUE),
  #        NRBsd = sample(0:11, n(), replace = TRUE)) %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_simplify(preserveTopology = TRUE, dTolerance = 50) %>%
  # ms_simplify()
  ms_simplify(keep = 0.6, keep_shapes = FALSE, sys = TRUE, sys_mem = 24) %>%
  st_transform(epsg_gcs)
st_write(mofuss_regions2_simp, "regions_adm2/mofuss_regions2_simp.shp", delete_layer = TRUE)

}

# Project mofuss layers

st_read("regions_adm0/mofuss_regions0.gpkg") %>% 
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm0_p/mofuss_regions0_p.gpkg", delete_layer = TRUE)

st_read("regions_adm1/mofuss_regions1.gpkg") %>% 
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm1_p/mofuss_regions1_p.gpkg", delete_layer = TRUE)

st_read("regions_adm2/mofuss_regions2.gpkg") %>% 
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm2_p/mofuss_regions2_p.gpkg", delete_layer = TRUE)

# Update demand_in folder with latest mofuss_regions0.gpkg
file.copy(from="regions_adm0/mofuss_regions0.gpkg",
          to=paste0(demanddir,"/demand_in"),
          overwrite = TRUE)


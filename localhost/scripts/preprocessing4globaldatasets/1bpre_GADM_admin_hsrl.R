# MoFuSS
# Windows version
# Date: Mar 2023
# This script load the GADM datasets for levels 0, 1 and 2 and selects those countries
# with complete info in both the WHO and HRSL population maps 

# Run ms_simplfy?
run_ms = "Yes" # "Yes"

# Load packages ####
library(sf)
library(tictoc)
#library(mapview)
library(tidyverse)
library(readxl)
library(hacksaw)
library(rmapshaper)
library(svDialogs)

setwd(countrydir)
getwd()

# Read parameters table, checking if its delimiter is comma or semicolon ####
read_csv("LULCC/DownloadedDatasets/SourceDataGlobal/parameters.csv") %>% 
  {if(is.null(.$ParCHR[1])) read_csv2("LULCC/SourceData/parameters.csv") else .} -> country_parameters
# Append "+"
country_parameters$ParCHR[1] <- str_c("+", country_parameters$ParCHR[1])
country_parameters$ParCHR[2] <- str_c("+", country_parameters$ParCHR[2])
print(tibble::as_tibble(country_parameters), n=100) ####

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

tic()

# # setwd("G:/Mi unidad/webpages/2023_MoFuSSGlobal_Datasets/")
# choose_directory1 = function(caption = "Choose the directory where demand_in files are") {
#   if(.Platform$OS.type == "unix")  {
#     setwd(tk_choose.dir("/home/mofuss/Documents", caption = caption))
#   } else {
#     setwd(choose.dir("/home/mofuss/Documents", caption = caption)) # Elegir bien esta carpeta de inicio
#   }
# }
# choose_directory1()
# demanddir <- getwd()
# 
# # Set directory
# choose_directory1 = function(caption = "Choose the directory where admin_regions files are") {
#   if(.Platform$OS.type == "unix")  {
#     setwd(tk_choose.dir("/home/mofuss/Documents", caption = caption))
#   } else {
#     setwd(choose.dir("/home/mofuss/Documents", caption = caption)) # Elegir bien esta carpeta de inicio
#   }
# }
# choose_directory1()
# admindir <- getwd()

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


tic()
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
                          "Z08" = "IND",  # Pa-li-chia-ssu https://geographic.org/geographic_names/name.php?uni=-2924886&fid=2690&c=india
                          "Z09" = "IND"), # Sang, looks like India admin_regions, in Himachal Pradesh. 
           NAME_0 = recode(NAME_0,
                           "Jammu and Kashmir" = "India",
                           "Shaksgam Valley" = "China",
                           "Aksai Chin" = "China",
                           "Kaurik" = "India",
                           "Lapthal" = "India",
                           "Azad Kashmir" = "Pakistan",
                           "Arunachal Pradesh" = "India",
                           "Pa-li-chia-ssu" = "India",
                           "Sang" = "India"))
  return(adm_lyr_recoded)
}

### DEBUGGER CHUNK

# gadm_410original <- st_read("gadm_410original.gpkg")
# st_write(gadm_410original, "debug_BLZ/gadm_410original.shp", delete_layer = TRUE)

###

# gadm_adm0_sel <- st_read("gadm_410adm0D.shp") %>% 
gadm_adm0_sel <- st_read("gadm_410adm0.gpkg") %>% 
  dplyr::select(GID_0,NAME_0,VARNAME_0) %>%
  recodedisputed()
# st_write(gadm_adm0_sel, "debug_BLZ/gadm_adm0_sel.shp", delete_layer = TRUE)

# gadm_adm1_sel <- st_read("gadm_410adm1D.shp") %>% 
gadm_adm1_sel <- st_read("gadm_410adm1.gpkg") %>%
  dplyr::select(GID_0,NAME_0,VARNAME_0,GID_1,NAME_1) %>%
  recodedisputed()
# st_write(gadm_adm1_sel, "debug_BLZ/gadm_adm1_sel.shp", delete_layer = TRUE)

# gadm_adm2_sel <- st_read("gadm_410adm2D.shp") %>% 
gadm_adm2_sel <- st_read("gadm_410adm2.gpkg") %>%
  dplyr::select(GID_0,NAME_0,VARNAME_0,GID_1,NAME_1,GID_2,NAME_2) %>%
  recodedisputed()
# st_write(gadm_adm2_sel, "debug_BLZ/gadm_adm2_sel.shp", delete_layer = TRUE)

# # Remove water bodies
# wb <- st_read("HydroLAKES_polys_v10_clip.shp")
# sf_use_s2(FALSE)
# wbs <- wb %>% 
#   ms_simplify %>%
#   dplyr::mutate(iddissolve = 1) %>%
#   group_by(iddissolve) %>%
#   summarize() 
# 
# gadm_adm0_sel <- st_difference(gadm_adm0_selwb, wbs) %>%
#   dplyr::select(-iddissolve)
# gadm_adm1_sel <- st_difference(gadm_adm1_selwb, wbs) %>%
#   dplyr::select(-iddissolve)
# gadm_adm2_sel <- st_difference(gadm_adm2_selwb, wbs) %>%
#   dplyr::select(-iddissolve)

# Countries from GADM, before WHO or Facebook filter
gadm_adm0_sel_db <- gadm_adm0_sel %>% st_drop_geometry()
gadm_adm0_sel_db

# Read the WHO dataset i order to filter the GADM dataset by WHO availability (regions)
setwd(demanddir)
whodb <- read_excel("demand_in/A_LMIC_Estimates_2050_popmedian.xlsx")
head(whodb)
whodb_sel_u <- whodb %>% dplyr::select(iso3,country,region) %>% 
  unique()
whodb_sel_u
regions_u <- unique(whodb_sel_u$region)
regions_u
region1 <- whodb_sel_u %>% dplyr::filter(region %in% regions_u[1]) %>%
filter(iso3 != "IRN", # Iran missing from Facebook Population maps
       iso3 != "AFG") # Afganistan missing from Facebook Population maps
region2 <- whodb_sel_u %>% dplyr::filter(region %in% regions_u[2])
# regionNorAfr <- whodb_sel_u %>% dplyr::filter(region %in% regions_u[3]) %>%
# filter(iso3 != "AZE", # Middle East or near...
#        iso3 != "GEO", # Middle East or near...
#        iso3 != "IRQ", # Middle East or near...
#        iso3 != "JOR", # Middle East or near...
#        iso3 != "SYR", # Middle East or near...
#        iso3 != "TUR", # Middle East or near...
#        iso3 != "YEM", # Middle East or near...
#        iso3 != "ARM", # Middle East or near...
#        iso3 != "SDN") # Sudan missing from Facebook Population maps
regionSSA <- whodb_sel_u %>% dplyr::filter(region %in% regions_u[4]) %>%
  filter(iso3 != "CPV", # Cabo Verde missing from Facebook Population maps
         iso3 != "SSD", # South Sudan missing from Facebook Population maps
         iso3 != "SOM", # Somalia missing from Facebook Population maps
         iso3 != "SDN", # Sudan is missing from Facebook Population maps but it also belongs to North Africa, it was moved here
         iso3 != "LSO") # Lesotho doesn't have subnational admin and was buggy - fixed?
regionLATAM <- whodb_sel_u %>% dplyr::filter(region %in% regions_u[5]) %>%
filter(iso3 != "GUY", # Guyana missing from Facebook Population maps
       iso3 != "JAM", # Jamaica missing from Facebook Population mapss
       iso3 != "BLZ") # Belize doesn't have subnational admin and was buggy - fixed?
region6 <- whodb_sel_u %>% dplyr::filter(region %in% regions_u[6]) %>%
  filter(iso3 != "MMR", # Myanmar missing from Facebook Population maps
         iso3 != "PRK", # North Korea missing from Facebook Population maps
         iso3 != "AFG", # Afganistan missing from Facebook Population maps
         iso3 != "CHN") # China missing from Facebook Population maps
region7 <- whodb_sel_u %>% dplyr::filter(region %in% regions_u[7])

R1_adm0 <- gadm_adm0_sel %>% dplyr::filter(GID_0 %in% region1$iso3)
R2_adm0 <- gadm_adm0_sel %>% dplyr::filter(GID_0 %in% region2$iso3)
# NorAfr_adm0 <- gadm_adm0_sel %>% dplyr::filter(GID_0 %in% regionNorAfr$iso3)
SSA_adm0 <- gadm_adm0_sel %>% dplyr::filter(GID_0 %in% regionSSA$iso3)
LATAM_adm0 <- gadm_adm0_sel %>% dplyr::filter(GID_0 %in% regionLATAM$iso3)
R6_adm0 <- gadm_adm0_sel %>% dplyr::filter(GID_0 %in% region6$iso3)
R7_adm0 <- gadm_adm0_sel %>% dplyr::filter(GID_0 %in% region7$iso3)

ASIA_adm0 <- rbind(R1_adm0,R6_adm0) %>% 
  ms_dissolve(field = "GID_0", copy_fields = c("NAME_0")) %>% 
  dplyr::select(GID_0, NAME_0)
ASIA_adm0_db <- ASIA_adm0 %>% st_drop_geometry()

# Sub-Saharan Africa ----
setwd(admindir)
subregionsSSA <- read_excel("subregionsSSA.xlsx")
subregionsSSA

SSA_adm0_subregions <- SSA_adm0 %>% 
  right_join(subregionsSSA, by = "GID_0") %>%
  dplyr::select (-NAME_0.y, -VARNAME_0) %>% 
  dplyr::rename(NAME_0 = NAME_0.x) %>%
  filter(!is.na(NAME_0))
SSA_adm0_subregions
SSA_adm0_subregions %>% st_drop_geometry()

# Build regions
SSA_adm0_eastern <- SSA_adm0_subregions %>% dplyr::filter(Subregion %in% "Eastern Africa") %>% mutate(ID = 1:nrow(.))
SSA_adm0_eastern$mofuss_reg <- "SSA_adm0_eastern"
SSA_adm0_central <- SSA_adm0_subregions %>% dplyr::filter(Subregion %in% "Middle Africa") %>% mutate(ID = 1:nrow(.))
SSA_adm0_central$mofuss_reg <- "SSA_adm0_central"
SSA_adm0_southern <- SSA_adm0_subregions %>% dplyr::filter(Subregion %in% "Southern Africa") %>% mutate(ID = 1:nrow(.))
SSA_adm0_southern$mofuss_reg <- "SSA_adm0_southern"
SSA_adm0_western <- SSA_adm0_subregions %>% dplyr::filter(Subregion %in% "Western Africa") %>% mutate(ID = 1:nrow(.))
SSA_adm0_western$mofuss_reg <- "SSA_adm0_western"

# Saves each region separatedly for level 0
st_write(SSA_adm0_eastern, "regions_adm0/SSA_adm0_eastern.gpkg", delete_layer = TRUE)
st_write(SSA_adm0_central, "regions_adm0/SSA_adm0_central.gpkg", delete_layer = TRUE)
st_write(SSA_adm0_southern, "regions_adm0/SSA_adm0_southern.gpkg", delete_layer = TRUE)
st_write(SSA_adm0_western, "regions_adm0/SSA_adm0_western.gpkg", delete_layer = TRUE)

SSA_adm0_eastern %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm0_p/SSA_adm0_eastern_p.gpkg", delete_layer = TRUE)
SSA_adm0_central %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm0_p/SSA_adm0_central_p.gpkg", delete_layer = TRUE)
SSA_adm0_southern %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm0_p/SSA_adm0_southern_p.gpkg", delete_layer = TRUE)
SSA_adm0_western %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm0_p/SSA_adm0_western_p.gpkg", delete_layer = TRUE)

# Sub admin_regions #1
SSA_adm1_eastern <- gadm_adm1_sel %>% dplyr::filter(GID_0 %in% SSA_adm0_eastern$GID_0) %>% mutate(ID = 1:nrow(.))
SSA_adm1_central <- gadm_adm1_sel %>% dplyr::filter(GID_0 %in% SSA_adm0_central$GID_0) %>% mutate(ID = 1:nrow(.))
SSA_adm1_southern <- gadm_adm1_sel %>% dplyr::filter(GID_0 %in% SSA_adm0_southern$GID_0) %>% mutate(ID = 1:nrow(.))
SSA_adm1_western <- gadm_adm1_sel %>% dplyr::filter(GID_0 %in% SSA_adm0_western$GID_0) %>% mutate(ID = 1:nrow(.))

st_write(SSA_adm1_eastern, "regions_adm1/SSA_adm1_eastern.gpkg", delete_layer = TRUE)
st_write(SSA_adm1_central, "regions_adm1/SSA_adm1_central.gpkg", delete_layer = TRUE)
st_write(SSA_adm1_southern, "regions_adm1/SSA_adm1_southern.gpkg", delete_layer = TRUE)
st_write(SSA_adm1_western, "regions_adm1/SSA_adm1_western.gpkg", delete_layer = TRUE)

SSA_adm1_eastern %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm1_p/SSA_adm1_eastern_p.gpkg", delete_layer = TRUE)
SSA_adm1_central %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm1_p/SSA_adm1_central_p.gpkg", delete_layer = TRUE)
SSA_adm1_southern %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm1_p/SSA_adm1_southern_p.gpkg", delete_layer = TRUE)
SSA_adm1_western %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm1_p/SSA_adm1_western_p.gpkg", delete_layer = TRUE)

# Sub admin_regions #2
SSA_adm2_eastern <- gadm_adm2_sel %>% dplyr::filter(GID_0 %in% SSA_adm0_eastern$GID_0) %>% mutate(ID = 1:nrow(.))
SSA_adm2_central <- gadm_adm2_sel %>% dplyr::filter(GID_0 %in% SSA_adm0_central$GID_0) %>% mutate(ID = 1:nrow(.))
SSA_adm2_southern <- gadm_adm2_sel %>% dplyr::filter(GID_0 %in% SSA_adm0_southern$GID_0) %>% mutate(ID = 1:nrow(.))
SSA_adm2_western <- gadm_adm2_sel %>% dplyr::filter(GID_0 %in% SSA_adm0_western$GID_0) %>% mutate(ID = 1:nrow(.))

st_write(SSA_adm2_eastern, "regions_adm2/SSA_adm2_eastern.gpkg", delete_layer = TRUE)
st_write(SSA_adm2_central, "regions_adm2/SSA_adm2_central.gpkg", delete_layer = TRUE)
st_write(SSA_adm2_southern, "regions_adm2/SSA_adm2_southern.gpkg", delete_layer = TRUE)
st_write(SSA_adm2_western, "regions_adm2/SSA_adm2_western.gpkg", delete_layer = TRUE)

SSA_adm2_eastern %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm2_p/SSA_adm2_eastern_p.gpkg", delete_layer = TRUE)
SSA_adm2_central %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm2_p/SSA_adm2_central_p.gpkg", delete_layer = TRUE)
SSA_adm2_southern %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm2_p/SSA_adm2_southern_p.gpkg", delete_layer = TRUE)
SSA_adm2_western %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm2_p/SSA_adm2_western_p.gpkg", delete_layer = TRUE)


# Americas ----
subregionsLATAM <- read_excel("subregionsLATAM.xlsx")
subregionsLATAM
unique(subregionsLATAM$Subregion)

LATAM_adm0_subregions <- LATAM_adm0 %>% 
  right_join(subregionsLATAM, by = "GID_0") %>%
  dplyr::select (-NAME_0.y, -VARNAME_0) %>% 
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
LATAM_adm0_western <- LATAM_adm0_subregions %>% dplyr::filter(Subregion %in% "Western LATAM") %>% mutate(ID = 1:nrow(.))
LATAM_adm0_western$mofuss_reg <- "LATAM_adm0_western"
LATAM_adm0_northern <- LATAM_adm0_subregions %>% dplyr::filter(Subregion %in% "Northern LATAM") %>% mutate(ID = 1:nrow(.))
LATAM_adm0_northern$mofuss_reg <- "LATAM_adm0_northern"
LATAM_adm0_brazil <- LATAM_adm0_subregions %>% dplyr::filter(Subregion %in% "Brazil") %>% mutate(ID = 1:nrow(.))
LATAM_adm0_brazil$mofuss_reg <- "LATAM_adm0_brazil"
LATAM_adm0_mexico <- LATAM_adm0_subregions %>% dplyr::filter(Subregion %in% "Mexico") %>% mutate(ID = 1:nrow(.))
LATAM_adm0_mexico$mofuss_reg <- "LATAM_adm0_mexico"

# Saves each region separatedly for level 0
st_write(LATAM_adm0_southern, "regions_adm0/LATAM_adm0_southern.gpkg", delete_layer = TRUE)
st_write(LATAM_adm0_CA, "regions_adm0/LATAM_adm0_CA.gpkg", delete_layer = TRUE)
st_write(LATAM_adm0_western, "regions_adm0/LATAM_adm0_western.gpkg", delete_layer = TRUE)
st_write(LATAM_adm0_northern, "regions_adm0/LATAM_adm0_northern.gpkg", delete_layer = TRUE)
st_write(LATAM_adm0_brazil, "regions_adm0/LATAM_adm0_brazil.gpkg", delete_layer = TRUE)
st_write(LATAM_adm0_mexico, "regions_adm0/LATAM_adm0_mexico.gpkg", delete_layer = TRUE)

LATAM_adm0_southern %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm0_p/LATAM_adm0_southern_p.gpkg", delete_layer = TRUE)
LATAM_adm0_CA %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm0_p/LATAM_adm0_CA_p.gpkg", delete_layer = TRUE)
LATAM_adm0_western %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm0_p/LATAM_adm0_western_p.gpkg", delete_layer = TRUE)
LATAM_adm0_northern %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm0_p/LATAM_adm0_northern_p.gpkg", delete_layer = TRUE)
LATAM_adm0_brazil %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm0_p/LATAM_adm0_brazil_p.gpkg", delete_layer = TRUE)
LATAM_adm0_mexico %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm0_p/LATAM_adm0_mexico_p.gpkg", delete_layer = TRUE)

  
# # Sub admin_regions #1
LATAM_adm1_southern <- gadm_adm1_sel %>% dplyr::filter(GID_0 %in% LATAM_adm0_southern$GID_0) %>% mutate(ID = 1:nrow(.))
LATAM_adm1_CA <- gadm_adm1_sel %>% dplyr::filter(GID_0 %in% LATAM_adm0_CA$GID_0) %>% mutate(ID = 1:nrow(.))
LATAM_adm1_western <- gadm_adm1_sel %>% dplyr::filter(GID_0 %in% LATAM_adm0_western$GID_0) %>% mutate(ID = 1:nrow(.))
LATAM_adm1_northern <- gadm_adm1_sel %>% dplyr::filter(GID_0 %in% LATAM_adm0_northern$GID_0) %>% mutate(ID = 1:nrow(.))
LATAM_adm1_brazil <- gadm_adm1_sel %>% dplyr::filter(GID_0 %in% LATAM_adm0_brazil$GID_0) %>% mutate(ID = 1:nrow(.))
LATAM_adm1_mexico <- gadm_adm1_sel %>% dplyr::filter(GID_0 %in% LATAM_adm0_mexico$GID_0) %>% mutate(ID = 1:nrow(.))

# Saves each region separatedly for level 1
st_write(LATAM_adm1_southern, "regions_adm1/LATAM_adm1_southern.gpkg", delete_layer = TRUE)
st_write(LATAM_adm1_CA, "regions_adm1/LATAM_adm1_CA.gpkg", delete_layer = TRUE)
st_write(LATAM_adm1_western, "regions_adm1/LATAM_adm1_western.gpkg", delete_layer = TRUE)
st_write(LATAM_adm1_northern, "regions_adm1/LATAM_adm1_northern.gpkg", delete_layer = TRUE)
st_write(LATAM_adm1_brazil, "regions_adm1/LATAM_adm1_brazil.gpkg", delete_layer = TRUE)
st_write(LATAM_adm1_mexico, "regions_adm1/LATAM_adm1_mexico.gpkg", delete_layer = TRUE)

LATAM_adm1_southern %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm1_p/LATAM_adm1_southern_p.gpkg", delete_layer = TRUE)
LATAM_adm1_CA %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm1_p/LATAM_adm1_CA_p.gpkg", delete_layer = TRUE)
LATAM_adm1_western %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm1_p/LATAM_adm1_western_p.gpkg", delete_layer = TRUE)
LATAM_adm1_northern %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm1_p/LATAM_adm1_northern_p.gpkg", delete_layer = TRUE)
LATAM_adm1_brazil %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm1_p/LATAM_adm1_brazil_p.gpkg", delete_layer = TRUE)
LATAM_adm1_mexico %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm1_p/LATAM_adm1_mexico_p.gpkg", delete_layer = TRUE)

# Sub admin_regions #2
LATAM_adm2_southern <- gadm_adm2_sel %>% dplyr::filter(GID_0 %in% LATAM_adm0_southern$GID_0) %>% mutate(ID = 1:nrow(.)) 
LATAM_adm2_CA <- gadm_adm2_sel %>% dplyr::filter(GID_0 %in% LATAM_adm0_CA$GID_0) %>% mutate(ID = 1:nrow(.))
LATAM_adm2_western <- gadm_adm2_sel %>% dplyr::filter(GID_0 %in% LATAM_adm0_western$GID_0) %>% mutate(ID = 1:nrow(.))
LATAM_adm2_northern <- gadm_adm2_sel %>% dplyr::filter(GID_0 %in% LATAM_adm0_northern$GID_0) %>% mutate(ID = 1:nrow(.))
LATAM_adm2_brazil <- gadm_adm2_sel %>% dplyr::filter(GID_0 %in% LATAM_adm0_brazil$GID_0) %>% mutate(ID = 1:nrow(.))
LATAM_adm2_mexico <- gadm_adm2_sel %>% dplyr::filter(GID_0 %in% LATAM_adm0_mexico$GID_0) %>% mutate(ID = 1:nrow(.))

# Saves each region separatedly for level 2
st_write(LATAM_adm2_southern, "regions_adm2/LATAM_adm2_southern.gpkg", delete_layer = TRUE)
st_write(LATAM_adm2_CA, "regions_adm2/LATAM_adm2_CA.gpkg", delete_layer = TRUE)
st_write(LATAM_adm2_western, "regions_adm2/LATAM_adm2_western.gpkg", delete_layer = TRUE)
st_write(LATAM_adm2_northern, "regions_adm2/LATAM_adm2_northern.gpkg", delete_layer = TRUE)
st_write(LATAM_adm2_brazil, "regions_adm2/LATAM_adm2_brazil.gpkg", delete_layer = TRUE)
st_write(LATAM_adm2_mexico, "regions_adm2/LATAM_adm2_mexico.gpkg", delete_layer = TRUE)

LATAM_adm2_southern %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm2_p/LATAM_adm2_southern_p.gpkg", delete_layer = TRUE)
LATAM_adm2_CA %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm2_p/LATAM_adm2_CA_p.gpkg", delete_layer = TRUE)
LATAM_adm2_western %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm2_p/LATAM_adm2_western_p.gpkg", delete_layer = TRUE)
LATAM_adm2_northern %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm2_p/LATAM_adm2_northern_p.gpkg", delete_layer = TRUE)
LATAM_adm2_brazil %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm2_p/LATAM_adm2_brazil_p.gpkg", delete_layer = TRUE)
LATAM_adm2_mexico %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm2_p/LATAM_adm2_mexico_p.gpkg", delete_layer = TRUE)

# Asia ----
subregionsASIA <- read_excel("subregionsASIA.xlsx")
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
ASIA_adm0_indian <- ASIA_adm0_subregions %>% dplyr::filter(Subregion %in% "Indian subcontinent") %>% mutate(ID = 1:nrow(.))
ASIA_adm0_indian$mofuss_reg <- "ASIA_adm0_indian"
ASIA_adm0_seasia <- ASIA_adm0_subregions %>% dplyr::filter(Subregion %in% "SEAsia") %>% mutate(ID = 1:nrow(.))
ASIA_adm0_seasia$mofuss_reg <- "ASIA_adm0_seasia"
# ASIA_adm0_china <- ASIA_adm0_subregions %>% dplyr::filter(Subregion %in% "China") %>% mutate(ID = 1:nrow(.))
# ASIA_adm0_china$mofuss_reg <- "ASIA_adm0_china"
ASIA_adm0_mongolia <- ASIA_adm0_subregions %>% dplyr::filter(Subregion %in% "Mongolia") %>% mutate(ID = 1:nrow(.))
ASIA_adm0_mongolia$mofuss_reg <- "ASIA_adm0_mongolia"

# Saves each region separatedly for level 0
st_write(ASIA_adm0_central, "regions_adm0/ASIA_adm0_central.gpkg", delete_layer = TRUE)
st_write(ASIA_adm0_indian, "regions_adm0/ASIA_adm0_indian.gpkg", delete_layer = TRUE)
st_write(ASIA_adm0_seasia, "regions_adm0/ASIA_adm0_seasia.gpkg", delete_layer = TRUE)
# st_write(ASIA_adm0_china, "regions_adm0/ASIA_adm0_china.gpkg", delete_layer = TRUE)
st_write(ASIA_adm0_mongolia, "regions_adm0/ASIA_adm0_mongolia.gpkg", delete_layer = TRUE)

unique(ASIA_adm0_seasia$GID_0)

ASIA_adm0_central %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm0_p/ASIA_adm0_central_p.gpkg", delete_layer = TRUE)
ASIA_adm0_indian %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm0_p/ASIA_adm0_indian_p.gpkg", delete_layer = TRUE)
ASIA_adm0_seasia %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm0_p/ASIA_adm0_seasia_p.gpkg", delete_layer = TRUE)
# ASIA_adm0_china %>%
#   st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
#   st_write("regions_adm0_p/ASIA_adm0_china_p.gpkg", delete_layer = TRUE)
ASIA_adm0_mongolia %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm0_p/ASIA_adm0_mongolia_p.gpkg", delete_layer = TRUE)


# # Sub admin_regions #1
ASIA_adm1_central <- gadm_adm1_sel %>% dplyr::filter(GID_0 %in% ASIA_adm0_central$GID_0) %>% mutate(ID = 1:nrow(.))
ASIA_adm1_indian <- gadm_adm1_sel %>% dplyr::filter(GID_0 %in% ASIA_adm0_indian$GID_0) %>% mutate(ID = 1:nrow(.))
ASIA_adm1_seasia <- gadm_adm1_sel %>% dplyr::filter(GID_0 %in% ASIA_adm0_seasia$GID_0) %>% mutate(ID = 1:nrow(.))
# ASIA_adm1_china <- gadm_adm1_sel %>% dplyr::filter(GID_0 %in% ASIA_adm0_china$GID_0) %>% mutate(ID = 1:nrow(.))
ASIA_adm1_mongolia <- gadm_adm1_sel %>% dplyr::filter(GID_0 %in% ASIA_adm0_mongolia$GID_0) %>% mutate(ID = 1:nrow(.))

# Saves each region separatedly for level 1
st_write(ASIA_adm1_central, "regions_adm1/ASIA_adm1_central.gpkg", delete_layer = TRUE)
st_write(ASIA_adm1_indian, "regions_adm1/ASIA_adm1_indian.gpkg", delete_layer = TRUE)
st_write(ASIA_adm1_seasia, "regions_adm1/ASIA_adm1_seasia.gpkg", delete_layer = TRUE)
# st_write(ASIA_adm1_china, "regions_adm1/ASIA_adm1_china.gpkg", delete_layer = TRUE)
st_write(ASIA_adm1_mongolia, "regions_adm1/ASIA_adm1_mongolia.gpkg", delete_layer = TRUE)

ASIA_adm1_central %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm1_p/ASIA_adm1_central_p.gpkg", delete_layer = TRUE)
ASIA_adm1_indian %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm1_p/ASIA_adm1_indian_p.gpkg", delete_layer = TRUE)
ASIA_adm1_seasia %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm1_p/ASIA_adm1_seasia_p.gpkg", delete_layer = TRUE)
# ASIA_adm1_china %>%
#   st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
#   st_write("regions_adm1_p/ASIA_adm1_china_p.gpkg", delete_layer = TRUE)
ASIA_adm1_mongolia %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm1_p/ASIA_adm1_mongolia_p.gpkg", delete_layer = TRUE)

# # Sub admin_regions #2
ASIA_adm2_central <- gadm_adm2_sel %>% dplyr::filter(GID_0 %in% ASIA_adm0_central$GID_0) %>% mutate(ID = 1:nrow(.))
ASIA_adm2_indian <- gadm_adm2_sel %>% dplyr::filter(GID_0 %in% ASIA_adm0_indian$GID_0) %>% mutate(ID = 1:nrow(.))
ASIA_adm2_seasia <- gadm_adm2_sel %>% dplyr::filter(GID_0 %in% ASIA_adm0_seasia$GID_0) %>% mutate(ID = 1:nrow(.))
# ASIA_adm2_china <- gadm_adm2_sel %>% dplyr::filter(GID_0 %in% ASIA_adm0_china$GID_0) %>% mutate(ID = 1:nrow(.))
ASIA_adm2_mongolia <- gadm_adm2_sel %>% dplyr::filter(GID_0 %in% ASIA_adm0_mongolia$GID_0) %>% mutate(ID = 1:nrow(.))

# Saves each region separatedly for level 2
st_write(ASIA_adm2_central, "regions_adm2/ASIA_adm2_central.gpkg", delete_layer = TRUE)
st_write(ASIA_adm2_indian, "regions_adm2/ASIA_adm2_indian.gpkg", delete_layer = TRUE)
st_write(ASIA_adm2_seasia, "regions_adm2/ASIA_adm2_seasia.gpkg", delete_layer = TRUE)
# st_write(ASIA_adm2_china, "regions_adm2/ASIA_adm2_china.gpkg", delete_layer = TRUE)
st_write(ASIA_adm2_mongolia, "regions_adm2/ASIA_adm2_mongolia.gpkg", delete_layer = TRUE)

ASIA_adm2_central %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm2_p/ASIA_adm2_central_p.gpkg", delete_layer = TRUE)
ASIA_adm2_indian %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm2_p/ASIA_adm2_indian_p.gpkg", delete_layer = TRUE)
ASIA_adm2_seasia %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm2_p/ASIA_adm2_seasia_p.gpkg", delete_layer = TRUE)
# ASIA_adm2_china %>%
#   st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
#   st_write("regions_adm2_p/ASIA_adm2_china_p.gpkg", delete_layer = TRUE)
ASIA_adm2_mongolia %>%
  st_transform(paste0(proj_authority,":",epsg_pcs)) %>%
  st_write("regions_adm2_p/ASIA_adm2_mongolia_p.gpkg", delete_layer = TRUE)

# North Africa ----
# Libya missing from WHO and Sudan missing from Facebook Pop maps
# # Saves each region separatedly for level 0
# st_write(NorAfr_adm0, "regions_adm0/NorAfr_adm0.gpkg", delete_layer = TRUE)
# 
# # Sub admin_regions #1
# NorAfr_adm1 <- gadm_adm1_sel %>% dplyr::filter(GID_0 %in% NorAfr_adm0$GID_0)
# st_write(NorAfr_adm1, "regions_adm1/NorAfr_adm1.gpkg", delete_layer = TRUE)
# 
# # Sub admin_regions #2
# NorAfr_adm2 <- gadm_adm2_sel %>% dplyr::filter(GID_0 %in% NorAfr_adm0$GID_0)
# st_write(NorAfr_adm2, "regions_adm2/NorAfr_adm2.gpkg", delete_layer = TRUE)

# Load all vector layers for each admin_regions level
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


# https://epsg.io/3395 World Mercator
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

toc()

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


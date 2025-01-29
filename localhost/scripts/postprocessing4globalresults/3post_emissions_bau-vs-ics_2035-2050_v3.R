# Copyright 2025 Stockholm Environment Institute ----

# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# http://www.apache.org/licenses/LICENSE-2.0
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# MoFuSS version 4
# Windows version
# Date: Jan 2025
# Description: Each run will produce all BaU and ICS for a given time period and region defined interactively.

# 2dolist ----
# 1.- Error: [*] cannot compute with nothing FIRST FIX - puede ser la causa que falten paises
# 2.- Rob: FUELS CONS + EF + FNRB(BAU)
# 3.- CONNECT TO fNRB and NRB RESULTS FROM MOFUSS END, now uses fNRB 2020-2030 1MC hardwired in Rob's end
# 4.- PROPAGATE UNCERTAINTY ASSUMING NO CORRELATION, FROM (3)
# 5.- Temporal chunk for debugging - wire back to MoFuSS
# 6.- Ideally compare tet a tet with Demand scripts to align homolgous chunks
# 7.- LINEA 229 scenario.list <- c("BaU") BORRAR!!!
# 8.- Check for Linux 
# 9.- Neighboring countries when continent or region is selected, something with the croping vect layer, simplified?

### ----
# # 10.- IMPORTANT! STANDARD ERROR PROPAGATION, START IN LINE 1225
# if (sdm == mean){
#   AvEm20xx_gcs_tpp <- BaU20xxa - ICS20xxa # tpp stands for tonnes per pixel per period
# } else if (sdm == se) {
#   AvEm20xx_gcs_tpp <- BaU20xxa - ICS20xxa # tpp stands for tonnes per pixel per period
# }

# # Summary tables over GCS rasters
# AvEm_gcs_tppr_sum <- as.data.frame(zonal(AvEm_gcs_tppr, admin_r, 'sum')) %>% #CORRECT SUM FOR SE PROPAGATION
# Start in 1359

### ----
# 11.- Integrate mean and SE in the same table


# Internal parameters ----
temdirdefined = 1
string_pattern_yes <- "adm0" #String pattern to be searched when selecting folders for the rasters' geocomputation
string_pattern_no <- "idw" #String pattern to be searched when selecting folders for the rasters' geocomputation

#***#
startfromscratch = 1 # WARNING: Will erase all temporal folders along with any temp datasets - never too bad
eraseallemissions = 0 # WARNING: Will erase all EMISSIONS OUTPUTS FOLDERS - could be bad
#***#

efchratio <- 6 # wood to charcoal yield

mergecountries = 1 
avoidedemissions = 1
zonalstats = 1

optimize = 0 # geoprocessing optimization

# Define all folders based on node ----
# Detect OS and node name
os <- Sys.info()["sysname"]
node_name <- Sys.info()[["nodename"]]
cat(os,node_name)

if (os == "Windows" & node_name == "WINLANASE") {
  #ADD node
  demanddir <- "F:/demand"
  admindir <- "F:/admin_regions"
  emissionsdir <- "F:/emissions"
  rTempdir <- "F:/rTemp"
  
} else if (os == "Windows" & node_name == "ASUSLAP"){
  #ADD node
  demanddir <- "D:/demand"
  admindir <- "D:/admin_regions"
  emissionsdir <- "D:/emissions"
  rTempdir <- "D:/rTemp"
  
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
  
}

# Erase all plots in R Studio
Sys.sleep(2)
for (p in 1:100) {
  if (length(dev.list()!=0)) {
    dev.off()
  }
}
Sys.sleep(3)

# Load packages ----
library(terra)
# terraOptions(steps = 55)
if (temdirdefined == 1) {
  terraOptions(tempdir = rTempdir)
  # List all files and directories inside the folder
  contents <- list.files(rTempdir, full.names = TRUE, recursive = TRUE)
  # Delete the contents but keep the folder
  unlink(contents, recursive = TRUE, force = TRUE)
}
# terraOptions(memfrac=0.9)
# terraOptions(progress=0)
# library(compare)
library(dplyr)
library(fasterize)
library(fs)
library(gdata)
library(hacksaw)
library(mapview)
library(raster)
library(readxl)
library(sf)
library(stringr)
library(svDialogs)
library(tcltk)
library(tibble)
library(tictoc)
library(tidyterra)
library(tidyverse)

# # Define the directory to search for fNRB values
setwd(tk_choose.dir(default = getwd(), caption = "Define the directory to search"))
search_path <- getwd()
# search_path <- "G:/Mi unidad/webpages/2024_MoFuSSGlobal_Datasets/webmofussDS_v2/globalsouth_mofuss_bindingfolder"
# search_path <- "G:/My Drive/webpages/2024_MoFuSSGlobal_Datasets/webmofussDS_v2/globalsouth_mofuss_bindingfolder"
# G:\Mi unidad\webpages\2024_MoFuSSGlobal_Datasets\webmofussDS_v2\globalsouth_mofuss_bindingfolder NRV

# List all directories in the specified path
all_dirs <- dir_ls(search_path, type = "directory")

# Filter directories that match string_pattern_yes and do not match string_pattern_no
adm0_dirs <- all_dirs[grepl(string_pattern_yes, all_dirs) & !grepl(string_pattern_no, all_dirs)]
adm0_dirs

setwd(demanddir)

if (startfromscratch == 1){
  
  if (eraseallemissions == 1){
    setwd(emissionsdir)
    unlink("*/", recursive=TRUE) # WARNING as this erases all final emissions results: added eraseallemissions in parameters
    setwd(demanddir)
  }
  
  unlink("pop_maps_byregionE*/", recursive=TRUE)
  unlink("pop_tempE*/", recursive=TRUE)
  unlink("pop_outE*/", recursive=TRUE)
  unlink("demand_tempE*/", recursive=TRUE)
  unlink("demand_temp_summaryE*/", recursive=TRUE)
  unlink("demand_outE*/", recursive=TRUE)
  unlink("demand_out_summaryE*/", recursive=TRUE)
  unlink("to_idwE*/", recursive=TRUE)
  unlink("emissions_temp*/", recursive=TRUE)
  unlink("emissions_out*/", recursive=TRUE)
  unlink("emissions_out_summaryE*/", recursive=TRUE)
  
}

# Reads WHO dataset
whodb <- read_excel("demand_in/A_LMIC_Estimates_2050_popmedian.xlsx")
# undb <- read_excel("admin_regions/UN_Rural-Urban_Pop_projections_formatted.xlsx") # https://population.un.org/wpp/Download/Standard/Population/
# terra::unique(whodb$fuel)
# terra::unique(whodb$year)
# terra::unique(whodb$iso3)

# Reads a population dataset
pop_db <- c("HSRL","WorldPop")
popversions.input <- dlg_list(as.character(pop_db), 
                              preselect = "WorldPop",
                              multiple = FALSE,
                              title = "Choose your population datset",
                              gui = .GUI
)
pop_ver <- popversions.input$res

if (pop_ver == "HSRL"){
  poprast <- "demand_in/hsrl_global.tif"
  yr <- 2015 # WARNING! this is the base year from the population map
} else if (pop_ver == "WorldPop"){
  poprast <- "demand_in/wp_global1000m_gcs.tif"
  yr <- 2020 # WARNING! this is the base year from the population map
}  else if (pop_ver == "na1"){
  "na1"
}  else if (pop_ver == "na2"){
  "na2"
}  else if (pop_ver == "na3"){
  "na3"
} 

# Get mofuss region for parameters below
mofuss_regions0_gpkg <- vect(st_read("demand_in/mofuss_regions0.gpkg"))
mofuss_regions0 <- as.data.frame(mofuss_regions0_gpkg)

### WORKING SLIME ----

continent.list <- mofuss_regions0 %>%
  dplyr::select(mofuss_reg) %>%
  terra::unique() %>%
  dplyr::mutate(continent = str_extract(mofuss_reg, "^[^_]+")) %>%
  dplyr::pull(continent) %>%
  unique()

regions.list <- mofuss_regions0 %>%
  dplyr::select(mofuss_reg) %>%
  terra::unique()

countries.list <-  mofuss_regions0 %>%
  dplyr::select(NAME_0) %>%
  terra::unique() %>%
  arrange(NAME_0)

# Input region
region.list <- c("Global", "Continental", "Regional", "Country")
region.input <- dlg_list(as.character(region.list), 
                         preselect = "Regional",
                         multiple = FALSE,
                         title = "Choose a region extent",
                         gui = .GUI
)
byregion <- region.input$res

# Select a region
if (byregion == "Global") {
  mofuss_region <- "Global"
  if (!length(mofuss_region)) {
    cat("You cancelled the choice\n")
  } else {
    cat("You selected:\n")
    print(mofuss_region)
  }

} else if (byregion == "Continental") {
  cont.list <- continent.list
  region.input <- dlg_list(as.character(cont.list), 
                           preselect = "AFRICA",
                           multiple = FALSE, 
                           title = "Choose a continent to process",
                           gui = .GUI
  )
  mofuss_region <- region.input$res
  
  if (!length(mofuss_region)) {
    cat("You cancelled the choice\n")
  } else {
    cat("You selected:\n")
    print(region.input$res)
  }
  
} else if (byregion == "Regional") {
  region.input <- dlg_list(as.character(regions.list[ , ]), 
                           preselect = "SSA_adm0_eastern",
                           multiple = FALSE,
                           title = "Choose a region to process",
                           gui = .GUI
  )
  mofuss_region <- region.input$res
  
  if (!length(mofuss_region)) {
    cat("You cancelled the choice\n")
  } else {
    cat("You selected:\n")
    print(region.input$res)
  }
} else if (byregion == "Country") {
  # Select a country
  country.input <- dlg_list(as.character(countries.list[ , ]), 
                            preselect = "Kenya",
                            multiple = FALSE, # Check if multiple countries or values is doable
                            title = "Choose one country to process",
                            gui = .GUI
  )
  mofuss_region <- country.input$res
  
} else {
  print("Error")
}

# Select a time period
period.list <- (c("2020-2035", "2020-2050")) # This period must coincide with the fNRB period and be hardwired from the previous scripts
period.input <- dlg_list(as.character(period.list), 
                         preselect = "2020-2050",
                         multiple = FALSE, 
                         title = "Choose a time period to process",
                         gui = .GUI
)
mofuss_period <- period.input$res

if (mofuss_period == "2020-2050") {
  annos.listX <- 2020:2050 # c(1990:2017,2019:2050) 
  annos.list <- annos.listX[!annos.listX %in% yr] 
} else if (mofuss_period == "2020-2035") {
  annos.listX <- 2020:2035 # c(1990:2017,2019:2050) 
  annos.list <- annos.listX[!annos.listX %in% yr] 
}
annos <- as.numeric(annos.list)

# annos.listX <- c(2000:2050) # c(1990:2017,2019:2050) 
# annos.list <- annos.listX[!annos.listX %in% yr]
# 
# region.years <- dlg_list(annos.list, 
#                          preselect = 2022,
#                          multiple =TRUE,
#                          title = "Choose one or many years to process",
#                          gui = .GUI
# )
# annos <- as.numeric(region.years$res)

# BaU or ICS ----
scenario.list <- c("BaU", "ICS")
for (scex in scenario.list) { # Start scex loop ----
  # scex = "BaU"
  # scex = "ICS"
  
  unlink(paste0("pop_maps_byregionE_",scex,"/"), recursive=TRUE)
  unlink(paste0("pop_tempE_",scex,"/"), recursive=TRUE)
  unlink(paste0("pop_outE_",scex,"/"), recursive=TRUE)
  unlink(paste0("demand_tempE_",scex,"/"), recursive=TRUE)
  unlink(paste0("demand_temp_summaryE_",scex,"/"), recursive=TRUE)
  unlink(paste0("demand_outE_",scex,"/"), recursive=TRUE)
  unlink(paste0("demand_out_summaryE_",scex,"/"), recursive=TRUE)
  unlink(paste0("to_idwE_",scex,"/"), recursive=TRUE)
  unlink(paste0("emissions_temp_",scex,"/"), recursive=TRUE)
  unlink(paste0("emissions_out_",scex,"/"), recursive=TRUE)
  unlink(paste0("emissions_out_summaryE_",scex,"/"), recursive=TRUE)
  
  if (!dir.exists(paste0("pop_maps_byregionE_",scex))) {dir.create(paste0("pop_maps_byregionE_",scex))}
  if (!dir.exists(paste0("pop_tempE_",scex))) {dir.create(paste0("pop_tempE_",scex))} 
  # if (!dir.exists(paste0("pop_outE_",scex))) {dir.create(paste0("pop_outE_",scex))} 
  if (!dir.exists(paste0("demand_tempE_",scex))) {dir.create(paste0("demand_tempE_",scex))}
  if (!dir.exists(paste0("demand_temp_summaryE_",scex))) {dir.create(paste0("demand_temp_summaryE_",scex))}
  # if (!dir.exists(paste0("demand_outE_",scex))) {dir.create(paste0("demand_outE_",scex))}
  # if (!dir.exists(paste0("to_idwE_",scex))) {dir.create(paste0("to_idwE_",scex))} 
  if (!dir.exists(paste0("emissions_temp_",scex))) {dir.create(paste0("emissions_temp_",scex))}
  if (!dir.exists(paste0("emissions_out_",scex))) {dir.create(paste0("emissions_out_",scex))}
  
  if (scex == "BaU") {
    wfdb <- read_csv("demand_in/cons_fuels_years.csv") # UPDATE WITH NEW DATASET WITH THREE OPTIONS
    head(wfdb)
    # terra::unique(wfdb$fuel)
    demand_col <- "fuel_tons3" #"fuel_tons1" #"fuel_tons3"
  } else if (scex == "ICS") {
    wfdb <- read_csv("demand_in/cons_fuels_years_proj.csv") # UPDATE WITH NEW DATASET WITH THREE OPTIONS
    head(wfdb)
    # terra::unique(wfdb$fuel)
    demand_col <- "fuel_tons3" #Its one to simulate reduction from original one "fuel_tons1" #"fuel_tons3"
  }
  print(scex) # save as text to recover later down the river
  
  # i="KEN" 
  totpopWHO <- whodb %>% 
    # dplyr::filter(grepl(i, iso3)) %>% # searchs for the pattern, anywhere within the string
    dplyr::filter(grepl('Total', fuel)) %>%
    dplyr::filter(grepl(yr, year)) %>%
    dplyr::filter(!grepl('Over', area)) %>%
    group_by(iso3) %>% 
    summarise(sum_pop=sum(pop)*1000,
              .groups = 'drop')
  
  # Reads furb in 2018 from WHO dataset
  whodb_join <- whodb %>%
    dplyr::select(iso3, country) %>%
    terra::unique()
  
  furb_who <- whodb %>% #algo pasa con algunas librerias rio abajo que rompen esta parte si ya estan cargadas
    # dplyr::filter(grepl(i, iso3)) %>% # searchs for the pattern, anywhere within the string
    dplyr::filter(grepl('Total', fuel)) %>%
    dplyr::filter(grepl(yr, year)) %>%
    dplyr::filter(grepl('Urban', area)) %>%
    dplyr::group_by(iso3) %>% 
    summarise(urb_pop=sum(pop)*1000,
              .groups = 'drop') %>%
    dplyr::left_join(totpopWHO, ., by="iso3") %>% 
    dplyr::mutate(furb = round(urb_pop/sum_pop,2)) %>%
    dplyr::left_join(whodb_join, ., by = "iso3") %>%
    dplyr::select(iso3, country, furb) %>%
    dplyr::rename(GID_0 = iso3,
                  NAME_0 = country)
  
  # La suma total en la resolución nativa: HRSL: 56,861,964.76; GPW: 44,953,897.44.
  pop0 <- rast(poprast) #in base year, 2018
  
  if (byregion == "Global"){
    print("***NOW RUNNING GLOBAL DEMAND SCENARIOS***")
    adm0_reg <- mofuss_regions0_gpkg
    pop0_K <- crop(pop0, ext(adm0_reg) + .01)
    pop0_reg <- mask(pop0_K, adm0_reg)
    plot(pop0_reg,main=c("Region to be processed"))
    lines(adm0_reg)
    Sys.sleep(10)
    
  } else if (byregion == "Continental"){
    print("***NOW RUNNING CONTINENTAL DEMAND SCENARIOS***")
    adm0_reg <- mofuss_regions0_gpkg %>%
      dplyr::filter(grepl(paste0(mofuss_region,"*"), mofuss_reg))
    pop0_K <- crop(pop0, ext(adm0_reg) + .01)
    pop0_reg <- mask(pop0_K, adm0_reg)
    plot(pop0_reg,main=c("Region to be processed"))
    lines(adm0_reg)
    Sys.sleep(10)
    
  } else if (byregion == "Regional"){
    print("***NOW RUNNING REGION DEMAND SCENARIOS***")
    adm0_reg <- mofuss_regions0_gpkg %>% 
      dplyr::filter(grepl(mofuss_region, mofuss_reg))
    pop0_K <- crop(pop0, ext(adm0_reg) + .01)
    pop0_reg <- mask(pop0_K, adm0_reg)
    plot(pop0_reg,main=c("Region to be processed"))
    lines(adm0_reg)
    Sys.sleep(10)

  } else if (byregion == "Country"){
    print("***NOW RUNNING COUNTRY DEMAND SCENARIOS***")
    adm0_reg <- mofuss_regions0_gpkg %>% 
      dplyr::filter(NAME_0 == mofuss_region) # Check if multiple countries or values is doable
    pop0_K <- crop(pop0, ext(adm0_reg) + .01)
    pop0_reg <- mask(pop0_K, adm0_reg)
    plot(pop0_reg, main=paste0("You selected ",mofuss_region))
    lines(adm0_reg)
    Sys.sleep(10)
    
  }
  
  # To cross-check with excel demand dataset: cons_fuels_yearsxlsx
  unique(adm0_reg$GID_0)
  
  # Ask Diana to translate this ugly loop into a split-apply-compile process with apply (mapply?)
  # Will be much faster but less easy to debug
  
  if (annos[1] == yr+1) {
    firstyr <- yr
  } else if (annos[1]-1 > yr) {
    firstyr <- annos[1]
  } else if (annos[1] < yr) {
    firstyr <- annos[1]
  } else {
    print("Error?")
  }
  firstyr
  lastyr <- annos[length(annos)]
  lastyr
  firstyr > yr
  
  for (i in adm0_reg$GID_0) { ## start 1st subscex loop ----
    # i = "PNG"
    print(i)
    
    # Search for 'i' in all directories within adm0_dirs
    found_in_csv <- FALSE
    fnrb_2020_2030 <- NA
    fnrb_2020_2050 <- NA
    
    for (dir in adm0_dirs) {
      csv_path <- file.path(dir, "OutBaU/webmofuss_results/summary_adm0_fr.csv") # ADD OutICS!! 
      
      if (file.exists(csv_path)) {
        summary_data <- read.csv(csv_path)
        
        # Check if 'i' is in the GID_0 column
        if (i %in% summary_data$GID_0) {
          message(paste("Found GID_0", i, "in", csv_path))
          
          # Retrieve the values from row 1 for 'fNRB_2020_2030_mean' and 'fNRB_2020_2050_mean'
          # fnrb_2020_2030 <- summary_data$fNRB_2020_2030_mean[1] # Calcular fnrb_2020_2035!!!
          # fnrb_2020_2050 <- summary_data$fNRB_2020_2050_mean[1]
          found_in_csv <- TRUE
          break  # Exit the directory loop since 'i' was found
        }
      }
    }
    
    # If 'i' wasn't found in any of the directories, skip to the next country
    if (!found_in_csv) {
      message(paste("No data found for GID_0", i, "in any of the directories - skipping to next country"))
      next
    }
    
    ctry_furb <- furb_who %>%
      dplyr::filter(GID_0 == i) %>%
      pull(furb)
    ctry_name <- furb_who %>%
      dplyr::filter(GID_0 == i) %>%
      pull(NAME_0)
    who_ctry_pop <- totpopWHO %>%
      dplyr::filter(iso3 == i) %>%
      pull(sum_pop)
    ctry_vector <- adm0_reg %>%
      dplyr::filter(GID_0 == i)
    
    pop0_K2 <- crop(pop0_reg, ext(ctry_vector) + .01)
    pop0_ctry_ras <- mask(pop0_K2, ctry_vector)
    
    png(file=paste0("pop_maps_byregionE_",scex,"/",ctry_name,scex,".png"),
        width=600, height=350)
    plot(pop0_ctry_ras, main=ctry_name, xlab = "Long", ylab = "Lat")
    lines(ctry_vector, lwd=0.2)
    Sys.sleep(5)
    dev.off()
    
    totpop <- round(global(pop0_ctry_ras, "sum", na.rm=TRUE),0) %>%
      pull(sum)
    urbpop <- round(totpop * ctry_furb,0) 
    rurpop <- totpop - urbpop
    totpop
    urbpop
    rurpop
    
    pop0_ctry_rasadj <- pop0_ctry_ras*who_ctry_pop/totpop
    totpopadj <- round(global(pop0_ctry_rasadj, "sum", na.rm=TRUE),0) %>%
      pull(sum)
    urbpopadj <- round(totpopadj * ctry_furb,0)
    rurpopadj <- totpopadj - urbpopadj
    totpopadj
    urbpopadj
    rurpopadj
    if (firstyr <= yr) { # Why this if - is not in Demand script?
      terra::writeRaster(pop0_ctry_rasadj, paste0("pop_tempE_",scex,"/",pop_ver,"_",i,"_",yr,"_popadj.tif"), filetype = "GTiff", overwrite = T)  
    }
    
    for (j in annos) { 
      # i="PNG"
      # j=1990
      
      print(j)
      
      totpopWHO_annual <- whodb %>% 
        # dplyr::filter(grepl(i, iso3)) %>% # searchs for the pattern, anywhere within the string
        dplyr::filter(grepl('Total', fuel)) %>% #Porque usar grelp?
        # dplyr::filter(grepl(yr, year)) %>%
        dplyr::filter(!grepl('Over', area)) %>%
        group_by(iso3,year) %>% 
        summarise(sum_pop=sum(pop)*1000,
                  .groups = 'drop')
      
      who_ctry_pop_annual <- totpopWHO_annual %>%
        dplyr::filter(iso3 == i) %>%
        dplyr::filter(year == j) %>%
        pull(sum_pop)
      
      pop0_ctry_rasadj.anno<- pop0_ctry_ras*who_ctry_pop_annual/totpop
      totpopadj.anno <- round(global(pop0_ctry_rasadj.anno, "sum", na.rm=TRUE),0) %>%
        pull(sum)
      urbpopadj.anno <- round(totpopadj.anno * ctry_furb,0)
      rurpopadj.anno <- totpopadj.anno - urbpopadj.anno
      totpopadj.anno
      urbpopadj.anno
      rurpopadj.anno
      terra::writeRaster(pop0_ctry_rasadj.anno, paste0("pop_tempE_",scex,"/HSRL_",i,"_",j,"_popadj.tif"), filetype = "GTiff", overwrite = T)
      
    }
    
    # Saca el umbral de corte urbano/rural para el año base de 2018
    if (pop_ver == "HSRL") {
      vec <- as_tibble(pop0_ctry_rasadj, na.rm = TRUE) %>% 
        arrange(desc(.)) %>%
        pull(GlobalHSRL)
    } else if (pop_ver == "WorldPop") {
      vec <- as_tibble(pop0_ctry_rasadj, na.rm = TRUE) %>% 
        arrange(desc(.)) %>%
        pull(GlobalWorldPop)
    }
    ix <- length(which(cumsum(vec) <= urbpopadj)) 
    vec[ix] # Valor de corte
    
    # filtra por el umbral
    if (pop_ver == "HSRL") {
      urbanpopulation <- pop0_ctry_rasadj %>% 
        filter(GlobalHSRL > vec[ix])
    } else if (pop_ver == "WorldPop") {
      urbanpopulation <- pop0_ctry_rasadj %>% 
        filter(GlobalWorldPop > vec[ix])
    }
    # terra::writeRaster(urbanpopulation, paste0("pop_temp_",scex,"/HSRL_",i,"_",yr,"_urbpop.tif"), filetype = "GTiff", overwrite = TRUE)
    m_urb <- c(-Inf, 0, NA,
               0, Inf, 2)
    rcl_urb <- matrix(m_urb, ncol=3, byrow=TRUE)
    urbanpopulationR <- urbanpopulation %>%
      classify(rcl_urb, include.lowest=TRUE)
    # terra::writeRaster(urbanpopulation, paste0("pop_temp_",scex,"/HSRL_",i,"_",yr,"_urbpopR.tif"), filetype = "GTiff", overwrite = TRUE)
    
    if (pop_ver == "HSRL") {
      ruralpopulation <- pop0_ctry_rasadj %>% 
        filter(GlobalHSRL <= vec[ix])
    } else if (pop_ver == "WorldPop") {
      ruralpopulation <- pop0_ctry_rasadj %>% 
        filter(GlobalWorldPop <= vec[ix])
    }
    # terra::writeRaster(ruralpopulation, paste0("pop_temp_",scex,"/HSRL_",i,"_",yr,"_rurpop.tif"), filetype = "GTiff", overwrite = TRUE)
    m_rur <- c(-Inf, 0, NA,
               0, Inf, 1)
    rcl_rur <- matrix(m_rur, ncol=3, byrow=TRUE)
    ruralpopulationR <- ruralpopulation %>%
      classify(rcl_rur, include.lowest=TRUE)
    # terra::writeRaster(ruralpopulation, paste0("pop_temp_",scex,"/HSRL_",i,"_",yr,"_rurpopR.tif"), filetype = "GTiff", overwrite = TRUE)
    
    rururbpopulationR <- merge(urbanpopulationR, ruralpopulationR)
    if (pop_ver == "HSRL") {
      rururbpopulationR_plot <- rururbpopulationR %>%
        mutate(GlobalHSRL = recode(GlobalHSRL,
                                   "1" = "Rural",
                                   "2" = "Urban"))
    } else if (pop_ver == "WorldPop") {
      rururbpopulationR_plot <- rururbpopulationR %>%
        mutate(GlobalWorldPop = recode(GlobalWorldPop,
                                       "1" = "Rural",
                                       "2" = "Urban"))
    }
    plot(rururbpopulationR_plot, main=ctry_name)
    lines(ctry_vector, lwd=2)
    if (firstyr <= yr) {
      terra::writeRaster(rururbpopulationR, paste0("pop_tempE_",scex,"/",pop_ver,"_",i,"_",yr,"_rururbR.tif"), filetype = "GTiff", overwrite = T)
    }
    # Validation
    urbpopmap <- round(global(urbanpopulation, "sum", na.rm=TRUE),0) %>% 
      pull(sum)
    urbpopmap
    urbpopadj
    round((urbpopmap/totpopadj),2)
    
    rurpopmap <- round(global(ruralpopulation, "sum", na.rm=TRUE),0) %>% 
      pull(sum)
    rurpopmap
    rurpopadj
    round((rurpopmap/totpopadj),2)
    
    ### spread population (whodb) and demand (wfdb) by FUEL TYPE use and urban vs rural ----
    # fuels <- whodb %>%
    #   dplyr::select (fuel) %>%
    #   unique()
    fuelschr <- c("Kerosene",       
                  "Gas",            
                  "Electricity",    
                  "Biomass",        
                  "Charcoal",       
                  "Coal")
    
    for (f in fuelschr) {
      # f = "Biomass"  
      
      fuelpopWHO <- whodb %>% 
        dplyr::filter(grepl(i, iso3)) %>% # searchs for the pattern, anywhere within the string
        dplyr::filter(grepl(f, fuel)) %>%
        dplyr::filter(grepl(yr, year)) %>%
        dplyr::filter(grepl('Rur|Urb', area))
      
      fuelwfdb <- wfdb %>% #We need fuel consumption other than WF! 
        dplyr::filter(grepl(i, iso3)) %>% # searchs for the pattern, anywhere within the string
        dplyr::filter(grepl(f, fuel)) %>%
        dplyr::filter(grepl(yr, year)) %>%
        dplyr::filter(grepl('Rur|Urb', area))
      
      fuelurb <- fuelpopWHO %>% 
        dplyr::filter(grepl('Urb', area)) %>%
        dplyr::select(pop) %>% 
        sum()*1000
      urbpopmap
      urbfuel_Sctry <- urbanpopulation*fuelurb/urbpopmap
      round(global(urbfuel_Sctry, "sum", na.rm=TRUE),0) %>% 
        pull(sum)
      fuelurb_d_tons <- fuelwfdb %>% 
        dplyr::filter(grepl('Urb', area)) %>%
        dplyr::select(all_of(demand_col)) %>% 
        sum()
      urbfuelDem_Sctry <- urbanpopulation*fuelurb_d_tons/urbpopmap
      
      fuelrur <- fuelpopWHO %>% 
        dplyr::filter(grepl('Rur', area)) %>%
        dplyr::select(pop) %>% 
        sum()*1000  
      rurpopmap
      rurfuel_Sctry <- ruralpopulation*fuelrur/rurpopmap
      round(global(rurfuel_Sctry, "sum", na.rm=TRUE),0) %>% 
        pull(sum)
      fuelrur_d_tons <- fuelwfdb %>% 
        dplyr::filter(grepl('Rur', area)) %>%
        dplyr::select(all_of(demand_col)) %>% 
        sum()
      rurfuelDem_Sctry <- ruralpopulation*fuelrur_d_tons/rurpopmap
      
      rururbfuel <- merge(rurfuel_Sctry,urbfuel_Sctry)
      # plot(rururbfuel)
      if (firstyr <= yr) {
        terra::writeRaster(rururbfuel, paste0("pop_tempE_",scex,"/",pop_ver,"_",i,"_",yr,"_",f,"_users.tif"), filetype = "GTiff", overwrite = T)
      }
      terra::global(rururbfuel, fun="notNA")
      
      rururbfuelDem <- merge(rurfuelDem_Sctry,urbfuelDem_Sctry)
      # plot(rururbfuel)
      if (firstyr <= yr) {
        terra::writeRaster(rururbfuelDem, paste0("demand_tempE_",scex,"/",pop_ver,"_",i,"_",yr,"_",f,"_demand.tif"), filetype = "GTiff", overwrite = T)
      }
      terra::global(rururbfuelDem, fun="notNA")
      
      
      if ( f == "Biomass") {
        wf_wBio <- rurfuelDem_Sctry
        wf_vBio <- urbfuelDem_Sctry # Beware that, Biomass shall run BEFORE Charcoal
        if (firstyr <= yr) {
          terra::writeRaster(wf_wBio, paste0("demand_tempE_",scex,"/",pop_ver,"_",i,"_",yr,"_wftons_w.tif"), filetype = "GTiff", overwrite = T)
        }
        terra::global(wf_wBio, fun="notNA")
      }
      
      if ( f == "Charcoal") {
        wf_v_stack <- c(wf_vBio,urbfuelDem_Sctry,rurfuelDem_Sctry)
        wf_v_stack.sum <- app(wf_v_stack, fun=sum, na.rm = TRUE)
        wf_v <- wf_v_stack.sum
        if (firstyr <= yr) {
          terra::writeRaster(wf_v, paste0("demand_tempE_",scex,"/",pop_ver,"_",i,"_",yr,"_wftons_v.tif"), filetype = "GTiff", overwrite = T)
        }
        terra::global(wf_v, fun="notNA")
      }
      
      for (j in annos) { 
        # i="AFG"
        # j=1990
        
        print(j)
        # Loop for fuelwood
        fuelpopWHO.anno <- whodb %>% 
          dplyr::filter(iso3 == i) %>%
          dplyr::filter(fuel == f) %>%
          # dplyr::filter(grepl(j, year)) %>%
          dplyr::filter(grepl('Rur|Urb', area))
        
        fuelwfdb.anno <- wfdb %>% 
          dplyr::filter(iso3 == i) %>%
          dplyr::filter(fuel == f) %>%
          # dplyr::filter(grepl(j, year)) %>%
          dplyr::filter(grepl('Rur|Urb', area))
        
        fuelurb.anno <- fuelpopWHO.anno %>% 
          dplyr::filter(area == "Urban") %>%
          dplyr::filter(year == j) %>%
          dplyr::select(pop) %>% 
          sum()*1000
        urbpopmap
        urbfuel_Sctry.anno <- urbanpopulation*fuelurb.anno/urbpopmap
        round(global(urbfuel_Sctry, "sum", na.rm=TRUE),0) %>% 
          pull(sum)
        fuelurb_d_tons.anno <- fuelwfdb.anno %>% 
          dplyr::filter(area == "Urban") %>%
          dplyr::filter(year == j) %>%
          dplyr::select(all_of(demand_col)) %>% 
          sum()
        urbpopmap
        urbfuelDem_Sctry.anno <- urbanpopulation*fuelurb_d_tons.anno/urbpopmap
        
        fuelrur.anno <- fuelpopWHO.anno %>% 
          dplyr::filter(area == "Rural") %>%
          dplyr::filter(year == j) %>%
          dplyr::select(pop) %>% 
          sum()*1000  
        rurpopmap
        rurfuel_Sctry.anno <- ruralpopulation*fuelrur.anno/rurpopmap
        round(global(rurfuel_Sctry, "sum", na.rm=TRUE),0) %>% 
          pull(sum)
        fuelrur_d_tons.anno <- fuelwfdb.anno %>% 
          dplyr::filter(area == "Rural") %>%
          dplyr::filter(year == j) %>%
          dplyr::select(all_of(demand_col)) %>% 
          sum()  
        rurpopmap
        rurfuelDem_Sctry.anno <- ruralpopulation*fuelrur_d_tons.anno/rurpopmap
        
        rururbfuel.anno <- merge(rurfuel_Sctry.anno,urbfuel_Sctry.anno)
        # plot(rururb)
        terra::writeRaster(rururbfuel.anno, paste0("pop_tempE_",scex,"/",pop_ver,"_",i,"_",j,"_",f,"_users.tif"), filetype = "GTiff", overwrite = T)
        terra::global(rururbfuel.anno, fun="notNA")
        
        rururbfuelDem.anno <- merge(rurfuelDem_Sctry.anno,urbfuelDem_Sctry.anno)
        # plot(rururb)
        terra::writeRaster(rururbfuelDem.anno, paste0("demand_tempE_",scex,"/",pop_ver,"_",i,"_",j,"_",f,"_demand.tif"), filetype = "GTiff", overwrite = T)
        terra::global(rururbfuelDem.anno, fun="notNA")
        
        if ( f == "Biomass") {
          terra::writeRaster(rurfuelDem_Sctry.anno, paste0("demand_tempE_",scex,"/",pop_ver,"_",i,"_",j,"_",f,"_rur_demand.tif"), filetype = "GTiff", overwrite = T)
          terra::writeRaster(urbfuelDem_Sctry.anno, paste0("demand_tempE_",scex,"/",pop_ver,"_",i,"_",j,"_",f,"_urb_demand.tif"), filetype = "GTiff", overwrite = T)
        }
        
        if ( f == "Charcoal") {
          terra::writeRaster(rurfuelDem_Sctry.anno, paste0("demand_tempE_",scex,"/",pop_ver,"_",i,"_",j,"_",f,"_rur_demand.tif"), filetype = "GTiff", overwrite = T)
          terra::writeRaster(urbfuelDem_Sctry.anno, paste0("demand_tempE_",scex,"/",pop_ver,"_",i,"_",j,"_",f,"_urb_demand.tif"), filetype = "GTiff", overwrite = T)
        }
        
        
        # Save walking or selfgathered woodfuel demand (rur) and vehicle or marketed demand (urb+urbcha+rurcha)
        
        if ( f == "Biomass") {
          wf_w.anno <- rast(paste0("demand_tempE_",scex,"/",pop_ver,"_",i,"_",j,"_Biomass_rur_demand.tif")) # Leer desde la base
          terra::writeRaster(wf_w.anno, paste0("demand_tempE_",scex,"/",pop_ver,"_",i,"_",j,"_wftons_w.tif"), filetype = "GTiff", overwrite = T)
          terra::global(wf_w.anno, fun="notNA")
        }
        
        if ( f == "Charcoal") {
          urbDem_Sctry.annor <- rast(paste0("demand_tempE_",scex,"/",pop_ver,"_",i,"_",j,"_Biomass_urb_demand.tif"))
          urbchaDem_Sctry.annor <- rast(paste0("demand_tempE_",scex,"/",pop_ver,"_",i,"_",j,"_Charcoal_urb_demand.tif"))
          rurchaDem_Sctry.annor <- rast(paste0("demand_tempE_",scex,"/",pop_ver,"_",i,"_",j,"_Charcoal_rur_demand.tif"))
          
          wf_v_stack.anno <- c(urbDem_Sctry.annor,urbchaDem_Sctry.annor,rurchaDem_Sctry.annor)
          wf_v_stack.sum.anno <- app(wf_v_stack.anno, fun=sum, na.rm = TRUE)
          wf_v.anno <- wf_v_stack.sum.anno
          terra::writeRaster(wf_v.anno, paste0("demand_tempE_",scex,"/",pop_ver,"_",i,"_",j,"_wftons_v.tif"), filetype = "GTiff", overwrite = T)
          terra::global(wf_v.anno, fun="notNA")
        }
        
        
      }
      
    }
    
  } ## end 1st subscex loop ----
  
  # scex = "BaU" # WARNING, only use for debugging 
  # scex = "ICS" # WARNING, only use for debugging 
  
  Sys.sleep(3)
  
  ## convert summed demand into EF for an entire period 2050 / 2035 ----
  ## reads EF tables: one out of four ----
  annos__2018 <- sort(c(annos,yr))
  
  # setwd(countrydir) #Why this?? ----
  # write.table(annos__2018, "annos__2018.txt")
  # annos__2018txt <- read.table("annos__2018.txt") %>% .$x
  setwd(demanddir)
  
  # Read emission factors database
  efdb <- read_csv("demand_in/efdb_all.csv")
  head(efdb)
  
  for (i in adm0_reg$GID_0) { 
    # i = "KEN"
    
    # Check if data for the current 'i' (country code) exists in efdb
    if (!i %in% efdb$GID_0) {
      message(paste("No data for GID_0", i, "in efdb - skipping to next country"))
      next  # Skip to the next 'i'
    }
    
    # Search for 'i' in all directories within adm0_dirs
    found_in_csv <- FALSE
    fnrb_2020_2030_mean <- NA
    fnrb_2020_2030_se <- NA
    fnrb_2020_2050_mean <- NA
    fnrb_2020_2050_se <- NA
    
    for (dir in adm0_dirs) {
      csv_path <- file.path(dir, "OutBaU/webmofuss_results/summary_adm0_fr.csv") # ADD OutICS!! 
      
      if (file.exists(csv_path)) {
        summary_data <- read.csv(csv_path)
        
        # Check if 'i' is in the GID_0 column
        if (i %in% summary_data$GID_0) {
          message(paste("Found GID_0", i, "in", csv_path))
          
          # Retrieve the values from row 1 for 'fNRB_2020_2030_mean' and 'fNRB_2020_2050_mean'
          fnrb_2020_2030_mean <- summary_data$fNRB_2020_2030_mean[1] # Calcular fnrb_2020_2035!!!
          fnrb_2020_2030_se <- summary_data$fNRB_2020_2030_se[1] # Calcular fnrb_2020_2035!!!
          fnrb_2020_2050_mean <- summary_data$fNRB_2020_2050_mean[1]
          fnrb_2020_2050_se <- summary_data$fNRB_2020_2050_se[1]
          found_in_csv <- TRUE
          break  # Exit the directory loop since 'i' was found
        }
      }
    }
    
    # If 'i' wasn't found in any of the directories, skip to the next country
    if (!found_in_csv) {
      message(paste("No data found for GID_0", i, "in any of the directories - skipping to next country"))
      next
    }
    
    # Now proceed with the rest of the inner loop for 'f'
    for (f in fuelschr) {
      print(f)
      
      # List files for the current 'i' and 'f'
      fuelDem_listS <- list.files(path = paste0("demand_tempE_", scex, "/"),
                                  pattern = paste0(pop_ver,"_", i, ".*\\", f, "_demand.*\\.tif$"), 
                                  full.names = TRUE)
      
      # If no files are found for the current 'f', skip to the next fuel type
      if (length(fuelDem_listS) == 0) {
        message(paste("No files found for fuel", f, "and GID_0", i))
        next  # Skip to the next 'f'
      }
      
      # Convert to raster
      fuelDem_raster_listS <- lapply(fuelDem_listS, rast)
      
      # Sum all rasters
      fuelDem_all <- Reduce("+", fuelDem_raster_listS)
      
      # Save the summed raster
      terra::writeRaster(fuelDem_all,
                         paste0("demand_temp_summaryE_", scex, "/",pop_ver,"_", i, "_", f, "_demandSum.tif"),
                         filetype = "GTiff", overwrite = TRUE)
      
      ### Calculate emissions ----
      efvalueCO2 <- efdb %>%
        dplyr::filter(GID_0 == i) %>%
        dplyr::filter(fueltype == f) %>%
        dplyr::select(CO2)
      
      efvalueCH4 <- efdb %>%
        dplyr::filter(GID_0 == i) %>%
        dplyr::filter(fueltype == f) %>%
        dplyr::select(CH4)
      
      efvalueN2O <- efdb %>%
        dplyr::filter(GID_0 == i) %>%
        dplyr::filter(fueltype == f) %>%
        dplyr::select(N2O)
      
      # If no emission factor is found for the current 'i' and 'f', skip to the next fuel type
      if (nrow(efvalueCO2) == 0) {
        message(paste("No emission factor found for fuel", f, "and GID_0", i))
        next  # Skip to the next 'f'
      }
      
      # # Calculate emissions
      # if (mofuss_period == "2020-2035") { # Watchout that fNRB period is not exactly fnrb_2020_2035 but 2030. Fix from tables
      #   if (f %in% c("Biomass")) {
      #     fuelDem_allE <- fuelDem_all * ((efvalueCO2$CO2 * (fnrb_2020_2030 / 100)) + efvalueCH4$CH4 + efvalueN2O$N2O) 
      #   } else if (f %in% c("Charcoal")) {
      #     fuelDem_allE <- fuelDem_all * (((efvalueCO2$CO2 * (fnrb_2020_2030 / 100)) + efvalueCH4$CH4 + efvalueN2O$N2O) / efchratio ) # This values that divides the sum of ef is to convert from emission per unit of charcoal
      #     # to emissions per unit of wood that is converted to charcoal
      #   } else {
      #     fuelDem_allE <- fuelDem_all * (efvalueCO2$CO2 + efvalueCH4$CH4 + efvalueN2O$N2O)
      #   }
      # } else if (mofuss_period == "2020-2050") { # Watchout that fNRB period is not exactly fnrb_2020_2035 but 2030. Fix from tables
      #   if (f %in% c("Biomass")) {
      #     fuelDem_allE <- fuelDem_all * ((efvalueCO2$CO2 * (fnrb_2020_2050 / 100)) + efvalueCH4$CH4 + efvalueN2O$N2O) 
      #   } else if (f %in% c("Charcoal")) {
      #     fuelDem_allE <- fuelDem_all * (((efvalueCO2$CO2 * (fnrb_2020_2050 / 100)) + efvalueCH4$CH4 + efvalueN2O$N2O) / efchratio ) # This values that divides the sum of ef is to convert from emission per unit of charcoal
      #     # to emissions per unit of wood that is converted to charcoal
      #   } else {
      #     fuelDem_allE <- fuelDem_all * (efvalueCO2$CO2 + efvalueCH4$CH4 + efvalueN2O$N2O)
      #   }
      # }
      
      # Calculate emissions w/uncertainty
      if (mofuss_period == "2020-2035") { # Watchout that fNRB period is not exactly fnrb_2020_2035 but 2030. Fix from tables
        if (f %in% c("Biomass")) {
          fuelDem_allE_mean <- fuelDem_all * ((efvalueCO2$CO2 * (fnrb_2020_2030_mean / 100)) + efvalueCH4$CH4 + efvalueN2O$N2O)
          fuelDem_allE_se <- fuelDem_all * ((efvalueCO2$CO2 * (fnrb_2020_2030_se / 100)) + efvalueCH4$CH4 + efvalueN2O$N2O) 
        } else if (f %in% c("Charcoal")) {
          fuelDem_allE_mean <- fuelDem_all * (((efvalueCO2$CO2 * (fnrb_2020_2030_mean / 100)) + efvalueCH4$CH4 + efvalueN2O$N2O) / efchratio ) # This values that divides the sum of ef is to convert from emission per unit of charcoal
          fuelDem_allE_se <- fuelDem_all * (((efvalueCO2$CO2 * (fnrb_2020_2030_se / 100)) + efvalueCH4$CH4 + efvalueN2O$N2O) / efchratio ) # This values that divides the sum of ef is to convert from emission per unit of charcoal
          # to emissions per unit of wood that is converted to charcoal
        } else {
          fuelDem_allE_mean <- fuelDem_all * (efvalueCO2$CO2 + efvalueCH4$CH4 + efvalueN2O$N2O)
          fuelDem_allE_se <- fuelDem_all * 0
        }
      } else if (mofuss_period == "2020-2050") { # Watchout that fNRB period is not exactly fnrb_2020_2035 but 2030. Fix from tables
        if (f %in% c("Biomass")) {
          fuelDem_allE_mean <- fuelDem_all * ((efvalueCO2$CO2 * (fnrb_2020_2050_mean / 100)) + efvalueCH4$CH4 + efvalueN2O$N2O)
          fuelDem_allE_se <- fuelDem_all * ((efvalueCO2$CO2 * (fnrb_2020_2050_se / 100)) + efvalueCH4$CH4 + efvalueN2O$N2O) 
        } else if (f %in% c("Charcoal")) {
          fuelDem_allE_mean <- fuelDem_all * (((efvalueCO2$CO2 * (fnrb_2020_2050_mean / 100)) + efvalueCH4$CH4 + efvalueN2O$N2O) / efchratio ) # This values that divides the sum of ef is to convert from emission per unit of charcoal
          fuelDem_allE_se <- fuelDem_all * (((efvalueCO2$CO2 * (fnrb_2020_2050_mean / 100)) + efvalueCH4$CH4 + efvalueN2O$N2O) / efchratio ) # This values that divides the sum of ef is to convert from emission per unit of charcoal
          # to emissions per unit of wood that is converted to charcoal
        } else {
          fuelDem_allE_mean <- fuelDem_all * (efvalueCO2$CO2 + efvalueCH4$CH4 + efvalueN2O$N2O)
          fuelDem_allE_se <- fuelDem_all * 0
        }
      }
      
      # Save emissions raster
      terra::writeRaster(fuelDem_allE_mean,
                         paste0("emissions_temp_", scex, "/",pop_ver,"_", i, "_", f, "_emissionsSum_mean.tif"),
                         filetype = "GTiff", overwrite = TRUE)

      terra::writeRaster(fuelDem_allE_se,
                         paste0("emissions_temp_", scex, "/",pop_ver,"_", i, "_", f, "_emissionsSum_se.tif"),
                         filetype = "GTiff", overwrite = TRUE)
    }
    
  } ## end 2nd subscex loop ----
  
  # ###
  # fuelDem_all %>%
  #   raster() %>%
  #   cellStats(stat='sum', na.rm=TRUE)
  # firstyr
  # # 85920582 - 2010-2035 - BaU, charcoal
  # 426166087 / 4.96 # !!!!!!!!!!!!!!!!!!!!!!!!!!!
  # # 77154413 - 2010-2035 - ICS, charcoal
  # 378056623 / 4.9
  # 85920582 - 77154413
  # # BaU - ICA = 8766169
  # 
  # # 64342388 - 2020-2035 - BaU, charcoal
  # 319138246 / 4.96 # !!!!!!!!!!!!!!!
  # # 55576219 - 2020-2035 - ICS, charcoal
  # 272323473 / 4.9
  # 64342388 - 55576219
  # # BaU - ICA = 8766169 FUCKIN OK!
  
  # ### ef RWA Charcoal = 4.96
  # fuelDem_allE %>%
  #   raster() %>%
  #   cellStats(stat='sum', na.rm=TRUE)
  # firstyr
  # #  426166087 - 2010-2035 - BaU, charcoal
  # #  378056623 - 2010-2035 - ICS, charcoal
  # (426166087 / 4.96) - (378056623 / 4.9)
  # 426166087 - 378056623
  # # BaU - ICA = 48109464
  # 
  # # 319138246 - 2020-2035 - BaU, charcoal
  # # 272323473 - 2020-2035 - ICS, charcoal
  # (319138246 / 4.96) - (272323473 / 4.9)
  # 319138246 - 272323473
  # # BaU - ICA = 46814773 NOT FUCKING OK, its just an emissions BUG in the BAU scenario
  
  
  
  
  
  
  
  ## load country rasters and merge into original region ----
  
  if (mergecountries == 1) { 
    setwd(demanddir)
    
    for (f in fuelschr) {
      # f = "Charcoal"

      emissions_list_mean <- list.files(path = paste0("emissions_temp_",scex,"/"),
                                   pattern = paste0("_",f,".*\\_mean.tif$"), full.names = TRUE)
      emissions_list_se <- list.files(path = paste0("emissions_temp_",scex,"/"),
                                        pattern = paste0("_",f,".*\\_se.tif$"), full.names = TRUE)

      if (length(emissions_list_mean) > 1) { 
        emissions_raster_list_mean <- lapply(emissions_list_mean, rast)
        emissions_users_mean <- do.call("merge", emissions_raster_list_mean)
        terra::writeRaster(emissions_users_mean,
                           paste0("emissions_out_",scex,"/",pop_ver,"_",firstyr,"-",
                                  lastyr,"_",f,"_",scex,"_emissions_mean.tif"),
                           filetype = "GTiff", overwrite = T)
        
        emissions_raster_list_se <- lapply(emissions_list_se, rast)
        emissions_users_se <- do.call("merge", emissions_raster_list_se)
        terra::writeRaster(emissions_users_se,
                           paste0("emissions_out_",scex,"/",pop_ver,"_",firstyr,"-",
                                  lastyr,"_",f,"_",scex,"_emissions_se.tif"),
                           filetype = "GTiff", overwrite = T)
        
      } else {
        
        rast(emissions_list_mean) %>%
          terra::writeRaster(paste0("emissions_out_",scex,"/",pop_ver,"_",firstyr,"-",
                                    lastyr,"_",f,"_",scex,"_emissions_mean.tif"),
                             filetype = "GTiff", overwrite = T)
        rast(emissions_list_se) %>%
          terra::writeRaster(paste0("emissions_out_",scex,"/",pop_ver,"_",firstyr,"-",
                                    lastyr,"_",f,"_",scex,"_emissions_se.tif"),
                             filetype = "GTiff", overwrite = T)
        
      }
      
    }
    
  }
  
  ## final emissions maps ----
  # Sum all fuels and transform to various units
  # Project for TNC
  
  setwd(demanddir)
  
  for (sdm in c("_mean", "_se")){
    # sdm = "_mean"
    # sdm = "_se"
    
    finalemissions_list <- list.files(path = paste0("emissions_out_",scex,"/"),
                                           pattern = paste0(pop_ver,"_.*\\",scex,"_emissions.*\\",sdm,".tif$"), full.names = TRUE)
    finalemissions_raster_list <- lapply(finalemissions_list, rast)
    finalemissions_all <- Reduce("+",finalemissions_raster_list)
    terra::writeRaster(finalemissions_all,
                       paste0("emissions_out_",scex,"/e",firstyr,"-",
                              lastyr,"_",scex,"_tCO2e_gcs",sdm,".tif"),
                       filetype = "GTiff", overwrite = T)
    
    eCO2e_pNoAdj <- finalemissions_all %>%
      terra::project("epsg:3395", method = "bilinear", gdal = FALSE, res=1000) #, threads=TRUE) # Check PROJECTION with TNC
    # Perhaps is better not to project and adjust by area in WSG84 pixels?
    terra::writeRaster(eCO2e_pNoAdj, paste0("emissions_out_",scex,"/e",firstyr,"-",
                                                 lastyr,"_",scex,"_tCO2e_noadj",sdm,".tif"),
                       filetype = "GTiff", overwrite = T)
    
    # Correction due to projection - any year.
    eCO2e_preProj <- finalemissions_all %>%
      raster() %>%
      cellStats(stat='sum', na.rm=TRUE)
    eCO2e_pstProj <- eCO2e_pNoAdj %>%
      raster() %>%
      cellStats(stat='sum', na.rm=TRUE)
    proj_factor_eCO2e <- eCO2e_preProj/eCO2e_pstProj
    # proj_factor_eCO2e = 1
    eCO2e_p <- eCO2e_pNoAdj*proj_factor_eCO2e
    terra::writeRaster(eCO2e_p, paste0("emissions_out_",scex,"/e",firstyr,"-",
                                       lastyr,"_",scex,"_tCO2e",sdm,".tif"),
                       filetype = "GTiff", overwrite = T)
    
    perha <- (res(eCO2e_p)[1]^2)/(100^2)
    peryr <- length(annos) 
    eCO2e_p_unitx <- eCO2e_p / perha
    eCO2e_p_unit <- eCO2e_p_unitx / peryr
    terra::writeRaster(eCO2e_p_unit, paste0("emissions_out_",scex,"/e",firstyr,"-",
                                                 lastyr,"_",scex,"_tCO2e_thay",sdm,".tif"),
                       filetype = "GTiff", overwrite = T)
    
  }
  ## copy emissions factors to TNC temporal results ----
  if (byregion == "Global") {
    # Extract the part after the last "_"
    after_last_underscore <- sub(".*_", "", mofuss_region)
    # Concatenate with a hyphen
    regiontag <- paste0(after_last_underscore)
  } else if (byregion == "Continental") {
    # Extract the part before the first "_"
    before_first_underscore <- sub("_.*", "", mofuss_region)
    # Extract the part after the last "_"
    after_last_underscore <- sub(".*_", "", mofuss_region)
    # Concatenate with a hyphen
    regiontag <- paste0(after_last_underscore,"_",substr(byregion, 1, 3))
  } else if (byregion == "Regional") {
    # Extract the part before the first "_"
    before_first_underscore <- sub("_.*", "", mofuss_region)
    # Extract the part after the last "_"
    after_last_underscore <- sub(".*_", "", mofuss_region)
    # Concatenate with a hyphen
    regiontag <- paste0(before_first_underscore, "-", after_last_underscore,"_",substr(byregion, 1, 3))
  } else if (byregion == "Country") {
    # Extract the part before the first "_"
    before_first_underscore <- sub("_.*", "", mofuss_region)
    # Extract the part after the last "_"
    after_last_underscore <- sub(".*_", "", mofuss_region)
    # Concatenate with a hyphen
    regiontag <- paste0(before_first_underscore, "-", after_last_underscore,"_",substr(byregion, 1, 3))
  }
  print(regiontag)
  
  if (annos[length(annos)] == 2050 & scex == "BaU"){
    print("Copy files to 2050 - BaU")
    unlink(paste0(emissionsdir,"/2050",regiontag,"/BaU"), recursive = TRUE)
    if (!dir.exists(paste0(emissionsdir,"/2050",regiontag,"/BaU"))) {dir.create(paste0(emissionsdir,"/2050",regiontag,"/BaU"), recursive = TRUE)}
    file.copy(from = paste0(demanddir,"/emissions_out_",scex),
              to = paste0(emissionsdir,"/2050",regiontag,"/BaU"),
              overwrite = TRUE, recursive = TRUE, copy.mode = TRUE)
    
  } else if (annos[length(annos)] == 2050 & scex == "ICS"){
    print("Copy files to 2050 - ICS")
    unlink(paste0(emissionsdir,"/2050",regiontag,"/ICS"), recursive = TRUE)
    if (!dir.exists(paste0(emissionsdir,"/2050",regiontag,"/ICS"))) {dir.create(paste0(emissionsdir,"/2050",regiontag,"/ICS"), recursive = TRUE)}
    file.copy(from = paste0(demanddir,"/emissions_out_",scex),
              to = paste0(emissionsdir,"/2050",regiontag,"/ICS"),
              overwrite = TRUE, recursive = TRUE, copy.mode = TRUE)
    
  } else if (annos[length(annos)] == 2035 & scex == "BaU"){
    print("Copy files to 2035 - BaU")
    unlink(paste0(emissionsdir,"/2035/BaU"), recursive = TRUE)
    if (!dir.exists(paste0(emissionsdir,"/2035",regiontag,"/BaU"))) {dir.create(paste0(emissionsdir,"/2035",regiontag,"/BaU"), recursive = TRUE)}
    file.copy(from = paste0(demanddir,"/emissions_out_",scex),
              to = paste0(emissionsdir,"/2035",regiontag,"/BaU"),
              overwrite = TRUE, recursive = TRUE, copy.mode = TRUE)
    
  }  else if (annos[length(annos)] == 2035 & scex == "ICS"){
    print("Copy files to 2035 - ICS")
    unlink(paste0(emissionsdir,"/2035",regiontag,"/ICS"), recursive = TRUE)
    if (!dir.exists(paste0(emissionsdir,"/2035",regiontag,"/ICS"))) {dir.create(paste0(emissionsdir,"/2035",regiontag,"/ICS"), recursive = TRUE)}
    file.copy(from = paste0(demanddir,"/emissions_out_",scex),
              to = paste0(emissionsdir,"/2035",regiontag,"/ICS"),
              overwrite = TRUE, recursive = TRUE, copy.mode = TRUE)
    
  } else {
    print("error: nothing will be saved")
  } 
  
  
  paste0(annos[length(annos)],"-",scex )
  
} # End scex loop ----
# At the end of the scex loop, both scenarios BaU and ICS are ready, for one or many time periods
# 2020-2035 and 2020-2050 for TNC 

# Avoided emissions Bau vs Sce ----

# # This chunk is repeated from above. Uncomment when running this code partially
# if (annos[1] == yr+1) {
#   firstyr <- yr
# } else if (annos[1]-1 > yr) {
#   firstyr <- annos[1]
# } else if (annos[1] < yr) {
#   firstyr <- annos[1]
# } else {
#   print("Error?")
# }
# firstyr
# lastyr <- annos[length(annos)]
# lastyr
# firstyr > yr

simlength <- (lastyr - firstyr)+1
# simlength = 2035 # Test for 2035
print(simlength)

if (avoidedemissions == 1){
  
  for (sdm in c("_mean", "_se")){
    # sdm = "_mean"
    # sdm = "_se"
    
    if (file.exists(paste0(emissionsdir,"/",lastyr,regiontag,"/BaU/emissions_out_BaU/e",firstyr,"-",lastyr,"_BaU_tCO2e_gcs",sdm,".tif")) == TRUE &
        file.exists(paste0(emissionsdir,"/",lastyr,regiontag,"/ICS/emissions_out_ICS/e",firstyr,"-",lastyr,"_ICS_tCO2e_gcs",sdm,".tif")) == TRUE) {
      
      BaU20xxa <- rast(paste0(emissionsdir,"/",lastyr,regiontag,"/BaU/emissions_out_BaU/e",firstyr,"-",lastyr,"_BaU_tCO2e_gcs",sdm,".tif"))
      ICS20xxa <- rast(paste0(emissionsdir,"/",lastyr,regiontag,"/ICS/emissions_out_ICS/e",firstyr,"-",lastyr,"_ICS_tCO2e_gcs",sdm,".tif"))
      if (sdm == "_mean"){
        AvEm20xx_gcs_tpp <- BaU20xxa - ICS20xxa # tpp stands for tonnes per pixel per period
      } else if (sdm == "_se") {
        AvEm20xx_gcs_tpp <- sqrt((BaU20xxa - ICS20xxa)^2)/sqrt(30) # WARNING !!! PROPAGATE THE ERROR CORRECTLY PLEASE - PLACEHOLDER FOR TNC
      }
      terra::writeRaster(AvEm20xx_gcs_tpp, paste0(emissionsdir,"/",lastyr,regiontag,"/AE",lastyr,"_gcs_tpp",sdm,".tif"),
                         filetype = "GTiff", overwrite = T)
      
      AvEm20xx_gcs_tpyr <- (AvEm20xx_gcs_tpp/simlength) # tpyr stands for tonnes per pixel per year
      terra::writeRaster(AvEm20xx_gcs_tpyr, paste0(emissionsdir,"/",lastyr,regiontag,"/AE",lastyr,"_gcs_tpyr",sdm,".tif"),
                         filetype = "GTiff", overwrite = T)
      
      BaU20xxb <- rast(paste0(emissionsdir,"/",lastyr,regiontag,"/BaU/emissions_out_BaU/e",firstyr,"-",lastyr,"_BaU_tCO2e",sdm,".tif"))
      ICS20xxb <- rast(paste0(emissionsdir,"/",lastyr,regiontag,"/ICS/emissions_out_ICS/e",firstyr,"-",lastyr,"_ICS_tCO2e",sdm,".tif"))
      if (sdm == "_mean"){
        AvEm20xx_wm_tpp <- BaU20xxb - ICS20xxb #tpp stands for tonnes per pixel per period
      } else if (sdm == "_se") {
        AvEm20xx_wm_tpp <- sqrt((BaU20xxb - ICS20xxb)^2)/sqrt(30) # # WARNING !!! PROPAGATE THE ERROR CORRECTLY PLEASE - PLACEHOLDER FOR TNC
      }
      terra::writeRaster(AvEm20xx_wm_tpp, paste0(emissionsdir,"/",lastyr,regiontag,"/AE",lastyr,"_wm_tpp",sdm,".tif"),
                         filetype = "GTiff", overwrite = T)
      
      BaU20xxc <- rast(paste0(emissionsdir,"/",lastyr,regiontag,"/BaU/emissions_out_BaU/e",firstyr,"-",lastyr,"_BaU_tCO2e_thay",sdm,".tif"))
      ICS20xxc <- rast(paste0(emissionsdir,"/",lastyr,regiontag,"/ICS/emissions_out_ICS/e",firstyr,"-",lastyr,"_ICS_tCO2e_thay",sdm,".tif"))
      if (sdm == "_mean"){
        AvEm20xx_wm_thay <- BaU20xxc - ICS20xxc #thay stands for tonnes per hectare per yr
      } else if (sdm == "_se") {
        AvEm20xx_wm_thay <- sqrt((BaU20xxc - ICS20xxc)^2)/sqrt(30) # # WARNING !!! PROPAGATE THE ERROR CORRECTLY PLEASE - PLACEHOLDER FOR TNC
      }
      terra::writeRaster(AvEm20xx_wm_thay, paste0(emissionsdir,"/",lastyr,regiontag,"/AE",lastyr,"_wm_thay",sdm,".tif"),
                         filetype = "GTiff", overwrite = T)
      
      
    } else {
      print(paste0("ERROR: One or more files in ",emissionsdir,"/",lastyr,"/ seem to be missing")) # Is this valid? or only one works
    }
    
    
    
    # Zonal statistics for cross-validation and debugging ----
    if (zonalstats == 1){
      
      setwd(admindir)
      # lastyr = 2035
      
      if (byregion == "Global"){
        # print("fix this chunk!") # Not to be used in the short term really...
        adminnew <- st_read("regions_adm0/mofuss_regions0.gpkg") %>%
          dplyr::mutate(zone = 1:100)
        admindb <- adminnew %>% st_drop_geometry()
        head(adminnew)
        sort(adminnew$NAME_0)
        
        adminnew_p <- st_read("regions_adm0_p/mofuss_regions0_p.gpkg") %>%
          dplyr::mutate(zone = 1:100) %>%
          dplyr::mutate(km2_vector = round(st_area(.)/1000000,0)) %>%
          units::drop_units()
        admindb_p <- adminnew_p %>% st_drop_geometry()
        head(adminnew_p)
        sort(adminnew_p$NAME_0)
        
      } else if (byregion == "Continental"){
        adminnew <- st_read("regions_adm0/mofuss_regions0.gpkg") %>%
          dplyr::mutate(zone = 1:100) %>%
          dplyr::filter(grepl(mofuss_region,mofuss_reg))
        admindb <- adminnew %>% st_drop_geometry()
        head(adminnew)
        sort(adminnew$NAME_0)
        
        adminnew_p <- st_read("regions_adm0_p/mofuss_regions0_p.gpkg") %>%
          dplyr::mutate(zone = 1:100) %>%
          dplyr::filter(grepl(mofuss_region,mofuss_reg)) %>%
          dplyr::mutate(km2_vector = round(st_area(.)/1000000,0)) %>%
          units::drop_units()
        admindb_p <- adminnew_p %>% st_drop_geometry()
        head(adminnew_p)
        sort(adminnew_p$NAME_0)
        
      } else if (byregion == "Regional"){
        adminnew <- st_read("regions_adm0/mofuss_regions0.gpkg") %>%
          dplyr::mutate (zone = 1:100) %>%
          dplyr::filter (mofuss_reg == mofuss_region)
        admindb <- adminnew %>% st_drop_geometry()
        sort(adminnew$NAME_0)
        
        adminnew_p <- st_read("regions_adm0_p/mofuss_regions0_p.gpkg") %>%
          dplyr::mutate (zone = 1:100) %>%
          dplyr::filter (mofuss_reg == mofuss_region) %>%
          dplyr::mutate(km2_vector = round(st_area(.)/1000000,0)) %>%
          units::drop_units()
        admindb_p <- adminnew_p %>% st_drop_geometry()
        head(adminnew_p)
        sort(adminnew_p$NAME_0)
        
      } else if (byregion == "Country"){
        adminnew <- st_read("regions_adm0/mofuss_regions0.gpkg") %>%
          dplyr::mutate (zone = 1:100) %>%
          dplyr::filter (NAME_0 == mofuss_region)
        admindb <- adminnew %>% st_drop_geometry()
        sort(adminnew$NAME_0)
        
        adminnew_p <- st_read("regions_adm0_p/mofuss_regions0_p.gpkg") %>%
          dplyr::mutate (zone = 1:100) %>%
          dplyr::filter (NAME_0 == mofuss_region) %>%
          dplyr::mutate(km2_vector = round(st_area(.)/1000000,0)) %>%
          units::drop_units()
        admindb_p <- adminnew_p %>% st_drop_geometry()
        sort(adminnew_p$NAME_0)
        
      }
      
      AvEm_gcs_tppr <- raster(paste0(emissionsdir,"/",lastyr,regiontag,"/AE",lastyr,"_gcs_tpp",sdm,".tif"))
      AvEm_gcs_tpyr <- raster(paste0(emissionsdir,"/",lastyr,regiontag,"/AE",lastyr,"_gcs_tpyr",sdm,".tif"))
      AvEm_wm_tppr <- raster(paste0(emissionsdir,"/",lastyr,regiontag,"/AE",lastyr,"_wm_tpp",sdm,".tif"))
      AvEm_wm_thayr <- raster(paste0(emissionsdir,"/",lastyr,regiontag,"/AE",lastyr,"_wm_thay",sdm,".tif"))
      
      # plot(adminnew)
      admin_r <- fasterize(adminnew, AvEm_gcs_tppr, field = "zone") # %>% mask(AvEm2035r)
      admin_rp <- fasterize(adminnew_p, AvEm_wm_tppr, field = "zone") # %>% mask(AvEm2035r)
      # plot(admin_r)
      # plot(admin_rp)
      
      # Summary tables over GCS rasters
      AvEm_gcs_tppr_sum <- as.data.frame(zonal(AvEm_gcs_tppr, admin_r, 'sum')) %>% # CORRECT SUM FOR SE PROPAGATION
        dplyr::left_join(.,adminnew, by = "zone") %>%
        dplyr::select(-zone, -Subregion, -ID, -mofuss_reg,-geom) %>%
        dplyr::mutate(eMtCO2e = round(sum/1000000,2)) %>% # tonnes to megatonnes
        dplyr::mutate(eMtCO2e_yr = round(eMtCO2e/simlength,2)) %>% # period to year
        dplyr::relocate(sum, .after = NAME_0) %>%
        rename_with(., .fn = ~paste0(firstyr,"-",lastyr,"_tpp"), .cols = all_of("sum"))
      AvEm_gcs_tppr_sum
      setwd(emissionsdir)
      write.csv(AvEm_gcs_tppr_sum,paste0(emissionsdir,"/",lastyr,regiontag,"/AE",lastyr,"_gcs_tpp_sum",sdm,".csv"), row.names=FALSE, quote=FALSE)
      
      # Summary tables over PCS rasters
      AvEm_wm_tppr_areaT <- as.data.frame(zonal(admin_rp, admin_rp, 'count'))
      AvEm_wm_tppr_sum <- as.data.frame(zonal(AvEm_wm_tppr, admin_rp, 'sum')) %>%
        dplyr::left_join(.,adminnew_p, by = "zone") %>%
        dplyr::left_join(.,AvEm_wm_tppr_areaT, by = "zone") %>%
        dplyr::rename("km2_raster" = "count") %>%
        dplyr::select(-zone, -Subregion, -ID, -mofuss_reg,-geom) %>%
        dplyr::mutate(eMtCO2e = round(sum/1000000,2)) %>% # tonnes to megatonnes
        dplyr::mutate(eMtCO2e_yr = round(eMtCO2e/simlength,2)) %>% # period to year
        dplyr::mutate(etCO2e_hayr = round(sum/(km2_raster*100)/simlength,4)) %>% # by hectare (entire country) and by year
        dplyr::relocate(sum, .after = NAME_0) %>%
        dplyr::relocate(km2_raster, .after = etCO2e_hayr) %>%
        dplyr::relocate(km2_vector, .after = km2_raster) %>%
        rename_with(AvEm_wm_tppr_sum, .fn = ~paste0(firstyr,"-",lastyr,"_tpp"), .cols = all_of("sum"))
      AvEm_wm_tppr_sum
      write.csv(AvEm_wm_tppr_sum,paste0(emissionsdir,"/",lastyr,regiontag,"/AE",lastyr,"_wm_tpp_sum",sdm,".csv"), row.names=FALSE, quote=FALSE)
      
      AvEm_wm_thayr_areaT <- as.data.frame(zonal(admin_rp, admin_rp, 'count'))
      AvEm_wm_thayr_areaP <- as.data.frame(zonal(AvEm_wm_thayr, admin_rp, 'count'))
      AvEm_wm_thayr_sum <- as.data.frame(zonal(AvEm_wm_thayr, admin_rp, 'sum')) %>%
        dplyr::mutate(e20xx_tpp_eq = sum*100*simlength) %>% # OJO ACA
        dplyr::left_join(.,adminnew_p, by = "zone") %>%
        dplyr::left_join(.,AvEm_wm_thayr_areaT, by = "zone") %>%
        dplyr::rename("km2_raster" = "count") %>%
        dplyr::left_join(.,AvEm_wm_thayr_areaP, by = "zone") %>%
        dplyr::rename("km2_raster_pop" = "count") %>%
        dplyr::select(-zone, -Subregion, -ID, -mofuss_reg,-geom) %>%
        dplyr::mutate(etCO2e_hayr_xr = round(sum/km2_raster,4)) %>% # for comparison x ref
        dplyr::relocate(sum, .after = NAME_0) %>%
        dplyr::relocate(km2_raster, .after = etCO2e_hayr_xr) %>%
        dplyr::relocate(km2_raster_pop, .after = km2_raster) %>%
        dplyr::relocate(e20xx_tpp_eq , .after = sum) %>%
        rename_with(., .fn = ~paste0(firstyr,"-",lastyr,"_thayr"), .cols = all_of("sum"))
      AvEm_wm_thayr_sum
      write.csv(AvEm_wm_thayr_sum,paste0(emissionsdir,"/",lastyr,regiontag,"/AE",lastyr,"_wm_thayr_sum",sdm,".csv"), row.names=FALSE, quote=FALSE)
      
      
      ## raster and tables summaries for cross validation ----
      AvEm_gcs_tpyrt <- rast(paste0(emissionsdir,"/",lastyr,regiontag,"/AE",lastyr,"_gcs_tpyr",sdm,".tif"))
      exf1 <- round(AvEm_gcs_tpyrt %>%
                      terra::global(., 'sum', na.rm=TRUE) %>%
                      pull(sum)/1000000,0)
      print(paste0(exf1, " mt/yr for ",lastyr," 1.- Sums all non-null pixel values of a raster in GCS depicting tonnes per pixel per year; and convert them in the code to megatonnes per year = annual avoided emissions in SSA in 2050/35"))
      
      exf2 <- AvEm_gcs_tppr_sum %>%
        summarise("X_validation_gcs" = sum(eMtCO2e_yr)) %>%
        pull(X_validation_gcs) %>%
        round(.,0)
      print(paste0(exf2, " mt/yr for ",lastyr," 2.- Sums all rows of a table (from a GCS layer) showing megatonnes per country per year = annual avoided emissions in SSA in 2050/35"))
      
      AvEm_wm_tpprt <- rast(paste0(emissionsdir,"/",lastyr,regiontag,"/AE",lastyr,"_wm_tpp",sdm,".tif"))
      exf3 <- round(AvEm_wm_tpprt %>%
                      terra::global(., 'sum', na.rm=TRUE) %>%
                      pull(sum)/1000000/simlength,0)
      print(paste0(exf3, " mt/yr for ",lastyr," 3.- Sums all non-null pixel values of a raster in PCS depicting tonnes per km2 per period; and convert them in the code to megatonnes per year = annual avoided emissions in SSA in 2050/35"))
      
      print(paste0(exf3, " mt/yr for ",lastyr," 4.- Sums all rows of a table (from a PCS layer) showing megatonnes per country per year = annual avoided emissions in SSA in 2050/35"))
      exf4 <- AvEm_wm_tppr_sum %>%
        summarise("X_validation_pcs" = sum(eMtCO2e_yr)) %>%
        pull(X_validation_pcs) %>%
        round(.,0)
      
      AvEm_wm_thayrt <- rast(paste0(emissionsdir,"/",lastyr,regiontag,"/AE",lastyr,"_wm_thay",sdm,".tif"))
      exf5 <- round(AvEm_wm_thayrt %>%
                      terra::global(., 'sum', na.rm=TRUE) %>%
                      pull(sum)*100/1000000,0)
      print(paste0(exf5, " mt/yr for ",lastyr," 5.- Sums all non-null pixel values of a raster in PCS depicting tonnes per ha per year; and convert them in the code to megatonnes per year = annual avoided emissions in SSA in 2050/35"))
      
      exf6 <- round(sum(AvEm_wm_thayr_sum[,3])*100/1000000,0)
      print(paste0(exf6, " mt/yr for ",lastyr," 6.- Sums all rows of a table (from a PCS layer) showing tonnes per ha per year; and convert them in the code to megatonnes per year = annual avoided emissions in SSA in 2050/35"))
      
    }
  }
}

# Integrate mean and se into one summary table - THIS IS A PLACEHOLDER FOR TNC - RE CODE THE UNCERTAINTY ANALYSIS
setwd(paste0(emissionsdir,"/",lastyr,regiontag))

# Function to merge mean and SE tables
merge_tables <- function(mean_file, se_file, common_cols, value_cols, output_file) {
  # Read the data
  mean_df <- read_csv(mean_file)
  se_df <- read_csv(se_file)
  
  # Rename columns in SE table
  se_df <- se_df %>%
    rename_with(~ paste0(., "_se"), all_of(value_cols))
  
  # Rename columns in Mean table
  mean_df <- mean_df %>%
    rename_with(~ paste0(., "_mean"), all_of(value_cols))
  
  # Merge tables
  merged_df <- mean_df %>%
    left_join(se_df, by = common_cols)
  
  # Order columns to keep each mean-se pair together
  ordered_cols <- c(common_cols, as.vector(t(outer(value_cols, c("_mean", "_se"), paste0))))
  merged_df <- merged_df %>%
    select(all_of(ordered_cols))
  
  # Save the merged table
  write_csv(merged_df, output_file)
  
  # Delete original mean and SE files
  # file_delete(c(mean_file, se_file))
  
  return(output_file)
}

# Define the file pairs and variables
tables <- list(
  list(
    mean_file = "AE2035_gcs_tpp_sum_mean.csv",
    se_file = "AE2035_gcs_tpp_sum_se.csv",
    common_cols = c("GID_0", "NAME_0"),
    value_cols = c("2020-2035_tpp", "eMtCO2e", "eMtCO2e_yr"),
    output_file = "AE2035_gcs_tpp_sum_merged.csv"
  ),
  list(
    mean_file = "AE2035_wm_thayr_sum_mean.csv",
    se_file = "AE2035_wm_thayr_sum_se.csv",
    common_cols = c("GID_0", "NAME_0"),
    value_cols = c("2020-2035_thayr", "e20xx_tpp_eq", "etCO2e_hayr_xr"),
    output_file = "AE2035_wm_thayr_sum_merged.csv"
  ),
  list(
    mean_file = "AE2035_wm_tpp_sum_mean.csv",
    se_file = "AE2035_wm_tpp_sum_se.csv",
    common_cols = c("GID_0", "NAME_0"),
    value_cols = c("2020-2035_tpp", "eMtCO2e", "eMtCO2e_yr", "etCO2e_hayr"),
    output_file = "AE2035_wm_tpp_sum_merged.csv"
  )
)

# Process each table pair
for (table in tables) {
  output <- merge_tables(
    mean_file = table$mean_file,
    se_file = table$se_file,
    common_cols = table$common_cols,
    value_cols = table$value_cols,
    output_file = table$output_file
  )
  print(paste("Merged table saved:", output))
}

print("Merging completed and original files deleted!")


#End ----

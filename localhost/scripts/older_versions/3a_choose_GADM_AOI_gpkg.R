# MoFuSS
# Windows version
# Date: Mar 2023

# Define extent_mask.gpkg and extent_analysis.gpkg  for any area of analysis ----
run_mask0 = "Yes" # Dejando este prendido hay que guardar los diferentes niveles admin requeridos por UNFCCC y TNC, 
                  # y retomarlos en MoFuSS last script
run_mask1 = "No" # Not working yet !!!!
run_mask2 = "No" # Not working yet !!!!
# TRABAJAR ESTO JUNTO CON EL FRNB TUNEABLE POR PERIODO

# Load packages ####
library(sf)
library(tictoc)
library(mapview)
library(tidyverse)
library(readxl)
library(hacksaw)
library(rmapshaper)
library(terra)
library(svDialogs)
library(dplyr)

# Set directory
choose_directory1 = function(caption = "Choose the directory where admin_regions files are") {
  if(.Platform$OS.type == "unix")  {
    setwd(tk_choose.dir("/home/mofuss/Documents", caption = caption))
  } else {
    setwd(choose.dir("/home/mofuss/Documents", caption = caption)) # Elegir bien esta carpeta de inicio
  }
}
choose_directory1()
admindir <- getwd()

unlink("InVector/extent_*.*")

if (run_mask0 == "Yes"){
  
  # ADM LEVEL = 0
  mofuss_regions0_gpkg <- vect("regions_adm0_p/mofuss_regions0_p.gpkg")
  mofuss_regions0 <- as.data.frame(mofuss_regions0_gpkg)
  regions.list <- mofuss_regions0 %>%
    dplyr::select(mofuss_reg) %>%
    unique()
  region.input <- dlg_list(as.character(regions.list[ , ]), 
                           preselect = "SSA_adm0_eastern",
                           multiple = FALSE,
                           title = "Choose a region to process",
                           gui = .GUI
  )
  mofuss_region <- region.input$res

  extent_mask0 <- vect(paste0("regions_adm0_p/",mofuss_region,"_p.gpkg"))
  terra::writeVector(extent_mask0, "InVector/extent_mask.gpkg", overwrite = TRUE)

  countries.list <- extent_mask0 %>%
    as.data.frame() %>%
    dplyr::select(NAME_0) %>%
    unique() %>%
    arrange(NAME_0)
  
  country.input <- dlg_list(as.character(countries.list[ , ]), 
                            preselect = "Kenya",
                            multiple = FALSE, # Check if multiple countries or values is doable
                            title = "Choose one country to process",
                            gui = .GUI
  )
  mofuss_country <- country.input$res

  extent_mask0 %>%
    terra::subset(.$NAME_0 == mofuss_country) %>% #Remember to change in the parameters table
    terra::writeVector("InVector/extent_analysis.gpkg", overwrite = TRUE)
  
  # ADM LEVEL = 1, when running ADM LEVEL = 0
  mofuss_regions1_gpkg <- vect("regions_adm1_p/mofuss_regions1_p.gpkg")
  mofuss_regions1 <- as.data.frame(mofuss_regions1_gpkg)
  
  mofuss_region1 <- gsub(0,1, mofuss_region)
  extent_mask1 <- vect(paste0("regions_adm1_p/",mofuss_region1,"_p.gpkg"))
  terra::writeVector(extent_mask1, "InVector/extent_mask1.gpkg", overwrite = TRUE)
  
  extent_mask1 %>%
    terra::subset(.$NAME_0 == mofuss_country) %>% #Remember to change in the parameters table
    terra::writeVector("InVector/extent_analysis1.gpkg", overwrite = TRUE)
  
  mofuss_region2 <- gsub(0,2, mofuss_region)
  extent_mask2 <- vect(paste0("regions_adm2_p/",mofuss_region2,"_p.gpkg"))
  terra::writeVector(extent_mask2, "InVector/extent_mask2.gpkg", overwrite = TRUE)
 
  extent_mask2 %>%
    terra::subset(.$NAME_0 == mofuss_country) %>% #Remember to change in the parameters table
    terra::writeVector("InVector/extent_analysis2.gpkg", overwrite = TRUE)
  
}

if (run_mask1 == "Yes"){
  
  # ADM LEVEL = 1
  mofuss_regions1_gpkg <- vect("regions_adm1_p/mofuss_regions1_p.gpkg")
  mofuss_regions1 <- as.data.frame(mofuss_regions1_gpkg)
  ctry.list <- mofuss_regions1 %>%
    dplyr::select(NAME_0) %>%
    unique() %>%
    arrange(NAME_0)
  ctry.input <- dlg_list(as.character(ctry.list[ , ]), 
                         # preselect = "SSA_adm0_eastern",
                         multiple = FALSE,
                         title = "Choose a country to process",
                         gui = .GUI
  )
  mofuss_ctry <- ctry.input$res
  
  extent_mask1 <- mofuss_regions1_gpkg %>%
    # dplyr::filter(NAME_0 == mofuss_ctry)
    terra::subset(.$NAME_0 == mofuss_ctry)
    terra::writeVector(extent_mask1, "InVector/extent_mask.gpkg", overwrite = TRUE)

  adm1.list <- extent_mask1 %>%
    as.data.frame() %>%
    dplyr::select(NAME_1) %>%
    unique() %>%
    arrange(NAME_1)
  
  adm1.input <- dlg_list(as.character(adm1.list[ , ]), 
                            # preselect = "Kenya",
                            multiple = FALSE, # Check if multiple countries or values is doable
                            title = "Choose one polygon to process",
                            gui = .GUI
  )
  adm1_country <- adm1.input$res
  
  extent_mask1 %>%
    terra::subset(.$NAME_1 == adm1_country) %>%
    terra::writeVector("InVector/extent_analysis.gpkg", overwrite = TRUE)

}

# Copy new mask and analysis to **SouceData** in GlobalMoFuSS

# Copy to MoFuSS ----
choose_directory2 = function(caption = "Choose your MoFuSS working directory") {
  if(.Platform$OS.type == "unix")  {
    setwd(tk_choose.dir("/home/mofuss/Documents", caption = caption))
  } else {
    setwd(choose.dir("/home/mofuss/Documents", caption = caption)) # Elegir bien esta carpeta de inicio
  }
}
choose_directory2()
mofussdir <- getwd()

#ADM 0
file.copy(from=paste0(admindir,"/InVector/extent_mask.gpkg"),
          to=paste0(mofussdir,"/LULCC/SourceData/InVector"),
          overwrite = TRUE)

file.copy(from=paste0(admindir,"/InVector/extent_analysis.gpkg"),
          to=paste0(mofussdir,"/LULCC/SourceData/InVector"),
          overwrite = TRUE)

file.copy(from=paste0(admindir,"/InVector/extent_mask.gpkg"),
          to=paste0(mofussdir,"/LULCC/DownloadedDatasets/SourceDataGlobal/InVector"),
          overwrite = TRUE)

file.copy(from=paste0(admindir,"/InVector/extent_analysis.gpkg"),
          to=paste0(mofussdir,"/LULCC/DownloadedDatasets/SourceDataGlobal/InVector"),
          overwrite = TRUE)

#ADM 1
file.copy(from=paste0(admindir,"/InVector/extent_mask1.gpkg"),
          to=paste0(mofussdir,"/LULCC/SourceData/InVector"),
          overwrite = TRUE)

file.copy(from=paste0(admindir,"/InVector/extent_analysis1.gpkg"),
          to=paste0(mofussdir,"/LULCC/SourceData/InVector"),
          overwrite = TRUE)

file.copy(from=paste0(admindir,"/InVector/extent_mask1.gpkg"),
          to=paste0(mofussdir,"/LULCC/DownloadedDatasets/SourceDataGlobal/InVector"),
          overwrite = TRUE)

file.copy(from=paste0(admindir,"/InVector/extent_analysis1.gpkg"),
          to=paste0(mofussdir,"/LULCC/DownloadedDatasets/SourceDataGlobal/InVector"),
          overwrite = TRUE)

#ADM 2
file.copy(from=paste0(admindir,"/InVector/extent_mask2.gpkg"),
          to=paste0(mofussdir,"/LULCC/SourceData/InVector"),
          overwrite = TRUE)

file.copy(from=paste0(admindir,"/InVector/extent_analysis2.gpkg"),
          to=paste0(mofussdir,"/LULCC/SourceData/InVector"),
          overwrite = TRUE)

file.copy(from=paste0(admindir,"/InVector/extent_mask2.gpkg"),
          to=paste0(mofussdir,"/LULCC/DownloadedDatasets/SourceDataGlobal/InVector"),
          overwrite = TRUE)

file.copy(from=paste0(admindir,"/InVector/extent_analysis2.gpkg"),
          to=paste0(mofussdir,"/LULCC/DownloadedDatasets/SourceDataGlobal/InVector"),
          overwrite = TRUE)





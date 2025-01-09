# MoFuSS
# Version 3
# Date: Mar 2024

# 2dolist ----
# Fix for linux cluster
# Land Use Land Cover Module

# Internal parameters ----
# Attraction buffer zones (in linear meters)
w0 = 0 
w1 = 50000 #250000
w2 = 100000 #500000
w3 = 150000 #750000
w4 = 200000 #1000000

# # Select MoFuSS platform:
# webmofuss = 1 # "1" is  web-MoFuSS running in our Ubuntu server, "0" is localcal host (Windows or Linux)
# source(paste0(scriptsmofuss,"00_webmofuss.R"))

# Load libraries ----
library(readr)
library(dplyr)
library(fasterize)
library(glue)
library(igraph)
library(raster)
library(rgl)
library(sf)
library(tictoc)
library(stars)
library(gitlabr)
library(inline)
library(tidyverse)
library(spam)
library(svDialogs)
library(terra)
library(readxl)
library(purrr)
library(tcltk)

setwd(countrydir)
getwd()
country_name

# Read parameters table ----
read.csv("LULCC/TempTables/Country.csv") %>%
  dplyr::filter(Key. == "1") %>%
  pull(Country) -> country_name
country_parameters <- read_csv(paste0("LULCC/DownloadedDatasets/SourceData",country_name,"/",parameters_file))
print(tibble::as_tibble(country_parameters), n=100)

country_parameters %>%
  dplyr::filter(Var == "proj_gcs") %>%
  pull(ParCHR) -> proj_gcs #GCSproj

country_parameters %>%
  dplyr::filter(Var == "epsg_gcs") %>%
  pull(ParCHR) %>%
  as.integer(.) -> epsg_gcs #epsg_gcs

country_parameters %>%
  dplyr::filter(Var == "proj_pcs") %>%
  pull(ParCHR) -> proj_pcs #UTMproj

country_parameters %>%
  dplyr::filter(Var == "epsg_pcs") %>%
  pull(ParCHR) %>%
  as.integer(.) -> epsg_pcs #epsg_utm

country_parameters %>%
  dplyr::filter(Var == "proj_authority") %>%
  pull(ParCHR) -> proj_authority

country_parameters %>%
  dplyr::filter(Var == "LULCt1map") %>%
  pull(ParCHR) -> LULCt1map

country_parameters %>%
  dplyr::filter(Var == "LULCt2map") %>%
  pull(ParCHR) -> LULCt2map

country_parameters %>%
  dplyr::filter(Var == "LULCt3map") %>%
  pull(ParCHR) -> LULCt3map

country_parameters %>%
  dplyr::filter(Var == "nameuser") %>%
  pull(ParCHR) -> nameuser

country_parameters %>%
  dplyr::filter(Var == "ads") %>%
  pull(ParCHR) -> ads

country_parameters %>%
  dplyr::filter(Var == "ads_ctry") %>%
  pull(ParCHR) -> ads_ctry

country_parameters %>%
  dplyr::filter(Var == "GEE_scale") %>%
  pull(ParCHR) %>%
  as.integer(.) -> resolution

country_parameters %>%
  dplyr::filter(Var == "GEpoly") %>%
  pull(ParCHR) %>%
  as.integer(.) -> GEpoly

country_parameters %>%
  dplyr::filter(Var == "scenario_ver") %>%
  pull(ParCHR) -> scenario_ver

country_parameters %>%
  dplyr::filter(Var == "byregion") %>%
  pull(ParCHR) -> byregion

country_parameters %>%
  dplyr::filter(Var == "add_subadmin") %>%
  pull(ParCHR) -> add_subadmin

# Select a region
if (byregion == "Continental") {
  country_parameters %>%
    dplyr::filter(Var == "region2BprocessedCont") %>%
    pull(ParCHR) -> mofuss_region
} else if (byregion == "Regional") {
  country_parameters %>%
    dplyr::filter(Var == "region2BprocessedReg") %>%
    pull(ParCHR) -> mofuss_region
} else if (byregion == "Country") {
  country_parameters %>%
    dplyr::filter(Var == "region2BprocessedCtry") %>%
    pull(ParCHR) -> mofuss_region
} else {
  print("Error")
}

# Define paths and patterns to delete
paths_to_clean <- list(
  "Debugging/*.*",
  "Temp/*.*",
  "HTML_animation*/*",
  "*.Rout",
  # "*.txt", # Uncomment to erase specific txt files
  "*.log",
  "Mofuss_Summary_Report.aux",
  "Mofuss_Summary_Report.lof",
  "Mofuss_Summary_Report.lot",
  "Mofuss_Summary_Report.out",
  "Mofuss_Summary_Report.toc",
  "In/*.*",
  "In/IDW_boost/*.*",
  "Out*/*",
  "LaTeX/*.pdf",
  "LaTeX/*.mp4",
  "LaTeX/*.csv",
  "LaTeX/SimLength.txt",
  "LaTeX/MCruns.txt",
  "Summary_Report/*.*",
  "Logs/*.*",
  "LULCC/TempRaster/*.*",
  "LULCC/TempTables/Friction_drivingoverroads.csv",
  "LULCC/TempTables/Friction_drivingoverroads_calcdist.csv",
  "LULCC/TempTables/Friction_rivers_reclass.csv",
  "LULCC/TempTables/Friction_rivers_reclass_calcdist.csv",
  "LULCC/TempTables/Friction_lakes_reclass.csv",
  "LULCC/TempTables/Friction_lakes_reclass_calcdist.csv",
  "LULCC/TempTables/Friction_borders_reclass.csv",
  "LULCC/TempTables/Friction_borders_reclass_calcdist.csv",
  "LULCC/TempTables/Friction_walkingcrosscountry.csv",
  "LULCC/TempTables/Friction_walkingoverroads.csv",
  "LULCC/TempTables/Supply_parameters.csv",
  "LULCC/TempTables/Resolution.csv",
  "LULCC/TempVector/*.*",
  "LULCC/TempVector_GCS/*.*",
  # "LULCC//*.txt", # Uncomment to delete specific txt files
  "LULCC/*.csv",	
  "LULCC/InTables//fwuse*.*",
  "LULCC/CroppingCountrySpecific.R",
  "LULCC/CroppingCountrySpecific.Rout"
)

# Apply unlink to all paths
walk(paths_to_clean, ~ unlink(.x, recursive = TRUE, force = TRUE))

# Define directories to remove if they exist
dirs_to_check <- c(
  "LULCC/lucdynamics_luc1", 
  "LULCC/lucdynamics_luc2", 
  "LULCC/lucdynamics_luc3"
)

# Check and remove directories if they exist
walk(dirs_to_check, ~ if (dir.exists(.x)) unlink(.x, recursive = TRUE, force = TRUE))

Country <- readLines("LULCC/TempTables/Country.txt")

flist <- list.files("LULCC/SourceData/InTables")
file.copy(paste0("LULCC/SourceData/InTables/",flist), "LULCC/TempTables")

## Read supply parameters table, checking if its delimiter is comma or semicolon ####

growth_parameters1 <- if (file.exists("LULCC/TempTables/growth_parameters1.csv") == TRUE) {
  read_csv("LULCC/TempTables/growth_parameters1.csv") %>% 
    {if(is.null(.$TOF[1])) read_csv2("LULCC/TempTables/growth_parameters1.csv") else .} 
}

growth_parameters2 <- if (file.exists("LULCC/TempTables/growth_parameters2.csv") == TRUE) {
  read_csv("LULCC/TempTables/growth_parameters2.csv") %>% 
    {if(is.null(.$TOF[1])) read_csv2("LULCC/TempTables/growth_parameters2.csv") else .}
}

growth_parameters3 <- if (file.exists("LULCC/TempTables/growth_parameters3.csv") == TRUE) {
  read_csv("LULCC/TempTables/growth_parameters3.csv") %>% 
    {if(is.null(.$TOF[1])) read_csv2("LULCC/TempTables/growth_parameters3.csv") else .}
}

# Save resolution as csv for down stream uses
write.csv(resolution, "LULCC/TempTables/Resolution.csv")

#save user data as txt for down stream uses
writeLines(paste0(gsub("_"," ",nameuser),", from the ",
                  gsub("_"," ",ads),", in ", 
                  gsub("_"," ",ads_ctry),","),"LULCC/TempTables/UserData.txt", useBytes=T)
readLines("LULCC/TempTables/UserData.txt")

# AoI SELECTION **STARTS** ----

if (GEpoly == 1) {
  
  # Define source and destination directories
  source_dir <- paste0(countrydir, "/LULCC/DownloadedDatasets/SourceDataMadagascar/InVector_GCS/")
  destination_dir <- paste0(countrydir, "/LULCC/SourceData/InVector_GCS/")
  # List all .kml files in the source directory
  kml_files <- list.files(path = source_dir, pattern = "\\.kml$", full.names = TRUE)
  # Copy the files to the destination directory
  file.copy(from = kml_files, to = destination_dir, overwrite = TRUE)
  
  
  if (length(list.files("LULCC/SourceData/InVector_GCS", ".kml")) == 0) {
    # shell.exec("https://gitlab.com/mofuss/mofuss/-/blob/master/windows/scripts/LULCC/Wizard_imgs/NoKMLs.pdf")
  } else {
    if (length(list.files("LULCC/SourceData/InVector_GCS", ".kml")) > 1) {
      rm(nrbpoly)
      nrbpoly <- tk_choose.files(default = paste0(getwd(), "/LULCC/SourceData/InVector_GCS/*"))
      # Filter out invalid paths (like wildcards)
      nrbpoly <- nrbpoly[!grepl("\\*$", nrbpoly)]
    } else { 
      nrbpoly<-paste0("LULCC/SourceData/InVector_GCS/",list.files("LULCC/SourceData/InVector_GCS", ".kml"))
    }
  }
  polykml = st_read(nrbpoly) 
  dfx =  data.frame(1,"GoogleEarthPoly")
  colnames(dfx) <- c( country_parameters %>%
                        dplyr::filter(Var == "ext_analysis_ID") %>%
                        pull(ParCHR), country_parameters %>%
                        dplyr::filter(Var == "ext_analysis_NAME") %>%
                        pull(ParCHR))
  if (webmofuss == 1){
    bind_cols (polykml, dfx) %>%
      st_zm() %>%
      subset(select=-c(Name,description)) -> userarea_GCS
  } else if (webmofuss == 0){
    bind_cols (polykml, dfx) %>%
      st_zm() %>%
      subset(select=-c(Name,Description)) -> userarea_GCS
    }
  userarea <- st_transform(userarea_GCS, epsg_pcs)
  
  # Vector masks and extents Google Polygon
  setwd(admindir)
  extent_mask0 <- vect(st_read("regions_adm0_p/mofuss_regions0_p.gpkg")) %>%
    terra::subset(.$NAME_0 == mofuss_region)
  mask <- st_as_sf(extent_mask0)
  setwd(countrydir)
  # terra::writeVector(extent_mask0, "InVector/extent_mask.gpkg", overwrite = TRUE)

  # mask <- userarea
  analysisshp <- st_intersection(mask, userarea)
  analysisshp_GCS <- st_transform(analysisshp, epsg_gcs)

  st_write(userarea_GCS, "LULCC/TempVector_GCS/userarea_GCS.gpkg", delete_layer=TRUE)
  st_write(userarea, "LULCC/TempVector/userarea.gpkg", delete_layer=TRUE)
  
  mask_GCS<-st_transform(mask,epsg_gcs)
  st_write (analysisshp,"LULCC/TempVector/ext_analysis.gpkg", delete_layer=TRUE)
  st_write (mask_GCS,"LULCC/TempVector_GCS/mask_gcs.gpkg", delete_layer=TRUE)
  st_write (analysisshp_GCS,"LULCC/TempVector_GCS/ext_analysis_gcs.gpkg", delete_layer=TRUE)
  st_write(mask, "LULCC/SourceData/InVector/extent_mask.gpkg", delete_layer=TRUE)
  
} else if (GEpoly == 0) {
  
  setwd(admindir)
  
  unlink("InVector/extent_*.*")
  
  if (byregion == "Continental"){ ## Continent ---- THIS LEVEL REQUIERS TO BE UPDATED!!!
    
    extent_mask0 <- st_read("regions_adm0_p/mofuss_regions0_p.gpkg") %>%
      dplyr::filter(grepl(mofuss_region,mofuss_reg)) %>%
      dplyr::mutate(ID = seq(1:nrow(.)))
    st_write(extent_mask0, "InVector/extent_mask.gpkg", overwrite = TRUE)
    
    countries.list <- extent_mask0 %>%
      as.data.frame() %>%
      dplyr::select(NAME_0) %>%
      unique() %>%
      arrange(NAME_0)
    
    country.input <- dlgList(as.character(countries.list[ , ]),
                             preselect = "Kenya",
                             multiple = FALSE, # Check if multiple countries or values is doable
                             title = "Choose one country to process",
                             gui = .GUI
    )
    mofuss_country <- country.input$res
    
    extent_mask0 %>%
      terra::subset(.$NAME_0 == mofuss_country) %>% #Remember to change in the parameters table
      st_write("InVector/extent_analysis.gpkg", overwrite = TRUE)
    
    # ADM LEVEL = 1, when running ADM LEVEL = 0
    mask1list <- paste0("regions_adm1_p/",list.files(path = paste0("regions_adm1_p/"),
                                                     pattern = mofuss_region, full.names = FALSE))
    mask1list_sf <- lapply(mask1list, st_read)
    mofuss_regions1_gpkg <- do.call(rbind, mask1list_sf) %>%
      dplyr::mutate(ID = seq(1:nrow(.)))
    st_write(mofuss_regions1_gpkg, "InVector/extent_mask1.gpkg", overwrite = TRUE)
    debugdfadm1 <- mofuss_regions1_gpkg %>% st_drop_geometry()
    
    mofuss_regions1_gpkg %>%
      terra::subset(.$NAME_0 == mofuss_country) %>% # Remember to change in the parameters table FIX
      st_write("InVector/extent_analysis1.gpkg", overwrite = TRUE)
    
    # ADM LEVEL = 2, when running ADM LEVEL = 0 #Slow process, elapsed time:
    mask2list <- paste0("regions_adm2_p/",list.files(path = paste0("regions_adm2_p/"),
                                                     pattern = mofuss_region, full.names = FALSE))
    mask2list_sf <- lapply(mask2list, st_read)
    mofuss_regions2_gpkg <- do.call(rbind, mask2list_sf) %>%
      dplyr::mutate(ID = seq(1:nrow(.)))
    st_write(mofuss_regions2_gpkg, "InVector/extent_mask2.gpkg", overwrite = TRUE)
    debugdfadm2 <- mofuss_regions2_gpkg %>% st_drop_geometry()
    
    mofuss_regions2_gpkg %>%
      terra::subset(.$NAME_0 == mofuss_country) %>% # Remember to change in the parameters table FIX
      st_write("InVector/extent_analysis2.gpkg", overwrite = TRUE)
    
  }
  
  
  if (byregion == "Regional"){ ## Regional ----
    
    extent_mask0 <- vect(st_read(paste0("regions_adm0_p/",mofuss_region,"_p.gpkg")))
    terra::writeVector(extent_mask0, "InVector/extent_mask.gpkg", overwrite = TRUE)
    
    countries.list <- extent_mask0 %>%
      as.data.frame() %>%
      dplyr::select(NAME_0) %>%
      unique() %>%
      arrange(NAME_0)
    
    country.input <- dlgList(as.character(countries.list[ , ]),
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
    
    if (add_subadmin == "YES") {
      extent_mask1 %>%
        terra::subset(.$NAME_0 == mofuss_country) %>% #Remember to change in the parameters table
        terra::writeVector("InVector/extent_analysis1.gpkg", overwrite = TRUE)
    }
    
    mofuss_region2 <- gsub(0,2, mofuss_region)
    extent_mask2 <- vect(paste0("regions_adm2_p/",mofuss_region2,"_p.gpkg"))
    terra::writeVector(extent_mask2, "InVector/extent_mask2.gpkg", overwrite = TRUE)
    
    if (add_subadmin == "YES") {
      extent_mask2 %>%
        terra::subset(.$NAME_0 == mofuss_country) %>% #Remember to change in the parameters table
        terra::writeVector("InVector/extent_analysis2.gpkg", overwrite = TRUE)
    }
    
  }
  
  if (byregion == "Country"){ ## Country ----
    
    extent_mask0 <- vect(st_read("regions_adm0_p/mofuss_regions0_p.gpkg")) %>%
      terra::subset(.$NAME_0 == mofuss_region)
    terra::writeVector(extent_mask0, "InVector/extent_mask.gpkg", overwrite = TRUE)
    
    extent_mask1 <- vect(st_read("regions_adm1_p/mofuss_regions1_p.gpkg")) %>%
      terra::subset(.$NAME_0 == mofuss_region)
    terra::writeVector(extent_mask1, "InVector/extent_mask1.gpkg", overwrite = TRUE)
    
    extent_mask2 <- vect(st_read("regions_adm2_p/mofuss_regions2_p.gpkg")) %>%
      terra::subset(.$NAME_0 == mofuss_region)
    terra::writeVector(extent_mask2, "InVector/extent_mask2.gpkg", overwrite = TRUE)
    
    if (add_subadmin == "YES") {
      
      adm1.list <- extent_mask1 %>%
        as.data.frame() %>%
        dplyr::select(NAME_1) %>%
        unique() %>%
        arrange(NAME_1)
      
      adm1.input <- dlgList(as.character(adm1.list[ , ]),
                            # preselect = "Kenya",
                            multiple = FALSE, # Check if multiple countries or values is doable
                            title = "Choose one polygon to process",
                            gui = .GUI
      )
      adm1_country <- adm1.input$res
      
      extent_mask1 %>%
        terra::subset(.$NAME_1 == adm1_country) %>%
        terra::writeVector("InVector/extent_analysis.gpkg", overwrite = TRUE)
      
      extent_mask2 %>%
        terra::subset(.$NAME_1 == adm1_country) %>%
        terra::writeVector("InVector/extent_analysis1.gpkg", overwrite = TRUE)
      
      extent_mask2 %>% # Repeats previous admin 2 level for being a country crop
        terra::subset(.$NAME_1 == adm1_country) %>%
        terra::writeVector("InVector/extent_analysis2.gpkg", overwrite = TRUE)
    } else {
      # terra::writeVector(extent_mask, "InVector/extent_analysis.gpkg", overwrite = TRUE)
      # terra::writeVector(extent_mask1, "InVector/extent_analysis1.gpkg", overwrite = TRUE)
      # terra::writeVector(extent_mask2, "InVector/extent_analysis2.gpkg", overwrite = TRUE)
    }

  }
  
  # Copy to MoFuSS ----
  setwd(countrydir)
  
  #ADM 0
  file.copy(from=paste0(admindir,"/InVector/extent_mask.gpkg"),
            to=paste0(countrydir,"/LULCC/SourceData/InVector"),
            overwrite = TRUE)
  
  # file.copy(from=paste0(admindir,"/InVector/extent_analysis.gpkg"),
  #           to=paste0(countrydir,"/LULCC/SourceData/InVector"),
  #           overwrite = TRUE)
  
  #ADM 1
  file.copy(from=paste0(admindir,"/InVector/extent_mask1.gpkg"),
            to=paste0(countrydir,"/LULCC/SourceData/InVector"),
            overwrite = TRUE)
  
  # file.copy(from=paste0(admindir,"/InVector/extent_analysis1.gpkg"),
  #           to=paste0(countrydir,"/LULCC/SourceData/InVector"),
  #           overwrite = TRUE)
  
  #ADM 2
  file.copy(from=paste0(admindir,"/InVector/extent_mask2.gpkg"),
            to=paste0(countrydir,"/LULCC/SourceData/InVector"),
            overwrite = TRUE)
  
  # file.copy(from=paste0(admindir,"/InVector/extent_analysis2.gpkg"),
  #           to=paste0(countrydir,"/LULCC/SourceData/InVector"),
  #           overwrite = TRUE)
  
  if (add_subadmin == "YES") {
    #ADM 0
    file.copy(from=paste0(admindir,"/InVector/extent_analysis.gpkg"),
              to=paste0(countrydir,"/LULCC/SourceData/InVector"),
              overwrite = TRUE)
    #ADM 1
    file.copy(from=paste0(admindir,"/InVector/extent_analysis1.gpkg"),
              to=paste0(countrydir,"/LULCC/SourceData/InVector"),
              overwrite = TRUE)
    #ADM 2
    file.copy(from=paste0(admindir,"/InVector/extent_analysis2.gpkg"),
              to=paste0(countrydir,"/LULCC/SourceData/InVector"),
              overwrite = TRUE)
    userarea <- st_read("LULCC/SourceData/InVector/extent_analysis.gpkg")
    userarea_GCS <- st_transform(userarea, epsg_gcs)
    
    userarea1 <- st_read("LULCC/SourceData/InVector/extent_analysis1.gpkg")
    userarea_GCS1 <- st_transform(userarea, epsg_gcs)
    
    userarea2 <- st_read("LULCC/SourceData/InVector/extent_analysis2.gpkg")
    userarea_GCS2 <- st_transform(userarea, epsg_gcs)
    
  } else {
    
    userarea <- st_read("LULCC/SourceData/InVector/extent_mask.gpkg")
    userarea_GCS <- st_transform(userarea, epsg_gcs)
    
    userarea1 <- st_read("LULCC/SourceData/InVector/extent_mask1.gpkg")
    userarea_GCS1 <- st_transform(userarea, epsg_gcs)
    
    userarea2 <- st_read("LULCC/SourceData/InVector/extent_mask2.gpkg")
    userarea_GCS2 <- st_transform(userarea, epsg_gcs)
    
  }
  
  # Vector masks and extents GADM_AOI
  mask <- userarea
  analysisshp <- st_intersection(mask, userarea)
  analysisshp_GCS <- st_transform(analysisshp, epsg_gcs)
  
  mask1 <- userarea1
  analysisshp1 <- st_intersection(mask1, userarea1)
  analysisshp_GCS1 <- st_transform(analysisshp1, epsg_gcs)
  
  mask2 <- userarea2
  analysisshp2 <- st_intersection(mask2, userarea2)
  analysisshp_GCS2 <- st_transform(analysisshp2, epsg_gcs)
  
  st_write(userarea_GCS, "LULCC/TempVector_GCS/userarea_GCS.gpkg", delete_layer=TRUE)
  st_write(userarea, "LULCC/TempVector/userarea.gpkg", delete_layer=TRUE)
  
  st_write(userarea_GCS1, "LULCC/TempVector_GCS/userarea_GCS1.gpkg", delete_layer=TRUE)
  st_write(userarea1, "LULCC/TempVector/userarea1.gpkg", delete_layer=TRUE)
  
  st_write(userarea_GCS2, "LULCC/TempVector_GCS/userarea_GCS2.gpkg", delete_layer=TRUE)
  st_write(userarea2, "LULCC/TempVector/userarea2.gpkg", delete_layer=TRUE)
  
  # For future use in figures and IDW_Boost  ####
  mask_GCS<-st_transform(mask,epsg_gcs)
  st_write (analysisshp,"LULCC/TempVector/ext_analysis.gpkg", delete_layer=TRUE)
  st_write (mask_GCS,"LULCC/TempVector_GCS/mask_gcs.gpkg", delete_layer=TRUE)
  st_write (analysisshp_GCS,"LULCC/TempVector_GCS/ext_analysis_gcs.gpkg", delete_layer=TRUE)
  mask_GCS1<-st_transform(mask1,epsg_gcs)
  st_write (analysisshp1,"LULCC/TempVector/ext_analysis1.gpkg", delete_layer=TRUE)
  st_write (mask_GCS1,"LULCC/TempVector_GCS/mask_gc1s.gpkg", delete_layer=TRUE)
  st_write (analysisshp_GCS1,"LULCC/TempVector_GCS/ext_analysis_gcs1.gpkg", delete_layer=TRUE)
  mask_GCS2<-st_transform(mask2,epsg_gcs)
  st_write (analysisshp2,"LULCC/TempVector/ext_analysis2.gpkg", delete_layer=TRUE)
  st_write (mask_GCS2,"LULCC/TempVector_GCS/mask_gcs2.gpkg", delete_layer=TRUE)
  st_write (analysisshp_GCS2,"LULCC/TempVector_GCS/ext_analysis_gcs2.gpkg", delete_layer=TRUE)
  
} else {
  print("NO SELECTION AREA")
}
# AoI SELECTION **ENDS** ----


# Create a raster mask of a certain size size, extent, resolution and projection as a provided raster template for the following operations. ####
userarea_ras <- raster(userarea, res=resolution)
userarea_ras <- fasterize(userarea, userarea_ras,
                          field = country_parameters %>%
                            dplyr::filter(Var == "ext_analysis_ID") %>%
                            pull(ParCHR))
userarea_r_m <- crop(userarea_ras, extent(mask)) %>%
  mask(mask)
userarea_r <- (userarea_r_m*0)+1
writeRaster(userarea_r, filename="LULCC/TempRaster/mask_c.tif", datatype="INT2S", overwrite=TRUE)

mask_r <- fasterize(mask, userarea_r,
                    field = country_parameters %>%
                      dplyr::filter(Var == "ext_analysis_ID") %>%
                      pull(ParCHR))

mask_r_m <- crop(mask_r, extent(userarea_r)) %>%
  mask(userarea_r)

writeRaster(mask_r_m, filename="LULCC/TempRaster/admin_c.tif", datatype="INT2S", overwrite=TRUE)

if (GEpoly != 1) {
  userarea_ras1 <- fasterize(userarea1, userarea_ras,
                             field = country_parameters %>%
                               dplyr::filter(Var == "ext_analysis_ID_1") %>%
                               pull(ParCHR))
  userarea_r_m1 <- crop(userarea_ras1, extent(mask)) %>%
    mask(mask)
  userarea_r1 <- (userarea_r_m1*0)+1
  writeRaster(userarea_r1, filename="LULCC/TempRaster/mask_c1.tif", datatype="INT2S", overwrite=TRUE)
  
  mask_r1 <- fasterize(mask1, userarea_r1,
                       field = country_parameters %>%
                         dplyr::filter(Var == "ext_analysis_ID_1") %>%
                         pull(ParCHR))
  
  mask_r_m1 <- crop(mask_r1, extent(userarea_r1)) %>%
    mask(userarea_r1)
  
  writeRaster(mask_r_m1, filename="LULCC/TempRaster//admin_c1.tif", datatype="INT2S", overwrite=TRUE)
  
  userarea_ras2 <- fasterize(userarea2, userarea_ras,
                             field = country_parameters %>%
                               dplyr::filter(Var == "ext_analysis_ID_2") %>%
                               pull(ParCHR))
  userarea_r_m2 <- crop(userarea_ras2, extent(mask)) %>%
    mask(mask)
  userarea_r2 <- (userarea_r_m2*0)+1
  writeRaster(userarea_r2, filename="LULCC/TempRaster/mask_c2.tif", datatype="INT2S", overwrite=TRUE)
  
  mask_r2 <- fasterize(mask2, userarea_r2,
                       field = country_parameters %>%
                         dplyr::filter(Var == "ext_analysis_ID_2") %>%
                         pull(ParCHR))
  
  mask_r_m2 <- crop(mask_r2, extent(userarea_r2)) %>%
    mask(userarea_r2)
  
  writeRaster(mask_r_m2, filename="LULCC/TempRaster//admin_c2.tif", datatype="INT2S", overwrite=TRUE)
  
}

# userarea_r_GCS <- raster(userarea_GCS, res=0.0008333333)
# userarea_r_codes_GCS <- fasterize(userarea_GCS, userarea_r_codes_GCS, field = ext_analysis_ID)
# userarea_r_GCS<-(userarea_r_codes_GCS*0)+1

###############################TERRA#################################
# # Transform simple features to SpatVector and SpatRaster ####
# 
# mapview(mask) +
#   mapview(userarea)
# 
# userarea <- vect(userarea)
# 
# userarea_r <- rast(userarea_r)
# 
# 
# 
# # process DEM #### Con terra genera un mapa incompatible para dinamica. 
# dtem <- rast("LULCC/SourceData/InRaster/DTEM.tif")
# if (res(dtem)[1] == resolution){
#   dtem_c <- crop(dtem, ext(userarea) + .01) %>%
#     terra::mask(userarea)
#   plot(dtem_c)
#   lines(userarea)
#   writeRaster(dtem_c, filename="LULCC/TempRaster//DEM_c.tif", datatype="INT2S", overwrite=TRUE)
# } else {
#   # DEM_r_m <- dtem %>%
#   #   crop(extent(userarea_r)) %>%
#   #   resample(userarea_r, "bilinear") %>%
#   #   mask(userarea_r)  
#   # writeRaster(DEM_r_m, filename="LULCC/TempRaster//DEM_c.tif", datatype="INT2S", overwrite=TRUE)
#   dtem_c <- crop(dtem, ext(userarea) + .01) %>%
#     resample(userarea_r, "bilinear") %>%
#     terra::mask(userarea)
#   writeRaster(DEM_r_m, filename="LULCC/TempRaster//DEM_c.tif", datatype="INT2S", overwrite=TRUE)
# }
# 
# 
# # Transform SpatVector and SpatRaster to simple features or raster ####
# 
# userarea <- st_as_sf(userarea)
# 
# userarea_r <- raster(userarea_r)
#################################TERRA#####################################
##### VOY POR ACÁ!! Pasar a a Terra

# process DEM ----
country_parameters %>%
  dplyr::filter(Var == "DTEM_name") %>%
  pull(ParCHR) -> DTEM_name
dtem <- raster(paste0("LULCC/SourceData/InRaster/",DTEM_name))
DEM_r_m <- dtem %>%
  crop(extent(userarea_r)) %>%
  raster::resample(userarea_r, "bilinear") %>%
  mask(userarea_r)
writeRaster(DEM_r_m, filename="LULCC/TempRaster/DEM_c.tif", datatype="INT2S", overwrite=TRUE)

# tree cover, forest loss and forest gain ####
country_parameters %>%
  dplyr::filter(Var == "treecover_name") %>%
  pull(ParCHR) -> treecover_name
raster(paste0("LULCC/SourceData/InRaster/",treecover_name)) %>%
  crop(extent(userarea_r)) %>%
  raster::resample(userarea_r, "bilinear") %>%
  mask(userarea_r) -> tc2000_r_m 
writeRaster(tc2000_r_m, filename="LULCC/TempRaster/tc2000_c.tif", datatype="INT2S", overwrite=TRUE)

country_parameters %>% # Something weird here, it takes too long
  dplyr::filter(Var == "gain_name") %>%
  pull(ParCHR) -> gain_name
raster(paste0("LULCC/SourceData/InRaster/",gain_name)) %>%
  crop(extent(userarea_r)) %>%
  raster::resample(userarea_r, "ngb") %>%
  mask(userarea_r) -> gain_r_m 
writeRaster(gain_r_m, filename="LULCC/TempRaster/gain_c.tif", datatype="INT2S", overwrite=TRUE)

country_parameters %>%
  dplyr::filter(Var == "lossyear_name") %>%
  pull(ParCHR) -> lossyear_name
raster(paste0("LULCC/SourceData/InRaster/",lossyear_name)) %>%
  crop(extent(userarea_r)) %>%
  raster::resample(userarea_r, "ngb") %>%
  mask(userarea_r) -> lossyear_r_m 
writeRaster(lossyear_r_m, filename="LULCC/TempRaster/lossyear_c.tif", datatype="INT2S", overwrite=TRUE)

# memory.limit(size=56000)
# Loss maps annualizations for LULCC modeling ####
raster("LULCC/TempRaster/lossyear_c.tif") %>%
  {.} %in% 1:20 %>% 
  writeRaster(filename="LULCC/TempRaster/lossyear01_20_c.tif", datatype="INT2S", overwrite=TRUE)

raster("LULCC/TempRaster/lossyear_c.tif") %>%
  {.} %in% 1:10 %>% 
  writeRaster(filename="LULCC/TempRaster/lossyear01_10_c.tif", datatype="INT2S", overwrite=TRUE)

# Annual losses using apply or map ####
tic()
seq(1:20) %>% 
  walk(function(i){
    lossyear_r_m[lossyear_r_m == i] <- 1
    lossyear_r_m[lossyear_r_m != i] <- 0
    #lossyear_r_m[lossyear_r_m != i | is.na(lossyear_r_m)] <- 0
    lossyear_r_m %>%
      writeRaster(str_c("LULCC/TempRaster/AnnLoss", str_pad(i, width = 2, pad = "0"),".tif"), datatype = "INT2S",overwrite = T)
    })
toc()

# Annual losses using apply or map PARALLELIZED --- try in linux ####
# tic()
# plan(multisession, workers = 8)
# seq(1:12) %>%
#   future_map(function(i){
#     lossyear_r_m[lossyear_r_m == i] <- 1
#     lossyear_r_m[lossyear_r_m != i] <- 0
#     #lossyear_r_m[lossyear_r_m != i | is.na(lossyear_r_m)] <- 0
#     lossyear_r_m %>%
#       writeRaster(str_c("TempRaster/AnnLoss", str_pad(i, width = 2, pad = "0"),".tif"), datatype = "INT2S",overwrite = T)
#   })
# toc()

# LULC maps ####
country_parameters %>%
  dplyr::filter(Var == "LULCt1map") %>%
  pull(ParCHR) -> LULCt1map
if (identical(LULCt1map, NA_character_)) {
  print("No LULCt1 map available")
  } else if (LULCt1map == "YES"){
    raster(paste0("LULCC/SourceData/InRaster/",country_parameters %>% # NAME IS HARDWIRED! ONE YEAR ONLY
                    dplyr::filter(Var == "LULCt1map_name") %>%
                    pull(ParCHR))) %>%
      crop(extent(userarea_r)) %>%
      raster::resample(userarea_r, "ngb") %>%
      mask(userarea_r) -> LULCt1_r_m
    writeRaster(LULCt1_r_m, filename="LULCC/TempRaster/LULCt1_c.tif", datatype="INT2S", overwrite=TRUE)
    } else {
      "No LULCt1 map available"
    }

country_parameters %>%
  dplyr::filter(Var == "LULCt2map") %>%
  pull(ParCHR) -> LULCt2map
if (identical(LULCt2map, NA_character_)) {
  "No LULCt2 map available"
} else if (LULCt2map == "YES"){
  raster(paste0("LULCC/SourceData/InRaster/",country_parameters %>%
                  dplyr::filter(Var == "LULCt2map_name") %>%
                  pull(ParCHR))) %>%
    crop(extent(userarea_r)) %>%
    raster::resample(userarea_r, "ngb") %>%
    mask(userarea_r) -> LULCt2_r_m
  writeRaster(LULCt2_r_m, filename="LULCC/TempRaster/LULCt2_c.tif", datatype="INT2S", overwrite=TRUE)
} else {
  "No LULCt2 map available"
}

country_parameters %>%
  dplyr::filter(Var == "LULCt3map") %>%
  pull(ParCHR) -> LULCt3map
if (identical(LULCt3map, NA_character_)) {
  "No LULCt3 map available"
} else if (LULCt3map == "YES"){
  raster(paste0("LULCC/SourceData/InRaster/",country_parameters %>%
                  dplyr::filter(Var == "LULCt3map_name") %>%
                  pull(ParCHR))) %>%
    crop(extent(userarea_r)) %>%
    raster::resample(userarea_r, "ngb") %>%
    mask(userarea_r) -> LULCt3_r_m
  writeRaster(LULCt3_r_m, filename="LULCC/TempRaster/LULCt3_c.tif", datatype="INT2S", overwrite=TRUE)
} else {
  "No LULCt3 map available"
}

# Reclassify LULC map into TOF (1) and FOR (0) binary map as mask for LULCC analysis mask #### 

if (identical(LULCt1map, NA_character_)) {
  "No LULCt1 map available"
} else if (LULCt1map == "YES"){
  TOFvsFOR_matrix1 <- as.matrix(growth_parameters1[,c("Key*","TOF")])
  reclassify(LULCt1_r_m, TOFvsFOR_matrix1,
             filename="LULCC/TempRaster/TOFvsFOR_mask1.tif",
             datatype="INT2S", overwrite=TRUE)
} else {
  "No LULCt1 map available"
}


if (identical(LULCt2map, NA_character_)) {
  "No LULCt2 map available"
} else if (LULCt2map == "YES"){
  TOFvsFOR_matrix2 <- as.matrix(growth_parameters2[,c("Key*","TOF")])
  reclassify(LULCt2_r_m, TOFvsFOR_matrix2,
             filename="LULCC/TempRaster/TOFvsFOR_mask2.tif",
             datatype="INT2S", overwrite=TRUE)
} else {
  "No LULCt2 map available"
}


if (identical(LULCt3map, NA_character_)) {
  "No LULCt3 map available"
} else if (LULCt3map == "YES"){
  TOFvsFOR_matrix3 <- as.matrix(growth_parameters3[,c("Key*","TOF")])
  reclassify(LULCt3_r_m, TOFvsFOR_matrix3,
             filename="LULCC/TempRaster/TOFvsFOR_mask3.tif",
             datatype="INT2S", overwrite=TRUE)
} else {
  "No LULCt3 map available"
}
  
# AGB maps ----

country_parameters %>%
  dplyr::filter(Var == "AGB1map") %>%
  pull(ParCHR) -> AGB1map
if (identical(country_parameters %>%
              dplyr::filter(Var == "AGB1map") %>%
              pull(ParCHR), NA_character_)) {
  print("No AGB1 map available")
} else if (AGB1map == "YES"){
  if (resolution == 1000) {
    raster(paste0("LULCC/SourceData/InRaster/",country_parameters %>%
                    dplyr::filter(Var == "AGB1map_name") %>%
                    pull(ParCHR))) %>%
      crop(extent(userarea_r)) %>%
      raster::resample(userarea_r, "bilinear") %>%
      mask(userarea_r) * ((resolution^2)/(100^2)) -> outagb
    writeRaster(outagb, filename="LULCC/TempRaster//agb_c1.tif", datatype="INT4S", overwrite=TRUE)
  } else if (resolution == 100) { # USES TERRA FOR CTREES MAPS
    outagbnull <- rast(paste0("LULCC/SourceData/InRaster/",country_parameters %>%
                    dplyr::filter(Var == "AGB1map_name") %>%
                    pull(ParCHR)))
    outagb <- ifel(outagbnull < 0, 0, outagbnull) %>%
      terra::crop(ext(rast(userarea_r))) %>%
      terra::resample(rast(userarea_r), "bilinear") %>% 
      app(fun = as.integer)
    writeRaster(outagb, filename="LULCC/TempRaster//agb_c1.tif", datatype="INT4S", overwrite=TRUE)
  }
} else {
  "No AGB1 map available"
}

# country_parameters %>%
#   dplyr::filter(Var == "AGB2map") %>%
#   pull(ParCHR) -> AGB2map
# if (identical(country_parameters %>%
#               dplyr::filter(Var == "AGB2map") %>%
#               pull(ParCHR), NA_character_)) {
#   print("No AGB2 map available")
# } else if (AGB2map == "YES"){
#   raster(paste0("LULCC/SourceData/InRaster/",country_parameters %>%
#                   dplyr::filter(Var == "AGB2map_name") %>%
#                   pull(ParCHR))) %>%
#     crop(extent(userarea_r)) %>%
#     raster::resample(userarea_r, "bilinear") %>%
#     mask(userarea_r) %>% 
#     writeRaster(filename="LULCC/TempRaster//agb_c2.tif", datatype="INT4S", overwrite=TRUE)
# } else {
#   "No AGB2 map available"
# }
# 
# country_parameters %>%
#   dplyr::filter(Var == "AGB3map") %>%
#   pull(ParCHR) -> AGB3map
# if (identical(country_parameters %>%
#               dplyr::filter(Var == "AGB3map") %>%
#               pull(ParCHR), NA_character_)) { 
#   print("No AGB3 map available")
# } else if (AGB3map == "YES"){
#   raster(paste0("LULCC/SourceData/InRaster/",country_parameters %>%
#                   dplyr::filter(Var == "AGB3map_name") %>%
#                   pull(ParCHR))) %>%
#     crop(extent(userarea_r)) %>%
#     raster::resample(userarea_r, "bilinear") %>%
#     mask(userarea_r) %>% 
#     writeRaster(filename="LULCC/TempRaster//agb_c3.tif", datatype="INT4S", overwrite=TRUE)
# } else {
#   "No AGB3 map available"
# }

# Protected Areas ----
if (identical(country_parameters %>%
              dplyr::filter(Var == "npa_raster") %>%
              pull(ParCHR), NA_character_)) { # variable exists with no value
  tic()
  npa<-st_read(paste0("LULCC/SourceData/InVector/",country_parameters %>%
                          dplyr::filter(Var == "npa_name") %>%
                          pull(ParCHR)))
  npa_r <- rasterize(npa, userarea_r, country_parameters %>%
                         dplyr::filter(Var == "npa_fieldname") %>%
                         pull(ParCHR))
  npa_c<-npa_r*userarea_r
  writeRaster(npa_c, filename="LULCC/TempRaster/npa_c.tif", datatype="INT2S", overwrite=TRUE)
  toc()
} else if (country_parameters %>%
           dplyr::filter(Var == "npa_raster") %>%
           pull(ParCHR)  == "YES"){
  print("Processing protected areas from raster format")
  raster(paste0("LULCC/SourceData/InRaster/",country_parameters %>%
                  dplyr::filter(Var == "npa_name_r") %>%
                  pull(ParCHR))) %>%
    crop(extent(userarea_r)) %>%
    raster::resample(userarea_r, "ngb") %>%
    mask(userarea_r) %>% 
    writeRaster(filename="LULCC/TempRaster/npa_c.tif", datatype="INT4S", overwrite=TRUE)
} else if (country_parameters %>%
           dplyr::filter(Var == "npa_raster") %>%
           pull(ParCHR)  == character(0)){ # variable doesn't exist, assume vector format
  tic()
  npa<-st_read(paste0("LULCC/SourceData/InVector/",country_parameters %>%
                          dplyr::filter(Var == "npa_name") %>%
                          pull(ParCHR)))
  npa_r <- rasterize(npa, userarea_r, country_parameters %>%
                         dplyr::filter(Var == "npa_fieldname") %>%
                         pull(ParCHR))
  npa_c<-npa_r*userarea_r
  writeRaster(npa_c, filename="LULCC/TempRaster/npa_c.tif", datatype="INT2S", overwrite=TRUE)
  toc()
} else {
  tic()
  npa<-st_read(paste0("LULCC/SourceData/InVector/",country_parameters %>%
                          dplyr::filter(Var == "npa_name") %>%
                          pull(ParCHR)))
  npas_r <- rasterize(npa, userarea_r, country_parameters %>%
                         dplyr::filter(Var == "npa_fieldname") %>%
                         pull(ParCHR))
  npa_c<-npa_r*userarea_r
  writeRaster(npas_c, filename="LULCC/TempRaster/npa_c.tif", datatype="INT2S", overwrite=TRUE)
  toc()
}

# Rivers ----
if (identical(country_parameters %>%
              dplyr::filter(Var == "rivers_raster") %>%
              pull(ParCHR), NA_character_)) { # variable exists with no value
  tic()
  rivers<-st_read(paste0("LULCC/SourceData/InVector/",country_parameters %>%
                           dplyr::filter(Var == "rivers_name") %>%
                           pull(ParCHR)))
  rivers_r <- rasterize(rivers, userarea_r, country_parameters %>%
                          dplyr::filter(Var == "rivers_fieldname") %>%
                          pull(ParCHR))
  rivers_c<-rivers_r*userarea_r
  writeRaster(rivers_c, filename="LULCC/TempRaster/rivers_c.tif", datatype="INT2S", overwrite=TRUE)
  toc()
} else if (country_parameters %>%
           dplyr::filter(Var == "rivers_raster") %>%
           pull(ParCHR)  == "YES"){
  print("Processing rivers from raster format")
  raster(paste0("LULCC/SourceData/InRaster/",country_parameters %>%
                  dplyr::filter(Var == "rivers_name_r") %>%
                  pull(ParCHR))) %>%
    crop(extent(userarea_r)) %>%
    raster::resample(userarea_r, "ngb") %>%
    mask(userarea_r) %>% 
    writeRaster(filename="LULCC/TempRaster/rivers_c.tif", datatype="INT4S", overwrite=TRUE)
} else if (country_parameters %>%
           dplyr::filter(Var == "rivers_raster") %>%
           pull(ParCHR)  == character(0)){ # variable doesn't exist, assume vector format
  tic()
  rivers<-st_read(paste0("LULCC/SourceData/InVector/",country_parameters %>%
                           dplyr::filter(Var == "rivers_name") %>%
                           pull(ParCHR)))
  rivers_r <- rasterize(rivers, userarea_r, country_parameters %>%
                          dplyr::filter(Var == "rivers_fieldname") %>%
                          pull(ParCHR))
  rivers_c<-rivers_r*userarea_r
  writeRaster(rivers_c, filename="LULCC/TempRaster/rivers_c.tif", datatype="INT2S", overwrite=TRUE)
  toc()
} else {
  tic()
  rivers<-st_read(paste0("LULCC/SourceData/InVector/",country_parameters %>%
                           dplyr::filter(Var == "rivers_name") %>%
                           pull(ParCHR)))
  rivers_r <- rasterize(rivers, userarea_r, country_parameters %>%
                          dplyr::filter(Var == "rivers_fieldname") %>%
                          pull(ParCHR))
  rivers_c<-rivers_r*userarea_r
  writeRaster(rivers_c, filename="LULCC/TempRaster/rivers_c.tif", datatype="INT2S", overwrite=TRUE)
  toc()
}

# Lakes ----
if (identical(country_parameters %>%
              dplyr::filter(Var == "lakes_raster") %>%
              pull(ParCHR), NA_character_)) { # variable exists with no value
  tic()
  lakes<-st_read(paste0("LULCC/SourceData/InVector/",country_parameters %>%
                           dplyr::filter(Var == "lakes_name") %>%
                           pull(ParCHR)))
  lakes_r <- rasterize(lakes, userarea_r, country_parameters %>%
                          dplyr::filter(Var == "lakes_fieldname") %>%
                          pull(ParCHR))
  lakes_c<-lakes_r*userarea_r
  writeRaster(lakes_c, filename="LULCC/TempRaster/lakes_c.tif", datatype="INT2S", overwrite=TRUE)
  toc()
} else if (country_parameters %>%
           dplyr::filter(Var == "lakes_raster") %>%
           pull(ParCHR)  == "YES"){
  print("Processing lakes from raster format")
  raster(paste0("LULCC/SourceData/InRaster/",country_parameters %>%
                  dplyr::filter(Var == "lakes_name_r") %>%
                  pull(ParCHR))) %>%
    crop(extent(userarea_r)) %>%
    raster::resample(userarea_r, "ngb") %>%
    mask(userarea_r) %>% 
    writeRaster(filename="LULCC/TempRaster/lakes_c.tif", datatype="INT4S", overwrite=TRUE)
} else if (country_parameters %>%
           dplyr::filter(Var == "lakes_raster") %>%
           pull(ParCHR)  == character(0)){ # variable doesn't exist, assume vector format
  tic()
  lakes<-st_read(paste0("LULCC/SourceData/InVector/",country_parameters %>%
                           dplyr::filter(Var == "lakes_name") %>%
                           pull(ParCHR)))
  lakes_r <- rasterize(lakes, userarea_r, country_parameters %>%
                          dplyr::filter(Var == "lakes_fieldname") %>%
                          pull(ParCHR))
  lakes_c<-lakes_r*userarea_r
  writeRaster(lakes_c, filename="LULCC/TempRaster/lakes_c.tif", datatype="INT2S", overwrite=TRUE)
  toc()
} else {
  tic()
  lakes<-st_read(paste0("LULCC/SourceData/InVector/",country_parameters %>%
                           dplyr::filter(Var == "lakes_name") %>%
                           pull(ParCHR)))
  lakes_r <- rasterize(lakes, userarea_r, country_parameters %>%
                          dplyr::filter(Var == "lakes_fieldname") %>%
                          pull(ParCHR))
  lakes_c<-lakes_r*userarea_r
  writeRaster(lakes_c, filename="LULCC/TempRaster/lakes_c.tif", datatype="INT2S", overwrite=TRUE)
  toc()
}

# Roads ----
if (identical(country_parameters %>%
              dplyr::filter(Var == "roads_raster") %>%
              pull(ParCHR), NA_character_)) { # variable exists with no value
  tic()
  roads<-st_read(paste0("LULCC/SourceData/InVector/",country_parameters %>%
                           dplyr::filter(Var == "roads_name") %>%
                           pull(ParCHR)))
  roads_r <- rasterize(roads, userarea_r, country_parameters %>%
                          dplyr::filter(Var == "roads_fieldname") %>%
                          pull(ParCHR))
  roads_c<-roads_r*userarea_r
  writeRaster(roads_c, filename="LULCC/TempRaster/roads_c.tif", datatype="INT2S", overwrite=TRUE)
  toc()
} else if (country_parameters %>%
           dplyr::filter(Var == "roads_raster") %>%
           pull(ParCHR)  == "YES"){
  print("Processing roads from raster format")
  raster(paste0("LULCC/SourceData/InRaster/",country_parameters %>%
                  dplyr::filter(Var == "roads_name_r") %>%
                  pull(ParCHR))) %>%
    crop(extent(userarea_r)) %>%
    raster::resample(userarea_r, "ngb") %>%
    mask(userarea_r) %>% 
    writeRaster(filename="LULCC/TempRaster//roads_c.tif", datatype="INT4S", overwrite=TRUE)
} else if (country_parameters %>%
           dplyr::filter(Var == "roads_raster") %>%
           pull(ParCHR)  == character(0)){ # variable doesn't exist, assume vector format
  tic()
  roads<-st_read(paste0("LULCC/SourceData/InVector/",country_parameters %>%
                           dplyr::filter(Var == "roads_name") %>%
                           pull(ParCHR)))
  roads_r <- rasterize(roads, userarea_r, country_parameters %>%
                          dplyr::filter(Var == "roads_fieldname") %>%
                          pull(ParCHR))
  roads_c<-roads_r*userarea_r
  writeRaster(roads_c, filename="LULCC/TempRaster/roads_c.tif", datatype="INT2S", overwrite=TRUE)
  toc()
} else {
  tic()
  roads<-st_read(paste0("LULCC/SourceData/InVector/",country_parameters %>%
                           dplyr::filter(Var == "roads_name") %>%
                           pull(ParCHR)))
  roads_r <- rasterize(roads, userarea_r, country_parameters %>%
                          dplyr::filter(Var == "roads_fieldname") %>%
                          pull(ParCHR))
  roads_c<-roads_r*userarea_r
  writeRaster(roads_c, filename="LULCC/TempRaster/roads_c.tif", datatype="INT2S", overwrite=TRUE)
  toc()
}

# Borders ----
if (identical(country_parameters %>%
              dplyr::filter(Var == "borders_raster") %>%
              pull(ParCHR), NA_character_)) { # variable exists with no value
  tic()
  borders <- st_read(paste0("LULCC/SourceData/InVector/",country_parameters %>%
                          dplyr::filter(Var == "borders_name") %>%
                          pull(ParCHR)))
  borders_r <- rasterize(borders, userarea_r, country_parameters %>%
                         dplyr::filter(Var == "borders_fieldname") %>%
                         pull(ParCHR))
  borders_c<-borders_r*userarea_r
  writeRaster(borders_c, filename="LULCC/TempRaster/borders_c.tif", datatype="INT2S", overwrite=TRUE)
  toc()
} else if (country_parameters %>%
           dplyr::filter(Var == "borders_raster") %>%
           pull(ParCHR)  == "YES"){
  print("Processing borders from raster format")
  raster(paste0("LULCC/SourceData/InRaster/",country_parameters %>%
                  dplyr::filter(Var == "borders_name_r") %>%
                  pull(ParCHR))) %>%
    crop(extent(userarea_r)) %>%
    raster::resample(userarea_r, "ngb") %>%
    mask(userarea_r) %>% 
    writeRaster(filename="LULCC/TempRaster//borders_c.tif", datatype="INT4S", overwrite=TRUE)
} else if (country_parameters %>%
           dplyr::filter(Var == "borders_raster") %>%
           pull(ParCHR)  == character(0)){ # variable doesn't exist, assume vector format
  tic()
  borders<-st_read(paste0("LULCC/SourceData/InVector/",country_parameters %>%
                          dplyr::filter(Var == "borders_name") %>%
                          pull(ParCHR)))
  borders_r <- rasterize(borders, userarea_r, country_parameters %>%
                         dplyr::filter(Var == "borders_fieldname") %>%
                         pull(ParCHR))
  borders_c<-borders_r*userarea_r
  writeRaster(borders_c, filename="LULCC/TempRaster/borders_c.tif", datatype="INT2S", overwrite=TRUE)
  toc()
} else {
  tic()
  borders<-st_read(paste0("LULCC/SourceData/InVector/",country_parameters %>%
                          dplyr::filter(Var == "borders_name") %>%
                          pull(ParCHR)))
  borders_r <- rasterize(borders, userarea_r, country_parameters %>%
                         dplyr::filter(Var == "borders_fieldname") %>%
                         pull(ParCHR))
  borders_c<-borders_r*userarea_r
  writeRaster(borders_c, filename="LULCC/TempRaster/borders_c.tif", datatype="INT2S", overwrite=TRUE)
  toc()
}

# Maritime routes ----
if (identical(country_parameters %>%
              dplyr::filter(Var == "Maritime_lyr") %>%
              pull(ParCHR), NA_character_)) { # variable exists with no value
  print ("No Maritime layer") 
} else if (identical(country_parameters %>%
                     dplyr::filter(Var == "maritime_lyr") %>%
                     pull(ParCHR), "YES")) {
  
  maritime<-st_read(paste0("LULCC/SourceData/InVector/", country_parameters %>%
                             dplyr::filter(Var == "maritime_name") %>%
                             pull(ParCHR)))
  maritime_r <- rasterize(maritime, userarea_r, country_parameters %>%
                            dplyr::filter(Var == "maritime_name_ID") %>%
                            pull(ParCHR))
  maritime_c<-maritime_r*userarea_r
  writeRaster(maritime_c, filename="LULCC/TempRaster/maritime_c.tif", datatype="INT2S", overwrite=TRUE)
  
} else {
  "Do nothing"
}

# Attraction routes ---- 
if (identical(country_parameters %>%
              dplyr::filter(Var == "attraction_lyr") %>%
              pull(ParCHR), NA_character_)) { # variable exists with no value
  print ("No Attraction layer") 
} else if (identical(country_parameters %>%
                     dplyr::filter(Var == "attraction_lyr") %>%
                     pull(ParCHR), "YES")) {
  attraction <- st_read(paste0("LULCC/SourceData/InVector/", country_parameters %>%
                               dplyr::filter(Var == "attraction_name") %>%
                               pull(ParCHR))) %>%
    st_make_valid()
  attractionshpcrop<-st_intersection(attraction, userarea)
  # width <- c(0,10000,20000,50000,100000) # Adjusted for East Africa 10 to 1000 km
  widthnames <- c("b0","b1","b2","b3","b4")
  for (wn in widthnames){
    if (wn == "b0") { w = w0 
    } else if (wn == "b1") { w = w1
    } else if (wn == "b2") { w = w2
    } else if (wn == "b3") { w = w3
    }  else if (wn == "b4") { w = w4
    } else { print("error") }
    
    if (nrow(attractionshpcrop)==0) {
      attraction_c1<-rasterize(st_zm(attraction), userarea_r, country_parameters %>%
                                 dplyr::filter(Var == "attraction_name_ID") %>%
                                 pull(ParCHR))
    } else {
      attractionshpcrop_b<-st_buffer(attractionshpcrop, dist=w, nQuadSegs=100)
      attraction_c1<-rasterize(attractionshpcrop_b, userarea_r)
    }
    attraction_c<-attraction_c1*userarea_r
    writeRaster(attraction_c, filename=paste0("LULCC/TempRaster/attraction_c",wn,".tif"), datatype="INT2S", overwrite=TRUE)
  }
} else {
  "Do nothing"
}

# Demand modeling ---- 
# REVISE CAREFULLY AND UPDATE ALL CODES ---

# # Using raster
# tic("Using raster")
# locs_name_r_w <- raster("In/DemandScenarios/locs_raster_w.tif") %>%
#   crop(extent(userarea_r)) %>%
#   raster::resample(userarea_r, "ngb") %>%
#   mask(userarea_r) %>%
#   writeRaster(filename="LULCC/TempRaster/locs_c_w.tif", datatype="INT4S", overwrite=TRUE)
# 
# file.copy(from="LULCC/TempRaster/locs_c_w.tif",
#           to="In",
#           overwrite = TRUE)
# 
# locs_name_r_w <- raster("In/DemandScenarios/locs_raster_v.tif") %>%
#   crop(extent(userarea_r)) %>%
#   raster::resample(userarea_r, "ngb") %>%
#   mask(userarea_r) %>%
#   writeRaster(filename="LULCC/TempRaster/locs_c_v.tif", datatype="INT4S", overwrite=TRUE)
# 
# file.copy(from="LULCC/TempRaster/locs_c_v.tif",
#           to="In",
#           overwrite = TRUE)
# toc()

# Using Terra
tic("Using terra")

userarea_rt <- rast(userarea_r)

# Read the raster files using terra
locs_name_r_w <- terra::rast("In/DemandScenarios/locs_raster_w.tif")

# Crop, resample, and mask using terra functions
locs_name_r_w <- locs_name_r_w %>%
  terra::crop(userarea_rt) %>%
  terra::resample(userarea_rt, method = "near") %>%
  terra::mask(userarea_rt)

# Write the processed raster to a file
writeRaster(locs_name_r_w, filename = "LULCC/TempRaster/locs_c_w.tif", datatype = "INT4S", overwrite = TRUE)

# Copy the file to the "In" folder
file.copy(from = "LULCC/TempRaster/locs_c_w.tif",
          to = "In",
          overwrite = TRUE)

# Repeat the process for the second raster
locs_name_r_v <- rast("In/DemandScenarios/locs_raster_v.tif")

locs_name_r_v <- locs_name_r_v %>%
  terra::crop(userarea_rt) %>%
  terra::resample(userarea_rt, method = "near") %>%
  terra::mask(userarea_rt)

writeRaster(locs_name_r_v, filename = "LULCC/TempRaster/locs_c_v.tif", datatype = "INT4S", overwrite = TRUE)

file.copy(from = "LULCC/TempRaster/locs_c_v.tif",
          to = "In",
          overwrite = TRUE)
toc()

# Land Use Land Cover Module ####
if (webmofuss == 0) {
  
  if (LULCt1map == "YES"){
    dir.create("LULCC/lucdynamics_luc1")
    dir.create("LULCC/lucdynamics_luc1/out_lulcc")
    lulcc.egoml <- list.files (paste0(gitlabdir, "/localhost/scripts/LULCC/LULCt1_c/"))
    file.copy(from=paste0(gitlabdir, "/localhost/scripts/LULCC/LULCt1_c/",lulcc.egoml), 
              to=paste0(countrydir, "/LULCC/lucdynamics_luc1"), 
              overwrite = TRUE)
    system(paste0(countrydir, "/LULCC/lucdynamics_luc1/LULCC_blackbox_scripts2.bat"))
  }

  if (LULCt2map == "YES"){
    dir.create("LULCC/lucdynamics_luc2")
    dir.create("LULCC/lucdynamics_luc2/out_lulcc")
    lulcc.egoml <- list.files (paste0(gitlabdir, "/localhost/scripts/LULCC/LULCt2_c/"))
    file.copy(from=paste0(gitlabdir, "/localhost/scripts/LULCC/LULCt2_c/",lulcc.egoml), 
              to=paste0(countrydir, "/LULCC/lucdynamics_luc2"), 
              overwrite = TRUE)
    system(paste0(countrydir, "/LULCC/lucdynamics_luc2/LULCC_blackbox_scripts2.bat"))
  }
  
  if (LULCt3map == "YES"){
    dir.create("LULCC/lucdynamics_luc3")
    dir.create("LULCC/lucdynamics_luc3/out_lulcc")
    lulcc.egoml <- list.files (paste0(gitlabdir, "/localhost/scripts/LULCC/LULCt3_c/"))
    file.copy(from=paste0(gitlabdir, "/localhost/scripts/LULCC/LULCt3_c/",lulcc.egoml), 
              to=paste0(countrydir, "/LULCC/lucdynamics_luc3"), 
              overwrite = TRUE)
    system(paste0(countrydir, "/LULCC/lucdynamics_luc3/LULCC_blackbox_scripts2.bat"))
  }

  
} else {
  
  "This is webmofuss 4 Edgar"
  
}

# End of script ----



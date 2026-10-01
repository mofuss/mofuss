# SPDX-License-Identifier: Apache-2.0
#
# Copyright 2025-2027 Universidad Nacional Autónoma de México
# and Stockholm Environment Institute
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# https://www.apache.org/licenses/LICENSE-2.0
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# MoFuSS ----
# Script: 6_scenarios_v4.R
# Version: 4
# Date: Sep 2026
# Execution: Source from RStudio; Dinamica EGO does not invoke this script directly.
#
# Purpose: Convert harmonized inputs and annual demand trajectories into scenario,
# friction and lookup products consumed by the Dinamica EGO model.
# Inputs: Harmonized rasters/tables, demand maps, parameters.csv and inherited paths.
# Outputs: Demand-scenario CSVs, friction rasters and Dinamica supporting tables.
# Side effects: Changes working directory, clears prior Debugging/Temp/Out products
# and overwrites model inputs.

# 2dolist ----
# URGENTLY fix this very old and outdated chunck to make it 
# work smoothly with GEE at varying scales!!
# Fix for linux cluster
# Read validation dataset and based on that turn on/off deforestation module in dinamica
# Line 183: BaU vs ICS
# Friction ----
# WARNING: MARITIME AND ATRACTION LAYERS NEED TO BE FLESHED OUT AND DEBUG AS OF JULY 2023

# BEGIN USER INPUTS ----------------------------------------------------------
# Internal parameters ----
attdecay = 1.40  # decay rate of attraction kernels
# END USER INPUTS ------------------------------------------------------------


# Load libraries ----
library(conflicted)

library(dplyr)
library(fasterize)
# library(gitlabr)
library(igraph)
library(inline)
library(raster)
library(readr)
library(readr)
library(readxl)
library(rgl)
library(sf)
library(spam)
library(stars)
library(svDialogs)
library(tidyverse)

setwd(countrydir)
getwd()

# Demand exports are already on disk. The downstream scripts read those files;
# keeping script 3's duplicate tables and raster stacks leaves little room for
# scenario preparation in a single long-running RStudio session.
prepared_demand_intermediates <- c(
  "wf_w_db4idw", "wf_v_db4idw", "partitioned_w_dbs", "partitioned_v_dbs",
  "partitioned_w", "partitioned_v", "wf_w_st", "wf_v_st",
  "country_index_raster", "country_index_raster_centres",
  "country_index_raster_touches"
)
rm(list = base::intersect(prepared_demand_intermediates, ls(all.names = TRUE)),
   envir = environment())
rm(prepared_demand_intermediates)
invisible(gc())

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

country_parameters %>%
  dplyr::filter(Var == "scenario_ver") %>%
  pull(ParCHR) -> SceCode

country_parameters %>%
  dplyr::filter(Var == "idw_debug") %>%
  pull(ParCHR) -> idw_debug

country_parameters %>%
  dplyr::filter(Var == "friction") %>%
  pull(ParCHR) -> friction

country_parameters %>%
  dplyr::filter(Var == "maritime_lyr") %>%
  pull(ParCHR) -> maritime

country_parameters %>%
  dplyr::filter(Var == "attraction_lyr") %>%
  pull(ParCHR) -> attraction2

country_parameters %>%
  dplyr::filter(Var == "maritime_name") %>%
  pull(ParCHR) -> maritime_name

country_parameters %>%
  dplyr::filter(Var == "attraction_name") %>%
  pull(ParCHR) -> attraction_name

country_parameters %>%
  dplyr::filter(Var == "maritime_name_ID") %>%
  pull(ParCHR) -> maritime_name_ID

country_parameters %>%
  dplyr::filter(Var == "attraction_name_ID") %>%
  pull(ParCHR) -> attraction_name_ID

country_parameters %>%
  dplyr::filter(Var == "Model") %>%
  pull(ParCHR) -> Model

setwd(countrydir)

# Clean temps - keep inelegant list of unlinks as its the easiest layout for the moment ####
unlink("Debugging//*.*",force=TRUE)
unlink("Temp//*.*",force=TRUE)
unlink("HTML_animation*//*", recursive = TRUE,force=TRUE)
unlink("Out*//*", recursive = TRUE,force=TRUE)
unlink("LaTeX//*.pdf",force=TRUE)
unlink("LaTeX//*.mp4",force=TRUE)
# unlink("Summary_Report//*.*",force=TRUE)

Country<-readLines("LULCC/TempTables/Country.txt")

# Read supply parameters table, checking if its delimiter is comma or semicolon ####
if (file.exists("LULCC/TempTables/growth_parameters1.csv") == TRUE) {
  read_csv("LULCC/TempTables/growth_parameters1.csv") %>% 
    {if(is.null(.$TOF[1])) read_csv2("LULCC/TempTables/growth_parameters1.csv") else .} -> growth_parameters1
}

if (file.exists("LULCC/TempTables/growth_parameters2.csv") == TRUE) {
  read_csv("LULCC/TempTables/growth_parameters2.csv") %>% 
    {if(is.null(.$TOF[1])) read_csv2("LULCC/TempTables/growth_parameters2.csv") else .} -> growth_parameters2
}

if (file.exists("LULCC/TempTables/growth_parameters3.csv") == TRUE) {
  read_csv("LULCC/TempTables/growth_parameters3.csv") %>% 
    {if(is.null(.$TOF[1])) read_csv2("LULCC/TempTables/growth_parameters3.csv") else .} -> growth_parameters3
}

#save user data as txt for down stream uses
writeLines(paste0(substr(SceCode, 1, 3),"_scenario"), "LULCC/TempTables/SceCode.txt", useBytes=T)
readLines("LULCC/TempTables/SceCode.txt")
#userarea_GCS<-st_read("TempVector_GCS/userarea_GCS.shp")
#userarea<-st_read("TempVector/userarea.shp")
res<-read.csv("LULCC/TempTables/Resolution.csv", header=T)
resolution<-res[1,2]
userarea_r<-raster("LULCC/TempRaster/mask_c.tif")
annostxt <- read.table("LULCC/TempTables/annos.txt") %>% .$x 
first_yr <- annostxt[1]
last_yr <- tail(annostxt, n=1)
# setwd(paste0(countrydir,"/In/DemandScenarios"))
unlink("In/DemandScenarios/fwuse_*.csv",force=TRUE)
# Read whatever name and scenario!

# Keep the annual tables out of the shared preprocessing environment. Neither
# friction preparation nor the following scripts use these large intermediates.
local({
for (j in (c("v","w"))) {
  locs_c<-raster(paste0("LULCC/TempRaster/locs_c_",j,".tif"))
  db_locs <- as.data.frame(getValues(locs_c))
  db_locs_f<-db_locs[complete.cases(db_locs),]
  
  demand_file <- paste0("In/DemandScenarios/", substr(SceCode, 1, 3), "_fwch_", j, ".csv")
  demand_header <- readLines(demand_file, n = 1L, warn = FALSE)
  demand_delimiter <- if (grepl(";", demand_header, fixed = TRUE)) ";" else ","
  # Detect the delimiter from the header instead of reading the entire table
  # twice. Replace missing demand by column, avoiding a table-sized NA matrix.
  DemSce <- read.csv(demand_file, sep = demand_delimiter, stringsAsFactors = FALSE)
  for (column in seq_along(DemSce)) {
    missing <- is.na(DemSce[[column]])
    if (any(missing)) DemSce[[column]][missing] <- 0
  }
  locs_fieldname <- paste0("locs_c_", j)
  names(DemSce)[1L] <- locs_fieldname
  DemSce_clean <- DemSce[DemSce[[locs_fieldname]] %in% db_locs_f, , drop = FALSE]
  rm(DemSce, db_locs, db_locs_f)
  invisible(gc())
  yrs<-first_yr:last_yr #Calibration+Simulation period
  steps_dif<-(last_yr-first_yr)+1	
  steps<-1:steps_dif #Stdyn+1 e.g. 2027-2003=24->24+1=25
  # steps <- c(1,2) DEBUG FOR NON CONSECUTIVE YEARS!!
  # Save for Dinamica's ingestion
  for (i in steps) {
    if (nchar(i)==1) {
      
      if (j == "v") { 
        col_fwv<-paste0("X",yrs[i],"_fw_v")
        db_fwV<-as.data.frame(DemSce_clean[ ,c(locs_fieldname,col_fwv)])
        colnames(db_fwV)<-c("Key","Value")
        
        #col_chv<-paste0("X",yrs[i],"_ch_v")
        #db_chV<-as.data.frame(DemSce_clean[ ,c(locs_fieldname,col_chv)])
        #colnames(db_chV)<-c("Key","Value")
        #db_chV$Value<-(as.numeric(db_chV$Value)+as.numeric(db_fwV$Value))# /1000 In case the dataset is in kg # CHECK THIS OUT
        db_fwV$Value<-as.numeric(db_fwV$Value) # /1000 In case the dataset is in kg (what???)
        write.csv(db_fwV,paste0("In/DemandScenarios/fwuse_V",0,i,".csv"),row.names = FALSE)
        write.csv(db_fwV,paste0("In/DemandScenarios/fwuse_V_clipped",0,i,".csv"),row.names = FALSE)
        write.csv(db_fwV,paste0("In/DemandScenarios/fwuse_V_ext_fwdef",0,i,".csv"),row.names = FALSE)
        
      } else if (j == "w") {
        
        col_w<-paste0("X",yrs[i],"_fw_w")
        db_W<-as.data.frame(DemSce_clean[ ,c(locs_fieldname,col_w)])
        colnames(db_W)<-c("Key","Value")
        db_W$Value<-as.numeric(db_W$Value) # /1000 In case the dataset is in kg (what???)
        write.csv(db_W,paste0("In/DemandScenarios/fwuse_W",0,i,".csv"),row.names = FALSE)
        write.csv(db_W,paste0("In/DemandScenarios/fwuse_W_clipped",0,i,".csv"),row.names = FALSE)
        write.csv(db_W,paste0("In/DemandScenarios/fwuse_W_ext_fwdef",0,i,".csv"),row.names = FALSE)
      } else {
        print("ERROR")
      }
      
    } else {
      
      if (j == "v") { 
        col_fwv<-paste0("X",yrs[i],"_fw_v")
        db_fwV<-as.data.frame(DemSce_clean[ ,c(locs_fieldname,col_fwv)])
        colnames(db_fwV)<-c("Key","Value")
        
        # col_chv<-paste0("X",yrs[i],"_ch_v")
        # db_chV<-as.data.frame(DemSce_clean[ ,c(locs_fieldname,col_chv)])
        # colnames(db_chV)<-c("Key","Value")
        # db_chV$Value<-(as.numeric(db_chV$Value)+as.numeric(db_fwV$Value))/1000 # In case the dataset is in kg
        db_fwV$Value<-as.numeric(db_fwV$Value) # /1000  In case the dataset is in kg # CHECK THIS OUT
        write.csv(db_fwV,paste0("In/DemandScenarios/fwuse_V",i,".csv"),row.names = FALSE)
        write.csv(db_fwV,paste0("In/DemandScenarios/fwuse_V_clipped",i,".csv"),row.names = FALSE)
        write.csv(db_fwV,paste0("In/DemandScenarios/fwuse_V_ext_fwdef",i,".csv"),row.names = FALSE)
      } else if (j == "w") {
        col_w<-paste0("X",yrs[i],"_fw_w")
        db_W<-as.data.frame(DemSce_clean[ ,c(locs_fieldname,col_w)])
        colnames(db_W)<-c("Key","Value")
        db_W$Value<-as.numeric(db_W$Value) # /1000  In case the dataset is in tones
        write.csv(db_W,paste0("In/DemandScenarios/fwuse_W",i,".csv"),row.names = FALSE)
        write.csv(db_W,paste0("In/DemandScenarios/fwuse_W_clipped",i,".csv"),row.names = FALSE)
        write.csv(db_W,paste0("In/DemandScenarios/fwuse_W_ext_fwdef",i,".csv"),row.names = FALSE)
      } else {
        print("ERROR")
      }
    }
  }
  rm(DemSce_clean)
  invisible(gc())
  Sys.sleep(10)
}
})
invisible(gc())

make_tof_key_table <- function(data, label) {
  required <- c("Key*", "TOF")
  missing <- setdiff(required, names(data))
  if (length(missing)) {
    stop(label, " is missing columns: ", paste(missing, collapse = ", "))
  }
  key_values <- suppressWarnings(as.numeric(data[["Key*"]]))
  tof_values <- suppressWarnings(as.numeric(data[["TOF"]]))
  invalid_keys <- anyNA(key_values) || any(!is.finite(key_values)) ||
    any(key_values < 1) || any(key_values > .Machine$integer.max) ||
    any(key_values != floor(key_values))
  invalid_tof <- anyNA(tof_values) || any(!is.finite(tof_values)) ||
    any(!tof_values %in% c(0, 1))
  if (invalid_keys || invalid_tof) {
    stop(label, " contains invalid/duplicate keys or non-binary TOF values.")
  }
  keys <- as.integer(key_values)
  tof <- as.integer(tof_values)
  if (anyDuplicated(keys)) {
    stop(label, " contains invalid/duplicate keys or non-binary TOF values.")
  }
  data.frame(Key = keys, x = tof)
}

if (file.exists("LULCC/TempTables/growth_parameters1.csv") == TRUE) {
  data_semicolon<-read.csv("LULCC/TempTables/growth_parameters1.csv", sep=";", header=T, check.names=FALSE)
  data_comma<-read.csv("LULCC/TempTables/growth_parameters1.csv", sep=",", header=T, check.names=FALSE)
  if (is.null(data_semicolon$TOF[1])) { 
    data_all1<-data_comma
  } else {
    data_all1<-data_semicolon
  }
  # Produce TOF vs FOR categories ####
  data_FOR1<-subset(data_all1, data_all1$TOF==0)
  data_TOF1<-subset(data_all1, data_all1$TOF==1)
  dataTOFvsFOR1 <- make_tof_key_table(data_all1, "growth_parameters1.csv")
  write.csv(dataTOFvsFOR1,"LULCC/TempTables//TOFvsFOR_Categories1.csv",row.names = FALSE)
}

if (file.exists("LULCC/TempTables/growth_parameters2.csv") == TRUE) {
  data_semicolon<-read.csv("LULCC/TempTables/growth_parameters2.csv", sep=";", header=T, check.names=FALSE)
  data_comma<-read.csv("LULCC/TempTables/growth_parameters2.csv", sep=",", header=T, check.names=FALSE)
  if (is.null(data_semicolon$TOF[1])) { 
    data_all2<-data_comma
  } else {
    data_all2<-data_semicolon
  } 
  # Produce TOF vs FOR categories ####
  data_FOR2<-subset(data_all2, data_all2$TOF==0)
  data_TOF2<-subset(data_all2, data_all2$TOF==1)
  dataTOFvsFOR2 <- make_tof_key_table(data_all2, "growth_parameters2.csv")
  write.csv(dataTOFvsFOR2,"LULCC/TempTables//TOFvsFOR_Categories2.csv",row.names = FALSE)
}

if (file.exists("LULCC/TempTables/growth_parameters3.csv") == TRUE) {
  data_semicolon<-read.csv("LULCC/TempTables/growth_parameters3.csv", sep=";", header=T, check.names=FALSE)
  data_comma<-read.csv("LULCC/TempTables/growth_parameters3.csv", sep=",", header=T, check.names=FALSE)
  if (is.null(data_semicolon$TOF[1])) { 
    data_all3<-data_comma
  } else {
    data_all3<-data_semicolon
  }
  # Produce TOF vs FOR categories ####
  data_FOR3<-subset(data_all3, data_all3$TOF==0)
  data_TOF3<-subset(data_all3, data_all3$TOF==1)
  dataTOFvsFOR3 <- make_tof_key_table(data_all3, "growth_parameters3.csv")
  write.csv(dataTOFvsFOR3,"LULCC/TempTables//TOFvsFOR_Categories3.csv",row.names = FALSE)
}

# Dinamica external scripts ----
# Dirs for system ----
githubdir.sys <- gsub("/", "\\", githubdir, fixed=TRUE)
countrydir.sys <- gsub("/", "\\", countrydir, fixed=TRUE)

# Friction ----
# WARNING: MARITIME AND ATRACTION LAYERS NEED TO BE FLESHED OUT AND DEBUG AS OF JULY 2023
if (friction == "R"){
  build_friction_rasters <- function() {
    # A large regional grid must remain on disk throughout this calculation.
    # The legacy [] assignments bypassed raster's memory checks and loaded
    # each entire grid into R, even when its other operations used blocks.
    invisible(capture.output(previous_options <- raster::rasterOptions()))
    previous_progress <- getOption("rasterProgress")
    previous_options <- previous_options[
      names(previous_options) %in% setdiff(names(formals(raster::rasterOptions)), "progress")
    ]
    on.exit({
      do.call(raster::rasterOptions, previous_options)
      # rasterOptions reports its default as "none", but its setter rejects
      # that value. Restore the original underlying option directly.
      options(rasterProgress = previous_progress)
    }, add = TRUE)
    raster::rasterOptions(
      todisk = TRUE, maxmemory = 5e8, chunksize = 3.2e7,
      memfrac = 0.1, datatype = "FLT8S"
    )
    fill_missing_friction <- function(x) {
      raster::calc(x, fun = function(values) {
        values[is.na(values)] <- 0
        values
      })
    }
  
  unlink("in/fricc_w.tif")
  unlink("in/fricc_v.tif")
  unlink("in/*.xml")
  
  # Vehicle friction
  # roads <-  raster("LULCC/DownloadedDatasets/SourceDataGlobal/InRaster/roads.tif")
  # raster::unique(roads)
  # rivers <-  raster("LULCC/DownloadedDatasets/SourceDataGlobal/InRaster/rivers.tif")
  # raster::unique(rivers)
  
  # rivers
  if (maritime == "YES") {
    rivers_c <- raster("LULCC/TempRaster/rivers_c.tif")
    # unique(rivers_c)
    rivers_rectable <- read_csv("LULCC/TempTables/Friction_rivers_reclass_r.csv")
    rivers_reclass.m <- reclassify(rivers_c,
                                   as.data.frame(rivers_rectable),
                                   right=NA)
    rivers_reclass.m <- fill_missing_friction(rivers_reclass.m)
    
    # Add maritime chunk
    maritime_c <- raster("LULCC/TempRaster/maritime_c.tif")
    maritime_c <- fill_missing_friction(maritime_c)
    rivers_reclass <- overlay(maritime_c, rivers_reclass.m,  
                              fun = function(x,y) {ifelse(x==1, x*0.8, y)} ) #empezar por aca
    
    
  } else if (maritime == "NO"){
    rivers_c <- raster("LULCC/TempRaster/rivers_c.tif")
    # unique(rivers_c)
    rivers_rectable <- read_csv("LULCC/TempTables/Friction_rivers_reclass_r.csv")
    rivers_reclass_prelakes <- reclassify(rivers_c,
                                          as.data.frame(rivers_rectable),
                                          right=NA)
    rivers_reclass_prelakes <- fill_missing_friction(rivers_reclass_prelakes)
    # writeRaster(rivers_reclass_prelakes, "In/rivers_reclass_prelakes.tif", overwrite = TRUE)
    
  }
  
  # lakes
  if (maritime == "YES") {
    lakes_c <- raster("LULCC/TempRaster/lakes_c.tif")
    # unique(rivers_c)
    lakes_rectable <- read_csv("LULCC/TempTables/Friction_lakes_reclass_r.csv")
    lakes_reclass.m <- reclassify(lakes_c,
                                  as.data.frame(lakes_rectable),
                                  right=NA)
    lakes_reclass.m <- fill_missing_friction(lakes_reclass.m)
    
    # Add maritime chunk
    maritime_c <- raster("LULCC/TempRaster/maritime_c.tif")
    maritime_c <- fill_missing_friction(maritime_c)
    lakes_reclass <- overlay(lakes_c, lakes_reclass.m,  
                             fun = function(x,y) {ifelse(x==1, x*0.8, y)} ) #empezar por aca
    
  } else if (maritime == "NO"){
    lakes_c <- raster("LULCC/TempRaster/lakes_c.tif")
    # unique(rivers_c)
    lakes_rectable <- read_csv("LULCC/TempTables/Friction_lakes_reclass_r.csv")
    lakes_reclass <- reclassify(lakes_c,
                                as.data.frame(lakes_rectable),
                                right=NA)
    lakes_reclass <- fill_missing_friction(lakes_reclass)
    # writeRaster(lakes_reclass, "In/lakes_reclass.tif", overwrite = TRUE)
    
  }
  
  rivers_reclass <- overlay(lakes_reclass, rivers_reclass_prelakes, 
                            fun = function(x,y) {ifelse(x > 0, x, y)})
  # writeRaster(rivers_reclass, "In/rivers_reclass.tif", overwrite = TRUE)
  
  # roads
  roads_c <- raster("LULCC/TempRaster/roads_c.tif")
  # unique(roads_c)
  drivingoverroads_rectable <- read_csv("LULCC/TempTables/Friction_drivingoverroads_r.csv")
  roads_reclass_preborder <- reclassify(roads_c,
                                        as.data.frame(drivingoverroads_rectable),
                                        right=NA)
  # roads_reclass_preborder[is.na(roads_reclass_preborder[])] <- 0
  # writeRaster(roads_reclass_preborder, "In/roads_reclass_preborder.tif", overwrite = TRUE)
  
  # borders
  borders_c <- raster("LULCC/TempRaster/borders_c.tif")
  # unique(borders_c)
  borders_rectable <- read_csv("LULCC/TempTables/Friction_borders_reclass_r.csv")
  borders_reclass <- reclassify(borders_c,
                                as.data.frame(borders_rectable),
                                right=NA)
  borders_reclass <- fill_missing_friction(borders_reclass)
  # writeRaster(borders_reclass, "In/borders_reclass.tif", overwrite = TRUE)
  
  roads_reclass <- overlay(borders_reclass, roads_reclass_preborder, 
                           fun = function(x,y) {ifelse(x > 0, x, y)})
  roads_reclass <- fill_missing_friction(roads_reclass)
  # writeRaster(roads_reclass, "In/roads_reclass.tif", overwrite = TRUE)
  
  
  # slope
  DEM_c <- raster("LULCC/TempRaster/DEM_c.tif")
  slope_c <-  terrain(DEM_c, opt=c('slope'), unit='degrees', neighbors=8) # Prender luego de terminar de debuggear!
  # slope_c <- raster("LULCC/TempRaster/Slope_din.tif")
  walkingcrosscountry_table <- read_csv("LULCC/TempTables/Friction_walkingcrosscountry_r.csv")
  slope_reclass <- reclassify(slope_c,
                              as.data.frame(walkingcrosscountry_table))
  slope_reclass <- fill_missing_friction(slope_reclass)
  # writeRaster(slope_reclass, "In/slope_reclass.tif", overwrite = TRUE)
  
  rivers_O_Wslope <- overlay(rivers_reclass, slope_reclass, 
                             fun = function(x,y) {ifelse(x > 0, x, y)} )
  # writeRaster(rivers_O_Wslope, "In/rivers_O_Wslope.tif", overwrite = TRUE)
  
  fricc_vv <- overlay(roads_reclass, rivers_O_Wslope, 
                      fun = function(x,y) {ifelse(x > 0, x, y)} )
  # writeRaster(fricc_vv, "In/fricc_vv.tif", overwrite = TRUE)
  
  # Walking friction
  walkingoverroads_table <- read_csv("LULCC/TempTables/Friction_walkingoverroads_r.csv")
  slopeoverroads<-reclassify(roads_c,c(-Inf,Inf,0)) %>%
    stack(.,slope_c) %>%
    calc(., sum) %>%
    reclassify(.,as.data.frame(walkingoverroads_table))
  slopeoverroads <- fill_missing_friction(slopeoverroads)
  
  fricc_ww_preborder <- overlay(
    slopeoverroads,
    rivers_O_Wslope,
    fun = function(x, y) {ifelse(x > 0, x, y)}
  )

  # International borders must also affect self-gathered W. Retain the same
  # explicit, tunable border-friction table used by V, but apply it to the
  # walking surface instead of allowing cost-distance paths to cross borders at
  # zero additional cost. A finite table value permits transboundary walking;
  # 999999 makes the border impassable.
  fricc_ww <- overlay(
    borders_reclass,
    fricc_ww_preborder,
    fun = function(x, y) {ifelse(x > 0, x, y)}
  )
  
  if (attraction2 == "YES") { # Adjusted for East Africa 10 to 1000 km
    
    cb0 <- raster("LULCC/TempRaster/attraction_cb0.tif")
    cb1 <- raster("LULCC/TempRaster/attraction_cb1.tif")
    cb2 <- raster("LULCC/TempRaster/attraction_cb2.tif")
    cb3 <- raster("LULCC/TempRaster/attraction_cb3.tif")
    cb4 <- raster("LULCC/TempRaster/attraction_cb4.tif")
    
    # Apply attraction only to traversable cells. A friction value of 999999
    # represents an inviolable barrier and must remain unchanged.
    fun_att <- function(x, y) {
      ifelse(!is.na(x) & !is.na(y) & y != 999999,
             y / attdecay,
             y)
    }
    
    fricc_v10000 <- overlay(cb0, fricc_vv,
                            fun = fun_att) %>%
      overlay(cb1, .,
              fun = fun_att) %>%
      overlay(cb2, .,
              fun = fun_att) %>%
      overlay(cb3, .,
              fun = fun_att) %>%
      overlay(cb4, .,
              fun = fun_att)
    
    # Save slope and friction geotiffs
    if (maritime == "YES") {
      fricc_w.m <- reclassify(fricc_ww, cbind(-Inf, 0, NA), right=TRUE)
      maritime_c.r <- maritime_c * 999999
      fricc_w <- overlay(fricc_w.m, maritime_c.r,
                         fun = function(x,y) {ifelse(is.na(y), x, x+y)} )
      fricc_v <- reclassify(fricc_v10000, cbind(-Inf, 0, NA), right=TRUE)
      
      writeRaster(slope_c, "LULCC/TempRaster/Slope.tif", overwrite = TRUE, datatype = "FLT4S")
      writeRaster(fricc_w, "In/fricc_w.tif", overwrite = TRUE, datatype = "FLT4S")
      writeRaster(fricc_v, "In/fricc_v.tif", overwrite = TRUE, datatype = "FLT4S")
      
    } else if (maritime == "NO"){
      
      fricc_w <- reclassify(fricc_ww, cbind(-Inf, 0, NA), right=TRUE)
      fricc_v <- reclassify(fricc_v10000, cbind(-Inf, 0, NA), right=TRUE)
      
      writeRaster(slope_c, "LULCC/TempRaster/Slope.tif", overwrite = TRUE, datatype = "FLT4S")
      writeRaster(fricc_w, "In/fricc_w.tif", overwrite = TRUE, datatype = "FLT4S")
      writeRaster(fricc_v, "In/fricc_v.tif", overwrite = TRUE, datatype = "FLT4S")
      
    }
    
  } else if (attraction2 == "NO") {
    
    if (maritime == "YES") {
      fricc_w.m <- reclassify(fricc_ww, cbind(-Inf, 0, NA), right=TRUE)
      maritime_c.r <- maritime_c * 999999
      fricc_w <- overlay(fricc_w.m, maritime_c.r,
                         fun = function(x,y) {ifelse(is.na(y), x, x+y)} )
      fricc_v <- reclassify(fricc_vv, cbind(-Inf, 0, NA), right=TRUE)
      
      writeRaster(slope_c, "LULCC/TempRaster/Slope.tif", overwrite = TRUE, datatype = "FLT4S")
      writeRaster(fricc_w, "In/fricc_w.tif", overwrite = TRUE, datatype = "FLT4S")
      writeRaster(fricc_v, "In/fricc_v.tif", overwrite = TRUE, datatype = "FLT4S")
      
    } else if (maritime == "NO") {
      
      fricc_w <- reclassify(fricc_ww, cbind(-Inf, 0, NA), right=TRUE)
      fricc_v <- reclassify(fricc_vv, cbind(-Inf, 0, NA), right=TRUE)
      
      writeRaster(slope_c, "LULCC/TempRaster/Slope.tif", overwrite = TRUE, datatype = "FLT4S")
      writeRaster(fricc_w, "In/fricc_w.tif", overwrite = TRUE, datatype = "FLT4S")
      writeRaster(fricc_v, "In/fricc_v.tif", overwrite = TRUE, datatype = "FLT4S")
      
      unlink("in/*.xml")
      # plot(fricc_w)
      # plot(fricc_v)
      
    }
    
  }
  
  }
  build_friction_rasters()
  rm(build_friction_rasters)
  invisible(gc())
} else if (friction == "Dinamica"){
  unlink("in/fricc_w.tif")
  unlink("in/fricc_v.tif")
  unlink("in/*.xml")
  frictions51 <- paste0('"C:\\Program Files\\Dinamica EGO\\DinamicaConsole.exe\" -processors 0 -log-level 4 ','\"', countrydir.sys, '\\Friction3.egoml"')
  cat(frictions51)
  system(frictions51)  
}

# IDW  ----
if (idw_debug == "YES") {
  IDW51 <- paste0('"C:\\Program Files\\Dinamica EGO\\DinamicaConsole.exe\" -processors 0 -log-level 4 ','\"', countrydir.sys, '\\IDW_Sc3.egoml"')
  cat(IDW51)
  system(IDW51)
} else {
  "Do nothing"
}

# End of script ----

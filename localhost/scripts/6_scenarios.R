# MoFuSS
# Version 3
# Date: Mar 2024

# 2dolist
# URGENTLY fix this very old and outdated chunck to make it 
# work smoothly with GEE at varying scales!!
# Fix for linux cluster
# Read validation dataset and based on that turn on/off deforestation module in dinamica

# Internal parameters
attdecay = 1.15  # decay rate of attraction kernels
# Select MoFuSS platform:
webmofuss = 1 # "1" is  web-MoFuSS running in our Ubuntu server, "0" is localcal host (Windows or Linux)

# Load packages ----
library(readr)
library(dplyr)
library(fasterize)
library(igraph)
library(raster)
library(rgl)
library(sf)
library(readr)
library(stars)
library(gitlabr)
library(inline)
library(tidyverse)
library(spam)
library(svDialogs)
library(readxl)

setwd(countrydir)
getwd()
country_name

# Read parameters table ----
read.csv("LULCC/TempTables/Country.csv") %>%
  dplyr::filter(Key. == "1") %>%
  pull(Country) -> country_name

# Specify the directory where the file is located
parameters_directory <- paste0(getwd(),"/LULCC/DownloadedDatasets/SourceData",country_name)

# Use list.files() to find the file that matches the pattern
parameters_name <- list.files(path = parameters_directory, pattern = "^parameters.*\\.xlsx$", full.names = TRUE)

# Read parameters table ----
if (webmofuss == 1){
  country_parameters <- read_csv(paste0("LULCC/DownloadedDatasets/SourceData",country_name,"/",parameters_file))
} else if (webmofuss == 0){
  country_parameters <- read_excel(paste0("LULCC/DownloadedDatasets/SourceData",country_name,"/",parameters_file))
}
print(tibble::as_tibble(country_parameters), n=100)

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
writeLines(paste0(SceCode,"_scenario"), "LULCC/TempTables/SceCode.txt", useBytes=T)
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

for (j in (c("v","w"))) {
  locs_c<-raster(paste0("LULCC/TempRaster/locs_c_",j,".tif"))
  db_locs <- as.data.frame(getValues(locs_c))
  db_locs_f<-db_locs[complete.cases(db_locs),]
  
  DemSce_semicolon<-read.csv(paste0("In/DemandScenarios/",SceCode,"_fwch_",j,".csv"), sep=";", stringsAsFactors=FALSE)
  DemSce_comma<-read.csv(paste0("In/DemandScenarios/",SceCode,"_fwch_",j,".csv"), sep=",", stringsAsFactors=FALSE)
  if (is.null(DemSce_semicolon$X2027_ch_v[1])) { # Read in the arguments listed at the command line in DINAMICA'S "Run external process"
    DemSceX<-DemSce_comma
  } else {
    DemSceX<-DemSce_semicolon
  }
  
  DemSceX %>%
    replace(is.na(.), 0) -> DemSce
  
  # country_parameters %>%
  #   dplyr::filter(Var == "locs_fieldname") %>%
  #   pull(ParCHR) -> locs_fieldname
  locs_fieldname <- paste0("locs_c_",j)
  
  names(DemSce) [1] <- locs_fieldname
  lastrow<- nrow(DemSce) 
  lastcol<- ncol(DemSce) 
  DemSce_s<-DemSce[1:lastrow,1:lastcol]
  DemSce_clean <- DemSce_s[DemSce_s %>% pull(locs_fieldname) %in% db_locs_f, ]
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
  Sys.sleep(10)
}

if (file.exists("LULCC/TempTables/growth_parameters1.csv") == TRUE) {
  data_semicolon<-read.csv("LULCC/TempTables/growth_parameters1.csv", sep=";", header=T)
  data_comma<-read.csv("LULCC/TempTables/growth_parameters1.csv", sep=",", header=T)
  if (is.null(data_semicolon$TOF[1])) { 
    data_all1<-data_comma
  } else {
    data_all1<-data_semicolon
  }
  # Produce TOF vs FOR categories ####
  data_FOR1<-subset(data_all1, data_all1$TOF==0)
  data_TOF1<-subset(data_all1, data_all1$TOF==1)
  max_tot1<-nrow(data_all1)
  
  dataTOFvsFOR1<-as.data.frame(data_all1[ ,7])
  colnames(dataTOFvsFOR1)<-("x")
  dataTOFvsFOR1=data.frame(Key=c(1:max_tot1),dataTOFvsFOR1)
  write.csv(dataTOFvsFOR1,"LULCC/TempTables//TOFvsFOR_Categories1.csv",row.names = FALSE)
}

if (file.exists("LULCC/TempTables/growth_parameters2.csv") == TRUE) {
  data_semicolon<-read.csv("LULCC/TempTables/growth_parameters2.csv", sep=";", header=T)
  data_comma<-read.csv("LULCC/TempTables/growth_parameters2.csv", sep=",", header=T)
  if (is.null(data_semicolon$TOF[1])) { 
    data_all2<-data_comma
  } else {
    data_all2<-data_semicolon
  } 
  # Produce TOF vs FOR categories ####
  data_FOR2<-subset(data_all2, data_all2$TOF==0)
  data_TOF2<-subset(data_all2, data_all2$TOF==1)
  max_tot2<-nrow(data_all2)
  
  dataTOFvsFOR2<-as.data.frame(data_all2[ ,7])
  colnames(dataTOFvsFOR2)<-("x")
  dataTOFvsFOR2=data.frame(Key=c(1:max_tot2),dataTOFvsFOR2)
  write.csv(dataTOFvsFOR2,"LULCC/TempTables//TOFvsFOR_Categories2.csv",row.names = FALSE)
}

if (file.exists("LULCC/TempTables/growth_parameters3.csv") == TRUE) {
  data_semicolon<-read.csv("LULCC/TempTables/growth_parameters3.csv", sep=";", header=T)
  data_comma<-read.csv("LULCC/TempTables/growth_parameters3.csv", sep=",", header=T)
  if (is.null(data_semicolon$TOF[1])) { 
    data_all3<-data_comma
  } else {
    data_all3<-data_semicolon
  }
  # Produce TOF vs FOR categories ####
  data_FOR3<-subset(data_all3, data_all1$TOF==0)
  data_TOF3<-subset(data_all3, data_all1$TOF==1)
  max_tot3<-nrow(data_all3)
  
  dataTOFvsFOR3<-as.data.frame(data_all3[ ,7])
  colnames(dataTOFvsFOR3)<-("x")
  dataTOFvsFOR3=data.frame(Key=c(1:max_tot3),dataTOFvsFOR3)
  write.csv(dataTOFvsFOR3,"LULCC/TempTables//TOFvsFOR_Categories3.csv",row.names = FALSE)
}

# Dinamica external scripts ----
# Dirs for system ----
gitlabdir.sys <- gsub("/", "\\", gitlabdir, fixed=TRUE)
countrydir.sys <- gsub("/", "\\", countrydir, fixed=TRUE)

# Friction ----
# WARNING: MARITIME AND ATRACTION LAYERS NEED TO BE FLESHED OUT AND DEBUG AS OF JULY 2023
if (friction == "R"){ 
  
  unlink("in/fricc_w.tif")
  unlink("in/fricc_v.tif")
  unlink("in/*.xml")
  
  # Vehicle friction
  # roads <-  raster("LULCC/SourceData/InRaster/roads.tif")
  # raster::unique(roads)
  # rivers <-  raster("LULCC/SourceData/InRaster/rivers.tif")
  # raster::unique(rivers)
  
  # rivers
  if (maritime == "YES") {
    rivers_c <- raster("LULCC/TempRaster/rivers_c.tif")
    # unique(rivers_c)
    rivers_rectable <- read_csv("LULCC/TempTables/Friction_rivers_reclass_r.csv")
    rivers_reclass.m <- reclassify(rivers_c,
                                   as.data.frame(rivers_rectable),
                                   right=NA)
    rivers_reclass.m[is.na(rivers_reclass.m[])] <- 0
    
    # Add maritime chunk
    maritime_c <- raster("LULCC/TempRaster/maritime_c.tif")
    maritime_c[is.na(maritime_c[])] <- 0
    rivers_reclass <- overlay(maritime_c, rivers_reclass.m,  
                              fun = function(x,y) {ifelse(x==1, x*0.8, y)} ) #empezar por aca
    
    
  } else if (maritime == "NO"){
    rivers_c <- raster("LULCC/TempRaster/rivers_c.tif")
    # unique(rivers_c)
    rivers_rectable <- read_csv("LULCC/TempTables/Friction_rivers_reclass_r.csv")
    rivers_reclass_prelakes <- reclassify(rivers_c,
                                          as.data.frame(rivers_rectable),
                                          right=NA)
    rivers_reclass_prelakes[is.na(rivers_reclass_prelakes[])] <- 0
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
    lakes_reclass.m[is.na(lakes_reclass.m[])] <- 0
    
    # Add maritime chunk
    maritime_c <- raster("LULCC/TempRaster/maritime_c.tif")
    maritime_c[is.na(maritime_c[])] <- 0
    lakes_reclass <- overlay(lakes_c, lakes_reclass.m,  
                             fun = function(x,y) {ifelse(x==1, x*0.8, y)} ) #empezar por aca
    
  } else if (maritime == "NO"){
    lakes_c <- raster("LULCC/TempRaster/lakes_c.tif")
    # unique(rivers_c)
    lakes_rectable <- read_csv("LULCC/TempTables/Friction_lakes_reclass_r.csv")
    lakes_reclass <- reclassify(lakes_c,
                                as.data.frame(lakes_rectable),
                                right=NA)
    lakes_reclass[is.na(lakes_reclass[])] <- 0
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
  borders_reclass[is.na(borders_reclass[])] <- 0
  # writeRaster(borders_reclass, "In/borders_reclass.tif", overwrite = TRUE)
  
  roads_reclass <- overlay(borders_reclass, roads_reclass_preborder, 
                           fun = function(x,y) {ifelse(x > 0, x, y)})
  roads_reclass[is.na(roads_reclass[])] <- 0
  # writeRaster(roads_reclass, "In/roads_reclass.tif", overwrite = TRUE)
  
  
  # slope
  DEM_c <- raster("LULCC/TempRaster/DEM_c.tif")
  slope_c <-  terrain(DEM_c, opt=c('slope'), unit='degrees', neighbors=8) # Prender luego de terminar de debuggear!
  # slope_c <- raster("LULCC/TempRaster/Slope_din.tif")
  walkingcrosscountry_table <- read_csv("LULCC/TempTables/Friction_walkingcrosscountry_r.csv")
  slope_reclass <- reclassify(slope_c,
                              as.data.frame(walkingcrosscountry_table))
  slope_reclass[is.na(slope_reclass[])] <- 0
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
  slopeoverroads[is.na(slopeoverroads[])] <- 0
  
  fricc_ww <- overlay(slopeoverroads, rivers_O_Wslope, 
                      fun = function(x,y) {ifelse(x > 0, x, y)} )
  
  if (attraction2 == "YES") { # Adjusted for East Africa 10 to 1000 km
    
    cb0 <- raster("LULCC/TempRaster/attraction_cb0.tif")
    cb1 <- raster("LULCC/TempRaster/attraction_cb1.tif")
    cb2 <- raster("LULCC/TempRaster/attraction_cb2.tif")
    cb3 <- raster("LULCC/TempRaster/attraction_cb3.tif")
    cb4 <- raster("LULCC/TempRaster/attraction_cb4.tif")
    
    fun_att <- function(x,y) {ifelse(!is.na(x), y/attdecay, y)} # Adjusted for East Africa 10 to 1000 km
    
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
      
      writeRaster(slope_c, "LULCC/TempRaster/Slope.tif", overwrite = TRUE)
      writeRaster(fricc_w, "In/fricc_w.tif", overwrite = TRUE)
      writeRaster(fricc_v, "In/fricc_v.tif", overwrite = TRUE)
      
    } else if (maritime == "NO"){
      
      fricc_w <- reclassify(fricc_ww, cbind(-Inf, 0, NA), right=TRUE)
      fricc_v <- reclassify(fricc_v10000, cbind(-Inf, 0, NA), right=TRUE)
      
      writeRaster(slope_c, "LULCC/TempRaster/Slope.tif", overwrite = TRUE)
      writeRaster(fricc_w, "In/fricc_w.tif", overwrite = TRUE)
      writeRaster(fricc_v, "In/fricc_v.tif", overwrite = TRUE)
      
    }
    
  } else if (attraction2 == "NO") {
    
    if (maritime == "YES") {
      fricc_w.m <- reclassify(fricc_ww, cbind(-Inf, 0, NA), right=TRUE)
      maritime_c.r <- maritime_c * 999999
      fricc_w <- overlay(fricc_w.m, maritime_c.r,
                         fun = function(x,y) {ifelse(is.na(y), x, x+y)} )
      fricc_v <- reclassify(fricc_vv, cbind(-Inf, 0, NA), right=TRUE)
      
      writeRaster(slope_c, "LULCC/TempRaster/Slope.tif", overwrite = TRUE)
      writeRaster(fricc_w, "In/fricc_w.tif", overwrite = TRUE)
      writeRaster(fricc_v, "In/fricc_v.tif", overwrite = TRUE)
      
    } else if (maritime == "NO") {
      
      fricc_w <- reclassify(fricc_ww, cbind(-Inf, 0, NA), right=TRUE)
      fricc_v <- reclassify(fricc_vv, cbind(-Inf, 0, NA), right=TRUE)
      
      writeRaster(slope_c, "LULCC/TempRaster/Slope.tif", overwrite = TRUE)
      writeRaster(fricc_w, "In/fricc_w.tif", overwrite = TRUE)
      writeRaster(fricc_v, "In/fricc_v.tif", overwrite = TRUE)
      
      unlink("in/*.xml")
      # plot(fricc_w)
      # plot(fricc_v)
      
    }
    
  }
  
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


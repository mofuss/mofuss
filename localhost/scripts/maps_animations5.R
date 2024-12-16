# MDG
# MoFuSS
# Version 3
# Date: Mar 2024

# 2dolist
# Check after fNRB partition tables and vectors how it works for country-based and Google Earth polys
# CtyPar<-read.csv("LULCC/SourceData/parameters.csv", header=T)
# CtyPar[] <- lapply(CtyPar, as.character)
## WARNING Faltan los del peridoo completo 2010 y 2020 hasta final year!!! ###
## Aunque estos salen de dinamica hasta cierto punto, faltaria 2020-final year ###

# Internal parameters
videoson <- 1
compilelatex <- 0
uncertaintystacks <- 0
fNRB_partition_tables <- 1
mcthreshold <- 30
copy_old_dinamica_rasters <- 0

# Load packages ----
library(readr)
library(dplyr)
library(animation)
library(data.table)
library(fBasics)
library(glue)
library(jpeg)
library(plyr)
library(png)
library(raster)
library(sf)
library(readr)
library(tiff)
library(gitlabr)
library(tidyverse)
library(spam)
library(sf)
library(tictoc)
library(mapview)
library(tidyverse)
library(rmapshaper)
library(foreach)
library(readxl)

# Using the system mapshaper - COULD NOT INSTALL IT IN WIN10!!!
check_sys_mapshaper()
system("mapshaper --version")
# https://github.com/mbloch/mapshaper
# https://github.com/ateucher/rmapshaper#using-the-system-mapshaper
# https://nodejs.org/en

# Read in the arguments listed at the command line in Dinamica EGO'S "Run external process" ####
args=(commandArgs(TRUE))

# "args" is now a list of character vectors.
# First check to see if arguments are passed.
# Then cycle through each element of the list and evaluate the expressions.
if(length(args)==0){
  print("No arguments supplied by DINAMICA.")
  ##Supply default values here (to be used when running the script through R directly)
  MC = 2 # MonteCarlo runs
  IT = 2010 # Initial year
  K_MC=1
  TOF_MC=1
  Ini_st_MC=75
  Ini_st.factor.percentage=100
  COVER_MAP=1
  rmax_MC=1
  DEF_FW=0
  IL=48 # Iteration length in week - each year = 48 weeks
  # STdyn=10 # Simulation length set by parameters table, it cycles in the repeat functor is STdyn+1 as 2 cycles are needed for 1 year: 1jan->31dec
  Harv.Pix.W=7000000
  Prune.W=1 
  Harv.Pix.V=7000000
  Prune.V=1
  Harv.Pix_MC=0
  Prune_MC=0
  # Subset_locs=0
  MaxAGB=400 # Maximum K for all LULC classes at any MC
  MaxAGB_firstMC=400 # Maximum K for all LULC classes at first MC run
  MaxAGB_lastMC=400 # Maximum K for all LULC classes at last MC run
  AGBmap=1
  SumTables=0
  OSType=64
  BaUvsICS="BaU"
  RerunMC=1
  cutoff_yrs=10
  
}else{
  for(i in 1:length(args)){
    eval(parse(text=args[[i]]))
  }
}

# Read parameters table ----
read.csv("LULCC/TempTables/Country.csv") %>%
  dplyr::filter(Key. == "1") %>%
  pull(Country) -> country_name

# Specify the directory where the file is located
parameters_directory <- paste0(getwd(),"/LULCC/DownloadedDatasets/SourceData",country_name)

# Use list.files() to find the file that matches the pattern
parameters_name <- list.files(path = parameters_directory, pattern = "^parameters.*\\.xlsx$", full.names = TRUE)

# Read the Excel file
country_parameters <- read_excel(parameters_name)
print(tibble::as_tibble(country_parameters), n=30)

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
  dplyr::filter(Var == "AGB1map") %>%
  pull(ParCHR) -> AGB1map

country_parameters %>%
  dplyr::filter(Var == "AGB2map") %>%
  pull(ParCHR) -> AGB2map

country_parameters %>%
  dplyr::filter(Var == "AGB3map") %>%
  pull(ParCHR) -> AGB3map

country_parameters %>%
  dplyr::filter(Var == "end_year") %>%
  pull(ParCHR) %>%
  as.integer(.) -> end_year
STdyn <- end_year - IT

country_parameters %>%
  dplyr::filter(Var == "GEpoly") %>%
  pull(ParCHR) %>%
  as.integer(.) -> GEpoly

if (LULCt1map == "YES") {
  tof_vs_for <- read_csv("LULCC/TempTables/growth_parameters1.csv") %>% 
    {if(is.null(.$TOF[1])) read_csv2(glue("LULCC/TempTables/growth_parameters1.csv")) else .} 
} else if (LULCt2map == "YES") {
  tof_vs_for <- read_csv("LULCC/TempTables/growth_parameters2.csv") %>% 
    {if(is.null(.$TOF[1])) read_csv2(glue("LULCC/TempTables/growth_parameters2.csv")) else .} 
} else if (LULCt3map == "YES") {
  tof_vs_for <- read_csv("LULCC/TempTables/growth_parameters2.csv") %>% 
    {if(is.null(.$TOF[1])) read_csv2(glue("LULCC/TempTables/growth_parameters3.csv")) else .} 
}

res<- read.csv("LULCC/TempTables/Resolution.csv", header=T)
resolution <- res[1,2]
userarea_r <- raster("LULCC/TempRaster/mask_c.tif")

globe_name <- paste0("LULCC/Countries/",country_name,".tif") # REPLACE!!!

if (BaUvsICS == "ICS") {
  OutDir<-"OutICS"
} else {
  OutDir<-"OutBaU"
}

if (OSType == 64) {
  ffmpeg_path<-file.path(getwd(),"ffmpeg64/bin/ffmpeg.exe")
} else {
  ffmpeg_path<-file.path(getwd(),"ffmpeg32/bin/ffmpeg.exe")
}

country_parameters %>%
  dplyr::filter(Var == "mapscale") %>%
  pull(ParCHR) %>%
  as.numeric() -> scalebar_loi
label_scalebar_loi<-paste0(scalebar_loi/1000, "km", sep="")

ST = (48/IL*STdyn)+1

print(MC)
print(IT)
print(K_MC)
print(TOF_MC)
print(Ini_st_MC)
print(Ini_st.factor.percentage)
Ini_st.factor = Ini_st.factor.percentage/100
print(COVER_MAP)
print(rmax_MC)
print(DEF_FW)
print(IL)
print(STdyn)
print(ST)
print(Harv.Pix.W)
print(Prune.W)
print(Harv.Pix.V)
print(Prune.V)
print(Harv.Pix_MC)
print(Prune_MC)
print(ffmpeg_path)
#print(Subset_locs)
print(MaxAGB)
print(AGBmap)
print(SumTables)
print(OSType)
print(BaUvsICS)

if (K_MC == 1) {
  K_MC_yesno = "Yes"
} else {
  K_MC_yesno = "No"
}

if (TOF_MC == 1) {
  TOF_MC_yesno = "Yes"
} else {
  TOF_MC_yesno = "No"
}

if (Ini_st_MC == 1) {
  Ini_st_MC_yesno = "Yes"
} else {
  Ini_st_MC_yesno = "No"
}

if (COVER_MAP == 1) {
  COVER_MAP_yesno = "Yes"
  Ini_st_MC = 0
  Ini_st_MC_yesno = "Not applicable"
  Ini_st.factor.percentage = "Tree cover as a % of K"
  Ini_st.factor.percentage_L = "Tree cover as fraction of K" #Couldn't pass "%" to LaTeX in "\%" form.
} else {
  COVER_MAP_yesno = "No"
  Ini_st.factor.percentage = paste(Ini_st.factor.percentage,"% of K",sep="")
  Ini_st.factor.percentage_L = paste(Ini_st.factor.percentage,"fraction of K",sep="") #Couldn't pass "%" to LaTeX in "\%" form.
}

if (rmax_MC == 1) {
  rmax_MC_yesno = "Yes"
} else {
  rmax_MC_yesno = "No"
}

if (DEF_FW == 1) {
  DEF_FW_yesno = "Yes"
} else {
  DEF_FW_yesno = "No"
}

# if (Subset_locs == 1) {
# 	Subset_locs_yesno = "Yes"
# } else {
# 	Subset_locs_yesno = "No"
# }

if (AGBmap == 1) {
  AGBmap_yesno = "Yes"
  COVER_MAP_yesno = "Not applicable"
  Ini_st_MC_yesno = "Not applicable"
  Ini_st.factor.percentage = "Using AGB map"
  Ini_st.factor.percentage_L = "Using AGB map"
} else {
  AGBmap_yesno = "No"
}

if (SumTables == 1) {
  SumTables_yesno = "Yes"
} else {
  SumTables_yesno = "No"
}

if (OSType == 32) {
  res1000<-100
  res600<-100
  res300<-100
} else {
  res1000<-1000
  res600<-600
  res300<-300
}

if (RerunMC == 1) {
  RerunMC_yesno = "Yes"
} else {
  RerunMC_yesno = "No"
}

# Save tables in csv with input parameters for BaU and ICS scenarios ####

ParDF = data.frame(c(
  "Spatial resolution",
  #"Operating System Type",
  "Type of scenario",
  #"Sample of localities of interest?",
  "StartUp year",
  "Simulation Length (SL)",
  "Number of MC realizations",
  #"Initial Stock",
  #"Initial stock through MC?",
  "K through MC?",
  "rmax through MC?",
  "TOF through MC?",
  #"Iteration length",
  "Re-run Monte Carlo?",
  #"Tree cover map provided?",
  "Aboveground biomass map provided?",
  "Accounting for fuelwood from deforestation?"
),
value=(c(
  paste(resolution," meters",sep=""),
  #paste(OSType,"-bit",sep=""),
  BaUvsICS,
  #Subset_locs_yesno,
  IT,
  paste(STdyn," years",sep=""),
  paste(MC," runs",sep=""),
  #Ini_st.factor.percentage_L,
  #Ini_st_MC_yesno,
  K_MC_yesno,
  rmax_MC_yesno,
  TOF_MC_yesno,
  #paste(IL," weeks (",IL*0.25," months)",sep=""),
  RerunMC_yesno,
  #COVER_MAP_yesno,
  AGBmap_yesno,
  DEF_FW_yesno
)))							

colnames(ParDF) <- c("Parameter",paste("Value",substr(OutDir,4,6),sep=""))

# if (BaUvsICS == "ICS") {
# write.csv(ParDF, "LULCC/TempTables/InputParaICS.csv", row.names=FALSE)
# } else {
write.csv(ParDF, "LULCC/TempTables/InputParaBaU.csv", row.names=FALSE)
#}

#rBaUt<-file.exists("LULCC/TempTables/InputParaBaU.csv")
#rICSt<-file.exists("LULCC/TempTables/InputParaICS.csv")
#if (rBaUt == "TRUE" & rICSt == "TRUE") {
rBaU<-read.csv("LULCC/TempTables/InputParaBaU.csv")
#rICS<-read.csv("LULCC/TempTables/InputParaICS.csv")
#rBauICS <- merge(rBaU,rICS,by="Parameter", sort=FALSE)

write.csv(rBaU, "LULCC/TempTables/InputPara.csv", row.names=FALSE, quote=FALSE)
#write.csv(rBauICS, "LULCC/TempTables/InputPara.csv", row.names=FALSE, quote=FALSE)
#unlink("LaTeX/InputPara.csv", force=TRUE)
file.copy("LULCC/TempTables/InputPara.csv", "LaTeX/InputPara.csv")
# } else { 
# 	"One out of two scenario table parameters is missing"		
# }

# Area of interest map (in UTM now) ####
country_parameters %>%
  dplyr::filter(Var == "treecover_name") %>%
  pull(ParCHR) -> treecover_name
aoi<-raster(paste("LULCC/SourceData/InRaster/",treecover_name,sep=""))
#aoi_r<-reclassify(aoi,c(-Inf,-1,NA,101,Inf,NA))
#aoi[aoi > 100] = NA
aoi_IniSt<-raster("Temp//2_IniSt01.tif")
mask <- st_read("LULCC/SourceData/InVector/extent_mask.gpkg")
extent_analysis <- st_read("LULCC/TempVector/ext_analysis.gpkg") #ERROR IN READING GEOPACKAGE
aoi_c<-crop(aoi,mask)

country_parameters %>%
  dplyr::filter(Var == "EPSG_utm") %>%
  pull(ParCHR) %>%
  as.integer(.) -> epsg_utm

Locs_r<-raster("LULCC//TempRaster//locs_c_w.tif") #Check there is locs_c_v as well!
Locs_p<-as.data.frame(rasterToPoints(Locs_r, fun=NULL, n=4, na.rm=TRUE, digits=12, dissolve=TRUE))
coordinates(Locs_p)=c("x", "y")
proj4string(Locs_p) <- "+proj=utm +zone=18 +datum=WGS84 +units=m +no_defs" #UTMproj

tiff(filename=paste0(OutDir,"//Area_of_Interest.tif"),
     width=290,height=290,units="mm",res=res300,bg="white",
     compression=c("lzw"),type=c("windows"),
     pointsize=12,family="",restoreConsole=TRUE)
plot(aoi_c, main="Area of Interest: set by user (red polygon)",
     ylab="UTM S-N coords - try depicting GCS coords over UTM raster?",
     xlab="UTM W-E coords - try depicting GCS coords over UTM raster?",
     cex.main=1.5,
     legend=TRUE, legend.width=2.5,  
     legend.args=list(text=bquote(Tree~cover~"(%)"~year~2000),side=4, font=2, line=-1.35, cex=1))
#legend.args=list(text=bquote(Tree~cover~"(%)"~year~ .(IT)),side=4, font=2, line=-1.35, cex=1))
plot(mask, border="black", col="transparent", add=TRUE, lwd = 2)
#plot(locs_figures_GCS, pch=19, cex=(0.25), add=TRUE) #Plotea localidades de interes en el ?rea de an?lisis
plot(extent_analysis, border="red", col="transparent", add=TRUE, lwd = 2.5)

par(new=TRUE, 
    plt=c(0,1,0,1), 
    mar=c(42,4.2,4.2,42), 
    usr=c(0,1,0,1)
)
greypallete<-gray.colors(255, start = 0.15, end = 1, gamma = 2, alpha = 1)
# image((raster(Globe_name)), col=greypallete, axes=FALSE, ann=FALSE) #GLOBE_NAME SHOULD COME FROM MOFUSS GITLAB
dev.off()


# Localities of Interest (in UTM) ####

Areaadj<-((xres(aoi_IniSt))^2)/10000 # replace by "1" to byass
MaxAGB_initial<-(cellStats(aoi_IniSt,max)/Areaadj)

# if (Subset_locs == 1) {
# 	locs_figures<-Extent_Locs_p
# } else {
locs_figures<-Locs_p
#}

tiff(filename=paste(OutDir,"//Localities_of_Interest.tif",sep=""),width=290,height=290,units="mm",res=res300,bg="white",compression=c("lzw"),
     type=c("windows"),pointsize=12,family="",restoreConsole=TRUE)
plot((aoi_IniSt/Areaadj), main="Localities of Interest: set by user",ylab="UTM S-N coords",xlab="UTM W-E coords",cex.main=1.5,
     legend=TRUE, legend.width=2.5, 
     #legend.args=list(text=expression("Aboveground Biomass (t ha"^-1*") circa year 2000"),side=4, font=2, line=-1.35, cex=1),
     legend.args=list(text=bquote("Aboveground Biomass (t ha"^-1*") circa year"~.(IT)),side=4, font=2, line=-1.35, cex=1),
     zlim=c(0,MaxAGB_initial))
plot(locs_figures, pch=19, cex=(0.05), add=TRUE)
scalebar(scalebar_loi,type='line', divs=4, lwd=2.5, label=label_scalebar_loi, cex=1.5)
plot(extent_analysis, border="red", col="transparent", add=TRUE, lwd = 0.25)

#par(new=TRUE, plt=c(0,1,0,1), mar=c(42,4.2,4.2,42), usr=c(0,1,0,1))
#greypallete<-gray.colors(255, start = 0.15, end = 1, gamma = 2, alpha = 1)
#image((raster(HondurasGlobe_name)), col=greypallete, axes=FALSE, ann=FALSE)
dev.off()


# Read files according to simulation lenght (ST) ####

# Files paths and names
bal="debugging_1//Growth_less_harv"
harv_tot="debugging_1//Harvest_tot"

# Un ciclo de 1 hasta MC en el que se lean estos archivos
# Los nombres acutalizarlos de acuerdo al contador del ciclo
# Conservar todos los archivos, ie hay que haer un arreglo en donde meterlos todos

# string #1
s0<-rep(bal,ST)

# string #2
s1<-seq(1,ST,1)
if (length(s1)<10) {    # cuando ST es menor de 10
  cmen10<-as.character(s1)
  c0s<-rep("0",length(cmen10)) # construye vector con "0"
  c2<-paste(c0s,cmen10,sep="")
} else {
  smen10<-s1[s1<10] #separa los numeros menores a 10
  smas10<-s1[s1>9] #separa los numeros mayores a 9
  cmas10<-as.character(smas10)
  cmen10<-as.character(smen10)
  c0s<-rep("0",length(smen10)) # construye vector con "0"
  c01<-paste(c0s,cmen10,sep="")
  c2<-c(c01,cmas10)
}
First_frame<-min(c2)
Last_frame<-max(c2)

# string #3
s2<-rep(".tif",ST)

# Final names
bal_names<-paste0(s0,c2,s2)
bal_names

# string #1_b
s0_b<-rep(harv_tot,ST)
# Nombres finales
harv_tot_names<-paste(s0_b,c2,s2,sep="")
harv_tot_names

MCx_s1<-seq(1,MC,1)
if (length(MCx_s1)<10) {    # cuando ST es menor de 10
  MCx_cmen10<-as.character(MCx_s1)
  MCx_c0s<-rep("0",length(MCx_cmen10)) # construye vector con "0"
  MCx_c2<-paste(MCx_c0s,MCx_cmen10,sep="")
} else {
  MCx_smen10<-MCx_s1[MCx_s1<10] #separa los numeros menores a 10
  MCx_smas10<-MCx_s1[MCx_s1>9] #separa los numeros mayores a 9
  MCx_cmas10<-as.character(MCx_smas10)
  MCx_cmen10<-as.character(MCx_smen10)
  MCx_c0s<-rep("0",length(MCx_smen10)) # construye vector con "0"
  MCx_c01<-paste(MCx_c0s,MCx_cmen10,sep="")
  MCx_c2<-c(MCx_c01,MCx_cmas10)
}
Last_MC<-max(MCx_c2)

x_s1<-seq(1,STdyn,1)
if (length(x_s1)<10) {    # cuando ST es menor de 10
  x_cmen10<-as.character(x_s1)
  x_c0s<-rep("0",length(x_cmen10)) # construye vector con "0"
  x_c2<-paste(x_c0s,x_cmen10,sep="")
} else {
  x_smen10<-x_s1[x_s1<10] #separa los numeros menores a 10
  x_smas10<-x_s1[x_s1>9] #separa los numeros mayores a 9
  x_cmas10<-as.character(x_smas10)
  x_cmen10<-as.character(x_smen10)
  x_c0s<-rep("0",length(x_smen10)) # construye vector con "0"
  x_c01<-paste(x_c0s,x_cmen10,sep="")
  x_c2<-c(x_c01,x_cmas10)
}
Last_STdyn <- max(x_c2)


# Map AGB ####

runagbmap <- 1

if(runagbmap == 1){
  
  graphics.off()
  tiff(filename=paste(OutDir,"//Map_AGB.tif",sep=""),width=170,height=200,units="mm",res=res600,bg="white",compression=c("lzw"),type=c("windows"),pointsize=12,family="",restoreConsole=TRUE)
  
  par(mfrow = c(3, 2), oma=c(1.5,1.5,0,1), mar=c(3,3,4,1))
  
  # Graphic parameters AGB map
  mainsize <- 1.1
  legwidth <- 2.5
  axissize <- 1
  labelsize <- 0.75
  barline <- (-1.1)
  redline_wd <- 0.5
  # Define a color gradient from white to red
  color_pal <- colorRampPalette(c("white", "orange", "red"))
  # Create a color gradient from white to red
  colors <- color_pal(100)  # Create 100 intermediate colors
  
  # bal_t0_maxAGB <- cellStats((raster("Temp//2_IniSt01.tif")),max) # Initial AGB for 1st MC run
  # bal_tn_maxAGB <- cellStats((raster("Temp//2_AGBt101.tif")),max) # Final AGB for 1st MC run
  bal_t0_maxAGB <- cellStats((raster(paste0("debugging_1/Growth_less_harv",cutoff_yrs,".tif"))),max) # Initial AGB for 1st MC run
  bal_tn_maxAGB <- cellStats((raster(paste0("debugging_1/Growth_less_harv",STdyn+1,".tif"))),max) # Final AGB for 1st MC run
  MaxAGB_1stMC_bind<-cbind(bal_t0_maxAGB,bal_tn_maxAGB)
  MaxAGB_1stMC <- ((max(MaxAGB_1stMC_bind[1, ], na.rm=TRUE))/Areaadj)
  
  bal_t0<-(raster(paste0("debugging_1/Growth_less_harv",cutoff_yrs,".tif"))) # Initial AGB for 1st MC run
  plot((bal_t0/Areaadj), main=paste0("Aboveground Biomass ",IT+cutoff_yrs),cex.main=mainsize, useRaster=TRUE,
       legend=TRUE, legend.width=legwidth, legend.shrink=1,cex.axis=axissize,
       legend.args=list(text=expression("t ha"^-1*""),side=4, font=2, line=barline, cex=labelsize),zlim=c(0,MaxAGB_1stMC))
  plot(extent_analysis, border="red", col="transparent", add=TRUE, lwd = redline_wd)
  
  bal_tn<-(raster(paste0("debugging_1/Growth_less_harv",STdyn+1,".tif"))) # Final AGB for 1st MC run
  plot((bal_tn/Areaadj), main=paste0("Aboveground Biomass ",(IT+as.numeric(Last_STdyn))),cex.main=mainsize, useRaster=TRUE,
       legend=TRUE, legend.width=legwidth, legend.shrink=1,cex.axis=axissize,
       legend.args=list(text=expression("t ha"^-1*""),
                        side=4, font=2, line=barline, cex=labelsize),zlim=c(0,MaxAGB_1stMC),
       scalebar(scalebar_loi,type='line', divs=4, lwd=1.25, label=label_scalebar_loi, cex=1)
  )
  plot(extent_analysis, border="red", col="transparent", add=TRUE, lwd = redline_wd)
  
  agbdiff <- bal_tn - bal_t0
  NRBneg <- calc(agbdiff, fun = function(x) { ifelse(x >= 0, 0, x) })
  NRB <- NRBneg*-1
  NRBmax<-(cellStats(NRB,max)/Areaadj)
  if(NRBmax == 0){
    plot((NRB/Areaadj), main=paste0("NRB: period ",IT+cutoff_yrs," to ",(IT+as.numeric(Last_STdyn))),
         col=colors,  # Apply the custom color palette
         useRaster=TRUE,
         legend=TRUE, legend.width=legwidth, legend.shrink=1,cex.axis=axissize,
         legend.args=list(text=expression("t ha"^-1*""),
                          side=4, font=2, line=barline, cex=labelsize),
         zlim=c(0,1))
    plot(extent_analysis, border="red", col="transparent", add=TRUE, lwd = redline_wd)
  } else {
    plot((NRB/Areaadj), main=paste0("NRB: period ", IT + cutoff_yrs, " to ", (IT + as.numeric(Last_STdyn))),
         col=colors,  # Apply the custom color palette
         useRaster=TRUE,  # Use raster method for plotting if suitable
         legend=TRUE, legend.width=legwidth, legend.shrink=1, cex.axis=axissize,
         legend.args=list(text=expression("t ha"^-1*""), side=4, font=2, line=barline, cex=labelsize),
         zlim=c(0, NRBmax))  # Control the scale of the color gradient
    plot(extent_analysis, border="red", col="transparent", add=TRUE, lwd = redline_wd)
    
  }
  
  # Define the range of your raster files
  file_numbers <- cutoff_yrs:(STdyn+1)
  # Create a vector of file paths
  file_paths <- paste0("debugging_1/Harvest_tot", file_numbers, ".tif")
  # Load all rasters into a stack
  raster_stack <- stack(file_paths)
  # Sum all the rasters in the stack
  summed_harvest <- sum(raster_stack)
  fNRB <- NRB / summed_harvest *100 
  
  # fNRB<-(raster("Temp//2_fNRB01.tif"))*100 # fNRB for the entire simulation period for 1st MC run
  plot(fNRB, main=paste0("fNRB: period ",IT+cutoff_yrs," to ",(IT+as.numeric(Last_STdyn))),
       col=colors,  # Apply the custom color palette
       useRaster=TRUE,  # Use raster method for plotting if suitable
       legend=TRUE, legend.width=legwidth, legend.shrink=1, cex.axis=axissize,
       legend.args=list(text=expression("%"), side=4, font=2, line=barline, cex=labelsize),
       zlim=c(0,100))  # Control the scale of the color gradient
  plot(extent_analysis, border="red", col="transparent", add=TRUE, lwd = redline_wd)
  
  
  # WARNING!!! Fw_def_tot = 2010-2030 ----
  Fw_def_tot<-(raster("Temp//2_FW_DEF01.tif")) # Cumulative fuelwod from deforestation for simulation period for 1st MC
  Fw_defmax<-(cellStats(Fw_def_tot,max)/Areaadj)
  if(Fw_defmax == 0){	
    plot((Fw_def_tot/Areaadj), main=paste0("Fuelwood from deforestation: period ",IT+cutoff_yrs," to ",(IT+as.numeric(Last_STdyn))), 
         cex.main=mainsize, 
         useRaster=TRUE,
         legend=TRUE, legend.width=legwidth, legend.shrink=1,cex.axis=axissize,
         legend.args=list(text=expression("t ha"^-1*""), side=4, font=2, line=barline, cex=labelsize),
         zlim=c(0,1))
    plot(extent_analysis, border="red", col="transparent", add=TRUE, lwd = redline_wd)
  } else {
    plot((Fw_def_tot/Areaadj), main=paste0("Fuelwood from deforestation: period ",IT," to ",(IT+as.numeric(Last_STdyn))),
         cex.main=mainsize, 
         useRaster=TRUE,
         legend=TRUE, legend.width=legwidth, legend.shrink=1,cex.axis=axissize,
         legend.args=list(text=expression("t ha"^-1*""), side=4, font=2, line=barline, cex=labelsize),zlim=c(0,Fw_defmax))
    plot(extent_analysis, border="red", col="transparent", add=TRUE, lwd = redline_wd)
  }
  
  # Harv_tot <- (raster("Temp//2_CON_TOT01.tif")) # Cumulative fuelwood harvest for simulation period for 1st MC
  Harv_tot <- summed_harvest
  Harv_totmax<-(cellStats(Harv_tot,max)/Areaadj)
  if(Harv_totmax == 0){
    plot((Harv_tot/Areaadj), main=paste0("Harvested fuelwood: period ",IT+cutoff_yrs," to ",(IT+as.numeric(Last_STdyn))),
         col=colors,  # Apply the custom color palette 
         useRaster=TRUE,
         legend=TRUE, legend.width=legwidth, legend.shrink=1,cex.axis=axissize,
         legend.args=list(text=expression("t ha"^-1*""), side=4, font=2, line=barline, cex=labelsize),
         zlim=c(0,1))
    # plot(locs_figures, pch=19, cex=(0.10), add=TRUE)
    plot(extent_analysis, border="red", col="transparent", add=TRUE, lwd = redline_wd)
  } else {
    plot((Harv_tot/Areaadj), main=paste0("Harvested fuelwood: period ",IT+cutoff_yrs," to ",(IT+as.numeric(Last_STdyn))),
         col=colors,  # Apply the custom color palette
         useRaster=TRUE,
         legend=TRUE, legend.width=legwidth, legend.shrink=1,cex.axis=axissize,
         legend.args=list(text=expression("t ha"^-1*""), side=4, font=2, line=barline, cex=labelsize),
         zlim=c(0,Harv_totmax))
    # plot(locs_figures, pch=19, cex=(0.10), add=TRUE)
    plot(extent_analysis, border="red", col="transparent", add=TRUE, lwd = redline_wd)
  }
  
  title(ylab="UTM S-N coords", xlab="UTM W-E coords\n",sub="Showing first Monte Carlo run",outer=TRUE, line=0.35)
  dev.off()
}

# Animations ####
if (videoson == 1){
  ## HTML video - corresponds to last MC run ####
  
  MaxAGB_lastMC<-(MaxAGB_lastMC/Areaadj)
  
  seqyr<-seq(1,STdyn+1,(1/(48/IL)))
  seqyrtrunc<-trunc(seqyr)
  simyr<-as.data.frame(seqyrtrunc)
  
  saveHTML({
    par(mar = c(5, 5, 5, 1))
    for(i in 1:ST){
      if (simyr[i, ]<10) {
        cerohtml=NULL
      } else {
        cerohtml=NULL
      }
      plot((raster(bal_names[i])/Areaadj),main=paste("Aboveground Biomass: period ",IT," to ",(IT+as.numeric(Last_STdyn)),"\n year: ",cerohtml,(IT-1+simyr[i, ]),sep=""),
           sub="Showing first Monte Carlo run", xlab="UTM W-E coords", ylab="UTM S-N coords",cex.main=2,
           legend=TRUE, legend.width=3, legend.shrink=1,cex.axis=1,
           legend.args=list(text=expression("Aboveground Biomass (t ha"^-1*")"),
                            side=4, font=2, line=-1.37, cex=1.25),zlim=c(0,MaxAGB_lastMC))
      ani.options(autoplay=TRUE)
      scalebar(scalebar_loi,type='line', divs=4, lwd=2, label=label_scalebar_loi, cex=1.5)
      plot(locs_figures, pch=19, cex=(0.25), add=TRUE)
      plot(extent_analysis, border="red", col="transparent", add=TRUE, lwd = redline_wd)
    }
    
  },
  img.name = "norm_plot", single.opts = "utf8: false", autoplay = FALSE, interval = 0.5,
  imgdir = "norm_dir", htmlfile = "AGB_dynamics.html", ani.height = 550, ani.width = 900,
  title = "AGB Dynamics",
  description = c("Insertar en mi pagina web como html 5. Revisar que DINAMICA pueda abrir el browser desde la consola,",
                  "Pasar argumentos (args) pa'adelante y pa'atras...", "NRBv1.0",
                  ani.options(outdir = paste(getwd(),"/HTML_animation_",OutDir,"/",sep="")))
  )
  
  # Move HTML files into single directory (in case it was not done before - Win8 bug)
  
  AGBexist<-file.exists("AGB_dynamics.html")
  if (AGBexist == "TRUE") {
    file.copy("css", paste("HTML_animation_",OutDir,sep=""), recursive=TRUE)
    file.copy("norm_dir", paste("HTML_animation_",OutDir,sep=""), recursive=TRUE)
    file.copy("js", paste("HTML_animation_",OutDir,sep=""), recursive=TRUE)
    file.copy("AGB_dynamics.html", paste("HTML_animation_",OutDir,sep=""))
    unlink("css", recursive=TRUE, force=TRUE)
    unlink("norm_dir", recursive=TRUE, force=TRUE)
    unlink("js", recursive=TRUE, force=TRUE)
    unlink("AGB_dynamics.html", recursive=TRUE, force=TRUE)
  } else {
    "HTML animation OK!"
  }
  
  
  # # WMV video - corresponds to last MC run ####
  
  Harvest_MAX<-(raster(harv_tot_names[1]))
  HarMAX<-(cellStats(Harvest_MAX,max)/Areaadj)
  if(HarMAX == 0){
    HarMAX=1
  } else {
    HarMax<-(cellStats(Harvest_MAX,max)/Areaadj)
  }
  
  # Video_path<-paste(getwd(),"/",OutDir,"/Growth_Harvest_Ani.wmv",sep="")
  #
  # saveVideo(expr={
  # par(mfrow = c(1, 2), oma=c(1,1,1,1), mar=c(4,4,2,1))
  # 	for(i in 1:ST){
  # 		if (simyr[i, ]<10) {
  # 			cerowmv="0"
  # 		} else {
  # 			cerowmv=NULL
  # 		}
  #
  # 		plot((raster(harv_tot_names[i])/Areaadj),
  # 			main=paste("Annually harvested fuelwood ",(IT+as.numeric(cerohtml)),(IT+simyr[i, ]),sep=""),cex.main=1.5,
  # 			#main=paste("Annually harvested fuelwood ",cerowmv,simyr[i, ],sep=""),cex.main=1.5,
  # 			legend=TRUE, legend.width=6, legend.shrink=0.4, cex.axis=1,
  # 			legend.args=list(text=expression("t ha"^-1*"yr"^-1*""),side=4, font=2, line=(-1.35), cex=1.25),
  # 			zlim=c(0,HarMAX))
  # 		scalebar(scalebar_loi,type='line', divs=4, lwd=1.5, label=label_scalebar_loi, cex=1.15)
  # 		plot(locs_figures, pch=19, cex=(0.25), add=TRUE)
  # 		plot(extent_analysis, border="red", col="transparent", add=TRUE, lwd = redline_wd)
  # 		title(xlab="UTM W-E coords\nShowing last Monte Carlo run",ylab="UTM S-N coords", sub="Showing first Monte Carlo run", outer=TRUE, line=-0.5)
  #
  # 		plot((raster(bal_names[i])/Areaadj),
  # 			main=paste("Aboveground Biomass ",(IT+as.numeric(cerohtml)),(IT+simyr[i, ]),sep=""),cex.main=1.5,
  # 			#main=paste("Aboveground Biomass 20",cerowmv,simyr[i, ],sep=""),cex.main=1.5,
  # 			legend=TRUE, legend.width=6,legend.shrink=0.4, cex.axis=1,
  # 			legend.args=list(text=expression("t ha"^-1*""),side=4, font=2, line=(-1.35), cex=1.25),
  # 			zlim=c(0,MaxAGB_lastMC))
  # 		plot(locs_figures, pch=19, cex=(0.25), add=TRUE)
  # 		plot(extent_analysis, border="red", col="transparent", add=TRUE, lwd = redline_wd)
  # 		}
  # 	},
  # 		ffmpeg = ffmpeg_path, other.opts = "-vcodec mpeg4 -r 30000/1001 -b:v 6M",
  # 		video.name = Video_path, overwrite=TRUE,ani.height = 700, ani.width = 1200, interval = 1, nmax=ST)
  
  
  
  # # AVI video - corresponds to last MC run ####
  
  
  # Video_path<-paste(getwd(),"/",OutDir,"/Growth_Harvest_Ani.avi",sep="")
  #
  # saveVideo(expr={
  # par(mfrow = c(1, 2), oma=c(1,1,1,1), mar=c(4,4,2,1))
  # 	for(i in 1:ST){
  # 		if (simyr[i, ]<10) {
  # 			ceroavi="0"
  # 		} else {
  # 			ceroavi=NULL
  # 		}
  #
  # 		plot((raster(harv_tot_names[i])/Areaadj),
  # 			main=paste("Annually harvested fuelwood ",(IT+as.numeric(cerohtml)),(IT+simyr[i, ]),sep=""),cex.main=1.5,
  # 			#main=paste("Annually harvested fuelwood 20",ceroavi,simyr[i, ],sep=""),cex.main=1.5,
  # 			legend=TRUE, legend.width=6, legend.shrink=0.4, cex.axis=1,
  # 			legend.args=list(text=expression("t ha"^-1*"yr"^-1*""),side=4, font=2, line=(-1.35), cex=1.25),
  # 			zlim=c(0,HarMAX))
  # 		scalebar(scalebar_loi,type='line', divs=4, lwd=1.5, label=label_scalebar_loi, cex=1.15)
  # 		plot(locs_figures, pch=19, cex=(0.25), add=TRUE)
  # 		plot(extent_analysis, border="red", col="transparent", add=TRUE, lwd = redline_wd)
  # 		title(xlab="UTM W-E coords\nShowing last Monte Carlo run",ylab="UTM S-N coords", outer=TRUE, line=-0.5)
  #
  # 		plot((raster(bal_names[i])/Areaadj),
  # 			main=paste("Aboveground Biomass ",(IT+as.numeric(cerohtml)),(IT+simyr[i, ]),sep=""),cex.main=1.5,
  # 			#main=paste("Aboveground Biomass 20",ceroavi,simyr[i, ],sep=""),cex.main=1.5,
  # 			legend=TRUE, legend.width=6,legend.shrink=0.4, cex.axis=1,
  # 			legend.args=list(text=expression("t ha"^-1*""),side=4, font=2, line=(-1.35), cex=1.25),
  # 			zlim=c(0,MaxAGB_lastMC))
  # 		plot(locs_figures, pch=19, cex=(0.25), add=TRUE)
  # 		plot(extent_analysis, border="red", col="transparent", add=TRUE, lwd = redline_wd)
  # 		}
  # 	},
  # 		ffmpeg = ffmpeg_path, other.opts = "-vcodec mpeg4 -r 30000/1001 -b:v 6M",
  # 		video.name = Video_path, overwrite=TRUE,ani.height = 700, ani.width = 1200, interval = 1, nmax=ST)
  
  # MP4 video - corresponds to last MC run ####
  
  Video_path<-paste(getwd(),"/",OutDir,"/Growth_Harvest_Ani",OutDir,".mp4",sep="")
  
  saveVideo(expr={
    par(mfrow = c(1, 2), oma=c(1,1,1,1), mar=c(4,4,2,1))
    for(i in 1:ST){
      if (simyr[i, ]<10) {
        ceroavi="0"
      } else {
        ceroavi=NULL
      }
      
      plot((raster(harv_tot_names[i])/Areaadj),
           main=paste("Annually harvested fuelwood ",(IT+as.numeric(cerohtml)),(IT+simyr[i, ]),sep=""),cex.main=1.5,
           col=colors, 
           #main=paste("Annually harvested fuelwood 20",ceroavi,simyr[i, ],sep=""),cex.main=1.5,
           legend=TRUE, legend.width=6, legend.shrink=0.4, cex.axis=1,
           legend.args=list(text=expression("t ha"^-1*"yr"^-1*""),side=4, font=2, line=(-1.35), cex=1.25),
           zlim=c(0,HarMAX))
      scalebar(scalebar_loi,type='line', divs=4, lwd=1.5, label=label_scalebar_loi, cex=1.15)
      # plot(locs_figures, pch=19, cex=(0.25), add=TRUE)
      plot(extent_analysis, border="red", col="transparent", add=TRUE, lwd = redline_wd)
      title(xlab="UTM W-E coords\nShowing last Monte Carlo run",ylab="UTM S-N coords", outer=TRUE, line=-0.5)
      
      plot((raster(bal_names[i])/Areaadj),
           main=paste("Aboveground Biomass ",(IT+as.numeric(cerohtml)),(IT+simyr[i, ]),sep=""),cex.main=1.5,
           #main=paste("Aboveground Biomass 20",ceroavi,simyr[i, ],sep=""),cex.main=1.5,
           legend=TRUE, legend.width=6,legend.shrink=0.4, cex.axis=1,
           legend.args=list(text=expression("t ha"^-1*""),side=4, font=2, line=(-1.35), cex=1.25),
           zlim=c(0,MaxAGB_1stMC)) # Era este: MaxAGB_lastMC
      #plot(locs_figures, pch=19, cex=(0.25), add=TRUE)
      plot(extent_analysis, border="red", col="transparent", add=TRUE, lwd = redline_wd)
    }
  },
  ffmpeg = ffmpeg_path, other.opts = "-c:v libx264 -r 30000/1001 -b:v 6M -vf scale=iw*3:ih*3 -pix_fmt yuv420p",
  video.name = Video_path, overwrite=TRUE,ani.height = 700, ani.width = 1200, interval = 1, nmax=ST)
  unlink("LaTeX/Growth_Harvest_Ani.mp4", force=TRUE)
  file.copy(paste(OutDir,"/Growth_Harvest_Ani",OutDir,".mp4",sep=""), paste("LaTeX/Growth_Harvest_Ani",OutDir,".mp4",sep=""))
}


# Spatial stats ####
## Standard Deviation and Variance in NRB ####		
# listNRB <- list.files("Temp", pattern = "^2_NRB.+[.]tif$",ignore.case=F)
# StackNRB <- stack(paste("Temp/",listNRB,sep=""))
# nlay <- nlayers(StackNRB)
# NRBmean <- stackApply(StackNRB, indices=1, fun=mean)
# writeRaster(NRBmean, filename="Temp//aNRBmean.tif", datatype="FLT4S", overwrite=TRUE)
# writeRaster(NRBmean, filename="Temp//3_NRBmean.tif", datatype="FLT4S", overwrite=TRUE)

if (uncertaintystacks == 1) {
  NRBvar <- stackApply(StackNRB, indices=1, fun=var)
  NRBsd <- sqrt(NRBvar)
  writeRaster(NRBvar, filename="Temp//aNRBvar.tif", datatype="FLT4S", overwrite=TRUE)
  writeRaster(NRBsd, filename="Temp//aNRBsd.tif", datatype="FLT4S", overwrite=TRUE)
  writeRaster(NRBvar, filename="Temp//3_NRBvar.tif", datatype="FLT4S", overwrite=TRUE)
  writeRaster(NRBsd, filename="Temp//3_NRBsd.tif", datatype="FLT4S", overwrite=TRUE)
  
  NRBsdmax <- (cellStats(NRBsd,max)/Areaadj)
  
  tiff(filename=paste(OutDir,"//NRBsd.tif",sep=""),width=290,height=290,units="mm",res=res300,bg="white",compression=c("lzw"),type=c("windows"),pointsize=12,family="",restoreConsole=TRUE)
  plot((NRBsd/Areaadj), main=paste("Standard Deviation in NRB \n after ",MC," Monte Carlo runs for ",STdyn,"-year simulations",sep=""),ylab="UTM S-N coords",xlab="UTM W-E coords",
       col=colors,  # Apply the custom color palette
       cex.main=1.5,
       legend=TRUE, legend.width=2.5,
       legend.args=list(text=expression("t ha"^-1*""),side=4, font=2, line=-1.35, cex=1.35),zlim=c(0,NRBsdmax))
  #plot(locs_figures, pch=19, cex=(0.50), add=TRUE)
  scalebar(scalebar_loi,type='line', divs=4, lwd=2.5, label=label_scalebar_loi, cex=1.5)
  plot(extent_analysis, border="red", col="transparent", add=TRUE, lwd = 1.75)
  dev.off()
  
}

# listCON_NRB <-list.files("Temp", pattern = "^2_CON_NRBs.+[.]tif$",ignore.case=F) # Dinamica saves "2_CON_NRB##.tif AND "2_CON_NRBs##.tif. Exactly the same, erase one of them and save time
# StackCON_NRB <- stack(paste("Temp/",listCON_NRB,sep=""))
# nlayCON_NRB <- nlayers(StackCON_NRB)
# CON_NRBmean <- stackApply(StackCON_NRB, indices=1, fun=mean)
# writeRaster(CON_NRBmean, filename="Temp//aCON_NRBmean.tif", datatype="FLT4S", overwrite=TRUE)
# writeRaster(CON_NRBmean, filename="Temp//3_CON_NRBmean.tif", datatype="FLT4S", overwrite=TRUE)

if (uncertaintystacks == 1) {
  CON_NRBvar <- stackApply(StackCON_NRB, indices=1, fun=var)
  CON_NRBsd <- sqrt(CON_NRBvar)
  writeRaster(CON_NRBvar, filename="Temp//aCON_NRBvar.tif", datatype="FLT4S", overwrite=TRUE)
  writeRaster(CON_NRBsd, filename="Temp//aCON_NRBsd.tif", datatype="FLT4S", overwrite=TRUE)
  writeRaster(CON_NRBvar, filename="Temp//3_CON_NRBvar.tif", datatype="FLT4S", overwrite=TRUE)
  writeRaster(CON_NRBsd, filename="Temp//3_CON_NRBsd.tif", datatype="FLT4S", overwrite=TRUE)
  
  CON_NRBsdmax<-(cellStats(CON_NRBsd,max)/Areaadj)
  
  tiff(filename=paste(OutDir,"//CON_NRBsd.tif",sep=""),width=290,height=290,units="mm",res=res300,bg="white",compression=c("lzw"),type=c("windows"),pointsize=12,family="",restoreConsole=TRUE)
  plot((CON_NRBsd/Areaadj), main=paste("Standard Deviation in fuelwood use \n after ",MC," Monte Carlo runs for ",STdyn,"-year simulations",sep=""),ylab="UTM S-N coords",xlab="UTM W-E coords",
       col=colors,  # Apply the custom color palette
       cex.main=1.5,
       legend=TRUE, legend.width=2.5,
       legend.args=list(text=expression("t ha"^-1*""),side=4, font=2, line=-1.35, cex=1.35),zlim=c(0,CON_NRBsdmax))
  #plot(locs_figures, pch=19, cex=(0.50), add=TRUE)
  scalebar(scalebar_loi,type='line', divs=4, lwd=2.5, label=label_scalebar_loi, cex=1.5)
  plot(extent_analysis, border="red", col="transparent", add=TRUE, lwd = 1.75)
  dev.off()
  
}

# listCON_TOT<-list.files("Temp", pattern = "^2_CON_TOT.+[.]tif$",ignore.case=F)
# StackCON_TOT <- stack(paste("Temp/",listCON_TOT,sep=""))
# nlayCON_TOT<-nlayers(StackCON_TOT)
# CON_TOTmean<- stackApply(StackCON_TOT, indices=1, fun=mean)
# writeRaster(CON_TOTmean, filename="Temp//aCON_TOTmean.tif", datatype="FLT4S", overwrite=TRUE)
# writeRaster(CON_TOTmean, filename="Temp//3_CON_TOTmean.tif", datatype="FLT4S", overwrite=TRUE)

if (uncertaintystacks == 1) {
  CON_TOTvar <- stackApply(StackCON_TOT, indices=1, fun=var)
  CON_TOTsd <- sqrt(CON_TOTvar)
  writeRaster(CON_TOTvar, filename="Temp//aCON_TOTvar.tif", datatype="FLT4S", overwrite=TRUE)
  writeRaster(CON_TOTsd, filename="Temp//aCON_TOTsd.tif", datatype="FLT4S", overwrite=TRUE)
  writeRaster(CON_TOTvar, filename="Temp//3_CON_TOTvar.tif", datatype="FLT4S", overwrite=TRUE)
  writeRaster(CON_TOTsd, filename="Temp//3_CON_TOTsd.tif", datatype="FLT4S", overwrite=TRUE)
  
  CON_TOTsdmax<-(cellStats(CON_TOTsd,max)/Areaadj)
  
  tiff(filename=paste(OutDir,"//CON_TOTsd.tif",sep=""),width=290,height=290,units="mm",res=res300,bg="white",compression=c("lzw"),type=c("windows"),pointsize=12,family="",restoreConsole=TRUE)
  plot((CON_TOTsd/Areaadj), main=paste("Standard Deviation in fuelwood use driving degradation \n after ",MC," Monte Carlo runs for ",STdyn,"-year simulations",sep=""),ylab="UTM S-N coords",xlab="UTM W-E coords",
       col=colors,  # Apply the custom color palette
       cex.main=1.5,
       legend=TRUE, legend.width=2.5,
       legend.args=list(text=expression("t ha"^-1*""),side=4, font=2, line=-1.35, cex=1.35),zlim=c(0,CON_TOTsdmax))
  #plot(locs_figures, pch=19, cex=(0.50), add=TRUE)
  scalebar(scalebar_loi,type='line', divs=4, lwd=2.5, label=label_scalebar_loi, cex=1.5)
  plot(extent_analysis, border="red", col="transparent", add=TRUE, lwd = 1.75)
  dev.off()
  
}

# Summary tables ####

if (SumTables == 1) {
  
  dir.create("OutBaU/webmofuss_results/") 
  
  country_parameters %>%
    dplyr::filter(Var == "ext_analysis_ID") %>%
    pull(ParCHR) -> ext_analysis_ID
  
  country_parameters %>%
    dplyr::filter(Var == "ext_analysis_NAME") %>%
    pull(ParCHR) -> ext_analysis_NAME
  
  country_parameters %>%
    dplyr::filter(Var == "ext_analysis_ID_1") %>%
    pull(ParCHR) -> ext_analysis_ID_1
  
  country_parameters %>%
    dplyr::filter(Var == "ext_analysis_NAME_1") %>%
    pull(ParCHR) -> ext_analysis_NAME_1
  
  country_parameters %>%
    dplyr::filter(Var == "ext_analysis_ID_2") %>%
    pull(ParCHR) -> ext_analysis_ID_2
  
  country_parameters %>%
    dplyr::filter(Var == "ext_analysis_NAME_2") %>%
    pull(ParCHR) -> ext_analysis_NAME_2
  
  userarea_gpkg <- st_read("LULCC/TempVector/userarea.gpkg") 
  userarea_df <- userarea_gpkg %>% st_drop_geometry()
  
  admin <- raster("LULCC/TempRaster//admin_c.tif")
  StackNRB[StackNRB == 0] = NA #This is to sum and average only positive NRB values
  NRBzon_sum <- as.data.frame(zonal(StackNRB, admin, 'sum')) %>%
    as.data.table()
  NRBzon_sum_1mc <- as.data.frame(zonal(StackNRB[[1]], admin, 'sum')) %>%
    as.data.table() %>%
    setnames(.,"sum", "NRB_1MC")
  # zonal(StackNRB, admin, 'mean')
  # zonal(StackNRB, admin, 'sd')
  
  NRBzon_mean <- NRBzon_sum[, list(NRB_MC_mean = rowMeans(.SD)), by = zone]
  NRBzon_sd <- NRBzon_sum[, list(NRB_MC_sd = rowSds(.SD)), by = zone]
  
  NRBzon_sum_m2 <- merge(userarea_df, NRBzon_sum_1mc, by.x = ext_analysis_ID, by.y = "zone") %>%
    merge(NRBzon_mean, by.x = ext_analysis_ID, by.y = "zone") %>%
    merge(NRBzon_sd, by.x = ext_analysis_ID, by.y = "zone")
  
  StackCON_TOT[StackCON_TOT == 0] = NA 
  CON_TOTzon_sum <- as.data.frame(zonal(StackCON_TOT, admin, 'sum')) %>%
    as.data.table()
  CON_TOTzon_sum_1mc <- as.data.frame(zonal(StackCON_TOT[[1]], admin, 'sum')) %>%
    as.data.table() %>%
    setnames(.,"sum", "CON_TOT_1MC")
  #zonal(StackCON_TOT, admin, 'mean')
  #zonal(StackCON_TOT, admin, 'sd')
  
  CON_TOTzon_mean <- CON_TOTzon_sum[, list(CON_TOT_MC_mean = rowMeans(.SD)), by = zone]
  CON_TOTzon_sd <- CON_TOTzon_sum[, list(CON_TOT_MC_sd = rowSds(.SD)), by = zone]
  
  CON_TOTzon_sum_m2 <- merge(userarea_df, CON_TOTzon_sum_1mc, by.x = ext_analysis_ID, by.y = "zone") %>%
    merge(CON_TOTzon_mean, by.x = ext_analysis_ID, by.y = "zone") %>%
    merge(CON_TOTzon_sd, by.x = ext_analysis_ID, by.y = "zone")
  
  StackCON_NRB[StackCON_NRB == 0] = NA
  CON_NRBzon_sum <- as.data.frame(zonal(StackCON_NRB, admin, 'sum')) %>%
    as.data.table()
  CON_NRBzon_sum_1mc <- as.data.frame(zonal(StackCON_NRB[[1]], admin, 'sum')) %>%
    as.data.table() %>%
    setnames(.,"sum", "CON_NRB_1MC")
  #zonal(StackCON_NRB, admin, 'mean')
  #zonal(StackCON_NRB, admin, 'sd')
  
  CON_NRBzon_mean <- CON_NRBzon_sum[, list(CON_NRB_MC_mean = rowMeans(.SD)), by = zone]
  CON_NRBzon_sd <- CON_NRBzon_sum[, list(CON_NRB_MC_sd = rowSds(.SD)), by = zone]
  
  CON_NRBzon_sum_m2 <- merge(userarea_df, CON_NRBzon_sum_1mc, by.x = ext_analysis_ID, by.y = "zone") %>%
    merge(CON_NRBzon_mean, by.x = ext_analysis_ID, by.y = "zone") %>%
    merge(CON_NRBzon_sd, by.x = ext_analysis_ID, by.y = "zone")
  
  NRB_fNRB2x <- merge(NRBzon_sum_m2, CON_TOTzon_sum_m2, by.x = ext_analysis_ID, by.y = ext_analysis_ID) %>%
    merge(CON_NRBzon_sum_m2, by.x = ext_analysis_ID, by.y = ext_analysis_ID) %>%
    dplyr::select(ext_analysis_ID,
                  paste0(ext_analysis_NAME,".x"),
                  "NRB_1MC","NRB_MC_mean","NRB_MC_sd",
                  "CON_TOT_1MC", "CON_TOT_MC_mean", "CON_TOT_MC_sd",
                  "CON_NRB_1MC", "CON_NRB_MC_mean", "CON_NRB_MC_sd") %>%
    dplyr::mutate(fNRB1mc = round(NRB_1MC / CON_TOT_1MC * 100)) %>%
    dplyr::mutate(fNRB = round(NRB_MC_mean / CON_TOT_MC_mean * 100)) %>%
    dplyr::mutate(fNRB_sd = round(sqrt(((NRB_MC_sd/NRB_MC_mean)^2) + ((CON_TOT_MC_sd/CON_TOT_MC_mean)^2))*100)) %>%
    dplyr::mutate(fNRB1mc_nrb = round(NRB_1MC / CON_NRB_1MC * 100)) %>%
    dplyr::mutate(fNRB_nrb = round(NRB_MC_mean / CON_NRB_MC_mean * 100)) %>%
    dplyr::mutate(fNRB_nrb_sd = round(sqrt(((NRB_MC_sd/NRB_MC_mean)^2) + ((CON_NRB_MC_sd/CON_NRB_MC_mean)^2))*100)) %>%
    dplyr::mutate(across(3:11) / 1000) %>%
    dplyr::mutate(across(3:11, round, 0)) %>%
    dplyr::rename(ADM_0 = NAME_0.x) %>%
    dplyr::mutate(across(3:17, as.integer))
  
  str(NRB_fNRB2x)
  names(NRB_fNRB2x)
  
  if (MC < mcthreshold) {
    NRB_fNRB2 <- NRB_fNRB2x %>%
      dplyr::mutate(across(ends_with("sd"), ~ (.x = NA)))
    NRB_fNRB2
  } else {
    NRB_fNRB2 <- NRB_fNRB2x
  }
  
  write.csv(NRB_fNRB2, "LULCC/TempTables/summary_adm0.csv", row.names=FALSE, quote=FALSE)
  write.csv(NRB_fNRB2, "OutBaU/webmofuss_results/summary_adm0.csv", row.names=FALSE, quote=FALSE)
  
  NRB_fNRB2annual <- NRB_fNRB2 %>%
    dplyr::mutate(NRB_yr = round(NRB_MC_mean/ST,0)) %>%
    dplyr::mutate(CON_TOT_yr = round(CON_TOT_MC_mean/ST,0)) %>%
    dplyr::mutate(CON_NRB_yr = round(CON_NRB_MC_mean/ST,0))
  
  write.csv(NRB_fNRB2annual, "LULCC/TempTables/summary_adm0_yr.csv", row.names=FALSE, quote=FALSE)
  write.csv(NRB_fNRB2annual, "OutBaU/webmofuss_results/summary_adm0_yr.csv", row.names=FALSE, quote=FALSE)
  
  # Produce simplified shapefile for webmofuss
  userarea0_simpx <- userarea_gpkg %>%
    inner_join(.,NRB_fNRB2, by="ID") %>%
    # ms_simplify(sys = TRUE) %>%
    dplyr::select(# GID_0,
      ID,
      NAME_0,
      # Subregion,
      # mofuss_reg,
      # NAME_0.x,
      # NAME_1.x,
      # NAME_2.x,
      NRB_1MC,
      NRB_MC_mean,
      NRB_MC_sd,
      CON_TOT_1MC,
      CON_TOT_MC_mean,
      CON_TOT_MC_sd,
      CON_NRB_1MC,
      CON_NRB_MC_mean,
      CON_NRB_MC_sd,
      fNRB1mc,
      fNRB, 
      fNRB_sd, 
      fNRB1mc_nrb,
      fNRB_nrb,
      fNRB_nrb_sd) %>%
    dplyr::rename(ADM0 = NAME_0,
                  # ADM1 = NAME_1.x,
                  # ADM2 = NAME_2.x,
                  NRB1mc = NRB_1MC,
                  NRBm = NRB_MC_mean,
                  NRBsd = NRB_MC_sd,
                  D1mc = CON_TOT_1MC,
                  Dm = CON_TOT_MC_mean,
                  Dsd = CON_TOT_MC_sd,
                  Dnrb_1mc = CON_NRB_1MC,
                  Dnrb_m= CON_NRB_MC_mean,
                  Dnrb_sd = CON_NRB_MC_sd,
                  fNRB2_1mc = fNRB1mc_nrb,
                  fNRB2 = fNRB_nrb, 
                  fNRB2_sd = fNRB_nrb_sd) %>%
    replace(is.na(.), 0)
  
  if (MC < mcthreshold) {
    userarea0_simp <- userarea0_simpx %>%
      dplyr::mutate(across(ends_with("sd"), ~ (.x = NA)))
    userarea0_simp
  } else {
    userarea0_simp <- userarea0_simpx
  }
  
  st_write(userarea0_simp, "OutBaU/webmofuss_results/mofuss_regions0.gpkg", delete_layer = TRUE)
  
  if(file.exists("LULCC/TempVector/userarea1.gpkg")){
    userarea_gpkg1 <- st_read("LULCC/TempVector/userarea1.gpkg")
    userarea_df1 <- userarea_gpkg1 %>% st_drop_geometry()
    
    admin1 <- raster("LULCC/TempRaster//admin_c1.tif")
    # StackNRB[StackNRB == 0] = NA #This is to sum and average only positive NRB values
    NRBzon_sum1 <- as.data.frame(zonal(StackNRB, admin1, 'sum')) %>%
      as.data.table()
    NRBzon_sum_1mc1 <- as.data.frame(zonal(StackNRB[[1]], admin1, 'sum')) %>%
      as.data.table() %>%
      setnames(.,"sum", "NRB_1MC")
    # zonal(StackNRB, admin, 'mean')
    # zonal(StackNRB, admin, 'sd')
    
    NRBzon_mean1 <- NRBzon_sum1[, list(NRB_MC_mean = rowMeans(.SD)), by = zone]
    NRBzon_sd1 <- NRBzon_sum1[, list(NRB_MC_sd = rowSds(.SD)), by = zone]
    
    NRBzon_sum_m21 <- merge(userarea_df1, NRBzon_sum_1mc1, by.x = ext_analysis_ID_1, by.y = "zone") %>%
      merge(NRBzon_mean1, by.x = ext_analysis_ID_1, by.y = "zone") %>%
      merge(NRBzon_sd1, by.x = ext_analysis_ID_1, by.y = "zone")
    
    # StackCON_TOT[StackCON_TOT == 0] = NA 
    CON_TOTzon_sum1 <- as.data.frame(zonal(StackCON_TOT, admin1, 'sum')) %>%
      as.data.table()
    CON_TOTzon_sum_1mc1 <- as.data.frame(zonal(StackCON_TOT[[1]], admin1, 'sum')) %>%
      as.data.table() %>%
      setnames(.,"sum", "CON_TOT_1MC")
    #zonal(StackCON_TOT, admin, 'mean')
    #zonal(StackCON_TOT, admin, 'sd')
    
    CON_TOTzon_mean1 <- CON_TOTzon_sum1[, list(CON_TOT_MC_mean = rowMeans(.SD)), by = zone]
    CON_TOTzon_sd1 <- CON_TOTzon_sum1[, list(CON_TOT_MC_sd = rowSds(.SD)), by = zone]
    
    CON_TOTzon_sum_m21 <- merge(userarea_df1, CON_TOTzon_sum_1mc1, by.x = ext_analysis_ID_1, by.y = "zone") %>%
      merge(CON_TOTzon_mean1, by.x = ext_analysis_ID_1, by.y = "zone") %>%
      merge(CON_TOTzon_sd1, by.x = ext_analysis_ID_1, by.y = "zone")
    
    # StackCON_NRB[StackCON_NRB == 0] = NA
    CON_NRBzon_sum1 <- as.data.frame(zonal(StackCON_NRB, admin1, 'sum')) %>%
      as.data.table()
    CON_NRBzon_sum_1mc1 <- as.data.frame(zonal(StackCON_NRB[[1]], admin1, 'sum')) %>%
      as.data.table() %>%
      setnames(.,"sum", "CON_NRB_1MC")
    #zonal(StackCON_NRB, admin, 'mean')
    #zonal(StackCON_NRB, admin, 'sd')
    
    CON_NRBzon_mean1 <- CON_NRBzon_sum1[, list(CON_NRB_MC_mean = rowMeans(.SD)), by = zone]
    CON_NRBzon_sd1 <- CON_NRBzon_sum1[, list(CON_NRB_MC_sd = rowSds(.SD)), by = zone]
    
    CON_NRBzon_sum_m21 <- merge(userarea_df1, CON_NRBzon_sum_1mc1, by.x = ext_analysis_ID_1, by.y = "zone") %>%
      merge(CON_NRBzon_mean1, by.x = ext_analysis_ID_1, by.y = "zone") %>%
      merge(CON_NRBzon_sd1, by.x = ext_analysis_ID_1, by.y = "zone")
    
    NRB_fNRB21x <- merge(NRBzon_sum_m21, CON_TOTzon_sum_m21, by.x = ext_analysis_ID_1, by.y = ext_analysis_ID_1) %>%
      merge(CON_NRBzon_sum_m21, by.x = ext_analysis_ID_1, by.y = ext_analysis_ID_1) %>%
      dplyr::select(ext_analysis_ID_1,
                    paste0(ext_analysis_NAME,".x"),
                    paste0(ext_analysis_NAME_1,".x"),
                    "NRB_1MC","NRB_MC_mean","NRB_MC_sd",
                    "CON_TOT_1MC", "CON_TOT_MC_mean", "CON_TOT_MC_sd",
                    "CON_NRB_1MC", "CON_NRB_MC_mean", "CON_NRB_MC_sd") %>%
      dplyr::mutate(fNRB1mc = round(NRB_1MC / CON_TOT_1MC * 100)) %>%
      dplyr::mutate(fNRB = round(NRB_MC_mean / CON_TOT_MC_mean * 100)) %>%
      dplyr::mutate(fNRB_sd = round(sqrt(((NRB_MC_sd/NRB_MC_mean)^2) + ((CON_TOT_MC_sd/CON_TOT_MC_mean)^2))*100)) %>%
      dplyr::mutate(fNRB1mc_nrb = round(NRB_1MC / CON_NRB_1MC * 100)) %>%
      dplyr::mutate(fNRB_nrb = round(NRB_MC_mean / CON_NRB_MC_mean * 100)) %>%
      dplyr::mutate(fNRB_nrb_sd = round(sqrt(((NRB_MC_sd/NRB_MC_mean)^2) + ((CON_NRB_MC_sd/CON_NRB_MC_mean)^2))*100)) %>%
      dplyr::mutate(across(4:12) / 1000) %>%
      dplyr::mutate(across(4:12, round, 0)) %>%
      dplyr::rename(ADM_0 = NAME_0.x,
                    ADM_1 = NAME_1.x) %>%
      dplyr::mutate(across(all_of(4:18), as.integer))
    
    str(NRB_fNRB21x)
    names(NRB_fNRB21x)
    
    if (MC < mcthreshold) {
      NRB_fNRB21 <- NRB_fNRB21x %>%
        dplyr::mutate(across(ends_with("sd"), ~ (.x = NA)))
      NRB_fNRB21
    } else {
      NRB_fNRB21 <- NRB_fNRB21x
    }
    write.csv(NRB_fNRB21, "LULCC/TempTables/summary_adm1.csv", row.names=FALSE, quote=FALSE)
    write.csv(NRB_fNRB21, "OutBaU/webmofuss_results/summary_adm1.csv", row.names=FALSE, quote=FALSE)
    
    NRB_fNRB21annual <- NRB_fNRB21 %>%
      dplyr::mutate(NRB_yr = round(NRB_MC_mean/ST,0)) %>%
      dplyr::mutate(CON_TOT_yr = round(CON_TOT_MC_mean/ST,0)) %>%
      dplyr::mutate(CON_NRB_yr = round(CON_NRB_MC_mean/ST,0))
    write.csv(NRB_fNRB21annual, "LULCC/TempTables/summary_adm1_yr.csv", row.names=FALSE, quote=FALSE)
    write.csv(NRB_fNRB21annual, "OutBaU/webmofuss_results/summary_adm1_yr.csv", row.names=FALSE, quote=FALSE)
    
    # Produce simplified shapefile for webmofuss
    userarea1_simpx <- userarea_gpkg1 %>%
      inner_join(.,NRB_fNRB21, by="ID") %>%
      # ms_simplify(sys = TRUE) %>%
      dplyr::select(# GID_0,
        ID,
        NAME_0,
        # Subregion,
        # mofuss_reg,
        # NAME_0.x,
        NAME_1,
        # NAME_2.x,
        NRB_1MC,
        NRB_MC_mean,
        NRB_MC_sd,
        CON_TOT_1MC,
        CON_TOT_MC_mean,
        CON_TOT_MC_sd,
        CON_NRB_1MC,
        CON_NRB_MC_mean,
        CON_NRB_MC_sd,
        fNRB1mc,
        fNRB, 
        fNRB_sd, 
        fNRB1mc_nrb,
        fNRB_nrb,
        fNRB_nrb_sd) %>%
      dplyr::rename(ADM0 = NAME_0,
                    ADM1 = NAME_1,
                    # ADM2 = NAME_2.x,
                    NRB1mc = NRB_1MC,
                    NRBm = NRB_MC_mean,
                    NRBsd = NRB_MC_sd,
                    D1mc = CON_TOT_1MC,
                    Dm = CON_TOT_MC_mean,
                    Dsd = CON_TOT_MC_sd,
                    Dnrb_1mc = CON_NRB_1MC,
                    Dnrb_m= CON_NRB_MC_mean,
                    Dnrb_sd = CON_NRB_MC_sd,
                    fNRB2_1mc = fNRB1mc_nrb,
                    fNRB2 = fNRB_nrb, 
                    fNRB2_sd = fNRB_nrb_sd) %>%
      replace(is.na(.), 0)
    
    if (MC < mcthreshold) {
      userarea1_simp <- userarea1_simpx %>%
        dplyr::mutate(across(ends_with("sd"), ~ (.x = NA)))
      userarea1_simp
    } else {
      userarea1_simp <- userarea1_simpx
    }
    
    st_write(userarea1_simp, "OutBaU/webmofuss_results/mofuss_regions1.gpkg", delete_layer = TRUE)
    
    if(file.exists("LULCC/TempVector/userarea2.gpkg")){
      userarea_gpkg2 <- st_read("LULCC/TempVector/userarea2.gpkg")
      userarea_df2 <- userarea_gpkg2 %>% st_drop_geometry()
      
      admin2 <- raster("LULCC/TempRaster//admin_c2.tif")
      # StackNRB[StackNRB == 0] = NA #This is to sum and average only positive NRB values
      NRBzon_sum2 <- as.data.frame(zonal(StackNRB, admin2, 'sum')) %>%
        as.data.table()
      NRBzon_sum_1mc2 <- as.data.frame(zonal(StackNRB[[1]], admin2, 'sum')) %>%
        as.data.table() %>%
        setnames(.,"sum", "NRB_1MC")
      # zonal(StackNRB, admin, 'mean')
      # zonal(StackNRB, admin, 'sd')
      
      NRBzon_mean2 <- NRBzon_sum2[, list(NRB_MC_mean = rowMeans(.SD)), by = zone]
      NRBzon_sd2 <- NRBzon_sum2[, list(NRB_MC_sd = rowSds(.SD)), by = zone]
      
      NRBzon_sum_m22 <- merge(userarea_df2, NRBzon_sum_1mc2, by.x = ext_analysis_ID_2, by.y = "zone") %>%
        merge(NRBzon_mean2, by.x = ext_analysis_ID_2, by.y = "zone") %>%
        merge(NRBzon_sd2, by.x = ext_analysis_ID_2, by.y = "zone")
      
      # StackCON_TOT[StackCON_TOT == 0] = NA 
      CON_TOTzon_sum2 <- as.data.frame(zonal(StackCON_TOT, admin2, 'sum')) %>%
        as.data.table()
      CON_TOTzon_sum_1mc2 <- as.data.frame(zonal(StackCON_TOT[[1]], admin2, 'sum')) %>%
        as.data.table() %>%
        setnames(.,"sum", "CON_TOT_1MC")
      #zonal(StackCON_TOT, admin, 'mean')
      #zonal(StackCON_TOT, admin, 'sd')
      
      CON_TOTzon_mean2 <- CON_TOTzon_sum2[, list(CON_TOT_MC_mean = rowMeans(.SD)), by = zone]
      CON_TOTzon_sd2 <- CON_TOTzon_sum2[, list(CON_TOT_MC_sd = rowSds(.SD)), by = zone]
      
      CON_TOTzon_sum_m22 <- merge(userarea_df2, CON_TOTzon_sum_1mc2, by.x = ext_analysis_ID_2, by.y = "zone") %>%
        merge(CON_TOTzon_mean2, by.x = ext_analysis_ID_2, by.y = "zone") %>%
        merge(CON_TOTzon_sd2, by.x = ext_analysis_ID_2, by.y = "zone")
      
      # StackCON_NRB[StackCON_NRB == 0] = NA
      CON_NRBzon_sum2 <- as.data.frame(zonal(StackCON_NRB, admin2, 'sum')) %>%
        as.data.table()
      CON_NRBzon_sum_1mc2 <- as.data.frame(zonal(StackCON_NRB[[1]], admin2, 'sum')) %>%
        as.data.table() %>%
        setnames(.,"sum", "CON_NRB_1MC")
      #zonal(StackCON_NRB, admin, 'mean')
      #zonal(StackCON_NRB, admin, 'sd')
      
      CON_NRBzon_mean2 <- CON_NRBzon_sum2[, list(CON_NRB_MC_mean = rowMeans(.SD)), by = zone]
      CON_NRBzon_sd2 <- CON_NRBzon_sum2[, list(CON_NRB_MC_sd = rowSds(.SD)), by = zone]
      
      CON_NRBzon_sum_m22 <- merge(userarea_df2, CON_NRBzon_sum_1mc2, by.x = ext_analysis_ID_2, by.y = "zone") %>%
        merge(CON_NRBzon_mean2, by.x = ext_analysis_ID_2, by.y = "zone") %>%
        merge(CON_NRBzon_sd2, by.x = ext_analysis_ID_2, by.y = "zone")
      
      NRB_fNRB22x <- merge(NRBzon_sum_m22, CON_TOTzon_sum_m22, by.x = ext_analysis_ID_2, by.y = ext_analysis_ID_2) %>%
        merge(CON_NRBzon_sum_m22, by.x = ext_analysis_ID_2, by.y = ext_analysis_ID_2) %>%
        dplyr::select(ext_analysis_ID_2,
                      paste0(ext_analysis_NAME,".x"),
                      paste0(ext_analysis_NAME_1,".x"),
                      paste0(ext_analysis_NAME_2,".x"),
                      "NRB_1MC","NRB_MC_mean","NRB_MC_sd",
                      "CON_TOT_1MC", "CON_TOT_MC_mean", "CON_TOT_MC_sd",
                      "CON_NRB_1MC", "CON_NRB_MC_mean", "CON_NRB_MC_sd") %>%
        dplyr::mutate(fNRB1mc = round(NRB_1MC / CON_TOT_1MC * 100)) %>%
        dplyr::mutate(fNRB = round(NRB_MC_mean / CON_TOT_MC_mean * 100)) %>%
        dplyr::mutate(fNRB_sd = round(sqrt(((NRB_MC_sd/NRB_MC_mean)^2) + ((CON_TOT_MC_sd/CON_TOT_MC_mean)^2))*100)) %>%
        dplyr::mutate(fNRB1mc_nrb = round(NRB_1MC / CON_NRB_1MC * 100)) %>%
        dplyr::mutate(fNRB_nrb = round(NRB_MC_mean / CON_NRB_MC_mean * 100)) %>%
        dplyr::mutate(fNRB_nrb_sd = round(sqrt(((NRB_MC_sd/NRB_MC_mean)^2) + ((CON_NRB_MC_sd/CON_NRB_MC_mean)^2))*100)) %>%
        dplyr::mutate(across(5:13) / 1000) %>%
        dplyr::mutate(across(5:13, round, 0)) %>%
        dplyr::rename(ADM_0 = NAME_0.x,
                      ADM_1 = NAME_1.x,
                      ADM_2 = NAME_2.x) %>%
        dplyr::mutate(across(all_of(5:19), as.integer))
      
      str(NRB_fNRB22x)
      names(NRB_fNRB22x)
      
      if (MC < mcthreshold) {
        NRB_fNRB22 <- NRB_fNRB22x %>%
          dplyr::mutate(across(ends_with("sd"), ~ (.x = NA)))
        NRB_fNRB22
      } else {
        NRB_fNRB22 <- NRB_fNRB22x
      }
      
      write.csv(NRB_fNRB22, "LULCC/TempTables/summary_adm2.csv", row.names=FALSE, quote=FALSE)
      write.csv(NRB_fNRB22, "OutBaU/webmofuss_results/summary_adm2.csv", row.names=FALSE, quote=FALSE)
      
      NRB_fNRB22annual <- NRB_fNRB22 %>%
        dplyr::mutate(NRB_yr = round(NRB_MC_mean/ST,0)) %>%
        dplyr::mutate(CON_TOT_yr = round(CON_TOT_MC_mean/ST,0)) %>%
        dplyr::mutate(CON_NRB_yr = round(CON_NRB_MC_mean/ST,0))
      write.csv(NRB_fNRB22annual, "LULCC/TempTables/summary_adm2_yr.csv", row.names=FALSE, quote=FALSE)
      write.csv(NRB_fNRB22annual, "OutBaU/webmofuss_results/summary_adm2_yr.csv", row.names=FALSE, quote=FALSE)
      
      # Produce simplified shapefile for webmofuss
      userarea2_simpx <- userarea_gpkg2 %>%
        inner_join(.,NRB_fNRB22, by="ID") %>%
        # ms_simplify(sys = TRUE) %>%
        dplyr::select(# GID_0,
          ID,
          NAME_0,
          # Subregion,
          # mofuss_reg,
          # NAME_0.x,
          NAME_1,
          NAME_2,
          NRB_1MC,
          NRB_MC_mean,
          NRB_MC_sd,
          CON_TOT_1MC,
          CON_TOT_MC_mean,
          CON_TOT_MC_sd,
          CON_NRB_1MC,
          CON_NRB_MC_mean,
          CON_NRB_MC_sd,
          fNRB1mc,
          fNRB, 
          fNRB_sd, 
          fNRB1mc_nrb,
          fNRB_nrb,
          fNRB_nrb_sd) %>%
        dplyr::rename(ADM0 = NAME_0,
                      ADM1 = NAME_1,
                      ADM2 = NAME_2,
                      NRB1mc = NRB_1MC,
                      NRBm = NRB_MC_mean,
                      NRBsd = NRB_MC_sd,
                      D1mc = CON_TOT_1MC,
                      Dm = CON_TOT_MC_mean,
                      Dsd = CON_TOT_MC_sd,
                      Dnrb_1mc = CON_NRB_1MC,
                      Dnrb_m= CON_NRB_MC_mean,
                      Dnrb_sd = CON_NRB_MC_sd,
                      fNRB2_1mc = fNRB1mc_nrb,
                      fNRB2 = fNRB_nrb, 
                      fNRB2_sd = fNRB_nrb_sd) %>%
        replace(is.na(.), 0)
      
      
      if (MC < mcthreshold) {
        userarea2_simp <- userarea2_simpx %>%
          dplyr::mutate(across(ends_with("sd"), ~ (.x = NA)))
        userarea2_simp
      } else {
        userarea2_simp <- userarea2_simpx
      }
      
      st_write(userarea2_simp, "OutBaU/webmofuss_results/mofuss_regions2.gpkg", delete_layer = TRUE)
      
    }
  }
  
  ## Saves summary tables in .csv for BaU and ICS SCENARIOS ####
  
  writeLines(paste(MC," Monte Carlo runs",sep=""), "LULCC/TempTables/MCruns.txt", useBytes=T)
  file.copy("LULCC/TempTables/MCruns.txt", "LaTeX/MCruns.txt", overwrite=TRUE)
  writeLines(paste(STdyn," years",sep=""), "LULCC/TempTables/SimLength.txt", useBytes=T)
  file.copy("LULCC/TempTables/SimLength.txt", "LaTeX/SimLength.txt", overwrite=TRUE)	
  
  cols1 <- c(1:3)
  NRBBaUICS <- NRB_fNRB2[,cols1]
  
  cols2 <- c(1, 8:9)
  fNRBBaUICS <- NRB_fNRB2[,cols2]
  
  # if (BaUvsICS == "ICS") {
  # 	colnames(NRB_fNRB2)<- c("Name","NRB","NRBsd","FWuse","FWusesd","FWu2","FWusd2","fNRB","fNRBsd","fNRB2","fNRBsd2")
  # 	write.csv(NRB_fNRB2, "LULCC/TempTables/SumTableICS.csv", row.names=FALSE, quote=FALSE)
  # 	unlink("LaTeX/SumTableICS.csv", force=TRUE)
  # 	file.copy("LULCC/TempTables/SumTableICS.csv", "LaTeX/SumTableICS.csv")
  # 
  # 	colnames(NRBBaUICS)<- c("Name", "NRB.ICS", "NRB.ICS.sd")
  # 	write.csv(NRBBaUICS, "LULCC/TempTables/NRBICS.csv", row.names=FALSE, quote=FALSE)
  # 
  # 	colnames(fNRBBaUICS)<- c("Name", "fNRB.ICS", "fNRB.ICS.sd")
  # 	write.csv(fNRBBaUICS, "LULCC/TempTables/fNRBICS.csv", row.names=FALSE, quote=FALSE)
  # } else {
  colnames(NRB_fNRB2)<- c("Name", "NRB","NRBsd","FWuse","FWusesd","FWu2","FWusd2","fNRB","fNRBsd","fNRB2","fNRBsd2")
  write.csv(NRB_fNRB2, "LULCC/TempTables/SumTableBaU.csv", row.names=FALSE, quote=FALSE)
  unlink("LaTeX/SumTableBaU.csv", force=TRUE)
  file.copy("LULCC/TempTables/SumTableBaU.csv", "LaTeX/SumTableBaU.csv")
  
  colnames(NRBBaUICS)<- c("Name", "NRB.BaU", "NRB.BaU.sd")
  write.csv(NRBBaUICS, "LULCC/TempTables/NRBBaU.csv", row.names=FALSE, quote=FALSE)
  
  colnames(fNRBBaUICS)<- c("Name", "fNRB.BaU", "fNRB.BaU.sd")
  write.csv(fNRBBaUICS, "LULCC/TempTables/fNRBBaU.csv", row.names=FALSE, quote=FALSE)
  
  #}
  
  rNRBBaU<-file.exists("LULCC/TempTables/NRBBaU.csv")
  #rNRBICS<-file.exists("LULCC/TempTables/NRBICS.csv")
  rfNRBBaU<-file.exists("LULCC/TempTables/fNRBBaU.csv")
  #rfNRBICS<-file.exists("LULCC/TempTables/fNRBICS.csv")
  rBaUSt<-file.exists("LULCC/TempTables/SumTableBaU.csv")
  #rICSSt<-file.exists("LULCC/TempTables/SumTableICS.csv")
  
  #if (rNRBBaU == "TRUE" & rNRBICS == "TRUE") {
  rNRBBaUt<-read.csv("LULCC/TempTables/NRBBaU.csv")
  #rNRBICSt<-read.csv("LULCC/TempTables/NRBICS.csv")
  #rNRBBauICSt <- merge(rNRBBaUt,rNRBICSt,by="Name", sort=FALSE, all.x=TRUE)
  #colnames(rNRBBauICSt)<- c("Name",
  #	paste("NRB.BaU (n=",MC,")",sep=""),paste("NRB.BaU.sd (n=",MC,")",sep=""),
  #	paste("NRB.ICS (n=",MC,")",sep=""),paste("NRB.ICS.sd (n=",MC,")",sep=""))
  write.csv(rNRBBaUt, "LULCC/TempTables/NRBTable.csv", row.names=FALSE, quote=FALSE)
  #write.csv(rNRBBauICSt, "LULCC/TempTables/NRBTable.csv", row.names=FALSE, quote=FALSE)
  unlink("LaTeX/NRBTable.csv", force=TRUE)
  file.copy("LULCC/TempTables/NRBTable.csv", "LaTeX/NRBTable.csv")
  # } else {
  # 	"One out of two scenario table parameters is missing"
  # }
  # 
  # if (rfNRBBaU == "TRUE" & rfNRBICS == "TRUE") {
  rfNRBBaUt<-read.csv("LULCC/TempTables/fNRBBaU.csv")
  # 	rfNRBICSt<-read.csv("LULCC/TempTables/fNRBICS.csv")
  # 	rfNRBBauICSt <- merge(rfNRBBaUt,rfNRBICSt,by="Name", sort=FALSE, all.x=TRUE)
  # 	colnames(rfNRBBauICSt)<- c("Name", 
  # 		paste("fNRB.BaU (n=",MC,")",sep=""),paste("fNRB.BaU.sd (n=",MC,")",sep=""),
  # 		paste("fNRB.ICS (n=",MC,")",sep=""),paste("fNRB.ICS.sd (n=",MC,")",sep=""))
  write.csv(rfNRBBaUt, "LULCC/TempTables/fNRBTable.csv", row.names=FALSE, quote=FALSE)
  # 	write.csv(rfNRBBauICSt, "LULCC/TempTables/fNRBTable.csv", row.names=FALSE, quote=FALSE)
  unlink("LaTeX/fNRBTable.csv", force=TRUE)
  file.copy("LULCC/TempTables/fNRBTable.csv", "LaTeX/fNRBTable.csv")
  # } else { 
  # 	"One out of two scenario table parameters is missing"		
  # }
  
  #if (rBaUSt == "TRUE" & rICSSt == "TRUE") {
  rBaUStt<-read.csv("LULCC/TempTables/SumTableBaU.csv")
  #rICSStt<-read.csv("LULCC/TempTables/SumTableICS.csv")
  #rBaUICStt <- merge(rBaUStt,rICSStt,by="Name", sort=FALSE, all.x=TRUE)
  write.csv(rBaUStt, "LULCC/TempTables/SumTable.csv", row.names=FALSE, quote=FALSE)
  #write.csv(rBaUICStt, "LULCC/TempTables/SumTable.csv", row.names=FALSE, quote=FALSE)
  unlink("LaTeX/SumTable.csv", force=TRUE)
  file.copy("LULCC/TempTables/SumTable.csv", "LaTeX/SumTable.csv")
  # } else {
  # 	"One out of two scenario table parameters is missing"
  # }
  
  
  # # areal_fNRB - just a test...it won't fly
  # NRB01test <- raster("Temp/2_NRB01.tif")
  # NRB01testna <- reclassify(NRB01test, cbind(-Inf, 0, NA), right=TRUE)
  # NRB01testRtG<- raster("Temp/2_NRB01_RtG.tif")
  # NRB01testRtGna <- reclassify(NRB01testRtG, cbind(-Inf, 0, NA), right=TRUE)
  # 
  # NRBzon_count<-as.data.frame(zonal(NRB01testna, admin, 'count', na.rm = TRUE))
  # NRBzon_countNA<-as.data.frame(zonal(NRB01testna, admin, 'count', na.rm = FALSE))
  # aereal_fNRB <- cbind(NRBzon_count[1],round((NRBzon_count[,2] / NRBzon_countNA[,2] * 100),0))
  # names_zonal <- unique(userarea_DF[,c(ext_analysis_ID, ext_analysis_NAME)])
  # aereal_fNRB_zone <- merge(names_zonal, aereal_fNRB, by.x = ext_analysis_ID, by.y = "zone")
  # colnames(aereal_fNRB_zone) <- c(ext_analysis_ID, ext_analysis_NAME, "areal_fNRB")
  # write.csv(aereal_fNRB_zone, "LULCC/TempTables/areal_fNRB.csv", row.names=FALSE, quote=FALSE)
  # 
  # NRBzon_count_RtG<-as.data.frame(zonal(NRB01testRtGna, admin, 'count', na.rm = TRUE))
  # NRBzon_countNA_RtG<-as.data.frame(zonal(NRB01testRtGna, admin, 'count', na.rm = FALSE))
  # aereal_fNRB_RtG <- cbind(NRBzon_count_RtG[1],round((NRBzon_count_RtG[,2] / NRBzon_countNA_RtG[,2] * 100),0))
  # aereal_fNRB_zone_RtG <- merge(names_zonal, aereal_fNRB_RtG, by.x = ext_analysis_ID, by.y = "zone")
  # colnames(aereal_fNRB_zone_RtG) <- c(ext_analysis_ID, ext_analysis_NAME, "areal_fNRB")
  # write.csv(aereal_fNRB_zone_RtG, "LULCC/TempTables/areal_fNRB_RtG.csv", row.names=FALSE, quote=FALSE)
  
  
  # Chunk code to produce additonal tables using higher level admin units IF THEY EXIST
  if(file.exists("LULCC/TempVector/ext_analysis1.gpkg")){
    NRBBaUICS1<-NRB_fNRB21[,cols1]
    fNRBBaUICS1<-NRB_fNRB21[,cols2]
    
    colnames(NRB_fNRB21)<- c("Name", "NRB","NRBsd","FWuse","FWusesd","FWu2","FWusd2","fNRB","fNRBsd","fNRB2","fNRBsd2")
    write.csv(NRB_fNRB21, "LULCC/TempTables/SumTableBaU1.csv", row.names=FALSE, quote=FALSE)
    unlink("LaTeX/SumTableBaU1.csv", force=TRUE)
    file.copy("LULCC/TempTables/SumTableBaU1.csv", "LaTeX/SumTableBaU1.csv")
    
    colnames(NRBBaUICS1)<- c("Name", "NRB.BaU", "NRB.BaU.sd")
    write.csv(NRBBaUICS1, "LULCC/TempTables/NRBBaU1.csv", row.names=FALSE, quote=FALSE)
    
    colnames(fNRBBaUICS1)<- c("Name", "fNRB.BaU", "fNRB.BaU.sd")
    write.csv(fNRBBaUICS1, "LULCC/TempTables/fNRBBaU1.csv", row.names=FALSE, quote=FALSE)
    
    rNRBBaU1<-file.exists("LULCC/TempTables/NRBBaU1.csv")
    #rNRBICS<-file.exists("LULCC/TempTables/NRBICS.csv")
    rfNRBBaU1<-file.exists("LULCC/TempTables/fNRBBaU1.csv")
    #rfNRBICS<-file.exists("LULCC/TempTables/fNRBICS.csv")
    rBaUSt1<-file.exists("LULCC/TempTables/SumTableBaU1.csv")
    #rICSSt<-file.exists("LULCC/TempTables/SumTableICS.csv")
    
    rNRBBaU1<-file.exists("LULCC/TempTables/NRBBaU1.csv")
    #rNRBICS<-file.exists("LULCC/TempTables/NRBICS.csv")
    rfNRBBaU1<-file.exists("LULCC/TempTables/fNRBBaU1.csv")
    #rfNRBICS<-file.exists("LULCC/TempTables/fNRBICS.csv")
    rBaUSt1<-file.exists("LULCC/TempTables/SumTableBaU1.csv")
    #rICSSt<-file.exists("LULCC/TempTables/SumTableICS.csv")
    
    #if (rNRBBaU == "TRUE" & rNRBICS == "TRUE") {
    rNRBBaUt1<-read.csv("LULCC/TempTables/NRBBaU1.csv")
    #rNRBICSt<-read.csv("LULCC/TempTables/NRBICS.csv")
    #rNRBBauICSt <- merge(rNRBBaUt,rNRBICSt,by="Name", sort=FALSE, all.x=TRUE)
    #colnames(rNRBBauICSt)<- c("Name",
    #	paste("NRB.BaU (n=",MC,")",sep=""),paste("NRB.BaU.sd (n=",MC,")",sep=""),
    #	paste("NRB.ICS (n=",MC,")",sep=""),paste("NRB.ICS.sd (n=",MC,")",sep=""))
    write.csv(rNRBBaUt1, "LULCC/TempTables/NRBTable1.csv", row.names=FALSE, quote=FALSE)
    #write.csv(rNRBBauICSt, "LULCC/TempTables/NRBTable.csv", row.names=FALSE, quote=FALSE)
    unlink("LaTeX/NRBTable1.csv", force=TRUE)
    file.copy("LULCC/TempTables/NRBTable1.csv", "LaTeX/NRBTable1.csv")
    # } else {
    # 	"One out of two scenario table parameters is missing"
    # }
    # 
    # if (rfNRBBaU == "TRUE" & rfNRBICS == "TRUE") {
    rfNRBBaUt1<-read.csv("LULCC/TempTables/fNRBBaU1.csv")
    # 	rfNRBICSt<-read.csv("LULCC/TempTables/fNRBICS.csv")
    # 	rfNRBBauICSt <- merge(rfNRBBaUt,rfNRBICSt,by="Name", sort=FALSE, all.x=TRUE)
    # 	colnames(rfNRBBauICSt)<- c("Name", 
    # 		paste("fNRB.BaU (n=",MC,")",sep=""),paste("fNRB.BaU.sd (n=",MC,")",sep=""),
    # 		paste("fNRB.ICS (n=",MC,")",sep=""),paste("fNRB.ICS.sd (n=",MC,")",sep=""))
    write.csv(rfNRBBaUt1, "LULCC/TempTables/fNRBTable1.csv", row.names=FALSE, quote=FALSE)
    # 	write.csv(rfNRBBauICSt, "LULCC/TempTables/fNRBTable.csv", row.names=FALSE, quote=FALSE)
    unlink("LaTeX/fNRBTable1.csv", force=TRUE)
    file.copy("LULCC/TempTables/fNRBTable1.csv", "LaTeX/fNRBTable1.csv")
    # } else { 
    # 	"One out of two scenario table parameters is missing"		
    # }
    
    #if (rBaUSt == "TRUE" & rICSSt == "TRUE") {
    rBaUStt1<-read.csv("LULCC/TempTables/SumTableBaU1.csv")
    #rICSStt<-read.csv("LULCC/TempTables/SumTableICS.csv")
    #rBaUICStt <- merge(rBaUStt,rICSStt,by="Name", sort=FALSE, all.x=TRUE)
    write.csv(rBaUStt1, "LULCC/TempTables/SumTable1.csv", row.names=FALSE, quote=FALSE)
    #write.csv(rBaUICStt, "LULCC/TempTables/SumTable.csv", row.names=FALSE, quote=FALSE)
    unlink("LaTeX/SumTable1.csv", force=TRUE)
    file.copy("LULCC/TempTables/SumTable1.csv", "LaTeX/SumTable1.csv")
    # } else {
    # 	"One out of two scenario table parameters is missing"
    # }
    
  }
  
  
  
  # Summary tables for PDF latex report ####
  
  # dir.create(paste(OutDir,"/summarytables",sep=""))
  
} else {
  SumTables_yesno = "No"
}


# Conversion of TIFF to JPG and PNG for PNG report ----

dir.create(paste(OutDir,"/png",sep=""))
dir.create(paste(OutDir,"/jpg",sep=""))

figlist <- c("Map_AGB", "Area_of_Interest", "Localities_of_Interest",
             "AGB_NRB_fNRB", "Boxplots")
for (i in figlist) {
  img1 <- readTIFF(paste(OutDir,"/",i,".tif",sep=""), native=TRUE)
  writeJPEG(img1, target = paste(OutDir,"/jpg/",i,".jpg",sep=""), quality = 1)
  img2 <- readTIFF(paste(OutDir,"/",i,".tif",sep=""), native=FALSE)
  writePNG(img2, target = paste(OutDir,"/png/",i,".png",sep=""))
}

# Copy key rasters to output folder - 1st MC ----
if (copy_old_dinamica_rasters == 1) {
  rasternames <- c("2_NRB01.tif", "2_CON_TOT01.tif", "2_CON_NRB01.tif", "2_fNRB01.tif", "2_IniSt01.tif", "2_AGBt101.tif",
                   "aNRBmean.tif", "aNRBsd.tif")
  for (p in rasternames){
    file.copy(from=paste0("Temp//",p),
              to="OutBaU/webmofuss_results/",
              overwrite = TRUE, recursive = TRUE, copy.mode = TRUE)
  }
}

# Compile LATEX file infot pdf report ####
if (compilelatex == 1) {
  
  # Three key files that must be present for full pdf report
  # BaUexist<-file.exists("OutBaU/png/Map_AGB.png")
  # ICSexist<-file.exists("OutICS/png/Map_AGB.png")
  # BaUexist_v<-file.exists("LaTeX/Growth_Harvest_AniOutBaU.mp4")
  # ICSexist_v<-file.exists("LaTeX/Growth_Harvest_AniOutICS.mp4")
  # BaUexist_b<-file.exists("OutBaU/png/Boxplots.png")
  # ICSexist_b<-file.exists("OutICS/png/Boxplots.png")
  #
  # if (BaUexist == FALSE | BaUexist_v == FALSE | BaUexist_b == FALSE) {
  # 	#shell.exec(file.path(getwd(), "LULCC/Wizard_imgs/BaU.pdf"))
  # } else {
  # 	if (ICSexist == FALSE | ICSexist_v == FALSE | ICSexist_b == FALSE) {
  # 		#shell.exec(file.path(getwd(), "LULCC/Wizard_imgs/ICS.pdf"))
  # 	} else {
  print("Run LaTeX and compile report in pdf")
  for (x in 1:5) {
    namepdf <- paste("errorpdf", x, sep = "")
    Sys.sleep(1)
    compilePDF <- function(x) {
      out <- tryCatch(
        {
          message("This is the 'try' part")
          tools::texi2dvi("LaTeX/Mofuss_Summary_Report_v3.tex", pdf = TRUE,
                          clean = TRUE, index=TRUE)
        },
        error=function(cond) {
          message(paste("Function caused an error!:", x))
          message("Here's the original error message:")
          message(cond)
          # Choose a return value in case of error
          return(conditionMessage(cond))
        },
        warning=function(cond) {
          message(paste("Function caused a warning:", x))
          message("Here's the original warning message:")
          message(cond)
          # Choose a return value in case of warning
          return(conditionMessage(cond))
        },
        finally={
          message(paste("\nTry number:", x))
          #message("Some other message at the end")
        }
      )
      return(out)
    }
    assign(namepdf, lapply(x, compilePDF))
  }
  
  if (errorpdf1=="pdflatex is not available"
      & errorpdf2=="pdflatex is not available"
      & errorpdf3=="pdflatex is not available"
      & errorpdf4=="pdflatex is not available"
      & errorpdf5=="pdflatex is not available") {
    #shell.exec(file.path(getwd(), "LULCC/Wizard_imgs/pdflatex.pdf"))
  } else {
    PDFreport<-file.exists("Mofuss_Summary_Report_v3.pdf")
    if (PDFreport == "TRUE") {
      file.copy("Mofuss_Summary_Report_v3.pdf", "Summary_Report", recursive=TRUE)
      unlink("Mofuss_Summary_Report_v3.*", force=TRUE)
      #shell.exec(file.path(getwd(), "Summary_Report/Mofuss_Summary_Report.pdf"))
    } else {
      latexlogerror<-file.exists("Mofuss_Summary_Report_v3.log")
      if (latexlogerror == TRUE) {
        #shell.exec(file.path(getwd(), "LULCC/Wizard_imgs/TexPkgMissing.pdf"))
        file.copy("Mofuss_Summary_Report_v3.log", "Logs", recursive=TRUE)
        count<-0
        
        while(file.exists("Mofuss_Summary_Report_v3.pdf")== FALSE) {
          count<-count+1;
          namepdfB <- paste("errorpdfB", count, sep = "");
          #assign(namepdfB, lapply(count, compilePDFB));
          assign(namepdfB, lapply(count, compilePDF));
          if (count == 10) {shell.exec(file.path(getwd(), "LULCC/Wizard_imgs/Houston.pdf"))}
          if (file.exists("Mofuss_Summary_Report_v3.pdf") == TRUE | count == 20) break; print(count);}
        
        if (file.exists("Mofuss_Summary_Report_v3.pdf") == TRUE) {
          file.copy("Mofuss_Summary_Report_v3.pdf", "Summary_Report", recursive=TRUE)
          unlink("Mofuss_Summary_Report_v3.*", force=TRUE)
          #shell.exec(file.path(getwd(), "Summary_Report/Mofuss_Summary_Report.pdf"))
        } else {
          #shell.exec(file.path(getwd(), "LULCC/Wizard_imgs/NoLuck.pdf"))
        }
        
      } else {
        #shell.exec(file.path(getwd(), "LULCC/Wizard_imgs/PDFrareerror.pdf"))
      }
    }
  }
  write.csv(errorpdf1, "Logs/errorpdf1.csv")
  write.csv(errorpdf2, "Logs/errorpdf2.csv")
  write.csv(errorpdf3, "Logs/errorpdf3.csv")
  write.csv(errorpdf4, "Logs/errorpdf4.csv")
  write.csv(errorpdf5, "Logs/errorpdf5.csv")
  write.csv(Sys.which("pdflatex"), "Logs/pdflatexpath.csv")
  # 	}
  # }
  
  SceCode_pdf <- 	readLines("LULCC/TempTables/SceCode.txt")
  origpdfname <- "Summary_Report/Mofuss_Summary_Report_v3.pdf"
  newpdfname <- paste0("Summary_Report/Mofuss_Summary_Report_v3_",SceCode_pdf,".pdf")
  file.rename(origpdfname, newpdfname)
  
}


if (fNRB_partition_tables == 1) {
  
  # fNRB partition tables and vectors ####
  if (GEpoly == 1) {
    admin <- raster("LULCC/TempRaster//admin_c.tif")
    userarea_gpkg <- st_read("LULCC/TempVector/userarea.gpkg")
  } else {
    admin <- raster("LULCC/TempRaster//admin_c.tif")
    admin1 <- raster("LULCC/TempRaster//admin_c1.tif")
    admin2 <- raster("LULCC/TempRaster//admin_c2.tif")
    
    userarea_gpkg <- st_read("LULCC/TempVector/userarea.gpkg")
    userarea_gpkg1 <- st_read("LULCC/TempVector/userarea1.gpkg")
    userarea_gpkg2 <- st_read("LULCC/TempVector/userarea2.gpkg")
    }
  
  dir.create("OutBaU/webmofuss_results/") 
  
  country_parameters %>%
    dplyr::filter(Var == "ext_analysis_ID") %>%
    pull(ParCHR) -> ext_analysis_ID
  
  country_parameters %>%
    dplyr::filter(Var == "ext_analysis_NAME") %>%
    pull(ParCHR) -> ext_analysis_NAME
  
  country_parameters %>%
    dplyr::filter(Var == "ext_analysis_ID_1") %>%
    pull(ParCHR) -> ext_analysis_ID_1
  
  country_parameters %>%
    dplyr::filter(Var == "ext_analysis_NAME_1") %>%
    pull(ParCHR) -> ext_analysis_NAME_1
  
  country_parameters %>%
    dplyr::filter(Var == "ext_analysis_ID_2") %>%
    pull(ParCHR) -> ext_analysis_ID_2
  
  country_parameters %>%
    dplyr::filter(Var == "ext_analysis_NAME_2") %>%
    pull(ParCHR) -> ext_analysis_NAME_2
  
  
  # # Only the following bins are possible from the 3_demand4IDW_v1 script
  # STdyn = 10 # 2020
  # STdyn = 20 # 2030
  # STdyn = 25 # 2035
  # STdyn = 30 # 2040
  # STdyn = 40 # 2050
  
  # For ALL MonteCarlo realizations and admin levels
  if (GEpoly == 1) {
    adminlevel <- c(admin)
    admin_name <- c("adm0")
  } else {
    adminlevel <- c(admin, admin1, admin2)
    admin_name <- c("adm0", "adm1", "adm2")
  }

  
  foreach(admm = adminlevel, admname = admin_name) %do% {
    #admm <- adminlevel[[1]] # Only the first admin level
    #admname <- admin_name[[1]]
    NRBzon_frlist <- list()
    # MC=2
    for (j in 1:MC) {
      # j = 1
      print(j)
      
      # NRB 
      listGlH <- list.files(paste0("debugging_",j), pattern = "^Growth_less_harv.+[.]tif$",ignore.case=F)
      stackGlH <- stack(paste0(paste0("debugging_",j,"/"),listGlH))
      nlay <- nlayers(stackGlH)
      
      listGx <- list.files(paste0("debugging_",j), pattern = "^Growth.+[.]tif$",ignore.case=F)
      listG <- listGx[ !grepl("_less_harv", listGx) ]
      stackG <- stack(paste0(paste0("debugging_",j,"/"),listG))
      nlayers(stackG) #for cross checking pattern
      
      nlay_yr <- nlay+2009
      nrb_name_per <- paste("nrb_sum_bin2010", nlay_yr, sep = "_")
      calculated_nrb_per <- stackG[[1]] - stackGlH[[nlay-1]] # Bin is entire period, do not use in final tables 
      calculated_nrb_per[calculated_nrb_per <= 0] = NA 
      calculated_sum_nrb_per <- as.data.frame(zonal(calculated_nrb_per, admm, 'sum')) %>%
        as.data.table() %>%
        setnames(.,"sum", paste0("NRB_2010_", nlay_yr))
      assign(nrb_name_per, calculated_sum_nrb_per)
      
      if (STdyn != 10){
      nrb_name <- paste("nrb_sum_bin2020", nlay_yr, sep = "_")
      calculated_nrb <- stackG[[11]] - stackGlH[[nlay-1]] # Bin will start in 2020 and end in the final year
      calculated_nrb[calculated_nrb <= 0] = NA 
      calculated_sum_nrb <- as.data.frame(zonal(calculated_nrb, admm, 'sum')) %>%
        as.data.table() %>%
        setnames(.,"sum", paste0("NRB_2020_",nlay_yr)) %>%
        dplyr::select(!zone)
      assign(nrb_name, calculated_sum_nrb)
      }
      
      if (STdyn == 10){
        nrb_bin2010_2020 <- stackG[[1]] - stackGlH[[nlay-1]] # Bin will be 2010-2020
        nrb_bin2010_2020[nrb_bin2010_2020 <= 0] = NA 
        nrb_sum_bin2010_2020 <- as.data.frame(zonal(nrb_bin2010_2020, admm, 'sum')) %>%
          as.data.table() %>%
          setnames(.,"sum", "NRB_2010_2020") #%>%
          #dplyr::select(!zone)
      }
      
      if (STdyn == 20){
        nrb_bin2010_2020 <- stackG[[1]] - stackGlH[[10]] # Bin will be 2010-2020
        nrb_bin2010_2020[nrb_bin2010_2020 <= 0] = NA 
        nrb_sum_bin2010_2020 <- as.data.frame(zonal(nrb_bin2010_2020, admm, 'sum')) %>%
          as.data.table() %>%
          setnames(.,"sum", "NRB_2010_2020") %>%
          dplyr::select(!zone)
        
        nrb_bin2020_2030 <- stackG[[11]] - stackGlH[[nlay-1]] # Bin will be 2020-2030
        nrb_bin2020_2030[nrb_bin2020_2030 <= 0] = NA 
        nrb_sum_bin2020_2030 <- as.data.frame(zonal(nrb_bin2020_2030, admm, 'sum')) %>%
          as.data.table() %>%
          setnames(.,"sum", "NRB_2020_2030") %>%
          dplyr::select(!zone)
      }
      
      if (STdyn == 25){
        nrb_bin2010_2020 <- stackG[[1]] - stackGlH[[10]] # Bin will be 2010-2020
        nrb_bin2010_2020[nrb_bin2010_2020 <= 0] = NA 
        nrb_sum_bin2010_2020 <- as.data.frame(zonal(nrb_bin2010_2020, admm, 'sum')) %>%
          as.data.table() %>%
          setnames(.,"sum", "NRB_2010_2020") %>%
          dplyr::select(!zone)
        
        nrb_bin2020_2035 <- stackG[[11]] - stackGlH[[nlay-1]] # Bin will be 2020-2035
        nrb_bin2020_2035[nrb_bin2020_2035 <= 0] = NA 
        nrb_sum_bin2020_2035 <- as.data.frame(zonal(nrb_bin2020_2035, admm, 'sum')) %>%
          as.data.table() %>%
          setnames(.,"sum", "NRB_2020_2035") %>%
          dplyr::select(!zone)
      }
      
      if (STdyn == 30){ # STdyn = 30 # 2040
        nrb_bin2010_2020 <- stackG[[1]] - stackGlH[[10]] # Bin will be 2010-2020
        nrb_bin2010_2020[nrb_bin2010_2020 <= 0] = NA 
        nrb_sum_bin2010_2020 <- as.data.frame(zonal(nrb_bin2010_2020, admm, 'sum')) %>%
          as.data.table() %>%
          setnames(.,"sum", "NRB_2010_2020") %>%
          dplyr::select(!zone)
        
        nrb_bin2020_2030 <- stackG[[11]] - stackGlH[[20]] # Bin will be 2020-2030
        nrb_bin2020_2030[nrb_bin2020_2030 <= 0] = NA 
        nrb_sum_bin2020_2030 <- as.data.frame(zonal(nrb_bin2020_2030, admm, 'sum')) %>%
          as.data.table() %>%
          setnames(.,"sum", "NRB_2020_2030") %>%
          dplyr::select(!zone)
        
        nrb_bin2030_2040 <- stackG[[21]] - stackGlH[[nlay-1]] # Bin will be 2030-2040
        nrb_bin2030_2040[nrb_bin2030_2040 <= 0] = NA 
        nrb_sum_bin2030_2040 <- as.data.frame(zonal(nrb_bin2030_2040, admm, 'sum')) %>%
          as.data.table() %>%
          setnames(.,"sum", "NRB_2030_2040") %>%
          dplyr::select(!zone)
        
        nrb_bin2020_2040 <- stackG[[11]] - stackGlH[[nlay-1]] # Bin will be 2020-2040
        nrb_bin2020_2040[nrb_bin2020_2040 <= 0] = NA 
        nrb_sum_bin2020_2040 <- as.data.frame(zonal(nrb_bin2020_2040, admm, 'sum')) %>%
          as.data.table() %>%
          setnames(.,"sum", "NRB_2020_2040") %>%
          dplyr::select(!zone)
      } 
      
      if (STdyn == 40){ # STdyn = 40 # 2050
        nrb_bin2010_2020 <- stackG[[1]] - stackGlH[[10]] # Bin will be 2010-2020
        nrb_bin2010_2020[nrb_bin2010_2020 <= 0] = NA 
        nrb_sum_bin2010_2020 <- as.data.frame(zonal(nrb_bin2010_2020, admm, 'sum')) %>%
          as.data.table() %>%
          setnames(.,"sum", "NRB_2010_2020") %>%
          dplyr::select(!zone)
        
        nrb_bin2020_2030 <- stackG[[11]] - stackGlH[[20]] # Bin will be 2020-2030
        nrb_bin2020_2030[nrb_bin2020_2030 <= 0] = NA 
        nrb_sum_bin2020_2030 <- as.data.frame(zonal(nrb_bin2020_2030, admm, 'sum')) %>%
          as.data.table() %>%
          setnames(.,"sum", "NRB_2020_2030") %>%
          dplyr::select(!zone)
        
        nrb_bin2030_2040 <- stackG[[21]] - stackGlH[[30]] # Bin will be 2030-2040
        nrb_bin2030_2040[nrb_bin2030_2040 <= 0] = NA 
        nrb_sum_bin2030_2040 <- as.data.frame(zonal(nrb_bin2030_2040, admm, 'sum')) %>%
          as.data.table() %>%
          setnames(.,"sum", "NRB_2030_2040") %>%
          dplyr::select(!zone)
        
        nrb_bin2040_2050 <- stackG[[31]] - stackGlH[[nlay-1]] # Bin will be 2040-2050
        nrb_bin2040_2050[nrb_bin2040_2050 <= 0] = NA 
        nrb_sum_bin2040_2050 <- as.data.frame(zonal(nrb_bin2040_2050, admm, 'sum')) %>%
          as.data.table() %>%
          setnames(.,"sum", "NRB_2040_2050") %>%
          dplyr::select(!zone)
        
        nrb_bin2020_2050 <- stackG[[11]] - stackGlH[[nlay-1]] # Bin will be 2020-2050
        nrb_bin2020_2050[nrb_bin2020_2050 <= 0] = NA 
        nrb_sum_bin2020_2050 <- as.data.frame(zonal(nrb_bin2020_2050, admm, 'sum')) %>%
          as.data.table() %>%
          setnames(.,"sum", "NRB_2020_2050") %>%
          dplyr::select(!zone)
      }
      
      # Define all potential variable names
      variable_nrb <- c("nrb_sum_bin2010_2050", # Do not use in summary tables
                        "nrb_sum_bin2010_2040", # Do not use in summary tables
                        "nrb_sum_bin2010_2035", # Do not use in summary tables
                        "nrb_sum_bin2010_2030", # Do not use in summary tables
                        "nrb_sum_bin2010_2020", # Do not use in summary tables
                        "nrb_sum_bin2020_2030", # STdyn = 20 # 2030
                        "nrb_sum_bin2020_2035", # STdyn = 25 # 2035
                        "nrb_sum_bin2020_2040", # STdyn = 30 # 2040 + "nrb_sum_bin2020_2030",
                        "nrb_sum_bin2020_2050", # STdyn = 40 # 2050 + "nrb_sum_bin2020_2030", + "nrb_sum_bin2030_2040"
                        "nrb_sum_bin2030_2040", # STdyn = 30 # 2040 + "nrb_sum_bin2020_2030",
                        "nrb_sum_bin2040_2050") # STdyn = 40 # 2050 + "nrb_sum_bin2020_2030", + "nrb_sum_bin2030_2040"
      
      # Use mget to try to get these variables from the global environment,
      # specifying NA for any that don't exist
      existing_nrb_x2 <- mget(variable_nrb, envir = .GlobalEnv, ifnotfound = list(NA))
      
      # Filter out the NAs from the list. Since each NA is actually a list element, we check for it differently
      existing_nrb_x <- Filter(function(x) {
        if (is.numeric(x) || is.character(x)) { # If the element is a vector or single value
          return(!is.na(x))
        } else if (is.data.frame(x) || is.list(x)) { # If the element is a data frame or list
          # Check if any value in the data frame or list is not NA
          return(any(!is.na(unlist(x))))
        } else {
          return(FALSE) # If the element is of a different type, exclude it
        }
      }, existing_nrb_x2)
      
      # Function to remove duplicates while preserving names
      remove_nrb <- function(lst) {
        unique_nrb <- list()  # Initialize an empty list for the unique elements
        seen_nrbhashes <- character()  # Keep track of hashes for seen elements
        
        for (name in names(lst)) {
          element <- lst[[name]]
          # Serialize the element to a raw vector and generate a hash
          element_nrbhash <- digest::digest(element, serialize = TRUE)
          
          if (!element_nrbhash %in% seen_nrbhashes) {
            unique_nrb[[name]] <- element  # Add to unique list with the original name
            seen_nrbhashes <- c(seen_nrbhashes, element_nrbhash)  # Mark this hash as seen
          }
        }
        
        return(unique_nrb)
      }
      
      # Use the function
      existing_nrb <- remove_nrb(existing_nrb_x)
      
      nrb_bind <- (bind_rows(existing_nrb))
      # Function to remove NAs and shift non-NA values upwards
      shift_up <- function(x) {
        # Remove NAs and return the non-NA values
        non_na_values <- x[!is.na(x)]
        # Calculate the number of NAs to pad
        na_pad <- rep(NA, length(x) - length(non_na_values))
        # Combine non-NA values with NA padding
        return(c(non_na_values, na_pad))
      }
      # Apply the function to each column
      nrb_sum_fr_unfil <- as.data.frame(lapply(nrb_bind, shift_up))
      nrb_sum_fr <- nrb_sum_fr_unfil[!is.na(nrb_sum_fr_unfil[[1]]), ]
      nrb_sum_fr
      
      if (STdyn == 10) {
        nrb_sum_fr <- nrb_sum_fr_unfil
      }
      
      # Harvest
      
      listharvx_per <- list.files(paste0("debugging_",j), pattern = "^Harvest_tot.+[.]tif$",ignore.case=F)
      listharv_per <- listharvx_per[ !grepl("_tot_nrb", listharvx_per) ]
      stackhar_per <- stack(paste0(paste0("debugging_",j,"/"),listharv_per))
      nlayers(stackhar_per)
      
      harv_name_per <- paste("harv_sum_bin2010", nlay_yr, sep = "_")
      harvest_st_per <- stackApply(stackhar_per, indices=1, fun=sum)
      # harv_mean <- stackApply(stackhar_mc1, indices=1, fun=mean)
      harv_sum_per <- as.data.frame(zonal(harvest_st_per, admm, 'sum')) %>% # Bin is entire period, do not use in final tables 
        as.data.table() %>%
        setnames(.,"sum", paste0("Harv_2010_",nlay_yr))
      assign(harv_name_per, harv_sum_per)
      
      harv_name <- paste("harv_sum_bin2020", nlay_yr, sep = "_")
      listharv <- listharv_per[11:(nlay-1)]
      stackharv <- stack(paste0(paste0("debugging_",j,"/"),listharv))
      nlayers(stackharv)
      
      harvest_st <- stackApply(stackharv, indices=1, fun=sum)
      # harv_mean <- stackApply(stackhar_mc1, indices=1, fun=mean)
      harvest_sum_st <- as.data.frame(zonal(harvest_st, admm, 'sum')) %>%
        as.data.table() %>%
        setnames(.,"sum", paste0("Harv_2010_",nlay_yr)) %>%
        dplyr::select(!zone)
      assign(harv_name, harvest_sum_st)
      
      if (STdyn == 10){
        listharv_bin2010_2020 <- listharv_per[1:(nlay-1)]
        stackharv_bin2010_2020 <- stack(paste0(paste0("debugging_",j,"/"),listharv_bin2010_2020))
        nlayers(stackharv_bin2010_2020)
        
        harvest_st_bin2010_2020 <- stackApply(stackharv_bin2010_2020, indices=1, fun=sum)
        # harv_mean <- stackApply(stackhar_mc1, indices=1, fun=mean)
        harv_sum_bin2010_2020 <- as.data.frame(zonal(harvest_st_bin2010_2020, admm, 'sum')) %>%
          as.data.table() %>%
          setnames(.,"sum", "Harv_2010_2020") #%>%
          #dplyr::select(!zone)
      }
      
      if (STdyn == 20){
        listharv_bin2010_2020 <- listharv_per[1:10]
        stackharv_bin2010_2020 <- stack(paste0(paste0("debugging_",j,"/"),listharv_bin2010_2020))
        nlayers(stackharv_bin2010_2020)
        
        harvest_st_bin2010_2020 <- stackApply(stackharv_bin2010_2020, indices=1, fun=sum)
        # harv_mean <- stackApply(stackhar_mc1, indices=1, fun=mean)
        harv_sum_bin2010_2020 <- as.data.frame(zonal(harvest_st_bin2010_2020, admm, 'sum')) %>%
          as.data.table() %>%
          setnames(.,"sum", "Harv_2010_2020") %>%
          dplyr::select(!zone)
        
        listharv_bin2020_2030 <- listharv_per[11:(nlay-1)]
        stackharv_bin2020_2030 <- stack(paste0(paste0("debugging_",j,"/"),listharv_bin2020_2030))
        nlayers(stackharv_bin2020_2030)
        
        harvest_st_bin2020_2030 <- stackApply(stackharv_bin2020_2030, indices=1, fun=sum)
        # harv_mean <- stackApply(stackhar_mc1, indices=1, fun=mean)
        harv_sum_bin2020_2030<- as.data.frame(zonal(harvest_st_bin2020_2030, admm, 'sum')) %>%
          as.data.table() %>%
          setnames(.,"sum", "Harv_2020_2030") %>%
          dplyr::select(!zone)
      }
      
      if (STdyn == 25){
        listharv_bin2010_2020 <- listharv_per[1:10]
        stackharv_bin2010_2020 <- stack(paste0(paste0("debugging_",j,"/"),listharv_bin2010_2020))
        nlayers(stackharv_bin2010_2020)
        
        harvest_st_bin2010_2020 <- stackApply(stackharv_bin2010_2020, indices=1, fun=sum)
        # harv_mean <- stackApply(stackhar_mc1, indices=1, fun=mean)
        harv_sum_bin2010_2020 <- as.data.frame(zonal(harvest_st_bin2010_2020, admm, 'sum')) %>%
          as.data.table() %>%
          setnames(.,"sum", "Harv_2010_2020") %>%
          dplyr::select(!zone)
        
        listharv_bin2020_2035 <- listharv_per[11:(nlay-1)]
        stackharv_bin2020_2035 <- stack(paste0(paste0("debugging_",j,"/"),listharv_bin2020_2035))
        nlayers(stackharv_bin2020_2035)
        
        harvest_st_bin2020_2035 <- stackApply(stackharv_bin2020_2035, indices=1, fun=sum)
        # harv_mean <- stackApply(stackhar_mc1, indices=1, fun=mean)
        harv_sum_bin2020_2035<- as.data.frame(zonal(harvest_st_bin2020_2035, admm, 'sum')) %>%
          as.data.table() %>%
          setnames(.,"sum", "Harv_2020_2035") %>%
          dplyr::select(!zone)
      }
      
      if (STdyn == 30){
        listharv_bin2010_2020 <- listharv_per[1:10]
        stackharv_bin2010_2020 <- stack(paste0(paste0("debugging_",j,"/"),listharv_bin2010_2020))
        nlayers(stackharv_bin2010_2020)
        
        harvest_st_bin2010_2020 <- stackApply(stackharv_bin2010_2020, indices=1, fun=sum)
        # harv_mean <- stackApply(stackhar_mc1, indices=1, fun=mean)
        harv_sum_bin2010_2020 <- as.data.frame(zonal(harvest_st_bin2010_2020, admm, 'sum')) %>%
          as.data.table() %>%
          setnames(.,"sum", "Harv_2010_2020") %>%
          dplyr::select(!zone)
        
        listharv_bin2020_2030 <- listharv_per[11:20]
        stackharv_bin2020_2030 <- stack(paste0(paste0("debugging_",j,"/"),listharv_bin2020_2030))
        nlayers(stackharv_bin2020_2030)
        
        harvest_st_bin2020_2030 <- stackApply(stackharv_bin2020_2030, indices=1, fun=sum)
        # harv_mean <- stackApply(stackhar_mc1, indices=1, fun=mean)
        harv_sum_bin2020_2030<- as.data.frame(zonal(harvest_st_bin2020_2030, admm, 'sum')) %>%
          as.data.table() %>%
          setnames(.,"sum", "Harv_2020_2030") %>%
          dplyr::select(!zone)
        
        listharv_bin2030_2040 <- listharv_per[21:(nlay-1)]
        stackharv_bin2030_2040 <- stack(paste0(paste0("debugging_",j,"/"),listharv_bin2030_2040))
        nlayers(stackharv_bin2030_2040)
        
        harvest_st_bin2030_2040 <- stackApply(stackharv_bin2030_2040, indices=1, fun=sum)
        # harv_mean <- stackApply(stackhar_mc1, indices=1, fun=mean)
        harv_sum_bin2030_2040 <- as.data.frame(zonal(harvest_st_bin2030_2040, admm, 'sum')) %>%
          as.data.table() %>% 
          setnames(.,"sum", "Harv_2030_2040") %>%
          dplyr::select(!zone)
        
        listharv_bin2020_2040 <- listharv_per[11:(nlay-1)]
        stackharv_bin2020_2040 <- stack(paste0(paste0("debugging_",j,"/"),listharv_bin2020_2040))
        nlayers(stackharv_bin2020_2040)
        
        harvest_st_bin2020_2040 <- stackApply(stackharv_bin2020_2040, indices=1, fun=sum)
        # harv_mean <- stackApply(stackhar_mc1, indices=1, fun=mean)
        harv_sum_bin2020_2040<- as.data.frame(zonal(harvest_st_bin2020_2040, admm, 'sum')) %>%
          as.data.table() %>%
          setnames(.,"sum", "Harv_2020_2040") %>%
          dplyr::select(!zone)
      }
      
      if (STdyn == 40){
        listharv_bin2010_2020 <- listharv_per[1:10]
        stackharv_bin2010_2020 <- stack(paste0(paste0("debugging_",j,"/"),listharv_bin2010_2020))
        nlayers(stackharv_bin2010_2020)
        
        harvest_st_bin2010_2020 <- stackApply(stackharv_bin2010_2020, indices=1, fun=sum)
        # harv_mean <- stackApply(stackhar_mc1, indices=1, fun=mean)
        harv_sum_bin2010_2020 <- as.data.frame(zonal(harvest_st_bin2010_2020, admm, 'sum')) %>%
          as.data.table() %>%
          setnames(.,"sum", "Harv_2010_2020") %>%
          dplyr::select(!zone)
        
        listharv_bin2020_2030 <- listharv_per[11:20]
        stackharv_bin2020_2030 <- stack(paste0(paste0("debugging_",j,"/"),listharv_bin2020_2030))
        nlayers(stackharv_bin2020_2030)
        
        harvest_st_bin2020_2030 <- stackApply(stackharv_bin2020_2030, indices=1, fun=sum)
        # harv_mean <- stackApply(stackhar_mc1, indices=1, fun=mean)
        harv_sum_bin2020_2030<- as.data.frame(zonal(harvest_st_bin2020_2030, admm, 'sum')) %>%
          as.data.table() %>%
          setnames(.,"sum", "Harv_2020_2030") %>%
          dplyr::select(!zone)
        
        listharv_bin2030_2040 <- listharv_per[21:30]
        stackharv_bin2030_2040 <- stack(paste0(paste0("debugging_",j,"/"),listharv_bin2030_2040))
        nlayers(stackharv_bin2030_2040)
        
        harvest_st_bin2030_2040 <- stackApply(stackharv_bin2030_2040, indices=1, fun=sum)
        # harv_mean <- stackApply(stackhar_mc1, indices=1, fun=mean)
        harv_sum_bin2030_2040 <- as.data.frame(zonal(harvest_st_bin2030_2040, admm, 'sum')) %>%
          as.data.table() %>% 
          setnames(.,"sum", "Harv_2030_2040") %>%
          dplyr::select(!zone)
        
        listharv_bin2040_2050 <- listharv_per[31:(nlay-1)]
        stackharv_bin2040_2050 <- stack(paste0(paste0("debugging_",j,"/"),listharv_bin2040_2050))
        nlayers(stackharv_bin2040_2050)
        
        harvest_st_bin2040_2050 <- stackApply(stackharv_bin2040_2050, indices=1, fun=sum)
        # harv_mean <- stackApply(stackhar_mc1, indices=1, fun=mean)
        harv_sum_bin2040_2050 <- as.data.frame(zonal(harvest_st_bin2040_2050, admm, 'sum')) %>%
          as.data.table() %>%
          setnames(.,"sum", "Harv_2040_2050") %>%
          dplyr::select(!zone)
        
        listharv_bin2020_2050 <- listharv_per[11:(nlay-1)]
        stackharv_bin2020_2050 <- stack(paste0(paste0("debugging_",j,"/"),listharv_bin2020_2050))
        nlayers(stackharv_bin2020_2050)
        
        harvest_st_bin2020_2050 <- stackApply(stackharv_bin2020_2050, indices=1, fun=sum)
        # harv_mean <- stackApply(stackhar_mc1, indices=1, fun=mean)
        harv_sum_bin2020_2050<- as.data.frame(zonal(harvest_st_bin2020_2050, admm, 'sum')) %>%
          as.data.table() %>%
          setnames(.,"sum", "Harv_2020_2050") %>%
          dplyr::select(!zone)
      }
      
      # Define all potential variable names
      variable_harv <- c("harv_sum_bin2010_2050", # Do not use in summary tables
                         "harv_sum_bin2010_2040", # Do not use in summary tables
                         "harv_sum_bin2010_2035", # Do not use in summary tables
                         "harv_sum_bin2010_2030", # Do not use in summary tables
                         "harv_sum_bin2010_2020", # Do not use in summary tables
                         "harv_sum_bin2020_2030", # STdyn = 20 # 2030
                         "harv_sum_bin2020_2035", # STdyn = 25 # 2035
                         "harv_sum_bin2020_2040", # STdyn = 30 # 2040 + "nrb_sum_bin2020_2030",
                         "harv_sum_bin2020_2050", # STdyn = 40 # 2050 + "nrb_sum_bin2020_2030", + "nrb_sum_bin2030_2040"
                         "harv_sum_bin2030_2040", # STdyn = 30 # 2040 + "nrb_sum_bin2020_2030",
                         "harv_sum_bin2040_2050") # STdyn = 40 # 2050 + "nrb_sum_bin2020_2030", + "nrb_sum_bin2030_2040"
      
      # Use mget to try to get these variables from the global environment,
      # specifying NA for any that don't exist
      existing_harv_x2 <- mget(variable_harv, envir = .GlobalEnv, ifnotfound = list(NA))
      
      # Filter out the NAs from the list. Since each NA is actually a list element, we check for it differently
      existing_harv_x <- Filter(function(x) {
        if (is.numeric(x) || is.character(x)) { # If the element is a vector or single value
          return(!is.na(x))
        } else if (is.data.frame(x) || is.list(x)) { # If the element is a data frame or list
          # Check if any value in the data frame or list is not NA
          return(any(!is.na(unlist(x))))
        } else {
          return(FALSE) # If the element is of a different type, exclude it
        }
      }, existing_harv_x2)
      
      # Function to remove duplicates while preserving names
      remove_harv <- function(lst) {
        unique_harv <- list()  # Initialize an empty list for the unique elements
        seen_harvhashes <- character()  # Keep track of hashes for seen elements
        
        for (name in names(lst)) {
          element <- lst[[name]]
          # Serialize the element to a raw vector and generate a hash
          element_harvhash <- digest::digest(element, serialize = TRUE)
          
          if (!element_harvhash %in% seen_harvhashes) {
            unique_harv[[name]] <- element  # Add to unique list with the original name
            seen_harvhashes <- c(seen_harvhashes, element_harvhash)  # Mark this hash as seen
          }
        }
        
        return(unique_harv)
      }
      
      # Use the function
      existing_harv <- remove_harv(existing_harv_x)
      
      harv_bind <- (bind_rows(existing_harv))
      # Function to remove NAs and shift non-NA values upwards
      shift_up <- function(x) {
        # Remove NAs and return the non-NA values
        non_na_values <- x[!is.na(x)]
        # Calculate the number of NAs to pad
        na_pad <- rep(NA, length(x) - length(non_na_values))
        # Combine non-NA values with NA padding
        return(c(non_na_values, na_pad))
      }
      # Apply the function to each column
      harv_sum_fr_unfil <- as.data.frame(lapply(harv_bind, shift_up))
      harv_sum_fr <- harv_sum_fr_unfil[!is.na(harv_sum_fr_unfil[[1]]), ]
      harv_sum_fr
      
      if (STdyn == 10) {
        harv_sum_fr <- harv_sum_fr_unfil
      }
      
      if (STdyn == 10){
        NRBzon_fr <- merge(nrb_sum_fr, harv_sum_fr, by = "zone") %>%
          # dplyr::rename(NRB_2010_2020 = x,
          #               Harv_2010_2020 = y) %>%
          dplyr::mutate(across(everything(), ~as.integer(as.numeric(trimws(.x))))) %>%
          dplyr::mutate(fNRB_2010_2020 = NRB_2010_2020/Harv_2010_2020*100) %>%
          round(.,0) 
        
        NRBzon_fr$MC <- j  # maybe you want to keep track of which iteration produced it?
        NRBzon_frlist[[j]] <- NRBzon_fr # add it to your list
        
        if (j == 1) {
          NRBzon_frlist1MC <- NRBzon_frlist %>%
            as.data.frame() %>%
            rename_with(.fn = ~ paste0(.x, "_1MC"))
        }  
      } else if (STdyn == 20){
        NRBzon_fr <- merge(nrb_sum_fr, harv_sum_fr, by = "zone") %>%
          dplyr::mutate(across(everything(), ~as.integer(as.numeric(trimws(.x))))) %>%
          dplyr::mutate(fNRB_2010_2030 = NRB_2010_2030/Harv_2010_2030*100,
                        fNRB_2020_2030 = NRB_2020_2030/Harv_2020_2030*100,
                        fNRB_2010_2020 = NRB_2010_2020/Harv_2010_2020*100) %>%
          round(.,0) 
        
        NRBzon_fr$MC <- j  # maybe you want to keep track of which iteration produced it?
        NRBzon_frlist[[j]] <- NRBzon_fr # add it to your list
        
        if (j == 1) {
          NRBzon_frlist1MC <- NRBzon_frlist %>%
            as.data.frame() %>%
            rename_with(.fn = ~ paste0(.x, "_1MC"))
        }  
        
      } else if (STdyn == 25){
        NRBzon_fr <- merge(nrb_sum_fr, harv_sum_fr, by = "zone") %>%
          dplyr::mutate(across(everything(), ~as.integer(as.numeric(trimws(.x))))) %>%
          dplyr::mutate(fNRB_2010_2035 = NRB_2010_2035/Harv_2010_2035*100,
                        fNRB_2020_2035 = NRB_2020_2035/Harv_2020_2035*100,
                        fNRB_2010_2020 = NRB_2010_2020/Harv_2010_2020*100) %>%
          round(.,0) 
        
        NRBzon_fr$MC <- j  # maybe you want to keep track of which iteration produced it?
        NRBzon_frlist[[j]] <- NRBzon_fr # add it to your list
        
        if (j == 1) {
          NRBzon_frlist1MC <- NRBzon_frlist %>%
            as.data.frame() %>%
            rename_with(.fn = ~ paste0(.x, "_1MC"))
        }  
      } else if (STdyn == 30){
        NRBzon_fr <- merge(nrb_sum_fr, harv_sum_fr, by = "zone") %>%
          dplyr::mutate(across(everything(), ~as.integer(as.numeric(trimws(.x))))) %>%
          dplyr::mutate(fNRB_2010_2040 = NRB_2010_2040/Harv_2010_2040*100,
                        fNRB_2020_2040 = NRB_2020_2040/Harv_2020_2040*100,
                        fNRB_2010_2020 = NRB_2010_2020/Harv_2010_2020*100,
                        fNRB_2020_2030 = NRB_2020_2030/Harv_2020_2030*100,
                        fNRB_2030_2040 = NRB_2030_2040/Harv_2030_2040*100) %>%
          round(.,0) 
        
        NRBzon_fr$MC <- j  # maybe you want to keep track of which iteration produced it?
        NRBzon_frlist[[j]] <- NRBzon_fr # add it to your list
        
        if (j == 1) {
          NRBzon_frlist1MC <- NRBzon_frlist %>%
            as.data.frame() %>%
            rename_with(.fn = ~ paste0(.x, "_1MC"))
        }  
      } else if (STdyn == 40){
        NRBzon_fr <- merge(nrb_sum_fr, harv_sum_fr, by = "zone") %>%
          dplyr::mutate(across(everything(), ~as.integer(as.numeric(trimws(.x))))) %>%
          dplyr::mutate(fNRB_2010_2050 = NRB_2010_2050/Harv_2010_2050*100,
                        fNRB_2020_2050 = NRB_2020_2050/Harv_2020_2050*100,
                        fNRB_2010_2020 = NRB_2010_2020/Harv_2010_2020*100,
                        fNRB_2020_2030 = NRB_2020_2030/Harv_2020_2030*100,
                        fNRB_2030_2040 = NRB_2030_2040/Harv_2030_2040*100,
                        fNRB_2040_2050 = NRB_2040_2050/Harv_2040_2050*100) %>%
          round(.,0) 
        
        NRBzon_fr$MC <- j  # maybe you want to keep track of which iteration produced it?
        NRBzon_frlist[[j]] <- NRBzon_fr # add it to your list
        
        if (j == 1) {
          NRBzon_frlist1MC <- NRBzon_frlist %>%
            as.data.frame() %>%
            rename_with(.fn = ~ paste0(.x, "_1MC"))
        } 
      } else {
        print("error with simulation length")  
      }
      
    } # for (j in 1:MC) {
    
    # Integrate tables with all the above datasets ----
    
    if (STdyn == 10){ # STdyn == 10 summary----
      print(10)
      
      NRBzon_frbind <- dplyr::bind_rows(NRBzon_frlist)
      summarycols <- c("NRB_2010_2020", "Harv_2010_2020", "fNRB_2010_2020")
      
      NRBzonfr_st <- NRBzon_frbind %>%
        group_by(zone) %>%
        summarise_at(vars(all_of(summarycols)), 
                     list(mean = mean, 
                          sd = sd, 
                          se = ~ sd(.) / sqrt(n()))) %>%
        round(.,0)
      
      NRBzonfr_stR <- reduce(
        .x = list(
          c('NRB_2010_2020_mean', 'NRB_2010_2020_sd', 'NRB_2010_2020_se'),
          c('Harv_2010_2020_mean', 'Harv_2010_2020_sd', 'Harv_2010_2020_se'),
          c('fNRB_2010_2020_mean', 'fNRB_2010_2020_sd', 'fNRB_2010_2020_se')
        ),
        .f = ~ relocate(.x, .y[2], .after = .y[1]) %>% relocate(.y[3], .after = .y[2]),
        .init = NRBzonfr_st
      )
      NRBzonfr_stR
      names(NRBzonfr_stR)
      
      # Calculate n assuming a spatial autocorrelation of 100km: 4 pixels in 10,000 kernels
      # se_ncell100km <- round((ncell(stackG[[1]])/2500),0)
      se_ncell100km <- MC
      
      NRBzonfr_statsx <- NRBzonfr_stR %>%
        dplyr::mutate(fNRB_2010_2020_mean = NRB_2010_2020_mean / Harv_2010_2020_mean * 100,
                      fNRB_2010_2020_sd = sqrt(((NRB_2010_2020_sd/NRB_2010_2020_mean)^2) + 
                                                 ((Harv_2010_2020_sd/Harv_2010_2020_mean)^2))*100,
                      fNRB_2010_2020_se = fNRB_2010_2020_sd/sqrt(se_ncell100km)) %>%
        round(.,0)
      
      if (MC > mcthreshold) {
        NRBzonfr_stats <- NRBzonfr_statsx %>%
          dplyr::mutate(across(ends_with(c("sd", "se")), ~ NA))
      } else {
        NRBzonfr_stats <- NRBzonfr_statsx
      }
      
      NRB_fNRB2_fr <- cbind(NRBzonfr_stats,NRBzon_frlist1MC)
      NRB_fNRB2_fr
      names(NRB_fNRB2_fr)
      
      if (admname == "adm0") {
        NRB_fNRB2_frcompl_madm0 <- userarea_gpkg %>%
          st_drop_geometry() %>%
          merge(., NRB_fNRB2_fr, by.x = ext_analysis_ID, by.y = "zone") %>%
          replace(is.na(.), 0)
        write.csv(NRB_fNRB2_frcompl_madm0, "LULCC/TempTables/summary_adm0_frcompl.csv", row.names=FALSE, quote=FALSE)
        write.csv(NRB_fNRB2_frcompl_madm0, "OutBaU/webmofuss_results/summary_adm0_frcompl.csv", row.names=FALSE, quote=FALSE)
        
        NRB_fNRB3_fr_madm0 <- NRB_fNRB2_frcompl_madm0
        write.csv(NRB_fNRB3_fr_madm0, "LULCC/TempTables/summary_adm0_fr.csv", row.names=FALSE, quote=FALSE)
        write.csv(NRB_fNRB3_fr_madm0, "OutBaU/webmofuss_results/summary_adm0_fr.csv", row.names=FALSE, quote=FALSE)
        
        print(paste0(admname," finished for tables"))
        
        if (GEpoly == 1) {
          userarea_simpx_fr0 <- userarea_gpkg %>%
            inner_join(.,NRB_fNRB3_fr_madm0, by="ID") %>%
            dplyr::select(-NAME_0.y) %>%
            dplyr::rename(NAME_0 = NAME_0.x) %>%
            replace(is.na(.), 0)
          st_write(userarea_simpx_fr0, "OutBaU/webmofuss_results/mofuss_adm0_fr.gpkg", delete_layer = TRUE)
          print(paste0(admname," finished for vector layers"))
        } else {
          userarea_simpx_fr0 <- userarea_gpkg %>%
            inner_join(.,NRB_fNRB3_fr_madm0, by="ID") %>%
            dplyr::select(-NAME_0.y, -Subregion.y, -mofuss_reg.y, -GID_0.y) %>%
            dplyr::rename(GID_0 = GID_0.x,
                          NAME_0 = NAME_0.x,
                          Subregion = Subregion.x,
                          mofuss_reg = mofuss_reg.x) %>%
            replace(is.na(.), 0)
          st_write(userarea_simpx_fr0, "OutBaU/webmofuss_results/mofuss_adm0_fr.gpkg", delete_layer = TRUE)
          print(paste0(admname," finished for vector layers"))
        }
        
      } else if (admname == "adm1") {
        NRB_fNRB2_frcompl_madm1 <- userarea_gpkg1 %>%
          st_drop_geometry() %>%
          merge(., NRB_fNRB2_fr, by.x = ext_analysis_ID, by.y = "zone") %>%
          dplyr::select(-GID_0, -GID_1) %>%
          replace(is.na(.), 0)
        write.csv(NRB_fNRB2_frcompl_madm1, "LULCC/TempTables/summary_adm1_frcompl.csv", row.names=FALSE, quote=FALSE)
        write.csv(NRB_fNRB2_frcompl_madm1, "OutBaU/webmofuss_results/summary_adm1_frcompl.csv", row.names=FALSE, quote=FALSE)
        
        NRB_fNRB3_fr_madm1 <- NRB_fNRB2_frcompl_madm1
        write.csv(NRB_fNRB3_fr_madm1, "LULCC/TempTables/summary_adm1_fr.csv", row.names=FALSE, quote=FALSE)
        write.csv(NRB_fNRB3_fr_madm1, "OutBaU/webmofuss_results/summary_adm1_fr.csv", row.names=FALSE, quote=FALSE)
        
        print(paste0(admname," finished for tables"))
        
        userarea_simpx_fr1 <- userarea_gpkg1 %>%
          inner_join(.,NRB_fNRB3_fr_madm1, by="ID") %>%
          dplyr::select(-NAME_0.y, -NAME_1.y) %>%
          dplyr::rename(NAME_0 = NAME_0.x,
                        NAME_1 = NAME_1.x) %>%
          replace(is.na(.), 0)
        st_write(userarea_simpx_fr1, "OutBaU/webmofuss_results/mofuss_adm1_fr.gpkg", delete_layer = TRUE)
        print(paste0(admname," finished for vector layers"))
        
      } else if (admname == "adm2") {
        NRB_fNRB2_frcompl_madm2 <- userarea_gpkg2 %>%
          st_drop_geometry() %>%
          merge(., NRB_fNRB2_fr, by.x = ext_analysis_ID, by.y = "zone") %>%
          dplyr::select(-GID_0, -GID_1, -GID_2) %>%
          replace(is.na(.), 0)
        write.csv(NRB_fNRB2_frcompl_madm2, "LULCC/TempTables/summary_adm2_frcompl.csv", row.names=FALSE, quote=FALSE)
        write.csv(NRB_fNRB2_frcompl_madm2, "OutBaU/webmofuss_results/summary_adm2_frcompl.csv", row.names=FALSE, quote=FALSE)
        
        NRB_fNRB3_fr_madm2 <- NRB_fNRB2_frcompl_madm2
        write.csv(NRB_fNRB3_fr_madm2, "LULCC/TempTables/summary_adm2_fr.csv", row.names=FALSE, quote=FALSE)
        write.csv(NRB_fNRB3_fr_madm2, "OutBaU/webmofuss_results/summary_adm2_fr.csv", row.names=FALSE, quote=FALSE)
        
        print(paste0(admname," finished for tables"))
        
        userarea_simpx_fr2 <- userarea_gpkg2 %>%
          inner_join(.,NRB_fNRB3_fr_madm2, by="ID") %>%
          dplyr::select(-NAME_0.y, -NAME_1.y, -NAME_2.y) %>%
          dplyr::rename(NAME_0 = NAME_0.x,
                        NAME_1 = NAME_1.x,
                        NAME_2 = NAME_2.x,) %>%
          replace(is.na(.), 0)
        st_write(userarea_simpx_fr2, "OutBaU/webmofuss_results/mofuss_adm2_fr.gpkg", delete_layer = TRUE)
        print(paste0(admname," finished for vector layers"))
        
      }
      
      
      
    } else if (STdyn == 20){ # STdyn == 20 summary----
      # print(20)
      
      NRBzon_frbind <- dplyr::bind_rows(NRBzon_frlist)
      summarycols <- c("NRB_2010_2030", "NRB_2010_2020", "NRB_2020_2030", 
                       "Harv_2010_2030", "Harv_2010_2020", "Harv_2020_2030",
                       "fNRB_2010_2030", "fNRB_2010_2020", "fNRB_2020_2030")
      
      NRBzonfr_st <- NRBzon_frbind %>%
        group_by(zone) %>%
        summarise_at(vars(all_of(summarycols)), 
                     list(mean = mean, 
                          sd = sd, 
                          se = ~ sd(.) / sqrt(n()))) %>%
        round(.,0)
      
      NRBzonfr_stR <- reduce(
        .x = list(
          c('NRB_2010_2030_mean', 'NRB_2010_2030_sd', 'NRB_2010_2030_se'),
          c('NRB_2010_2020_mean', 'NRB_2010_2020_sd', 'NRB_2010_2020_se'),
          c('NRB_2020_2030_mean', 'NRB_2020_2030_sd', 'NRB_2020_2030_se'),
          c('Harv_2010_2030_mean', 'Harv_2010_2030_sd', 'Harv_2010_2030_se'),
          c('Harv_2010_2020_mean', 'Harv_2010_2020_sd', 'Harv_2010_2020_se'),
          c('Harv_2020_2030_mean', 'Harv_2020_2030_sd', 'Harv_2020_2030_se'),
          c('fNRB_2010_2030_mean', 'fNRB_2010_2030_sd', 'fNRB_2010_2030_se'),
          c('fNRB_2010_2020_mean', 'fNRB_2010_2020_sd', 'fNRB_2010_2020_se'),
          c('fNRB_2020_2030_mean', 'fNRB_2020_2030_sd', 'fNRB_2020_2030_se')
        ),
        .f = ~ relocate(.x, .y[2], .after = .y[1]) %>% relocate(.y[3], .after = .y[2]),
        .init = NRBzonfr_st
      )
      NRBzonfr_stR
      names(NRBzonfr_stR)
      
      # Calculate n assuming a spatial autocorrelation of 100km: 4 pixels in 10,000 kernels
      # se_ncell100km <- round((ncell(stackG[[1]])/2500),0)
      se_ncell100km <- MC
      
      NRBzonfr_statsx <- NRBzonfr_stR %>%
        dplyr::mutate(fNRB_2010_2030_mean = NRB_2010_2030_mean / Harv_2010_2030_mean * 100,
                      fNRB_2010_2030_sd = sqrt(((NRB_2010_2030_sd/NRB_2010_2030_mean)^2) + 
                                                 ((Harv_2010_2030_sd/Harv_2010_2030_mean)^2))*100,
                      fNRB_2010_2030_se = fNRB_2010_2030_sd/sqrt(se_ncell100km),
                      fNRB_2010_2020_mean = NRB_2010_2020_mean / Harv_2010_2020_mean * 100,
                      fNRB_2010_2020_sd = sqrt(((NRB_2010_2020_sd/NRB_2010_2020_mean)^2) + 
                                                 ((Harv_2010_2020_sd/Harv_2010_2020_mean)^2))*100,
                      fNRB_2010_2020_se = fNRB_2010_2020_sd/sqrt(se_ncell100km),
                      fNRB_2020_2030_mean = NRB_2020_2030_mean / Harv_2020_2030_mean * 100,
                      fNRB_2020_2030_sd = sqrt(((NRB_2020_2030_sd/NRB_2020_2030_mean)^2) + 
                                                 ((Harv_2020_2030_sd/Harv_2020_2030_mean)^2))*100,
                      fNRB_2020_2030_se = fNRB_2020_2030_sd/sqrt(se_ncell100km)) %>%
        round(.,0)
      
      if (MC > mcthreshold) {
        NRBzonfr_stats <- NRBzonfr_statsx %>%
          dplyr::mutate(across(ends_with(c("sd", "se")), ~ NA))
      } else {
        NRBzonfr_stats <- NRBzonfr_statsx
      }
      
      NRB_fNRB2_fr <- cbind(NRBzonfr_stats,NRBzon_frlist1MC)
      NRB_fNRB2_fr
      names(NRB_fNRB2_fr)
      
      if (admname == "adm0") {
        NRB_fNRB2_frcompl_madm0 <- userarea_gpkg %>%
          st_drop_geometry() %>%
          merge(., NRB_fNRB2_fr, by.x = ext_analysis_ID, by.y = "zone") %>%
          replace(is.na(.), 0)
        write.csv(NRB_fNRB2_frcompl_madm0, "LULCC/TempTables/summary_adm0_frcompl.csv", row.names=FALSE, quote=FALSE)
        write.csv(NRB_fNRB2_frcompl_madm0, "OutBaU/webmofuss_results/summary_adm0_frcompl.csv", row.names=FALSE, quote=FALSE)
        
        NRB_fNRB3_fr_madm0 <- NRB_fNRB2_frcompl_madm0 %>%
          dplyr::select(-matches("_2010_2030|_2010_2020"), -ends_with("_sd")) %>%
          dplyr::select(-ends_with("_1MC"))
        write.csv(NRB_fNRB3_fr_madm0, "LULCC/TempTables/summary_adm0_fr.csv", row.names=FALSE, quote=FALSE)
        write.csv(NRB_fNRB3_fr_madm0, "OutBaU/webmofuss_results/summary_adm0_fr.csv", row.names=FALSE, quote=FALSE)
        
        print(paste0(admname," finished for tables"))
        
        if (GEpoly == 1) {
          userarea_simpx_fr0 <- userarea_gpkg %>%
            inner_join(.,NRB_fNRB3_fr_madm0, by="ID") %>%
            dplyr::select(-NAME_0.y) %>%
            dplyr::rename(NAME_0 = NAME_0.x) %>%
            replace(is.na(.), 0)
          st_write(userarea_simpx_fr0, "OutBaU/webmofuss_results/mofuss_adm0_fr.gpkg", delete_layer = TRUE)
          print(paste0(admname," finished for vector layers"))
        } else {
          userarea_simpx_fr0 <- userarea_gpkg %>%
            inner_join(.,NRB_fNRB3_fr_madm0, by="ID") %>%
            dplyr::select(-NAME_0.y, -Subregion.y, -mofuss_reg.y, -GID_0.y) %>%
            dplyr::rename(GID_0 = GID_0.x,
                          NAME_0 = NAME_0.x,
                          Subregion = Subregion.x,
                          mofuss_reg = mofuss_reg.x) %>%
            replace(is.na(.), 0)
          st_write(userarea_simpx_fr0, "OutBaU/webmofuss_results/mofuss_adm0_fr.gpkg", delete_layer = TRUE)
          print(paste0(admname," finished for vector layers"))
        }
        
      } else if (admname == "adm1") {
        NRB_fNRB2_frcompl_madm1 <- userarea_gpkg1 %>%
          st_drop_geometry() %>%
          merge(., NRB_fNRB2_fr, by.x = ext_analysis_ID, by.y = "zone") %>%
          dplyr::select(-GID_0, -GID_1) %>%
          replace(is.na(.), 0)
        write.csv(NRB_fNRB2_frcompl_madm1, "LULCC/TempTables/summary_adm1_frcompl.csv", row.names=FALSE, quote=FALSE)
        write.csv(NRB_fNRB2_frcompl_madm1, "OutBaU/webmofuss_results/summary_adm1_frcompl.csv", row.names=FALSE, quote=FALSE)
        
        NRB_fNRB3_fr_madm1 <- NRB_fNRB2_frcompl_madm1 %>%
          dplyr::select(-matches("_2010_2030|_2010_2020"), -ends_with("_sd")) %>%
          dplyr::select(-ends_with("_1MC"))
        write.csv(NRB_fNRB3_fr_madm1, "LULCC/TempTables/summary_adm1_fr.csv", row.names=FALSE, quote=FALSE)
        write.csv(NRB_fNRB3_fr_madm1, "OutBaU/webmofuss_results/summary_adm1_fr.csv", row.names=FALSE, quote=FALSE)
        
        print(paste0(admname," finished for tables"))
        
        userarea_simpx_fr1 <- userarea_gpkg1 %>%
          inner_join(.,NRB_fNRB3_fr_madm1, by="ID") %>%
          dplyr::select(-NAME_0.y, -NAME_1.y) %>%
          dplyr::rename(NAME_0 = NAME_0.x,
                        NAME_1 = NAME_1.x) %>%
          replace(is.na(.), 0)
        st_write(userarea_simpx_fr1, "OutBaU/webmofuss_results/mofuss_adm1_fr.gpkg", delete_layer = TRUE)
        print(paste0(admname," finished for vector layers"))
        
      } else if (admname == "adm2") {
        NRB_fNRB2_frcompl_madm2 <- userarea_gpkg2 %>%
          st_drop_geometry() %>%
          merge(., NRB_fNRB2_fr, by.x = ext_analysis_ID, by.y = "zone") %>%
          dplyr::select(-GID_0, -GID_1, -GID_2) %>%
          replace(is.na(.), 0)
        write.csv(NRB_fNRB2_frcompl_madm2, "LULCC/TempTables/summary_adm2_frcompl.csv", row.names=FALSE, quote=FALSE)
        write.csv(NRB_fNRB2_frcompl_madm2, "OutBaU/webmofuss_results/summary_adm2_frcompl.csv", row.names=FALSE, quote=FALSE)
        
        NRB_fNRB3_fr_madm2 <- NRB_fNRB2_frcompl_madm2 %>%
          dplyr::select(-matches("_2010_2030|_2010_2020"), -ends_with("_sd")) %>%
          dplyr::select(-ends_with("_1MC"))
        write.csv(NRB_fNRB3_fr_madm2, "LULCC/TempTables/summary_adm2_fr.csv", row.names=FALSE, quote=FALSE)
        write.csv(NRB_fNRB3_fr_madm2, "OutBaU/webmofuss_results/summary_adm2_fr.csv", row.names=FALSE, quote=FALSE)
        
        print(paste0(admname," finished for tables"))
        
        userarea_simpx_fr2 <- userarea_gpkg2 %>%
          inner_join(.,NRB_fNRB3_fr_madm2, by="ID") %>%
          dplyr::select(-NAME_0.y, -NAME_1.y, -NAME_2.y) %>%
          dplyr::rename(NAME_0 = NAME_0.x,
                        NAME_1 = NAME_1.x,
                        NAME_2 = NAME_2.x,) %>%
          replace(is.na(.), 0)
        st_write(userarea_simpx_fr2, "OutBaU/webmofuss_results/mofuss_adm2_fr.gpkg", delete_layer = TRUE)
        print(paste0(admname," finished for vector layers"))
        
      }
      
      
    } else if (STdyn == 25){ # STdyn == 25 summary----
      print(25)
      
      NRBzon_frbind <- dplyr::bind_rows(NRBzon_frlist)
      summarycols <- c("NRB_2010_2035", "NRB_2020_2035", "NRB_2010_2020", "NRB_2020_2035", 
                       "Harv_2010_2035", "Harv_2020_2035", "Harv_2010_2020", "Harv_2020_2035",
                       "fNRB_2010_2035", "fNRB_2020_2035", "fNRB_2010_2020", "fNRB_2020_2035")
      
      NRBzonfr_st <- NRBzon_frbind %>%
        group_by(zone) %>%
        summarise_at(vars(all_of(summarycols)), 
                     list(mean = mean, 
                          sd = sd, 
                          se = ~ sd(.) / sqrt(n()))) %>%
        round(.,0)
      
      NRBzonfr_stR <- reduce(
        .x = list(
          c('NRB_2010_2035_mean', 'NRB_2010_2035_sd', 'NRB_2010_2035_se'),
          c('NRB_2020_2035_mean', 'NRB_2020_2035_sd', 'NRB_2020_2035_se'),
          c('NRB_2010_2020_mean', 'NRB_2010_2020_sd', 'NRB_2010_2020_se'),
          c('NRB_2020_2035_mean', 'NRB_2020_2035_sd', 'NRB_2020_2035_se'),
          c('Harv_2010_2035_mean', 'Harv_2010_2035_sd', 'Harv_2010_2035_se'),
          c('Harv_2020_2035_mean', 'Harv_2020_2035_sd', 'Harv_2020_2035_se'),
          c('Harv_2010_2020_mean', 'Harv_2010_2020_sd', 'Harv_2010_2020_se'),
          c('Harv_2020_2035_mean', 'Harv_2020_2035_sd', 'Harv_2020_2035_se'),
          c('fNRB_2010_2035_mean', 'fNRB_2010_2035_sd', 'fNRB_2010_2035_se'),
          c('fNRB_2020_2035_mean', 'fNRB_2020_2035_sd', 'fNRB_2020_2035_se'),
          c('fNRB_2010_2020_mean', 'fNRB_2010_2020_sd', 'fNRB_2010_2020_se'),
          c('fNRB_2020_2035_mean', 'fNRB_2020_2035_sd', 'fNRB_2020_2035_se')
        ),
        .f = ~ relocate(.x, .y[2], .after = .y[1]) %>% relocate(.y[3], .after = .y[2]),
        .init = NRBzonfr_st
      )
      NRBzonfr_stR
      names(NRBzonfr_stR)
      
      # Calculate n assuming a spatial autocorrelation of 100km: 4 pixels in 10,000 kernels
      # se_ncell100km <- round((ncell(stackG[[1]])/2500),0)
      se_ncell100km <- MC
      
      NRBzonfr_statsx <- NRBzonfr_stR %>%
        dplyr::mutate(fNRB_2010_2035_mean = NRB_2010_2035_mean / Harv_2010_2035_mean * 100,
                      fNRB_2010_2035_sd = sqrt(((NRB_2010_2035_sd/NRB_2010_2035_mean)^2) + 
                                                 ((Harv_2010_2035_sd/Harv_2010_2035_mean)^2))*100,
                      fNRB_2010_2035_se = fNRB_2010_2035_sd/sqrt(se_ncell100km),
                      fNRB_2020_2035_mean = NRB_2020_2035_mean / Harv_2020_2035_mean * 100,
                      fNRB_2020_2035_sd = sqrt(((NRB_2020_2035_sd/NRB_2020_2035_mean)^2) + 
                                                 ((Harv_2020_2035_sd/Harv_2020_2035_mean)^2))*100,
                      fNRB_2020_2035_se = fNRB_2020_2035_sd/sqrt(se_ncell100km),
                      fNRB_2010_2020_mean = NRB_2010_2020_mean / Harv_2010_2020_mean * 100,
                      fNRB_2010_2020_sd = sqrt(((NRB_2010_2020_sd/NRB_2010_2020_mean)^2) + 
                                                 ((Harv_2010_2020_sd/Harv_2010_2020_mean)^2))*100,
                      fNRB_2010_2020_se = fNRB_2010_2020_sd/sqrt(se_ncell100km),
                      fNRB_2020_2035_mean = NRB_2020_2035_mean / Harv_2020_2035_mean * 100,
                      fNRB_2020_2035_sd = sqrt(((NRB_2020_2035_sd/NRB_2020_2035_mean)^2) + 
                                                 ((Harv_2020_2035_sd/Harv_2020_2035_mean)^2))*100,
                      fNRB_2020_2035_se = fNRB_2020_2035_sd/sqrt(se_ncell100km)) %>%
        round(.,0)
      
      if (MC > mcthreshold) {
        NRBzonfr_stats <- NRBzonfr_statsx %>%
          dplyr::mutate(across(ends_with(c("sd", "se")), ~ NA))
      } else {
        NRBzonfr_stats <- NRBzonfr_statsx
      }
      
      NRB_fNRB2_fr <- cbind(NRBzonfr_stats,NRBzon_frlist1MC)
      NRB_fNRB2_fr
      names(NRB_fNRB2_fr)
      
      if (admname == "adm0") {
        NRB_fNRB2_frcompl_madm0 <- userarea_gpkg %>%
          st_drop_geometry() %>%
          merge(., NRB_fNRB2_fr, by.x = ext_analysis_ID, by.y = "zone") %>%
          replace(is.na(.), 0)
        write.csv(NRB_fNRB2_frcompl_madm0, "LULCC/TempTables/summary_adm0_frcompl.csv", row.names=FALSE, quote=FALSE)
        write.csv(NRB_fNRB2_frcompl_madm0, "OutBaU/webmofuss_results/summary_adm0_frcompl.csv", row.names=FALSE, quote=FALSE)
        
        NRB_fNRB3_fr_madm0 <- NRB_fNRB2_frcompl_madm0 %>%
          dplyr::select(-matches("_2010_2035|_2010_2020"), -ends_with("_sd")) %>%
          dplyr::relocate(NRB_2020_2035_1MC, .after = zone_1MC) %>%
          dplyr::relocate(Harv_2020_2035_1MC, .after = NRB_2030_2035_1MC) %>% #Not sure this will work with STdyn==25
          dplyr::select(-ends_with("_1MC"))
        write.csv(NRB_fNRB3_fr_madm0, "LULCC/TempTables/summary_adm0_fr.csv", row.names=FALSE, quote=FALSE)
        write.csv(NRB_fNRB3_fr_madm0, "OutBaU/webmofuss_results/summary_adm0_fr.csv", row.names=FALSE, quote=FALSE)
        
        print(paste0(admname," finished for tables"))
        
        if (GEpoly == 1) {
          userarea_simpx_fr0 <- userarea_gpkg %>%
            inner_join(.,NRB_fNRB3_fr_madm0, by="ID") %>%
            dplyr::select(-NAME_0.y) %>%
            dplyr::rename(NAME_0 = NAME_0.x) %>%
            replace(is.na(.), 0)
          st_write(userarea_simpx_fr0, "OutBaU/webmofuss_results/mofuss_adm0_fr.gpkg", delete_layer = TRUE)
          print(paste0(admname," finished for vector layers"))
        } else {
          userarea_simpx_fr0 <- userarea_gpkg %>%
            inner_join(.,NRB_fNRB3_fr_madm0, by="ID") %>%
            dplyr::select(-NAME_0.y, -Subregion.y, -mofuss_reg.y, -GID_0.y) %>%
            dplyr::rename(GID_0 = GID_0.x,
                          NAME_0 = NAME_0.x,
                          Subregion = Subregion.x,
                          mofuss_reg = mofuss_reg.x) %>%
            replace(is.na(.), 0)
          st_write(userarea_simpx_fr0, "OutBaU/webmofuss_results/mofuss_adm0_fr.gpkg", delete_layer = TRUE)
          print(paste0(admname," finished for vector layers"))
        }
        
      } else if (admname == "adm1") {
        NRB_fNRB2_frcompl_madm1 <- userarea_gpkg1 %>%
          st_drop_geometry() %>%
          merge(., NRB_fNRB2_fr, by.x = ext_analysis_ID, by.y = "zone") %>%
          dplyr::select(-GID_0, -GID_1) %>%
          replace(is.na(.), 0)
        write.csv(NRB_fNRB2_frcompl_madm1, "LULCC/TempTables/summary_adm1_frcompl.csv", row.names=FALSE, quote=FALSE)
        write.csv(NRB_fNRB2_frcompl_madm1, "OutBaU/webmofuss_results/summary_adm1_frcompl.csv", row.names=FALSE, quote=FALSE)
        
        NRB_fNRB3_fr_madm1 <- NRB_fNRB2_frcompl_madm1 %>%
          dplyr::select(-matches("_2010_2035|_2010_2020"), -ends_with("_sd")) %>%
          dplyr::relocate(NRB_2020_2035_1MC, .after = zone_1MC) %>%
          dplyr::relocate(Harv_2020_2035_1MC, .after = NRB_2030_2035_1MC) %>% #Not sure this will work with STdyn==25
          dplyr::select(-ends_with("_1MC"))
        write.csv(NRB_fNRB3_fr_madm1, "LULCC/TempTables/summary_adm1_fr.csv", row.names=FALSE, quote=FALSE)
        write.csv(NRB_fNRB3_fr_madm1, "OutBaU/webmofuss_results/summary_adm1_fr.csv", row.names=FALSE, quote=FALSE)
        
        print(paste0(admname," finished for tables"))
        
        userarea_simpx_fr1 <- userarea_gpkg1 %>%
          inner_join(.,NRB_fNRB3_fr_madm1, by="ID") %>%
          dplyr::select(-NAME_0.y, -NAME_1.y) %>%
          dplyr::rename(NAME_0 = NAME_0.x,
                        NAME_1 = NAME_1.x) %>%
          replace(is.na(.), 0)
        st_write(userarea_simpx_fr1, "OutBaU/webmofuss_results/mofuss_adm1_fr.gpkg", delete_layer = TRUE)
        print(paste0(admname," finished for vector layers"))
        
      } else if (admname == "adm2") {
        NRB_fNRB2_frcompl_madm2 <- userarea_gpkg2 %>%
          st_drop_geometry() %>%
          merge(., NRB_fNRB2_fr, by.x = ext_analysis_ID, by.y = "zone") %>%
          dplyr::select(-GID_0, -GID_1, -GID_2) %>%
          replace(is.na(.), 0)
        write.csv(NRB_fNRB2_frcompl_madm2, "LULCC/TempTables/summary_adm2_frcompl.csv", row.names=FALSE, quote=FALSE)
        write.csv(NRB_fNRB2_frcompl_madm2, "OutBaU/webmofuss_results/summary_adm2_frcompl.csv", row.names=FALSE, quote=FALSE)
        
        NRB_fNRB3_fr_madm2 <- NRB_fNRB2_frcompl_madm2 %>%
          dplyr::select(-matches("_2010_2035|_2010_2020"), -ends_with("_sd")) %>%
          dplyr::relocate(NRB_2020_2035_1MC, .after = zone_1MC) %>%
          dplyr::relocate(Harv_2020_2035_1MC, .after = NRB_2030_2035_1MC) %>% #Not sure this will work with STdyn==25
          dplyr::select(-ends_with("_1MC"))
        write.csv(NRB_fNRB3_fr_madm2, "LULCC/TempTables/summary_adm2_fr.csv", row.names=FALSE, quote=FALSE)
        write.csv(NRB_fNRB3_fr_madm2, "OutBaU/webmofuss_results/summary_adm2_fr.csv", row.names=FALSE, quote=FALSE)
        
        print(paste0(admname," finished for tables"))
        
        userarea_simpx_fr2 <- userarea_gpkg2 %>%
          inner_join(.,NRB_fNRB3_fr_madm2, by="ID") %>%
          dplyr::select(-NAME_0.y, -NAME_1.y, -NAME_2.y) %>%
          dplyr::rename(NAME_0 = NAME_0.x,
                        NAME_1 = NAME_1.x,
                        NAME_2 = NAME_2.x,) %>%
          replace(is.na(.), 0)
        st_write(userarea_simpx_fr2, "OutBaU/webmofuss_results/mofuss_adm2_fr.gpkg", delete_layer = TRUE)
        print(paste0(admname," finished for vector layers"))
        
      }
      
      
      
    } else if (STdyn == 30){ # STdyn == 30 summary----
      print(30)

      NRBzon_frbind <- dplyr::bind_rows(NRBzon_frlist)
      summarycols <- c("NRB_2010_2040", "NRB_2020_2040", "NRB_2010_2020", "NRB_2020_2030", "NRB_2030_2040",
                       "Harv_2010_2040", "Harv_2020_2040", "Harv_2010_2020", "Harv_2020_2030",  "Harv_2030_2040", 
                       "fNRB_2010_2040", "fNRB_2020_2040", "fNRB_2010_2020", "fNRB_2020_2030", "fNRB_2030_2040")
      
      NRBzonfr_st <- NRBzon_frbind %>%
        group_by(zone) %>%
        summarise_at(vars(all_of(summarycols)), 
                     list(mean = mean, 
                          sd = sd, 
                          se = ~ sd(.) / sqrt(n()))) %>%
        round(.,0)
      
      NRBzonfr_stR <- reduce(
        .x = list(
          c('NRB_2010_2040_mean', 'NRB_2010_2040_sd', 'NRB_2010_2040_se'),
          c('NRB_2020_2040_mean', 'NRB_2020_2040_sd', 'NRB_2020_2040_se'),
          c('NRB_2010_2020_mean', 'NRB_2010_2020_sd', 'NRB_2010_2020_se'),
          c('NRB_2020_2030_mean', 'NRB_2020_2030_sd', 'NRB_2020_2030_se'),
          c('NRB_2030_2040_mean', 'NRB_2030_2040_sd', 'NRB_2030_2040_se'),
          c('Harv_2010_2040_mean', 'Harv_2010_2040_sd', 'Harv_2010_2040_se'),
          c('Harv_2020_2040_mean', 'Harv_2020_2040_sd', 'Harv_2020_2040_se'),
          c('Harv_2010_2020_mean', 'Harv_2010_2020_sd', 'Harv_2010_2020_se'),
          c('Harv_2020_2030_mean', 'Harv_2020_2030_sd', 'Harv_2020_2030_se'),
          c('Harv_2030_2040_mean', 'Harv_2030_2040_sd', 'Harv_2030_2040_se'),
          c('fNRB_2010_2040_mean', 'fNRB_2010_2040_sd', 'fNRB_2010_2040_se'),
          c('fNRB_2020_2040_mean', 'fNRB_2020_2040_sd', 'fNRB_2020_2040_se'),
          c('fNRB_2010_2020_mean', 'fNRB_2010_2020_sd', 'fNRB_2010_2020_se'),
          c('fNRB_2020_2030_mean', 'fNRB_2020_2030_sd', 'fNRB_2020_2030_se'),
          c('fNRB_2030_2040_mean', 'fNRB_2030_2040_sd', 'fNRB_2030_2040_se')
        ),
        .f = ~ relocate(.x, .y[2], .after = .y[1]) %>% relocate(.y[3], .after = .y[2]),
        .init = NRBzonfr_st
      )
      NRBzonfr_stR
      names(NRBzonfr_stR)
      
      # Calculate n assuming a spatial autocorrelation of 100km: 4 pixels in 10,000 kernels
      # se_ncell100km <- round((ncell(stackG[[1]])/2500),0)
      se_ncell100km <- MC
      
      NRBzonfr_statsx <- NRBzonfr_stR %>%
        dplyr::mutate(fNRB_2010_2040_mean = NRB_2010_2040_mean / Harv_2010_2040_mean * 100,
                      fNRB_2010_2040_sd = sqrt(((NRB_2010_2040_sd/NRB_2010_2040_mean)^2) + 
                                                 ((Harv_2010_2040_sd/Harv_2010_2040_mean)^2))*100,
                      fNRB_2010_2040_se = fNRB_2010_2040_sd/sqrt(se_ncell100km),
                      fNRB_2020_2040_mean = NRB_2020_2040_mean / Harv_2020_2040_mean * 100,
                      fNRB_2020_2040_sd = sqrt(((NRB_2020_2040_sd/NRB_2020_2040_mean)^2) + 
                                                 ((Harv_2020_2040_sd/Harv_2020_2040_mean)^2))*100,
                      fNRB_2020_2040_se = fNRB_2020_2040_sd/sqrt(se_ncell100km),
                      fNRB_2010_2020_mean = NRB_2010_2020_mean / Harv_2010_2020_mean * 100,
                      fNRB_2010_2020_sd = sqrt(((NRB_2010_2020_sd/NRB_2010_2020_mean)^2) + 
                                                 ((Harv_2010_2020_sd/Harv_2010_2020_mean)^2))*100,
                      fNRB_2010_2020_se = fNRB_2010_2020_sd/sqrt(se_ncell100km),
                      fNRB_2020_2030_mean = NRB_2020_2030_mean / Harv_2020_2030_mean * 100,
                      fNRB_2020_2030_sd = sqrt(((NRB_2020_2030_sd/NRB_2020_2030_mean)^2) + 
                                                 ((Harv_2020_2030_sd/Harv_2020_2030_mean)^2))*100,
                      fNRB_2020_2030_se = fNRB_2020_2030_sd/sqrt(se_ncell100km),
                      fNRB_2030_2040_mean = NRB_2030_2040_mean / Harv_2030_2040_mean * 100,
                      fNRB_2030_2040_sd = sqrt(((NRB_2030_2040_sd/NRB_2030_2040_mean)^2) + 
                                                 ((Harv_2030_2040_sd/Harv_2030_2040_mean)^2))*100,
                      fNRB_2030_2040_se = fNRB_2030_2040_sd/sqrt(se_ncell100km)) %>%
        round(.,0)
      
      if (MC > mcthreshold) {
        NRBzonfr_stats <- NRBzonfr_statsx %>%
          dplyr::mutate(across(ends_with(c("sd", "se")), ~ NA))
      } else {
        NRBzonfr_stats <- NRBzonfr_statsx
      }
      
      NRB_fNRB2_fr <- cbind(NRBzonfr_stats,NRBzon_frlist1MC)
      NRB_fNRB2_fr
      names(NRB_fNRB2_fr)
      
      if (admname == "adm0") {
        NRB_fNRB2_frcompl_madm0 <- userarea_gpkg %>%
          st_drop_geometry() %>%
          merge(., NRB_fNRB2_fr, by.x = ext_analysis_ID, by.y = "zone") %>%
          replace(is.na(.), 0)
        write.csv(NRB_fNRB2_frcompl_madm0, "LULCC/TempTables/summary_adm0_frcompl.csv", row.names=FALSE, quote=FALSE)
        write.csv(NRB_fNRB2_frcompl_madm0, "OutBaU/webmofuss_results/summary_adm0_frcompl.csv", row.names=FALSE, quote=FALSE)
        
        NRB_fNRB3_fr_madm0 <- NRB_fNRB2_frcompl_madm0 %>%
          dplyr::select(-matches("_2010_2040|_2010_2020"), -ends_with("_sd")) %>%
          dplyr::relocate(NRB_2020_2040_1MC, .after = zone_1MC) %>%
          dplyr::relocate(Harv_2020_2040_1MC, .after = NRB_2030_2040_1MC) %>%
          dplyr::select(-ends_with("_1MC"))
        write.csv(NRB_fNRB3_fr_madm0, "LULCC/TempTables/summary_adm0_fr.csv", row.names=FALSE, quote=FALSE)
        write.csv(NRB_fNRB3_fr_madm0, "OutBaU/webmofuss_results/summary_adm0_fr.csv", row.names=FALSE, quote=FALSE)
        
        print(paste0(admname," finished for tables"))
        
        if (GEpoly == 1) {
          userarea_simpx_fr0 <- userarea_gpkg %>%
            inner_join(.,NRB_fNRB3_fr_madm0, by="ID") %>%
            dplyr::select(-NAME_0.y) %>%
            dplyr::rename(NAME_0 = NAME_0.x) %>%
            replace(is.na(.), 0)
          st_write(userarea_simpx_fr0, "OutBaU/webmofuss_results/mofuss_adm0_fr.gpkg", delete_layer = TRUE)
          print(paste0(admname," finished for vector layers"))
        } else {
          userarea_simpx_fr0 <- userarea_gpkg %>%
            inner_join(.,NRB_fNRB3_fr_madm0, by="ID") %>%
            dplyr::select(-NAME_0.y, -Subregion.y, -mofuss_reg.y, -GID_0.y) %>%
            dplyr::rename(GID_0 = GID_0.x,
                          NAME_0 = NAME_0.x,
                          Subregion = Subregion.x,
                          mofuss_reg = mofuss_reg.x) %>%
            replace(is.na(.), 0)
          st_write(userarea_simpx_fr0, "OutBaU/webmofuss_results/mofuss_adm0_fr.gpkg", delete_layer = TRUE)
          print(paste0(admname," finished for vector layers"))
        }
        
      } else if (admname == "adm1") {
        NRB_fNRB2_frcompl_madm1 <- userarea_gpkg1 %>%
          st_drop_geometry() %>%
          merge(., NRB_fNRB2_fr, by.x = ext_analysis_ID, by.y = "zone") %>%
          dplyr::select(-GID_0, -GID_1) %>%
          replace(is.na(.), 0)
        write.csv(NRB_fNRB2_frcompl_madm1, "LULCC/TempTables/summary_adm1_frcompl.csv", row.names=FALSE, quote=FALSE)
        write.csv(NRB_fNRB2_frcompl_madm1, "OutBaU/webmofuss_results/summary_adm1_frcompl.csv", row.names=FALSE, quote=FALSE)
        
        NRB_fNRB3_fr_madm1 <- NRB_fNRB2_frcompl_madm1 %>%
          dplyr::select(-matches("_2010_2040|_2010_2020"), -ends_with("_sd")) %>%
          dplyr::relocate(NRB_2020_2040_1MC, .after = zone_1MC) %>%
          dplyr::relocate(Harv_2020_2040_1MC, .after = NRB_2030_2040_1MC) %>%
          dplyr::select(-ends_with("_1MC"))
        write.csv(NRB_fNRB3_fr_madm1, "LULCC/TempTables/summary_adm1_fr.csv", row.names=FALSE, quote=FALSE)
        write.csv(NRB_fNRB3_fr_madm1, "OutBaU/webmofuss_results/summary_adm1_fr.csv", row.names=FALSE, quote=FALSE)
        
        print(paste0(admname," finished for tables"))
        
        userarea_simpx_fr1 <- userarea_gpkg1 %>%
          inner_join(.,NRB_fNRB3_fr_madm1, by="ID") %>%
          dplyr::select(-NAME_0.y, -NAME_1.y) %>%
          dplyr::rename(NAME_0 = NAME_0.x,
                        NAME_1 = NAME_1.x) %>%
          replace(is.na(.), 0)
        st_write(userarea_simpx_fr1, "OutBaU/webmofuss_results/mofuss_adm1_fr.gpkg", delete_layer = TRUE)
        print(paste0(admname," finished for vector layers"))
        
      } else if (admname == "adm2") {
        NRB_fNRB2_frcompl_madm2 <- userarea_gpkg2 %>%
          st_drop_geometry() %>%
          merge(., NRB_fNRB2_fr, by.x = ext_analysis_ID, by.y = "zone") %>%
          dplyr::select(-GID_0, -GID_1, -GID_2) %>%
          replace(is.na(.), 0)
        write.csv(NRB_fNRB2_frcompl_madm2, "LULCC/TempTables/summary_adm2_frcompl.csv", row.names=FALSE, quote=FALSE)
        write.csv(NRB_fNRB2_frcompl_madm2, "OutBaU/webmofuss_results/summary_adm2_frcompl.csv", row.names=FALSE, quote=FALSE)
        
        NRB_fNRB3_fr_madm2 <- NRB_fNRB2_frcompl_madm2 %>%
          dplyr::select(-matches("_2010_2040|_2010_2020"), -ends_with("_sd")) %>%
          dplyr::relocate(NRB_2020_2040_1MC, .after = zone_1MC) %>%
          dplyr::relocate(Harv_2020_2040_1MC, .after = NRB_2030_2040_1MC) %>%
          dplyr::select(-ends_with("_1MC"))
        write.csv(NRB_fNRB3_fr_madm2, "LULCC/TempTables/summary_adm2_fr.csv", row.names=FALSE, quote=FALSE)
        write.csv(NRB_fNRB3_fr_madm2, "OutBaU/webmofuss_results/summary_adm2_fr.csv", row.names=FALSE, quote=FALSE)
        
        print(paste0(admname," finished for tables"))
        
        userarea_simpx_fr2 <- userarea_gpkg2 %>%
          inner_join(.,NRB_fNRB3_fr_madm2, by="ID") %>%
          dplyr::select(-NAME_0.y, -NAME_1.y, -NAME_2.y) %>%
          dplyr::rename(NAME_0 = NAME_0.x,
                        NAME_1 = NAME_1.x,
                        NAME_2 = NAME_2.x,) %>%
          replace(is.na(.), 0)
        st_write(userarea_simpx_fr2, "OutBaU/webmofuss_results/mofuss_adm2_fr.gpkg", delete_layer = TRUE)
        print(paste0(admname," finished for vector layers"))
        
      }
      
      
      
    } else if (STdyn == 40){ # STdyn == 40 summary----
      print(40)
      
      NRBzon_frbind <- dplyr::bind_rows(NRBzon_frlist)
      summarycols <- c("NRB_2010_2050", "NRB_2020_2050", "NRB_2010_2020", "NRB_2020_2030", "NRB_2030_2040", "NRB_2040_2050",
                       "Harv_2010_2050", "Harv_2020_2050", "Harv_2010_2020", "Harv_2020_2030",  "Harv_2030_2040", "Harv_2040_2050", 
                       "fNRB_2010_2050", "fNRB_2020_2050", "fNRB_2010_2020", "fNRB_2020_2030", "fNRB_2030_2040", "fNRB_2040_2050")
      
      NRBzonfr_st <- NRBzon_frbind %>%
        group_by(zone) %>%
        summarise_at(vars(all_of(summarycols)), 
                     list(mean = mean, 
                          sd = sd, 
                          se = ~ sd(.) / sqrt(n()))) %>%
        round(.,0)
      
      NRBzonfr_stR <- reduce(
        .x = list(
          c('NRB_2010_2050_mean', 'NRB_2010_2050_sd', 'NRB_2010_2050_se'),
          c('NRB_2020_2050_mean', 'NRB_2020_2050_sd', 'NRB_2020_2050_se'),
          c('NRB_2010_2020_mean', 'NRB_2010_2020_sd', 'NRB_2010_2020_se'),
          c('NRB_2020_2030_mean', 'NRB_2020_2030_sd', 'NRB_2020_2030_se'),
          c('NRB_2030_2040_mean', 'NRB_2030_2040_sd', 'NRB_2030_2040_se'),
          c('NRB_2040_2050_mean', 'NRB_2040_2050_sd', 'NRB_2040_2050_se'),
          c('Harv_2010_2050_mean', 'Harv_2010_2050_sd', 'Harv_2010_2050_se'),
          c('Harv_2020_2050_mean', 'Harv_2020_2050_sd', 'Harv_2020_2050_se'),
          c('Harv_2010_2020_mean', 'Harv_2010_2020_sd', 'Harv_2010_2020_se'),
          c('Harv_2020_2030_mean', 'Harv_2020_2030_sd', 'Harv_2020_2030_se'),
          c('Harv_2030_2040_mean', 'Harv_2030_2040_sd', 'Harv_2030_2040_se'),
          c('Harv_2040_2050_mean', 'Harv_2040_2050_sd', 'Harv_2040_2050_se'),
          c('fNRB_2010_2050_mean', 'fNRB_2010_2050_sd', 'fNRB_2010_2050_se'),
          c('fNRB_2020_2050_mean', 'fNRB_2020_2050_sd', 'fNRB_2020_2050_se'),
          c('fNRB_2010_2020_mean', 'fNRB_2010_2020_sd', 'fNRB_2010_2020_se'),
          c('fNRB_2020_2030_mean', 'fNRB_2020_2030_sd', 'fNRB_2020_2030_se'),
          c('fNRB_2030_2040_mean', 'fNRB_2030_2040_sd', 'fNRB_2030_2040_se'),
          c('fNRB_2040_2050_mean', 'fNRB_2040_2050_sd', 'fNRB_2040_2050_se')
        ),
        .f = ~ relocate(.x, .y[2], .after = .y[1]) %>% relocate(.y[3], .after = .y[2]),
        .init = NRBzonfr_st
      )
      NRBzonfr_stR
      names(NRBzonfr_stR)
      
      # Calculate n assuming a spatial autocorrelation of 100km: 4 pixels in 10,000 kernels
      # se_ncell100km <- round((ncell(stackG[[1]])/2500),0)
      se_ncell100km <- MC
      
      NRBzonfr_statsx <- NRBzonfr_stR %>%
        dplyr::mutate(fNRB_2010_2050_mean = NRB_2010_2050_mean / Harv_2010_2050_mean * 100,
                      fNRB_2010_2050_sd = sqrt(((NRB_2010_2050_sd/NRB_2010_2050_mean)^2) + 
                                                 ((Harv_2010_2050_sd/Harv_2010_2050_mean)^2))*100,
                      fNRB_2010_2050_se = fNRB_2010_2050_sd/sqrt(se_ncell100km),
                      fNRB_2020_2050_mean = NRB_2020_2050_mean / Harv_2020_2050_mean * 100,
                      fNRB_2020_2050_sd = sqrt(((NRB_2020_2050_sd/NRB_2020_2050_mean)^2) + 
                                                 ((Harv_2020_2050_sd/Harv_2020_2050_mean)^2))*100,
                      fNRB_2020_2050_se = fNRB_2020_2050_sd/sqrt(se_ncell100km),
                      fNRB_2010_2020_mean = NRB_2010_2020_mean / Harv_2010_2020_mean * 100,
                      fNRB_2010_2020_sd = sqrt(((NRB_2010_2020_sd/NRB_2010_2020_mean)^2) + 
                                                 ((Harv_2010_2020_sd/Harv_2010_2020_mean)^2))*100,
                      fNRB_2010_2020_se = fNRB_2010_2020_sd/sqrt(se_ncell100km),
                      fNRB_2020_2030_mean = NRB_2020_2030_mean / Harv_2020_2030_mean * 100,
                      fNRB_2020_2030_sd = sqrt(((NRB_2020_2030_sd/NRB_2020_2030_mean)^2) + 
                                                 ((Harv_2020_2030_sd/Harv_2020_2030_mean)^2))*100,
                      fNRB_2020_2030_se = fNRB_2020_2030_sd/sqrt(se_ncell100km),
                      fNRB_2030_2040_mean = NRB_2030_2040_mean / Harv_2030_2040_mean * 100,
                      fNRB_2030_2040_sd = sqrt(((NRB_2030_2040_sd/NRB_2030_2040_mean)^2) + 
                                                 ((Harv_2030_2040_sd/Harv_2030_2040_mean)^2))*100,
                      fNRB_2030_2040_se = fNRB_2030_2040_sd/sqrt(se_ncell100km),
                      fNRB_2040_2050_mean = NRB_2040_2050_mean / Harv_2040_2050_mean * 100,
                      fNRB_2040_2050_sd = sqrt(((NRB_2040_2050_sd/NRB_2040_2050_mean)^2) + 
                                                 ((Harv_2040_2050_sd/Harv_2040_2050_mean)^2))*100,
                      fNRB_2040_2050_se = fNRB_2040_2050_sd/sqrt(se_ncell100km)) %>%
        round(.,0)
      names(NRBzonfr_statsx)
      
      if (MC > mcthreshold) {
        NRBzonfr_stats <- NRBzonfr_statsx %>%
          dplyr::mutate(across(ends_with(c("sd", "se")), ~ NA))
      } else {
        NRBzonfr_stats <- NRBzonfr_statsx
      }

      NRB_fNRB2_fr <- cbind(NRBzonfr_stats,NRBzon_frlist1MC)
      NRB_fNRB2_fr
      names(NRB_fNRB2_fr)
      
      if (admname == "adm0") {
        NRB_fNRB2_frcompl_madm0 <- userarea_gpkg %>%
          st_drop_geometry() %>%
          merge(., NRB_fNRB2_fr, by.x = ext_analysis_ID, by.y = "zone") %>%
          replace(is.na(.), 0)
        write.csv(NRB_fNRB2_frcompl_madm0, "LULCC/TempTables/summary_adm0_frcompl.csv", row.names=FALSE, quote=FALSE)
        write.csv(NRB_fNRB2_frcompl_madm0, "OutBaU/webmofuss_results/summary_adm0_frcompl.csv", row.names=FALSE, quote=FALSE)
        
        NRB_fNRB3_fr_madm0 <- NRB_fNRB2_frcompl_madm0 %>%
          dplyr::select(-matches("_2010_2050|_2010_2020"), -ends_with("_sd")) %>%
          dplyr::relocate(NRB_2020_2050_1MC, .after = zone_1MC) %>%
          dplyr::relocate(Harv_2020_2050_1MC, .after = NRB_2040_2050_1MC) %>%
          dplyr::select(-ends_with("_1MC"))
        write.csv(NRB_fNRB3_fr_madm0, "LULCC/TempTables/summary_adm0_fr.csv", row.names=FALSE, quote=FALSE)
        write.csv(NRB_fNRB3_fr_madm0, "OutBaU/webmofuss_results/summary_adm0_fr.csv", row.names=FALSE, quote=FALSE)
        
        print(paste0(admname," finished for tables"))
        
        if (GEpoly == 1) {
          userarea_simpx_fr0 <- userarea_gpkg %>%
            inner_join(.,NRB_fNRB3_fr_madm0, by="ID") %>%
            dplyr::select(-NAME_0.y) %>%
            dplyr::rename(NAME_0 = NAME_0.x) %>%
            replace(is.na(.), 0)
          st_write(userarea_simpx_fr0, "OutBaU/webmofuss_results/mofuss_adm0_fr.gpkg", delete_layer = TRUE)
          print(paste0(admname," finished for vector layers"))
        } else {
          userarea_simpx_fr0 <- userarea_gpkg %>%
            inner_join(.,NRB_fNRB3_fr_madm0, by="ID") %>%
            dplyr::select(-NAME_0.y, -Subregion.y, -mofuss_reg.y, -GID_0.y) %>%
            dplyr::rename(GID_0 = GID_0.x,
                          NAME_0 = NAME_0.x,
                          Subregion = Subregion.x,
                          mofuss_reg = mofuss_reg.x) %>%
            replace(is.na(.), 0)
          st_write(userarea_simpx_fr0, "OutBaU/webmofuss_results/mofuss_adm0_fr.gpkg", delete_layer = TRUE)
          print(paste0(admname," finished for vector layers"))
        }
        
       
      } else if (admname == "adm1") {
        NRB_fNRB2_frcompl_madm1 <- userarea_gpkg1 %>%
          st_drop_geometry() %>%
          merge(., NRB_fNRB2_fr, by.x = ext_analysis_ID, by.y = "zone") %>%
          dplyr::select(-GID_0, -GID_1) %>%
          replace(is.na(.), 0)
        write.csv(NRB_fNRB2_frcompl_madm1, "LULCC/TempTables/summary_adm1_frcompl.csv", row.names=FALSE, quote=FALSE)
        write.csv(NRB_fNRB2_frcompl_madm1, "OutBaU/webmofuss_results/summary_adm1_frcompl.csv", row.names=FALSE, quote=FALSE)
        
        NRB_fNRB3_fr_madm1 <- NRB_fNRB2_frcompl_madm1 %>%
          dplyr::select(-matches("_2010_2050|_2010_2020"), -ends_with("_sd")) %>%
          dplyr::relocate(NRB_2020_2050_1MC, .after = zone_1MC) %>%
          dplyr::relocate(Harv_2020_2050_1MC, .after = NRB_2040_2050_1MC) %>%
          dplyr::select(-ends_with("_1MC"))
        write.csv(NRB_fNRB3_fr_madm1, "LULCC/TempTables/summary_adm1_fr.csv", row.names=FALSE, quote=FALSE)
        write.csv(NRB_fNRB3_fr_madm1, "OutBaU/webmofuss_results/summary_adm1_fr.csv", row.names=FALSE, quote=FALSE)
        
        print(paste0(admname," finished for tables"))
        
        userarea_simpx_fr1 <- userarea_gpkg1 %>%
          inner_join(.,NRB_fNRB3_fr_madm1, by="ID") %>%
          dplyr::select(-NAME_0.y, -NAME_1.y) %>%
          dplyr::rename(NAME_0 = NAME_0.x,
                        NAME_1 = NAME_1.x) %>%
          replace(is.na(.), 0)
        st_write(userarea_simpx_fr1, "OutBaU/webmofuss_results/mofuss_adm1_fr.gpkg", delete_layer = TRUE)
        print(paste0(admname," finished for vector layers"))
        
      } else if (admname == "adm2") {
        NRB_fNRB2_frcompl_madm2 <- userarea_gpkg2 %>%
          st_drop_geometry() %>%
          merge(., NRB_fNRB2_fr, by.x = ext_analysis_ID, by.y = "zone") %>%
          dplyr::select(-GID_0, -GID_1, -GID_2) %>%
          replace(is.na(.), 0)
        write.csv(NRB_fNRB2_frcompl_madm2, "LULCC/TempTables/summary_adm2_frcompl.csv", row.names=FALSE, quote=FALSE)
        write.csv(NRB_fNRB2_frcompl_madm2, "OutBaU/webmofuss_results/summary_adm2_frcompl.csv", row.names=FALSE, quote=FALSE)
        
        NRB_fNRB3_fr_madm2 <- NRB_fNRB2_frcompl_madm2 %>%
          dplyr::select(-matches("_2010_2050|_2010_2020"), -ends_with("_sd")) %>%
          dplyr::relocate(NRB_2020_2050_1MC, .after = zone_1MC) %>%
          dplyr::relocate(Harv_2020_2050_1MC, .after = NRB_2040_2050_1MC) %>%
          dplyr::select(-ends_with("_1MC"))
        write.csv(NRB_fNRB3_fr_madm2, "LULCC/TempTables/summary_adm2_fr.csv", row.names=FALSE, quote=FALSE)
        write.csv(NRB_fNRB3_fr_madm2, "OutBaU/webmofuss_results/summary_adm2_fr.csv", row.names=FALSE, quote=FALSE)
        
        print(paste0(admname," finished for tables"))
        
        userarea_simpx_fr2 <- userarea_gpkg2 %>%
          inner_join(.,NRB_fNRB3_fr_madm2, by="ID") %>%
          dplyr::select(-NAME_0.y, -NAME_1.y, -NAME_2.y) %>%
          dplyr::rename(NAME_0 = NAME_0.x,
                        NAME_1 = NAME_1.x,
                        NAME_2 = NAME_2.x,) %>%
          replace(is.na(.), 0)
        st_write(userarea_simpx_fr2, "OutBaU/webmofuss_results/mofuss_adm2_fr.gpkg", delete_layer = TRUE)
        print(paste0(admname," finished for vector layers"))
        
      }
      
    } else {
      print("error with simulation length")  
    }
    
    
  } # foreach(admm = adminlevel, admname = admin_name) %do% {
  
} # if (fNRB_partition_tables == 1) {

# END ----

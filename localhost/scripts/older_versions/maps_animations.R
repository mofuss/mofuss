# MoFuSS
# Version 2
# Date: Mar 2023

rm(list=ls(all=TRUE))

# Load packages ####
library(readr)
library(XML)
library(rgrass7)
library(dplyr)
library(animation)
library(bitops)
library(caTools)
library(colorspace)
library(data.table)
library(fasterize)
library(fBasics)
library(fields)
library(ggplot2)
library(glue)
library(gridExtra)
library(gstat)
library(htmltools)
library(htmlwidgets)
library(httpuv)
library(igraph)
library(jpeg)
library(knitr)
library(lattice)
library(latticeExtra)
library(maptools)
library(msm)
library(plyr)
library(png)
library(raster)
library(rasterVis)
library(reader)
library(rgdal)
library(rgeos)
library(rgl)
library(rmarkdown)
library(sf)
library(snow)
library(sp)
library(readr)
library(tiff)
library(tictoc)
library(furrr)
library(stars)
library(RCurl)
library(gitlabr)
library(mapview)
library(Rcpp)
library(RcppProgress)
library(rbenchmark)
library(inline)
library(timeDate)
library(tidyverse)
library(spam)
library(svDialogs)

# Read in the arguments listed at the command line in Dinamica EGO'S "Run external process" ####
args=(commandArgs(TRUE))

# "args" is now a list of character vectors.
# First check to see if arguments are passed.
# Then cycle through each element of the list and evaluate the expressions.
if(length(args)==0){
	print("No arguments supplied by DINAMICA.")
	##Supply default values here (to be used when running the script through R directly)
	MC = 10 # MonteCarlo runs
	IT = 2010 # Initial year
	K_MC=1
	TOF_MC=1
	Ini_st_MC=75
	Ini_st.factor.percentage=100
	COVER_MAP=1
	rmax_MC=1
	DEF_FW=1
	IL=48 # Iteration length in week - each year = 48 weeks
	STdyn=40 # Simulation length set by dinamica, but cycles in the repeat functor is STdyn+1 as 2 cycles are needed for 1 year: 1jan->31dec
	Harv.Pix.W=25400
	Prune.W=1 
	Harv.Pix.V=25400
	Prune.V=1
	Harv.Pix_MC=0
	Prune_MC=0
	# Subset_locs=0
	MaxAGB=400 # Maximum K for all LULC classes at any MC
	MaxAGB_firstMC=400 # Maximum K for all LULC classes at first MC run
	MaxAGB_lastMC=193 # Maximum K for all LULC classes at last MC run
	AGBmap=1
	SumTables=1
	OSType=64
	BaUvsICS="BaU"
	RerunMC=1
	
}else{
	for(i in 1:length(args)){
		eval(parse(text=args[[i]]))
	}
}



# Set master dir ####
#master_dir <- "/media/cdobler/extra_storage/google_drive_ciga/shared/000_MoFuSSCountryDatasets/MoFuSS_Peru_linux/LULCC"
# CurrentDir<- getwd()
# setwd(paste0(getwd(),"/LULCC"))
# master_dir <- getwd()

Country<-readLines("LULCC/TempTables/Country.txt")

# Read parameters table, checking if its delimiter is comma or semicolon ####
read_csv(glue("LULCC/SourceData/parameters.csv")) %>% 
  {if(is.null(.$ParCHR[1])) read_csv2(glue("LULCC/SourceData/parameters.csv")) else .} -> country_parameters

# Append "+"
country_parameters$ParCHR[1] <- str_c("+", country_parameters$ParCHR[1])
country_parameters$ParCHR[2] <- str_c("+", country_parameters$ParCHR[2])
# print(tbl_df(country_parameters), n=100) ####

# Read supply parameters table, checking if its delimiter is comma or semicolon ####
read_csv("LULCC/TempTables/growth_parameters1.csv") %>% 
  {if(is.null(.$TOF[1])) read_csv2(glue("LULCC/TempTables/growth_parameters1.csv")) else .} -> tof_vs_for # UPDATE


CtyPar_semicolon<-read.csv("LULCC/SourceData/parameters.csv", sep=";", header=T)
CtyPar_comma<-read.csv("LULCC/SourceData/parameters.csv",  sep=",", header=T)
if (is.null(CtyPar_semicolon$ParCHR[1])) { ## Read in the arguments listed at the command line in DINAMICA'S "Run external process"
	CtyPar<-CtyPar_comma
} else {
	CtyPar<-CtyPar_semicolon
}
# CtyPar<-read.csv("LULCC/SourceData/parameters.csv", header=T)
CtyPar[] <- lapply(CtyPar, as.character)

#userarea_GCS<-st_read("TempVector_GCS/userarea_GCS.shp")
#userarea<-st_read("TempVector/userarea.shp")
res<-read.csv("LULCC/TempTables/Resolution.csv", header=T)
resolution<-res[1,2]
userarea_r<-raster("LULCC/TempRaster/mask_c.tif")

# userarea_GCS<-readShapePoly("TempVector/userarea_GCS",proj4string=CRS(GCSproj))
# userarea<-readShapePoly("TempVector/userarea",proj4string=CRS(UTMproj))
# ext<-extent(userarea)
# DEM_c1<-raster("TempRaster/DEM_c1.tif")
# ProjExtent<-projectExtent(DEM_c1, UTMproj)
# res<-read.csv("TempTables/Resolution.csv", header=T)
# resolution<-res[1,2]
# analysis_r<-raster("TempRaster/mask_c.tif")

# setwd(CurrentDir)



# Globe_name<-paste0("LULCC/Countries/",Country,".tif") REPLACE!!!

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


# if (Subset_locs == 1) {
# 	Extent_Locs_r<-raster("LULCC//TempRaster//Ext_Locs_c.tif")
# 	Extent_Locs_p<-as.data.frame(rasterToPoints(Extent_Locs_r, fun=NULL, n=4, na.rm=TRUE, digits=12, dissolve=TRUE,proj4string=CRS(projUTM16N_WSG84)))
# 	coordinates(Extent_Locs_p)=c("x", "y")
# 	proj4string(Extent_Locs_p) <- UTMproj
# 	Extent_Locs_p_GCS<-spTransform(Extent_Locs_p,CRS(GCSproj))
# 	locs_figures_GCS<-Extent_Locs_p_GCS
# } else {
	Locs_r<-raster("LULCC//TempRaster//locs_c_w.tif") #Check there is locs_c_v as well!
	Locs_p<-as.data.frame(rasterToPoints(Locs_r, fun=NULL, n=4, na.rm=TRUE, digits=12, dissolve=TRUE))
	coordinates(Locs_p)=c("x", "y")
	proj4string(Locs_p) <- "+proj=utm +zone=18 +datum=WGS84 +units=m +no_defs" #UTMproj
	#Locs_p_GCS<-spTransform(Locs_p,CRS(GCSproj))
	#locs_figures_GCS<-Locs_p_GCS
#}

tiff(filename=paste0(OutDir,"//Area_of_Interest.tif"),
     width=290,height=290,units="mm",res=res300,bg="white",
     compression=c("lzw"),type=c("windows"),
     pointsize=12,family="",restoreConsole=TRUE)
plot(aoi_c, main="Area of Interest: set by user (red polygon)",
     ylab="UTM S-N coords -try depicting GCS coords over UTM raster?",
     xlab="UTM W-E coords -try depicting GCS coords over UTM raster?",
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
plot(locs_figures, pch=19, cex=(0.50), add=TRUE)
scalebar(scalebar_loi,type='line', divs=4, lwd=2.5, label=label_scalebar_loi, cex=1.5)
plot(extent_analysis, border="red", col="transparent", add=TRUE, lwd = 0.25)

#par(new=TRUE, plt=c(0,1,0,1), mar=c(42,4.2,4.2,42), usr=c(0,1,0,1))
#greypallete<-gray.colors(255, start = 0.15, end = 1, gamma = 2, alpha = 1)
#image((raster(HondurasGlobe_name)), col=greypallete, axes=FALSE, ann=FALSE)
dev.off()


# Read files according to simulation lenght (ST) ####

# Files paths and names
bal="Debugging//Growth_less_harv"
harv_tot="Debugging//Harvest_tot"

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
bal_names<-paste(s0,c2,s2,sep="")
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
Last_STdyn<-max(x_c2)


# Map AGB ####

graphics.off()
tiff(filename=paste(OutDir,"//Map_AGB.tif",sep=""),width=170,height=200,units="mm",res=res600,bg="white",compression=c("lzw"),type=c("windows"),pointsize=12,family="",restoreConsole=TRUE)

par(mfrow = c(3, 2), oma=c(1.5,1.5,0,1), mar=c(3,3,4,1))

#########################
mainsize<-1.1
legwidth<-2.5
axissize<-1
labelsize<-0.75
barline<-(-1.1)
redline_wd<-0.5
#########################

bal_t0_maxAGB<-cellStats((raster("Temp//2_IniSt01.tif")),max) # Initial AGB for 1st MC run
bal_tn_maxAGB<-cellStats((raster("Temp//2_AGBt101.tif")),max) # Final AGB for 1st MC run
MaxAGB_1stMC_bind<-cbind(bal_t0_maxAGB,bal_tn_maxAGB)
MaxAGB_1stMC<-((max(MaxAGB_1stMC_bind[1, ], na.rm=TRUE))/Areaadj)

bal_t0<-(raster("Temp//2_IniSt01.tif")) # Initial AGB for 1st MC run
plot((bal_t0/Areaadj), main=paste0("Aboveground Biomass ",IT),cex.main=mainsize, useRaster=TRUE,
	legend=TRUE, legend.width=legwidth, legend.shrink=1,cex.axis=axissize,
legend.args=list(text=expression("t ha"^-1*""),side=4, font=2, line=barline, cex=labelsize),zlim=c(0,MaxAGB_1stMC))
plot(extent_analysis, border="red", col="transparent", add=TRUE, lwd = redline_wd)

bal_tn<-(raster("Temp//2_AGBt101.tif")) # Final AGB for 1st MC run
plot((bal_tn/Areaadj), main=paste0("Aboveground Biomass ",(IT+as.numeric(Last_STdyn))),cex.main=mainsize, useRaster=TRUE,
	legend=TRUE, legend.width=legwidth, legend.shrink=1,cex.axis=axissize,
	legend.args=list(text=expression("t ha"^-1*""),
	side=4, font=2, line=barline, cex=labelsize),zlim=c(0,MaxAGB_1stMC),
	scalebar(scalebar_loi,type='line', divs=4, lwd=1.25, label=label_scalebar_loi, cex=1)
)
plot(extent_analysis, border="red", col="transparent", add=TRUE, lwd = redline_wd)

NRB<-(raster("Temp//2_NRB01.tif")) # Cumulative NRB for simulation period for 1st MC
NRBmax<-(cellStats(NRB,max)/Areaadj)
if(NRBmax == 0){
	plot((NRB/Areaadj), main=paste0("NRB: period ",IT," to ",(IT+as.numeric(Last_STdyn))),cex.main=mainsize, useRaster=TRUE,
		legend=TRUE, legend.width=legwidth, legend.shrink=1,cex.axis=axissize,
		legend.args=list(text=expression("t ha"^-1*""),
		side=4, font=2, line=barline, cex=labelsize),zlim=c(0,1))
	plot(extent_analysis, border="red", col="transparent", add=TRUE, lwd = redline_wd)
} else {
	plot((NRB/Areaadj), main=paste0("NRB: period ",IT," to ",(IT+as.numeric(Last_STdyn))),cex.main=mainsize, useRaster=TRUE,
		legend=TRUE, legend.width=legwidth, legend.shrink=1,cex.axis=axissize,
		legend.args=list(text=expression("t ha"^-1*""),
		side=4, font=2, line=barline, cex=labelsize),zlim=c(0,NRBmax))
	plot(extent_analysis, border="red", col="transparent", add=TRUE, lwd = redline_wd)
}

fNRB<-(raster("Temp//2_fNRB01.tif"))*100 # fNRB for the entire simulation period for 1st MC run
plot(fNRB, main=paste0("fNRB: period ",IT," to ",(IT+as.numeric(Last_STdyn))),cex.main=mainsize,  useRaster=TRUE,
	legend=TRUE, legend.width=legwidth, legend.shrink=1,cex.axis=axissize,
	legend.args=list(text=expression("%"),
	side=4, font=2, line=barline, cex=labelsize),zlim=c(0,100))
plot(extent_analysis, border="red", col="transparent", add=TRUE, lwd = redline_wd)

Fw_def_tot<-(raster("Temp//2_FW_DEF01.tif")) # Cumulative fuelwod from deforestation for simulation period for 1st MC
Fw_defmax<-(cellStats(Fw_def_tot,max)/Areaadj)
if(Fw_defmax == 0){	
	plot((Fw_def_tot/Areaadj), main=paste0("Fuelwood from deforestation: period ",IT," to ",(IT+as.numeric(Last_STdyn))),cex.main=mainsize, useRaster=TRUE,
		legend=TRUE, legend.width=legwidth, legend.shrink=1,cex.axis=axissize,
		legend.args=list(text=expression("t ha"^-1*""),
		side=4, font=2, line=barline, cex=labelsize),zlim=c(0,1))
	plot(extent_analysis, border="red", col="transparent", add=TRUE, lwd = redline_wd)
} else {
	plot((Fw_def_tot/Areaadj), main=paste0("Fuelwood from deforestation: period ",IT," to ",(IT+as.numeric(Last_STdyn))),cex.main=mainsize, useRaster=TRUE,
		legend=TRUE, legend.width=legwidth, legend.shrink=1,cex.axis=axissize,
		legend.args=list(text=expression("t ha"^-1*""),
		side=4, font=2, line=barline, cex=labelsize),zlim=c(0,Fw_defmax))
	plot(extent_analysis, border="red", col="transparent", add=TRUE, lwd = redline_wd)
}

Harv_tot<-(raster("Temp//2_CON_TOT01.tif")) # Cumulative fuelwood harvest for simulation period for 1st MC
Harv_totmax<-(cellStats(Harv_tot,max)/Areaadj)
if(Harv_totmax == 0){
	plot((Harv_tot/Areaadj), main=paste0("Harvested fuelwood: period ",IT," to ",(IT+as.numeric(Last_STdyn))),cex.main=mainsize, useRaster=TRUE,
		legend=TRUE, legend.width=legwidth, legend.shrink=1,cex.axis=axissize,
		legend.args=list(text=expression("t ha"^-1*""),
		side=4, font=2, line=barline, cex=labelsize),zlim=c(0,1))
	# plot(locs_figures, pch=19, cex=(0.10), add=TRUE)
	plot(extent_analysis, border="red", col="transparent", add=TRUE, lwd = redline_wd)
} else {
	plot((Harv_tot/Areaadj), main=paste0("Harvested fuelwood: period ",IT," to ",(IT+as.numeric(Last_STdyn))),cex.main=mainsize, useRaster=TRUE,
		legend=TRUE, legend.width=legwidth, legend.shrink=1,cex.axis=axissize,
		legend.args=list(text=expression("t ha"^-1*""),
		side=4, font=2, line=barline, cex=labelsize),zlim=c(0,Harv_totmax))
	# plot(locs_figures, pch=19, cex=(0.10), add=TRUE)
	plot(extent_analysis, border="red", col="transparent", add=TRUE, lwd = redline_wd)
}

title(ylab="UTM S-N coords", xlab="UTM W-E coords\n",sub="Showing first Monte Carlo run",outer=TRUE, line=0.35)
dev.off()


# HTML video - corresponds to last MC run ####

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
			sub="Showing last Monte Carlo run", xlab="UTM W-E coords", ylab="UTM S-N coords",cex.main=2,
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
# 		title(xlab="UTM W-E coords\nShowing last Monte Carlo run",ylab="UTM S-N coords", sub="Showing last Monte Carlo run", outer=TRUE, line=-0.5)
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

#########################
###SPATIAL STATISTICS
###Standard Deviation and Variance in NRB
#########################

listNRB<-list.files("Temp", pattern = "^2_NRB.+[.]tif$",ignore.case=F)
StackNRB <-stack(paste("Temp/",listNRB,sep=""))
nlay<-nlayers(StackNRB)
NRBmean<- stackApply(StackNRB, indices=1, fun=mean)
NRBvar <- stackApply(StackNRB, indices=1, fun=var)
NRBds <- sqrt(NRBvar)
writeRaster(NRBmean, filename="Temp//aNRBmean.tif", datatype="FLT4S", overwrite=TRUE)
writeRaster(NRBvar, filename="Temp//aNRBvar.tif", datatype="FLT4S", overwrite=TRUE)
writeRaster(NRBds, filename="Temp//aNRBds.tif", datatype="FLT4S", overwrite=TRUE)

NRBdsmax<-(cellStats(NRBds,max)/Areaadj)

tiff(filename=paste(OutDir,"//NRBds.tif",sep=""),width=290,height=290,units="mm",res=res300,bg="white",compression=c("lzw"),type=c("windows"),pointsize=12,family="",restoreConsole=TRUE)
plot((NRBds/Areaadj), main=paste("Standard Deviation in NRB \n after ",MC," Monte Carlo runs for ",STdyn,"-year simulations",sep=""),ylab="UTM S-N coords",xlab="UTM W-E coords",cex.main=1.5,
	legend=TRUE, legend.width=2.5, 
	legend.args=list(text=expression("t ha"^-1*""),side=4, font=2, line=-1.35, cex=1.35),zlim=c(0,NRBdsmax))
#plot(locs_figures, pch=19, cex=(0.50), add=TRUE)
scalebar(scalebar_loi,type='line', divs=4, lwd=2.5, label=label_scalebar_loi, cex=1.5)
plot(extent_analysis, border="red", col="transparent", add=TRUE, lwd = 1.75)
dev.off()

listCON_NRB<-list.files("Temp", pattern = "^2_CON_NRB.+[.]tif$",ignore.case=F)
StackCON_NRB <- stack(paste("Temp/",listCON_NRB,sep=""))
nlayCON_NRB<-nlayers(StackCON_NRB)
CON_NRBmean<- stackApply(StackCON_NRB, indices=1, fun=mean)
CON_NRBvar <- stackApply(StackCON_NRB, indices=1, fun=var)
CON_NRBds <- sqrt(CON_NRBvar)
writeRaster(CON_NRBmean, filename="Temp//aCON_NRBmean.tif", datatype="FLT4S", overwrite=TRUE)
writeRaster(CON_NRBvar, filename="Temp//aCON_NRBvar.tif", datatype="FLT4S", overwrite=TRUE)
writeRaster(CON_NRBds, filename="Temp//aCON_NRBds.tif", datatype="FLT4S", overwrite=TRUE)

CON_NRBdsmax<-(cellStats(CON_NRBds,max)/Areaadj)

tiff(filename=paste(OutDir,"//CON_NRBds.tif",sep=""),width=290,height=290,units="mm",res=res300,bg="white",compression=c("lzw"),type=c("windows"),pointsize=12,family="",restoreConsole=TRUE)
plot((CON_NRBds/Areaadj), main=paste("Standard Deviation in fuelwood use \n after ",MC," Monte Carlo runs for ",STdyn,"-year simulations",sep=""),ylab="UTM S-N coords",xlab="UTM W-E coords",cex.main=1.5,
	legend=TRUE, legend.width=2.5, 
	legend.args=list(text=expression("t ha"^-1*""),side=4, font=2, line=-1.35, cex=1.35),zlim=c(0,CON_NRBdsmax))
#plot(locs_figures, pch=19, cex=(0.50), add=TRUE)
scalebar(scalebar_loi,type='line', divs=4, lwd=2.5, label=label_scalebar_loi, cex=1.5)
plot(extent_analysis, border="red", col="transparent", add=TRUE, lwd = 1.75)
dev.off()

listCON_TOT<-list.files("Temp", pattern = "^2_CON_TOT.+[.]tif$",ignore.case=F)
StackCON_TOT <- stack(paste("Temp/",listCON_TOT,sep=""))
nlayCON_TOT<-nlayers(StackCON_TOT)
CON_TOTmean<- stackApply(StackCON_TOT, indices=1, fun=mean)
CON_TOTvar <- stackApply(StackCON_TOT, indices=1, fun=var)
CON_TOTds <- sqrt(CON_TOTvar)
writeRaster(CON_TOTmean, filename="Temp//aCON_TOTmean.tif", datatype="FLT4S", overwrite=TRUE)
writeRaster(CON_TOTvar, filename="Temp//aCON_TOTvar.tif", datatype="FLT4S", overwrite=TRUE)
writeRaster(CON_TOTds, filename="Temp//aCON_TOTds.tif", datatype="FLT4S", overwrite=TRUE)

CON_TOTdsmax<-(cellStats(CON_TOTds,max)/Areaadj)

tiff(filename=paste(OutDir,"//CON_TOTds.tif",sep=""),width=290,height=290,units="mm",res=res300,bg="white",compression=c("lzw"),type=c("windows"),pointsize=12,family="",restoreConsole=TRUE)
plot((CON_TOTds/Areaadj), main=paste("Standard Deviation in fuelwood use driving degradation \n after ",MC," Monte Carlo runs for ",STdyn,"-year simulations",sep=""),ylab="UTM S-N coords",xlab="UTM W-E coords",cex.main=1.5,
	legend=TRUE, legend.width=2.5, 
	legend.args=list(text=expression("t ha"^-1*""),side=4, font=2, line=-1.35, cex=1.35),zlim=c(0,CON_TOTdsmax))
#plot(locs_figures, pch=19, cex=(0.50), add=TRUE)
scalebar(scalebar_loi,type='line', divs=4, lwd=2.5, label=label_scalebar_loi, cex=1.5)
plot(extent_analysis, border="red", col="transparent", add=TRUE, lwd = 1.75)
dev.off()


# SUMMARY TABLES ####


if (SumTables == 1) {

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
  
  userarea_DF<-as.data.frame(st_read("LULCC/TempVector/ext_analysis.gpkg"))
  
	admin<-raster("LULCC/TempRaster//admin_c.tif")
	StackNRB[StackNRB == 0] = NA #This is to sum and average only positive NRB values
	NRBzon_sum<-as.data.frame(zonal(StackNRB, admin, 'sum'))
	#zonal(StackNRB, admin, 'mean')
	#zonal(StackNRB, admin, 'sd')
	
	NRBzon_sum<-as.data.table(NRBzon_sum)
	NRBzon_mean<-NRBzon_sum[, list(NRB_MC_mean = rowMeans(.SD)), by = zone]
	NRBzon_sd<-NRBzon_sum[, list(NRB_MC_sd = rowSds(.SD)), by = zone]

	NRBzon_sum_m1 <- merge(userarea_DF, NRBzon_mean, by.x = ext_analysis_ID, by.y = "zone")
	NRBzon_sum_m2 <- merge(NRBzon_sum_m1, NRBzon_sd, by.x = ext_analysis_ID, by.y = "zone")
	
	StackCON_TOT[StackCON_TOT == 0] = NA #This is to sum and average only positive NRB values
	CON_TOTzon_sum<-as.data.frame(zonal(StackCON_TOT, admin, 'sum'))
	#zonal(StackCON_TOT, admin, 'mean')
	#zonal(StackCON_TOT, admin, 'sd')

	CON_TOTzon_sum<-as.data.table(CON_TOTzon_sum)
	CON_TOTzon_mean<-CON_TOTzon_sum[, list(CON_TOT_MC_mean = rowMeans(.SD)), by = zone]
	CON_TOTzon_sd<-CON_TOTzon_sum[, list(CON_TOT_MC_sd= rowSds(.SD)), by = zone]

	CON_TOTzon_sum_m1 <- merge(userarea_DF, CON_TOTzon_mean, by.x = ext_analysis_ID, by.y = "zone")
	CON_TOTzon_sum_m2 <- merge(CON_TOTzon_sum_m1, CON_TOTzon_sd, by.x = ext_analysis_ID, by.y = "zone")

	fNRB_merge<- merge(NRBzon_sum_m2, CON_TOTzon_sum_m2, by.x = ext_analysis_ID, by.y = ext_analysis_ID)

	StackCON_NRB[StackCON_NRB == 0] = NA #This is to sum and average only positive NRB values
	CON_NRBzon_sum<-as.data.frame(zonal(StackCON_NRB, admin, 'sum'))

	CON_NRBzon_sum<-as.data.table(CON_NRBzon_sum)
	CON_NRBzon_mean<-CON_NRBzon_sum[, list(CON_NRB_MC_mean = rowMeans(.SD)), by = zone]
	CON_NRBzon_sd<-CON_NRBzon_sum[, list(CON_NRB_MC_sd= rowSds(.SD)), by = zone]

	CON_NRBzon_sum_m1 <- merge(userarea_DF, CON_NRBzon_mean, by.x = ext_analysis_ID, by.y = "zone")
	CON_NRBzon_sum_m2 <- merge(CON_NRBzon_sum_m1, CON_NRBzon_sd, by.x = ext_analysis_ID, by.y = "zone")

	fNRB_nrb_merge<- merge(fNRB_merge, CON_NRBzon_sum_m2, by.x = ext_analysis_ID, by.y = ext_analysis_ID)

	fNRB_nrb_sub<-fNRB_nrb_merge[,c(paste(ext_analysis_NAME,".x",sep=""),"NRB_MC_mean","NRB_MC_sd","CON_TOT_MC_mean","CON_TOT_MC_sd","CON_NRB_MC_mean","CON_NRB_MC_sd")]

	NRB_fNRB3<-transform(fNRB_nrb_sub, fNRB = NRB_MC_mean / CON_TOT_MC_mean * 100)
	NRB_fNRB2<-transform(NRB_fNRB3, fNRB_sd = (sqrt(((NRB_MC_sd/NRB_MC_mean)^2) + ((CON_TOT_MC_sd/CON_TOT_MC_mean)^2))*100))
	NRB_fNRB1<-transform(NRB_fNRB2, fNRB_nrb = NRB_MC_mean / CON_NRB_MC_mean * 100)
	NRB_fNRB<-transform(NRB_fNRB1, fNRB_nrb_sd = (sqrt(((NRB_MC_sd/NRB_MC_mean)^2) + ((CON_NRB_MC_sd/CON_NRB_MC_mean)^2))*100))
	is.num <- sapply(NRB_fNRB, is.numeric)
	NRB_fNRB[is.num] <- lapply(NRB_fNRB[is.num], round, 0)
	NRB_fNRB2<-NRB_fNRB[!duplicated(NRB_fNRB), ]
	write.csv(NRB_fNRB2, "LULCC/TempTables/summary_adm0.csv", row.names=FALSE, quote=FALSE)
	
	# Chunk code to produce additonal tables using higher level admin units IF THEY EXIST
	
	# Save as: fNRBBaU, fNRBTable, NRBBaU, NRBTable
	if(file.exists("LULCC/TempVector/ext_analysis1.gpkg")){
	  userarea_DF1<-as.data.frame(st_read("LULCC/TempVector/ext_analysis1.gpkg"))
	  
	  admin1 <- raster("LULCC/TempRaster//admin_c1.tif")
	  StackNRB[StackNRB == 0] = NA #This is to sum and average only positive NRB values
	  NRBzon_sum1 <- as.data.frame(zonal(StackNRB, admin1, 'sum'))
	  #zonal(StackNRB, admin, 'mean')
	  #zonal(StackNRB, admin, 'sd')
	  
	  NRBzon_sum1<-as.data.table(NRBzon_sum1)
	  NRBzon_mean1<-NRBzon_sum1[, list(NRB_MC_mean = rowMeans(.SD)), by = zone]
	  NRBzon_sd1<-NRBzon_sum1[, list(NRB_MC_sd = rowSds(.SD)), by = zone]

	  NRBzon_sum_m11 <- merge(userarea_DF1, NRBzon_mean1, by.x = ext_analysis_ID_1, by.y = "zone")
	  NRBzon_sum_m21 <- merge(NRBzon_sum_m11, NRBzon_sd1, by.x = ext_analysis_ID_1, by.y = "zone")

	  StackCON_TOT[StackCON_TOT == 0] = NA #This is to sum and average only positive NRB values
	  CON_TOTzon_sum1<-as.data.frame(zonal(StackCON_TOT, admin1, 'sum'))
	  #zonal(StackCON_TOT, admin, 'mean')
	  #zonal(StackCON_TOT, admin, 'sd')

	  CON_TOTzon_sum1<-as.data.table(CON_TOTzon_sum1)
	  CON_TOTzon_mean1<-CON_TOTzon_sum1[, list(CON_TOT_MC_mean = rowMeans(.SD)), by = zone]
	  CON_TOTzon_sd1<-CON_TOTzon_sum1[, list(CON_TOT_MC_sd= rowSds(.SD)), by = zone]

	  CON_TOTzon_sum_m11 <- merge(userarea_DF1, CON_TOTzon_mean1, by.x = ext_analysis_ID_1, by.y = "zone")
	  CON_TOTzon_sum_m21 <- merge(CON_TOTzon_sum_m11, CON_TOTzon_sd1, by.x = ext_analysis_ID_1, by.y = "zone")

	  fNRB_merge1 <- merge(NRBzon_sum_m21, CON_TOTzon_sum_m21, by.x = ext_analysis_ID_1, by.y = ext_analysis_ID_1)

	  StackCON_NRB[StackCON_NRB == 0] = NA #This is to sum and average only positive NRB values
	  CON_NRBzon_sum1<-as.data.frame(zonal(StackCON_NRB, admin1, 'sum'))

	  CON_NRBzon_sum1<-as.data.table(CON_NRBzon_sum1)
	  CON_NRBzon_mean1<-CON_NRBzon_sum1[, list(CON_NRB_MC_mean = rowMeans(.SD)), by = zone]
	  CON_NRBzon_sd1<-CON_NRBzon_sum1[, list(CON_NRB_MC_sd= rowSds(.SD)), by = zone]

	  CON_NRBzon_sum_m11 <- merge(userarea_DF1, CON_NRBzon_mean1, by.x = ext_analysis_ID_1, by.y = "zone")
	  CON_NRBzon_sum_m21 <- merge(CON_NRBzon_sum_m11, CON_NRBzon_sd1, by.x = ext_analysis_ID_1, by.y = "zone")

	  fNRB_nrb_merge1<- merge(fNRB_merge1, CON_NRBzon_sum_m21, by.x = ext_analysis_ID_1, by.y = ext_analysis_ID)

	  fNRB_nrb_sub1<-fNRB_nrb_merge1[,c(paste(ext_analysis_NAME_1,".x",sep=""),"NRB_MC_mean","NRB_MC_sd","CON_TOT_MC_mean","CON_TOT_MC_sd","CON_NRB_MC_mean","CON_NRB_MC_sd")]

	  NRB_fNRB31<-transform(fNRB_nrb_sub1, fNRB = NRB_MC_mean / CON_TOT_MC_mean * 100)
	  NRB_fNRB21<-transform(NRB_fNRB31, fNRB_sd = (sqrt(((NRB_MC_sd/NRB_MC_mean)^2) + ((CON_TOT_MC_sd/CON_TOT_MC_mean)^2))*100))
	  NRB_fNRB11<-transform(NRB_fNRB21, fNRB_nrb = NRB_MC_mean / CON_NRB_MC_mean * 100)
	  NRB_fNRB1<-transform(NRB_fNRB11, fNRB_nrb_sd = (sqrt(((NRB_MC_sd/NRB_MC_mean)^2) + ((CON_NRB_MC_sd/CON_NRB_MC_mean)^2))*100))
	  is.num <- sapply(NRB_fNRB1, is.numeric)
	  NRB_fNRB1[is.num] <- lapply(NRB_fNRB1[is.num], round, 0)
	  NRB_fNRB21<-NRB_fNRB1[!duplicated(NRB_fNRB1), ]
	  write.csv(NRB_fNRB21, "LULCC/TempTables/summary_adm1.csv", row.names=FALSE, quote=FALSE)
	  
	}
	
	if(file.exists("LULCC/TempVector/ext_analysis2.gpkg")){
	  userarea_DF2<-as.data.frame(st_read("LULCC/TempVector/ext_analysis2.gpkg"))
	  
	  ### FALTA COMPLETAR ESTA PARTE
	  
	  
	  # write.csv(NRB_fNRB22, "LULCC/TempTables/summary_adm2.csv", row.names=FALSE, quote=FALSE)
	  
	}
	

} else {
	SumTables_yesno = "No"
}

# SAVES SUMMARY TABLES OF RESULTS IN CSV FOR BAU AND ICS SCENARIOS ####

writeLines(paste(MC," Monte Carlo runs",sep=""), "LULCC/TempTables/MCruns.txt", useBytes=T)
file.copy("LULCC/TempTables/MCruns.txt", "LaTeX/MCruns.txt", overwrite=TRUE)
writeLines(paste(STdyn," years",sep=""), "LULCC/TempTables/SimLength.txt", useBytes=T)
file.copy("LULCC/TempTables/SimLength.txt", "LaTeX/SimLength.txt", overwrite=TRUE)	

cols1 <- c(1:3)
NRBBaUICS<-NRB_fNRB2[,cols1]

cols2 <- c(1, 8:9)
fNRBBaUICS<-NRB_fNRB2[,cols2]

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
	  


#########################
###CONVERSION OF TIFF IMAGES INTO PNG AND JPG FOR SUMMARY REPORT
#########################

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


# # COMPILE LATEX FILE INTO PDF SUMMARY REPORT ####

#Three key files that must be present for full pdf report
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



# END ####

#########################
###Static maps 3D (not in use yet)
#########################

#name_first <-paste("Debugging//Growth_less_harv",First_frame,".tif",sep="")
#name_last <-paste("Debugging//Growth_less_harv",Last_frame,".tif",sep="")
#Bal_first <-raster(name_first)
#Bal_last <-raster(name_last)
#filename(Bal_first)
#filename(Bal_last)
#hasValues(Bal_first)
#hasValues(Bal_last)
#plot3D(Bal_first, adjust=FALSE, zfac=50, rev=TRUE)
#plot3D(Bal_last, adjust=FALSE, zfac=50, rev=TRUE)
#rgl.quit()


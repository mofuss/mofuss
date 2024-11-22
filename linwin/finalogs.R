# Author: A Ghilardi
# Version: 1.1
# Date: 2016

rm(list=ls(all=TRUE))

library(raster) 
library(rasterVis) 
library(rgdal) 
library(rgl) 
library(rgeos) 
library(maptools) 
library(animation) 
library(sp) 
library(igraph) 
library(plyr) 
library(msm) 
library(png) 
library(fields) 
library(tiff)
library(jpeg)
library(png)
library(fBasics)
library(data.table)
library(gridExtra)
library(knitr)
library(ggplot2)
library(lattice)
library(htmltools)
library(caTools)
library(bitops)
library(rmarkdown)
library(knitr)
library(tools)


# Read in the arguments listed at the command line in Dinamica EGO'S "Run external process"
args=(commandArgs(TRUE))

# "args" is now a list of character vectors.
# First check to see if arguments are passed.
# Then cycle through each element of the list and evaluate the expressions.
if(length(args)==0){
	print("No arguments supplied by DINAMICA.")
	##Supply default values here (to be used when running the script through R directly)
	BaUvsICS="BaU"
	
}else{
	for(i in 1:length(args)){
		eval(parse(text=args[[i]]))
	}
}


if (BaUvsICS == "ICS") {
	OutDir<-"OutICS"
} else {
	OutDir<-"OutBaU"
}


#########################
###Copy logs to debug folder
#########################

outlist<-list(
"LULCC/000_Install_R_Packages.Rout",
"LULCC/000_Packages_autocheck.Rout",
"LULCC/00_Cropping.Rout",
"LULCC/0_Demand.Rout",
"LULCC/debug.txt",
"LULCC/log.txt",
"0_Ext_Locs_processors.Rout",
"2_IDW_boost_0.Rout",
"2_IDW_boost_1.Rout",
"2_IDW_boost_2.Rout",
"3_bypassMC.Rout",
"3_NRB_graphs_datasets.Rout",
"3_rnorm.Rout",
"4_bypass_maps_animations.Rout",
"4_maps_animations.Rout",
"Clean_Temps.Rout")

copy<-function(x) {
	file.copy(x, "Logs", recursive=TRUE)
}
lapply(outlist,copy)

Sys.sleep(1)
Routlist<-as.list(list.files("Logs",".Rout"))
timetabs<-as.list(gsub(".Rout", paste(OutDir,"_PT.csv",sep=""),(list.files("Logs",".Rout"))))

time<-function(x,y) {
	MapsProc<-readChar(paste("Logs/",x,sep=""), 
		file.info(paste("Logs/",x,sep=""))$size)
	if (any(grepl("proc.time()", MapsProc))== TRUE) {	
		MapsProc.t<-read.table(text = MapsProc,
			skip = grep("proc.time()",
			readLines(textConnection(MapsProc)))) 
		write.table(MapsProc.t, paste("Logs/",y,sep=""),col.names=FALSE)
	}
}
mapply(time,Routlist,timetabs)


#########################
###Zip logs withing "Logs" folder
#########################

BaUexist<-file.exists("OutBaU/jpg/Map_AGB.jpg")
ICSexist<-file.exists("OutICS/jpg/Map_AGB.jpg")
BaUexist_b<-file.exists("OutBaU/png/Boxplots.png")
ICSexist_b<-file.exists("OutICS/png/Boxplots.png")

if (BaUexist == TRUE & ICSexist == TRUE & BaUexist_b == TRUE & ICSexist_b == TRUE) {
	zip("Logs/all_logs.zip", "Logs", zip="LULCC/Wizard_imgs/zip.exe")
}


###############################
#########END OF SCRIPT#########
###############################
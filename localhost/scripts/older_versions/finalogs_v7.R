# Author: A Ghilardi
# Version: 1.1
# Date: 2016
# EGOML dependency bundle: V7

rm(list=ls(all=TRUE))

# This script only performs base-R file and timing-log operations. The original
# package list included retired rgdal/rgeos dependencies but used none of them.


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
	OutDir<-"Out"
} else {
	OutDir<-"Out"
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
"bypassMC_v7.Rout",
"NRB_graphs_datasets_v7.Rout",
"rnorm_v7.Rout",
"bypass_maps_animations_v7.Rout",
"maps_animations_v7.Rout",
"Clean_Temps.Rout")

copy<-function(x) {
	file.copy(x, "Logs", recursive=TRUE)
}
lapply(outlist,copy)

Sys.sleep(1)
Routlist<-as.list(list.files("Logs",".Rout"))
timetabs<-as.list(sub("\\.Rout$", "_PT.csv", list.files("Logs", "\\.Rout$")))

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

BaUICSexist<-file.exists("Out/jpg/Map_AGB.jpg")
BaUICSexist_b<-file.exists("Out/png/Boxplots.png")

if (BaUICSexist == TRUE & BaUICSexist_b == TRUE) {
	zip("Logs/all_logs.zip", "Logs", zip="LULCC/Wizard_imgs/zip.exe")
}


###############################
#########END OF SCRIPT#########
###############################

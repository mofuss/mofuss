# MoFuSS134545
# Linux version
# Date: Jan 2020

rm(list=ls(all=TRUE))

# INPUT PARAMETERS When managing gitlab scripts, update working directory from R Studio to the country modeling folder ####
# isRStudio <- Sys.getenv("RSTUDIO") == "1"
# if (isRStudio==1) {
#   setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# }else{
#   "Do Nothing"
# }

# Clean temps - keep inelegant list of unlinks as its the easiet layout for the moment ####
unlink("Debugging", recursive= TRUE, force=TRUE)
unlink("DebuggingBaU", recursive= TRUE, force=TRUE)
unlink("DebuggingICS", recursive= TRUE, force=TRUE)
unlink("HTML_animation_OutBaU", recursive= TRUE, force=TRUE)
unlink("HTML_animation_OutICS", recursive= TRUE, force=TRUE)
unlink("Logs", recursive= TRUE, force=TRUE)
unlink("OutBaU", recursive= TRUE, force=TRUE)
unlink("OutICS", recursive= TRUE, force=TRUE)
unlink("Summary_Report", recursive= TRUE, force=TRUE)
unlink("Temp", recursive= TRUE, force=TRUE)
unlink("TempBaU", recursive= TRUE, force=TRUE)
unlink("TempICS", recursive= TRUE, force=TRUE)
unlink("In", recursive= TRUE, force=TRUE)

unlink("LULCC/InVector", recursive= TRUE, force=TRUE)
unlink("LULCC/Out_lulcc", recursive= TRUE, force=TRUE)
unlink("LULCC/SourceData", recursive= TRUE, force=TRUE)
unlink("LULCC/TempRaster", recursive= TRUE, force=TRUE)
unlink("LULCC/TempTables", recursive= TRUE, force=TRUE)
unlink("LULCC/TempVector", recursive= TRUE, force=TRUE)
unlink("LULCC/TempVector_GCS", recursive= TRUE, force=TRUE)
unlink("LULCC/grass", recursive= TRUE, force=TRUE)

unlink("*.Rout",force=TRUE)
unlink("*.txt",force=TRUE)
unlink("*.log",force=TRUE)
unlink("*.aux",force=TRUE)
unlink("*.lof",force=TRUE)
unlink("*.lot",force=TRUE)
unlink("*.out",force=TRUE)
unlink("*.toc",force=TRUE)
unlink("LaTeX//*.pdf",force=TRUE)
unlink("LaTeX//*.mp4",force=TRUE)
unlink("LaTeX//*.csv",force=TRUE)
unlink("LaTeX//SimLength.txt",force=TRUE)
unlink("LaTeX//MCruns.txt",force=TRUE)
unlink("LULCC//*.Rout",force=TRUE)
#unlink("LULCC//*.txt",force=TRUE)
unlink("LULCC//*.csv",force=TRUE)	

# -------------------------------------------------------------------------


dir.create("Debugging")
dir.create("DebuggingBaU")
dir.create("DebuggingICS")
dir.create("HTML_animation_OutBaU")
dir.create("HTML_animation_OutICS")
dir.create("Logs")
dir.create("OutBaU")
dir.create("OutICS")
dir.create("Summary_Report")
dir.create("Temp")
dir.create("In")
  dir.create("In/DemandScenarios")

  dir.create("LULCC/InVector")
  dir.create("LULCC/Out_lulcc")
  dir.create("LULCC/SourceData")
    dir.create("LULCC/SourceData/InRaster")
    dir.create("LULCC/SourceData/InRaster_GCS")
    dir.create("LULCC/SourceData/InTables")
    dir.create("LULCC/SourceData/InVector")
    dir.create("LULCC/SourceData/InVector_GCS")

dir.create("LULCC/TempRaster")
dir.create("LULCC/TempTables")
dir.create("LULCC/TempVector")
dir.create("LULCC/TempVector_GCS")

unlink("Out_lulcc//*.*",force=TRUE)
unlink("TempRaster//*.*",force=TRUE)
unlink("TempVector//*.*",force=TRUE)
unlink("TempVector_GCS//*.*",force=TRUE)

# system("LULCC/RpathOSsystem.sh") NOT WORKING, CHECK OUT IN CASE NEEDED DOWNSTREAM

# # Process .txt to be properly read by Dinamica EGO ####
# # stx <- as.string(system("type R"))
# # R.home()
# # file.path(R.home("bin"), "R")
# stx = read.table("LULCC/TempTables/Rpath.txt")
# stx$V1
# st<-as.character(stx$V1)
# m <- matrix(c(1,st),ncol = 2, nrow = 1)
# m <- as.data.frame(m)
# m$V1 <- as.integer(as.character(m$V1))
# colnames(m)<-c("Key*","Rpath")
# m
# write.csv(m, "LULCC/TempTables/Rpath.csv",row.names=FALSE)
# 
# # Process .txt to be properly read by Dinamica EGO ####
# OSstx = read.table("LULCC/TempTables/OS_type.txt")
# OSstx$V1
# OSst<-as.character(OSstx$V1)
# OSm <- matrix(c(1,OSst),ncol = 2, nrow = 1)
# OSm <- as.data.frame(OSm)
# OSm$V1 <- as.integer(as.character(OSm$V1))
# OSm$V2 <- as.integer(as.character(OSm$V2))
# colnames(OSm)<-c("Key*","OS")
# str(OSm)
# write.csv(OSm, "LULCC/TempTables/OStype.csv",row.names=FALSE)
# 
# shell.exec(file.path(getwd(), "LULCC/Wizard_imgs/R_found.pdf"))
# 
# Sys.sleep(2)

# shell.exec(file.path(getwd(), "Wizard_imgs/PickCountry.pdf"))
# Choose country from Gitlab "local" repository e.g. "C:\Users\adrian\Documents\mofuss\countries"
Country<-file.choose() 
file.copy(Country, "LULCC/TempTables")
Country_name<-gsub(pattern = "(.*countries[/])(.*)(.tif.*)", 
                   replacement = "\\2",
                   Country)
Country_name
fileConn<-file("LULCC/TempTables/Country.txt")
writeLines(Country_name, fileConn)
close(fileConn)

# Process .txt to be properly read by Dinamica EGO ####
ctry = read.table("LULCC/TempTables/Country.txt") 
ctry$V1
cty<-as.character(ctry$V1)
mcty <- matrix(c(1,cty),ncol = 2, nrow = 1)
mcty <- as.data.frame(mcty)
mcty$V1 <- as.integer(as.character(mcty$V1))
colnames(mcty)<-c("Key*","Country")
mcty
write.csv(mcty, "LULCC/TempTables/Country.csv",row.names=FALSE) 

# -------------------------------------------------------------------------


dversion<-"v1" #Reemplazar como parametro en un futuro, por ahora son todos v1
sourcedatafile<-paste0(cty,"_MoFuSS_Dataset_",dversion,".zip")

Country<-readLines("LULCC/TempTables/Country.txt")

unlink("DownloadedDatasets//*.zip",force=TRUE)
unlink("SourceData//*.*",recursive = TRUE, force = TRUE)
unlink("SourceData",recursive = TRUE, force = TRUE)

src.dir <- paste0("LULCC/DownloadedDatasets/SourceData",Country)
file.copy(paste0(src.dir,"/parameters.csv"), "LULCC/SourceData", overwrite=TRUE)

dir.names <- dir(src.dir)
dir.names <- dir.names[!dir.names %in% "DemandScenarios"]
for (RO in dir.names){ #for (RO in dir.names[c(-1,-7)] ){
	src.dir <- paste0("LULCC/DownloadedDatasets/SourceData",Country,"/",RO,"/")
	dest.dir <- paste0("LULCC/SourceData/",RO,"/")
	file.names <- dir(src.dir) 
	sapply(file.names, function(x) {
                       file.copy(from=paste(src.dir, x, sep=""), 
                                 to=paste(dest.dir, x, sep=""), 
                                 overwrite = TRUE) }) 
  }

dest.dir.DS <- "In/DemandScenarios/"
src.dir.DS1 <- paste0("LULCC/DownloadedDatasets/SourceData",Country,"/DemandScenarios")
file.names.DS <- dir(src.dir.DS1) 

for (RO.DS in file.names.DS){ 
	src.dir.DS2 <- paste0("LULCC/DownloadedDatasets/SourceData",Country,"/DemandScenarios/",RO.DS)
	file.copy(src.dir.DS2, dest.dir.DS, overwrite=TRUE)
  }

# End ####


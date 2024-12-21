# MoFuSS
# Version 3
# Date: Mar 2024

# 2dolist
# URGENTLY fix this very old and outdated chunck to make it 
# work smoothly with GEE at varying scales!!
# Fix for linux cluster

# Internal parameters
# Select MoFuSS version
webmofuss = 0 # "1" is linux based webmofuss in server, "0" is local mofuss run

# Load libraries ----
library(stringr)
library(purrr)
library(tcltk)
library(dplyr)

# Detect OS
os <- Sys.info()["sysname"]

if (webmofuss == 0) {
  
  setwd(countrydir)
  
  # Clean temps - keep inelegant list of unlinks as its an easier layout for the moment ----
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
  # unlink("LULCC//*.txt",force=TRUE)
  unlink("LULCC//*.csv",force=TRUE)
  unlink("LULCC//*.egoml",force=TRUE)	
  
  # Create mofuss dir structure
  # dir.create("Debugging")
  # dir.create("DebuggingBaU")
  # dir.create("DebuggingICS")
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
  # dir.create("LULCC/Out_lulcc")
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
  
  dir.create("LULCC/Wizard_imgs")
  
  unlink("Out_lulcc//*.*",force=TRUE)
  unlink("TempRaster//*.*",force=TRUE)
  unlink("TempVector//*.*",force=TRUE)
  unlink("TempVector_GCS//*.*",force=TRUE)
  
  batfiles <- list.files (paste0(gitlabdir, "/localhost/scripts/LULCC/"), pattern = ".bat")
  file.copy(from=paste0(gitlabdir, "/localhost/scripts/LULCC/",batfiles), 
            to=paste0(countrydir, "/LULCC"), 
            overwrite = TRUE)
  
  shfiles <- list.files (paste0(gitlabdir, "/localhost/scripts/LULCC/"), pattern = ".sh")
  file.copy(from=paste0(gitlabdir, "/localhost/scripts/LULCC/",shfiles), 
            to=paste0(countrydir, "/LULCC"), 
            overwrite = TRUE)
  
  if (os == "Windows"){
    shell(file.path(getwd(), "LULCC/RpathOSsystem2.bat")) # WARNING, "shell" won't read blank spaces - End of Linux integration
  } else if (os == "Linux"){
    system(file.path(getwd(), "LULCC/RpathOSsystem2.sh"), intern = TRUE)
  }
  
  # Process .txt to be properly read by Dinamica EGO
  stx = read.table("LULCC/TempTables/Rpath.txt")
  stx$V1
  st<-as.character(stx$V1)
  m <- matrix(c(1,st),ncol = 2, nrow = 1)
  m <- as.data.frame(m)
  m$V1 <- as.integer(as.character(m$V1))
  colnames(m)<-c("Key*","Rpath")
  m
  write.csv(m, "LULCC/TempTables/Rpath.csv",row.names=FALSE)
  
  # Process .txt to be properly read by Dinamica EGO ----
  OSstx = read.table("LULCC/TempTables/OS_type.txt")
  OSstx$V1
  OSst<-as.character(OSstx$V1)
  OSm <- matrix(c(1,OSst),ncol = 2, nrow = 1)
  OSm <- as.data.frame(OSm)
  OSm$V1 <- as.integer(as.character(OSm$V1))
  OSm$V2 <- as.integer(as.character(OSm$V2))
  colnames(OSm)<-c("Key*","OS")
  str(OSm)
  write.csv(OSm, "LULCC/TempTables/OStype.csv",row.names=FALSE)
  
  # Fix all pop-ups from the repository!
  # shell.exec("https://gitlab.com/mofuss/mofuss/-/blob/master/localhost/scripts/LULCC/Wizard_imgs/R_found.pdf")
  # shell(file.path(getwd(), "LULCC/Wizard_imgs/R_found.pdf"))
  
  # Choose country from Gitlab "local" repository e.g. "C:\Users\adrian\Documents\mofuss\countries"
  # browseURL("https://gitlab.com/mofuss/mofuss/-/tree/master/countries")
  # Country<-choose.files(default=paste0(gitlabdir,"/countries/*.*")) 
  
  # Country<-paste0(gitlabdir,"/countries/*.tif") 
  
  
  # file.copy(country, "LULCC/TempTables") #What happened here?
  # country_name<-gsub(pattern = "(.*countries[\\])(.*)(.tif.*)", 
  #                    replacement = "\\2",
  #                    country)
  # country_name
  fileConn<-file("LULCC/TempTables/Country.txt")
  writeLines(country_name, fileConn)
  close(fileConn)
  
  
  # Process .txt to be properly read by Dinamica EGO 
  ctry = read.table("LULCC/TempTables/Country.txt") 
  ctry$V1
  cty<-as.character(ctry$V1)
  mcty <- matrix(c(1,cty),ncol = 2, nrow = 1)
  mcty <- as.data.frame(mcty)
  mcty$V1 <- as.integer(as.character(mcty$V1))
  colnames(mcty)<-c("Key*","Country")
  mcty
  write.csv(mcty, "LULCC/TempTables/Country.csv",row.names=FALSE) 
  
  dversion<-"v1" #Reemplazar como parametro en un futuro, por ahora son todos v1
  sourcedatafile<-paste0(cty,"_MoFuSS_Dataset_",dversion,".zip")
  
  country <- readLines("LULCC/TempTables/Country.txt")
  
  unlink("DownloadedDatasets//*.zip",force=TRUE)
  unlink("SourceData//*.*",recursive = TRUE, force = TRUE)
  unlink("SourceData",recursive = TRUE, force = TRUE)
  
  src.dir <- paste0("LULCC/DownloadedDatasets/SourceData",country)
  # file.copy(paste0(src.dir,"/parameters.csv"), "LULCC/SourceData", overwrite=TRUE) #WARNING - UNCOMMENT IF PROMPTED DOWN THE RIVER
  
  dir.names <- dir(src.dir)
  dir.names <- dir.names[!dir.names %in% "DemandScenarios"]
  for (RO in dir.names){ #for (RO in dir.names[c(-1,-7)] ){
    src.dir <- paste0("LULCC/DownloadedDatasets/SourceData",country,"/",RO,"/")
    dest.dir <- paste0("LULCC/SourceData/",RO,"/")
    file.names <- dir(src.dir) 
    sapply(file.names, function(x) {
      file.copy(from=paste(src.dir, x, sep=""), 
                to=paste(dest.dir, x, sep=""), 
                overwrite = TRUE) }) 
  }
  
  dest.dir.DS <- "In/DemandScenarios/"
  src.dir.DS1 <- paste0("LULCC/DownloadedDatasets/SourceData",country,"/DemandScenarios")
  file.names.DS <- dir(src.dir.DS1) 
  
  for (RO.DS in file.names.DS){ 
    src.dir.DS2 <- paste0("LULCC/DownloadedDatasets/SourceData",country,"/DemandScenarios/",RO.DS)
    file.copy(src.dir.DS2, dest.dir.DS, overwrite=TRUE)
  }
  
  # files 2B copied ----
  files2copy <- c("ffmpeg32/",
                  "ffmpeg64/",
                  "LaTeX/",
                  "friction3.egoml",
                  "IDW_Sc3.egoml",
                  #"8_FW_dyn_lulcc_Sc16.egoml",
                  # 7_FW_dyn_lulcc_Sc16b.egoml",
                  "7_FW_dyn_lulcc_Sc16b_luc1.egoml",
                  "7_FW_dyn_lulcc_Sc16b_luc2.egoml",
                  "7_FW_dyn_lulcc_Sc16b_luc1_lucbet.egoml",
                  "7_FW_dyn_lulcc_Sc16b_luc2.lucbet.egoml",
                  # "8_FW_dyn_lulcc_Sc17.egoml",
                  "debugging/0_debugv1.R",
                  "debugging/0_debugv2.R",
                  "rnorm2.R",
                  "NRB_graphs_datasets2.R",
                  # "maps_animations2.R",
                  # "maps_animations3.R",
                  #"maps_animations4.R",
                  "maps_animations5.R",
                  "finalogs.R",
                  "bypass_maps_animations.R")
  
  for (f in files2copy) {
    # f <- "ffmpeg32/"
    file.copy(from=paste0(gitlabdir, "/localhost/scripts/",f), 
              to=paste0(countrydir), 
              overwrite = TRUE, recursive = TRUE, copy.mode = TRUE)
  }
  
  # Copy wizard images
  file.copy(from=paste0(gitlabdir, "/localhost/scripts/LULCC/Wizard_imgs/"),
            to=paste0(countrydir, "/LULCC/"),
            overwrite = TRUE, recursive = TRUE, copy.mode = TRUE)
  
  # End of local script ####
  
} else {
  
  "This is webmofuss 4 Edgar"
  
  gitlabdir <- "F:/scripts/gitlab"
  country <- "F:\\scripts\\gitlab\\countries\\Haiti.tif"
  countrydir <- "F:/scripts/mofuss"  
  
  setwd(countrydir)
  
  # Clean temps - keep inelegant list of unlinks as its an easier layout for the moment ----
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
  unlink("LULCC", recursive= TRUE, force=TRUE)
  
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
  unlink("LULCC//*.egoml",force=TRUE)	
  
  #Create mofuss dir structure
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
  dir.create("In/vehicle")
  dir.create("In/walking")
  
  dir.create("LULCC")
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
  
  batfiles <- list.files (paste0(gitlabdir, "/linwgrax
                                 in/scripts/LULCC/"), pattern = ".bat")
  print(batfiles)
  file.copy(from=paste0(gitlabdir, "/localhost/scripts/LULCC/",batfiles), 
            to=paste0(countrydir, "/LULCC"), 
            overwrite = TRUE)
  
  write.table("F:/R-4.2.2/bin", "LULCC/TempTables/Rpath.csv",row.names=FALSE, col.names = FALSE)
  write.table(64, "LULCC/TempTables/OStype.csv",row.names=FALSE, col.names = FALSE)
  
}








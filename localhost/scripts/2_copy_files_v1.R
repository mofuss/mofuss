# MoFuSS
# Version 3
# Date: Mar 2024

# 2dolist ----
# URGENTLY fix this very old and outdated chunck to make it 
# work smoothly with GEE at varying scales!!
# Fix for linux cluster

# Internal parameters ----
# Select MoFuSS platform:
# webmofuss <- 1 # "1" is web-MoFuSS running in our Ubuntu server, "0" is localhost (Windows or Linux)
source(paste0(scriptsmofuss,"00_webmofuss.R"))
# Load libraries ----
library(stringr)
library(purrr)
library(tcltk)
library(dplyr)

# Detect OS
os <- Sys.info()["sysname"]

# Set working directory
setwd(countrydir)

# Clean temps ----
directories_to_remove <- c(
  "Debugging", "DebuggingBaU", "DebuggingICS", "HTML_animation_OutBaU", "HTML_animation_OutICS", 
  "Logs", "OutBaU", "OutICS", "Summary_Report", "Temp", "TempBaU", "TempICS", "In",
  "LULCC/InVector", "LULCC/Out_lulcc", "LULCC/SourceData", "LULCC/TempRaster", "LULCC/TempTables",
  "LULCC/TempVector", "LULCC/TempVector_GCS"
)

file_patterns_to_remove <- c(
  "*.Rout", "*.txt", "*.log", "*.aux", "*.lof", "*.lot", "*.out", "*.toc",
  "LaTeX//*.pdf", "LaTeX//*.mp4", "LaTeX//*.csv", "LaTeX//SimLength.txt", "LaTeX//MCruns.txt",
  "LULCC//*.Rout", "LULCC//*.csv", "LULCC//*.egoml"
)

# Remove directories
lapply(directories_to_remove, unlink, recursive = TRUE, force = TRUE)

# Remove files matching patterns
lapply(file_patterns_to_remove, unlink, force = TRUE)

# Create MoFuSS directory structure ----
dirs_to_create <- c(
  "HTML_animation_OutBaU", "HTML_animation_OutICS", "Logs", "OutBaU", "OutICS", "Summary_Report", "Temp",
  "In", "In/DemandScenarios", "LULCC/InVector", "LULCC/SourceData", "LULCC/SourceData/InRaster",
  "LULCC/SourceData/InRaster_GCS", "LULCC/SourceData/InTables", "LULCC/SourceData/InVector",
  "LULCC/SourceData/InVector_GCS", "LULCC/TempRaster", "LULCC/TempTables", "LULCC/TempVector",
  "LULCC/TempVector_GCS", "LULCC/Wizard_imgs"
)

lapply(dirs_to_create, dir.create, showWarnings = FALSE, recursive = TRUE)

# Additional clean-up
unlink("Out_lulcc//*.*", force = TRUE)
unlink("TempRaster//*.*", force = TRUE)
unlink("TempVector//*.*", force = TRUE)
unlink("TempVector_GCS//*.*", force = TRUE)

# Copy batch and shell files ----
batch_files <- list.files(paste0(gitlabdir, "/localhost/scripts/LULCC/"), pattern = ".bat")
sh_files <- list.files(paste0(gitlabdir, "/localhost/scripts/LULCC/"), pattern = ".sh")

lapply(batch_files, function(file) file.copy(
  from = paste0(gitlabdir, "/localhost/scripts/LULCC/", file),
  to = paste0(countrydir, "/LULCC"), overwrite = TRUE
))

lapply(sh_files, function(file) file.copy(
  from = paste0(gitlabdir, "/localhost/scripts/LULCC/", file),
  to = paste0(countrydir, "/LULCC"), overwrite = TRUE
))

# Execute OS-specific commands
if (os == "Windows") {
  shell(file.path(getwd(), "LULCC/RpathOSsystem2.bat"))
} else if (os == "Linux") {
  system(file.path(getwd(), "LULCC/RpathOSsystem2.sh"), intern = TRUE)
  system("bash LULCC/RpathOSsystem2.sh", intern = FALSE)
}

# Process .txt files for Dinamica EGO ----
stx <- read.table("LULCC/TempTables/Rpath.txt", stringsAsFactors = FALSE)
st <- as.character(stx$V1)
m <- data.frame(`Key*` = 1, Rpath = st)
write.csv(m, "LULCC/TempTables/Rpath.csv", row.names = FALSE)

if (os == "Windows") {
  OSstx <- read.table("LULCC/TempTables/OS_type.txt", stringsAsFactors = FALSE)
  OSst <- as.character(OSstx$V1)
  OSm <- data.frame(`Key*` = 1, OS = as.integer(ifelse(OSst == "64-bit", 64, ifelse(OSst == "32-bit", 32, NA))))
  write.csv(OSm, "LULCC/TempTables/OStype.csv", row.names = FALSE)
} else if (os == "Linux") {
  OSstx <- read.table("LULCC/TempTables/OS_type.txt", stringsAsFactors = FALSE)
  OSst <- as.character(OSstx$V1)
  OSm <- data.frame(`Key*` = 1, OS = as.integer(ifelse(OSst == "64-bit", 64, ifelse(OSst == "32-bit", 32, NA))))
  write.csv(OSm, "LULCC/TempTables/OStype.csv", row.names = FALSE)
}

# Process country selection ----
fileConn <- file("LULCC/TempTables/Country.txt")
writeLines(country_name, fileConn)
close(fileConn)

ctry <- read.table("LULCC/TempTables/Country.txt", stringsAsFactors = FALSE)
cty <- as.character(ctry$V1)
mcty <- data.frame(`Key*` = 1, Country = cty)
write.csv(mcty, "LULCC/TempTables/Country.csv", row.names = FALSE)

# dversion<-"v1" #Reemplazar como parametro en un futuro, por ahora son todos v1
# sourcedatafile<-paste0(cty,"_MoFuSS_Dataset_",dversion,".zip")

country <- readLines("LULCC/TempTables/Country.txt")

# Clean and prepare datasets ----
unlink("DownloadedDatasets//*.zip", force = TRUE)
unlink("SourceData//*.*", recursive = TRUE, force = TRUE)
unlink("SourceData", recursive = TRUE, force = TRUE)

src.dir <- paste0("LULCC/DownloadedDatasets/SourceData", country)
dir.names <- dir(src.dir)
dir.names <- dir.names[!dir.names %in% "DemandScenarios"]

lapply(dir.names, function(RO) {
  src.dir <- paste0("LULCC/DownloadedDatasets/SourceData", country, "/", RO, "/")
  dest.dir <- paste0("LULCC/SourceData/", RO, "/")
  file.names <- dir(src.dir)
  sapply(file.names, function(x) {
    file.copy(from = paste0(src.dir, x), to = paste0(dest.dir, x), overwrite = TRUE)
  })
})

dest.dir.DS <- "In/DemandScenarios/"
src.dir.DS1 <- paste0("LULCC/DownloadedDatasets/SourceData", country, "/DemandScenarios")
file.names.DS <- dir(src.dir.DS1)

lapply(file.names.DS, function(RO.DS) {
  src.dir.DS2 <- paste0(src.dir.DS1, "/", RO.DS)
  file.copy(src.dir.DS2, dest.dir.DS, overwrite = TRUE)
})

# Copy additional files ----
files2copy <- c(
  "ffmpeg32/", "ffmpeg64/", "LaTeX/", "friction3.egoml", "IDW_Sc3.egoml",
  "7_FW_dyn_lulcc_Sc16b_luc1.egoml", "7_FW_dyn_lulcc_Sc16b_luc2.egoml",
  "7_FW_dyn_lulcc_Sc16b_luc1_lucbet.egoml", "7_FW_dyn_lulcc_Sc16b_luc2.lucbet.egoml",
  "debugging/0_debugv1.R", "debugging/0_debugv2.R", "rnorm2.R", "NRB_graphs_datasets2.R",
  "maps_animations5.R", "finalogs.R", "bypass_maps_animations.R"
)

lapply(files2copy, function(f) {
  file.copy(from = paste0(gitlabdir, "/localhost/scripts/", f), to = paste0(countrydir), overwrite = TRUE, recursive = TRUE, copy.mode = TRUE)
})

# Copy wizard images
file.copy(
  from = paste0(gitlabdir, "/localhost/scripts/LULCC/Wizard_imgs/"),
  to = paste0(countrydir, "/LULCC/"),
  overwrite = TRUE, recursive = TRUE, copy.mode = TRUE
)

# End of script ----
  

# MoFuSS
# Version 4
# Date: Jan 2025

# 2dolist
# URGENTLY fix this very old and outdated chunck to make it 
# work smoothly with GEE at varying scales!!
# Fix for linux cluster

# Internal parameters
# Select MoFuSS platform:
webmofuss = 1 # "1" is  web-MoFuSS running in our Ubuntu server, "0" is localcal host (Windows or Linux)

# Load libraries ----
library(tcltk)

# Detect OS
os <- Sys.info()["sysname"]

# choose_directory = function(caption = "Choose the country to erase -and zip-") {
#   if(.Platform$OS.type == "unix")  {
#     setwd(tk_choose.dir("/", caption = caption))
#   } else {
#     setwd(choose.dir("/", caption = caption)) 
#   }
# }
# 
# choose_directory()
# countrydir <- getwd()

# writeLines(gitlabdir,paste0(gitlabdir,"/gitlabdir.txt"), useBytes=T)
# gitlabdir <- readLines(paste0(gitlabdir,"/gitlabdir.txt"))
# writeLines(countrydir,paste0(gitlabdir,"/countrydir.txt"), useBytes=T)
# countrydir <- readLines(paste0(gitlabdir,"/countrydir.txt"))

setwd(countrydir)

# Clean temps - keep inelegant list of unlinks as its the easiet layout for the moment ####
unlink("Debugging", recursive= TRUE, force=TRUE)
unlink("debug*", recursive= TRUE, force=TRUE)
unlink("norm*", recursive= TRUE, force=TRUE)
# unlink("DebuggingBaU", recursive= TRUE, force=TRUE)
# unlink("DebuggingICS", recursive= TRUE, force=TRUE)
unlink("HTML_animation_OutBaU", recursive= TRUE, force=TRUE)
unlink("HTML_animation_OutICS", recursive= TRUE, force=TRUE)
unlink("Logs", recursive= TRUE, force=TRUE)
unlink("OutBaU", recursive= TRUE, force=TRUE)
unlink("OutICS", recursive= TRUE, force=TRUE)
unlink("Summary_Report", recursive= TRUE, force=TRUE)
unlink("Temp", recursive= TRUE, force=TRUE)
# unlink("TempBaU", recursive= TRUE, force=TRUE)
# unlink("TempICS", recursive= TRUE, force=TRUE)
unlink("In", recursive= TRUE, force=TRUE)
unlink("ffmpeg32", recursive= TRUE, force=TRUE)
unlink("ffmpeg64", recursive= TRUE, force=TRUE)
unlink("LaTeX", recursive= TRUE, force=TRUE)


unlink("LULCC/InVector", recursive= TRUE, force=TRUE)
unlink("LULCC/Out_lulcc", recursive= TRUE, force=TRUE)
unlink("LULCC/SourceData", recursive= TRUE, force=TRUE)
unlink("LULCC/TempRaster", recursive= TRUE, force=TRUE)
unlink("LULCC/TempTables", recursive= TRUE, force=TRUE)
unlink("LULCC/TempVector", recursive= TRUE, force=TRUE)
unlink("LULCC/TempVector_GCS", recursive= TRUE, force=TRUE)
unlink("LULCC/Wizard_imgs", recursive= TRUE, force=TRUE)

unlink("LULCC/lucdynamics_luc1/", recursive= TRUE, force=TRUE)
unlink("LULCC/lucdynamics_luc2/", recursive= TRUE, force=TRUE)
unlink("LULCC/lucdynamics_luc3/", recursive= TRUE, force=TRUE)

unlink("*.Rout",force=TRUE)
unlink("*.txt",force=TRUE)
unlink("*.log",force=TRUE)
unlink("*.aux",force=TRUE)
unlink("*.lof",force=TRUE)
unlink("*.lot",force=TRUE)
unlink("*.out",force=TRUE)
unlink("*.toc",force=TRUE)
unlink("*.R",force=TRUE)
unlink("*.egoml",force=TRUE)

unlink("LaTeX//*.pdf",force=TRUE)
unlink("LaTeX//*.mp4",force=TRUE)
unlink("LaTeX//*.csv",force=TRUE)
unlink("LaTeX//SimLength.txt",force=TRUE)
unlink("LaTeX//MCruns.txt",force=TRUE)
unlink("LULCC//*.Rout",force=TRUE)
#unlink("LULCC//*.txt",force=TRUE)
unlink("LULCC//*.csv",force=TRUE)
unlink("LULCC//*.egoml",force=TRUE)
unlink("LULCC//*.bat",force=TRUE)
unlink("LULCC//*.sh",force=TRUE)



# MoFuSS
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


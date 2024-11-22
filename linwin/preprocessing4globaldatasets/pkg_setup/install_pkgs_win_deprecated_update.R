# MoFuSS
# Version 2
# Date: Nov 2019

rm(list=ls(all=TRUE))

# https://cran.rstudio.com/bin/windows/Rtools/

# INPUT PARAMETERS When managing gitlab scripts, update working directory from R Studio to the country modeling folder ####
# isRStudio <- Sys.getenv("RSTUDIO") == "1"
# if (isRStudio==1) {
#   setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# }else{
#     "Do Nothing"
#   }
# setwd(choose.dir(getwd(), "Choose a suitable MoFuSS root directory"))
# getwd()

# Read packages list ####
packages <- readLines("https://gitlab.com/mofuss/mofuss/raw/master/windows/scripts/LULCC/R_pkgs_win.txt")

Rpkgs_ok<-length(setdiff(packages, rownames(installed.packages()))) > 0

if (Rpkgs_ok == TRUE) {
	shell.exec("https://gitlab.com/mofuss/mofuss/-/blob/master/windows/scripts/LULCC/Wizard_imgs/R_Pkgs_installing.pdf")
	R.ver<-paste(R.Version()$major,".",substr(R.Version()$minor,1,1),sep="")
	dir.create("~/R")
	dir.create("~/R/win-library")
	dir.create(paste("~/R/win-library/",R.ver,sep=""))
	packages_path<-(paste("~/R/win-library/",R.ver,sep=""))
	Pckg_repos="https://cran.rstudio.com/"
	
	ipak <- function(pkg){
		new.pkg <- pkg[!(pkg %in% installed.packages(packages_path)[, "Package"])]
			if (length(new.pkg)) 
			install.packages(new.pkg, lib=packages_path, repos=Pckg_repos, dependencies = TRUE)
		}
	ipak(packages)
}

# End ####


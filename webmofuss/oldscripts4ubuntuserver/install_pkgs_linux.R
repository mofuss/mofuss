# MoFuSS linux
# Version 2
# Date: Nov 2019

rm(list=ls(all=TRUE))
# INPUT PARAMETERS When managing gitlab scripts, update working directory from R Studio to the country modeling folder ####
# install.packages("rstudioapi")
isRStudio <- Sys.getenv("RSTUDIO") == "1"
if (isRStudio==1) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}else{
    "Do Nothing"
  }

# Read packages list ####
packages <- readLines("LULCC/R_pkgs.txt")

Rpkgs_ok<-length(setdiff(packages, rownames(installed.packages()))) > 0

if (Rpkgs_ok == TRUE) {
  #file.show("LULCC/Wizard_imgs/R_Pkgs_installing.pdf")
  R.ver<-paste(R.Version()$major,".",substr(R.Version()$minor,1,1),sep="")
	dir.create("~/R")
	dir.create("~/R/x86_64-pc-linux-gnu-library")
	dir.create(paste0("~/R/x86_64-pc-linux-gnu-library/",R.ver))
	packages_path<-(paste0("~/R/x86_64-pc-linux-gnu-library/",R.ver))
	Pckg_repos="https://cran.rstudio.com/"
	
	ipak <- function(pkg){
		new.pkg <- pkg[!(pkg %in% installed.packages(packages_path)[, "Package"])]
			if (length(new.pkg)) 
			install.packages(new.pkg, lib=packages_path, repos=Pckg_repos, dependencies = TRUE)
		}
	ipak(packages)
}

# End ####

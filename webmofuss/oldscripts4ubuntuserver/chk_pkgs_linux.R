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

R.ver<-paste0(R.Version()$major,".",substr(R.Version()$minor,1,1))
packages_path<-(paste("~/R/x86_64-pc-linux-gnu-library/",R.ver,sep=""))
Pckg_repos="https://cran.rstudio.com/"

Rpkgs_ok<-length(setdiff(packages, rownames(installed.packages()))) > 0

if (Rpkgs_ok == TRUE) {
	# win cmd shell.exec(file.path(getwd(), "LULCC/Wizard_imgs/WatchOut_msg.pdf"))
	alarm()
	#shell.exec("http://redd.ciga.unam.mx/nrb/images/Models/Notallowed.gif")	
} else {
	#shell.exec(file.path(getwd(), "Wizard_imgs/R_Pkgs_OK.pdf"))
}

# Checks for packages integrity by trying to load each of them ####

for (PK in packages) {
	for (i in 1:3) {
		tryCatch(
			{
            		message("This is the 'try' part")
				library(PK,character.only=TRUE)
			},
        			error=function(cond)
			{
            		message("Function caused an error!")
            		message("Here's the original error message:")
            		message(cond)
            		# Choose a return value in case of error
            		# return(conditionMessage(cond))
				# shell.exec(file.path(getwd(), "LULCC/Wizard_imgs/Corrupt_R_Pckg.pdf"))
				return(install.packages(PK, lib=packages_path, repos=Pckg_repos, dependencies = TRUE))
        		},
        			warning=function(cond)
			{
            		message("Function caused a warning...")
            		message("Here's the original warning message:")
            		message(cond)
            		# Choose a return value in case of warning
            		return(conditionMessage(cond))
        		},
        			finally={
            		# message(paste("\nTry number:", x))
            		# message("Some other message at the end")
        		}
			)    
	}
}

for (PK in packages) {
	library(PK,character.only=TRUE)
			}
#file.show("LULCC/Wizard_imgs/R_Pkgs_OK.pdf")

# End ####

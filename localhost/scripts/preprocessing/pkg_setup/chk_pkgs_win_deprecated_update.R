# MoFuSS Katherine test
# test 2 -KL testing again
# Version 2
# Date: Nov 2019

rm(list=ls(all=TRUE))

# INPUT PARAMETERS When managing gitlab scripts, update working directory from R Studio to the country modeling folder ####
# isRStudio <- Sys.getenv("RSTUDIO") == "1"
# if (isRStudio==1) {
#   setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# }else{
#   "Do Nothing"
# }
# setwd(choose.dir(getwd(), "Choose a suitable MoFuSS root directory"))
# getwd()

# Read packages list ####
packages <- readLines("https://gitlab.com/mofuss/mofuss/raw/master/windows/scripts/LULCC/R_pkgs_win.txt")

R.ver<-paste(R.Version()$major,".",substr(R.Version()$minor,1,1),sep="")
packages_path<-(paste("~/R/win-library/",R.ver,sep=""))
Pckg_repos="https://cran.rstudio.com/"

Rpkgs_ok<-length(setdiff(packages, rownames(installed.packages()))) > 0

if (Rpkgs_ok == TRUE) {
	shell.exec("https://gitlab.com/mofuss/mofuss/-/blob/master/windows/scripts/LULCC/Wizard_imgs/WatchOut_msg.pdf")
	alarm()
	#shell.exec("https://gitlab.com/mofuss/mofuss/-/blob/master/windows/scripts/LULCC/Wizard_imgs/Notallowed.gif")
} else {
	shell.exec("https://gitlab.com/mofuss/mofuss/-/blob/master/windows/scripts/LULCC/Wizard_imgs/R_Pkgs_OK.pdf")
}

# Checks for packages integrity by trying to load each of them ####

for (PKs in packages) {
	for (i in 1:1) {
		tryCatch(
			{
            		message("This is the 'try' part")
			  library(PKs, character.only=TRUE, lib.loc=packages_path)
			},
        			error=function(cond)
			{
            		message("Function caused an error!")
            		message("Here's the original error message:")
            		message(cond)
            		# Choose a return value in case of error
            		# return(conditionMessage(cond))
				shell.exec("https://gitlab.com/mofuss/mofuss/-/blob/master/windows/scripts/LULCC/Wizard_imgs/Corrupt_R_Pckg.pdf")
				
				return(install.packages(PKs, lib=packages_path, repos=Pckg_repos, dependencies = TRUE))
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

for (PKs in packages) {
	library(PKs, character.only=TRUE, lib.loc=packages_path) 
			}

shell.exec("https://gitlab.com/mofuss/mofuss/-/blob/master/windows/scripts/LULCC/Wizard_imgs/R_Pkgs_OK.pdf")
# shell.exec(file.path(getwd(), "Wizard_imgs/R_Pkgs_OK_2.pdf"))

# End ####


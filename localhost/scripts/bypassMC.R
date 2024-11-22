# Author: A Ghilardi
# Version: 1.0
# Date: 2015

rm(list=ls(all=TRUE))

unlink("Summary_Report//Mofuss_Summary_Report.pdf", force=TRUE)
unlink("LaTeX//Mofuss_Summary_Report.pdf", force=TRUE)
unlink("Mofuss_Summary_Report.pdf", force=TRUE)
unlink("LaTeX//InputPara.csv", force=TRUE)
unlink("LaTeX//NRBTable.csv", force=TRUE)
unlink("LaTeX//fNRBTable.csv", force=TRUE)
unlink("LaTeX//SumTable.csv", force=TRUE)

# Read in the arguments listed at the command line in Dinamica EGO'S "Run external process"
args=(commandArgs(TRUE))

# "args" is now a list of character vectors.
# First check to see if arguments are passed.
# Then cycle through each element of the list and evaluate the expressions.
if(length(args)==0){
	print("No arguments supplied by DINAMICA.")
	##Supply default values here (to be used when running the script through R directly)
	RerunMC=1
	BaUvsICS="BaU"
}else{
    for(i in 1:length(args)){
         eval(parse(text=args[[i]]))
    }
}

if (BaUvsICS == "ICS") {
	unlink("OutICS//*", recursive = TRUE,force=TRUE)
	unlink("LaTeX//Growth_Harvest_AniOutICS.mp4", force=TRUE)
	unlink("HTML_animation_OutICS//*", recursive = TRUE,force=TRUE)	
	unlink("LaTeX//SumTableICS.csv", force=TRUE)
	OutDir<-"OutICS"
} else {
	unlink("OutBaU//*", recursive = TRUE,force=TRUE)
	unlink("LaTeX//Growth_Harvest_AniOutBaU.mp4", force=TRUE)
	unlink("HTML_animation_OutBaU//*", recursive = TRUE,force=TRUE)	
	unlink("LaTeX//SumTableBaU.csv", force=TRUE)
	OutDir<-"OutBaU"
}


#Three key files that should be present after MC
MCist<-file.exists("Temp/i_st_all.csv")
MCk<-file.exists("Temp/k_all.csv")
MCrmax<- file.exists("Temp/rmax_all.csv")

if (RerunMC==0) {
	if (MCist == FALSE | MCk == FALSE | MCrmax == FALSE) {
		shell.exec(file.path(getwd(), "LULCC/Wizard_imgs/bypassMCerror.pdf"))
		stop()		
	} else {
		"Do nothing"	
	}

} else{
	textmsg<-"Using same MonteCarlo datasets"
	write.csv(textmsg,paste("DebugOutputs//",textmsg,".csv",sep=""),row.names = FALSE)
} 



###############################
#########END OF SCRIPT#########
###############################
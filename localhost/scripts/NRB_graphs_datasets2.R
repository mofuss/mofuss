# MoFuSS
# Version 2
# Date: Sep 2023

# rm(list=ls(all=TRUE))

# Extra parameters for manually debugging

# Checks for unused packages
# library(nolock)
# libr_unused()

# Load packages ####
library(readr)
library(dplyr)
library(animation)
library(data.table)
library(fBasics)
library(glue)
library(jpeg)
library(plyr)
library(png)
library(raster)
library(sf)
library(readr)
library(tiff)
library(gitlabr)
library(tidyverse)
library(spam)
library(sf)
library(tictoc)
library(mapview)
library(tidyverse)
library(rmapshaper)
library(foreach)
library(readxl)

# Read in the arguments listed at the command line in Dinamica EGO'S "Run external process"
args=(commandArgs(TRUE))

# "args" is now a list of character vectors.
# First check to see if arguments are passed.
# Then cycle through each element of the list and evaluate the expressions.
if(length(args)==0){
	print("No arguments supplied by DINAMICA.")
	##Supply default values here (to be used when running the script through R directly)
	MC = 10 # MonteCarlo runs Can be picked for parameters table
	IT = 2010 # Initial year
	K_MC=1
	TOF_MC=1
	Ini_st_MC=1
	Ini_st.factor.percentage=100
	COVER_MAP=1
	rmax_MC=1
	DEF_FW=0
	IL=48 # Iteration length in week - each year = 48 weeks
	# STdyn=20 # Simulation length set by dinamica, but cycles in the repeat functor is STdyn+1 as 2 cycles are needed for 1 year: 1jan->31dec
	AGBmap=1
	SumTables=1
	OSType=64
	BaUvsICS="BaU"
	cutoff_yrs=10
}else{
    for(i in 1:length(args)){
         eval(parse(text=args[[i]]))
    }
}

# Read parameters table ----
read.csv("LULCC/TempTables/Country.csv") %>%
  dplyr::filter(Key. == "1") %>%
  pull(Country) -> country_name

# Specify the directory where the file is located
parameters_directory <- paste0(getwd(),"/LULCC/DownloadedDatasets/SourceData",country_name)

# Use list.files() to find the file that matches the pattern
parameters_name <- list.files(path = parameters_directory, pattern = "^parameters.*\\.xlsx$", full.names = TRUE)

# Read the Excel file
country_parameters <- read_excel(parameters_name)
print(tibble::as_tibble(country_parameters), n=30)

# country_parameters %>%
#   dplyr::filter(Var == "LULCt1map") %>%
#   pull(ParCHR) -> LULCt1map
# 
# country_parameters %>%
#   dplyr::filter(Var == "LULCt2map") %>%
#   pull(ParCHR) -> LULCt2map
# 
# country_parameters %>%
#   dplyr::filter(Var == "LULCt3map") %>%
#   pull(ParCHR) -> LULCt3map
# 
# country_parameters %>%
#   dplyr::filter(Var == "AGB1map") %>%
#   pull(ParCHR) -> AGB1map
# 
# country_parameters %>%
#   dplyr::filter(Var == "AGB2map") %>%
#   pull(ParCHR) -> AGB2map
# 
# country_parameters %>%
#   dplyr::filter(Var == "AGB3map") %>%
#   pull(ParCHR) -> AGB3map

country_parameters %>%
  dplyr::filter(Var == "end_year") %>%
  pull(ParCHR) %>%
  as.integer(.) -> end_year
STdyn <- end_year - IT

if (BaUvsICS == "ICS") {
	OutDir<-"OutICS"
} else {
	OutDir<-"OutBaU"
}
print(OutDir)

ST = (48/IL*STdyn)+1

print(MC)
print(IT)
print(K_MC)
print(TOF_MC)
print(Ini_st_MC)
print(Ini_st.factor.percentage)
Ini_st.factor = Ini_st.factor.percentage/100
print(COVER_MAP)
print(rmax_MC)
print (DEF_FW)
print(IL)
print(STdyn)
print(ST)
print(AGBmap)
print(SumTables)
print(cutoff_yrs)

if (K_MC == 1) {
	K_MC_yesno = "Yes"
} else {
	K_MC_yesno = "No"
}

if (TOF_MC == 1) {
	TOF_MC_yesno = "Yes"
} else {
	TOF_MC_yesno = "No"
}

if (Ini_st_MC == 1) {
	Ini_st_MC_yesno = "Yes"
} else {
	Ini_st_MC_yesno = "No"
}

if (COVER_MAP == 1) {
	COVER_MAP_yesno = "Yes"
	Ini_st_MC = 0
	Ini_st_MC_yesno = "Not applicable"
	Ini_st.factor.percentage = "Tree cover as a % of K"
} else {
	COVER_MAP_yesno = "No"
	Ini_st.factor.percentage = paste(Ini_st.factor.percentage,"% of K",sep="")
}

if (rmax_MC == 1) {
	rmax_MC_yesno = "Yes"
} else {
	rmax_MC_yesno = "No"
}

if (DEF_FW == 1) {
	DEF_FW_yesno = "Yes"
} else {
	DEF_FW_yesno = "No"
}

if (AGBmap == 1) {
	AGBmap_yesno = "Yes"
	COVER_MAP_yesno = "Not applicable"
	Ini_st_MC_yesno = "Not applicable"
	Ini_st.factor.percentage = "Using AGB map"
} else {
	AGBmap_yesno = "No"
}

if (SumTables == 1) {
	SumTables_yesno = "Yes"
} else {
	SumTables_yesno = "No"
}

if (OSType == 32) {
	res1000<-100
	res600<-100
	res300<-100
} else {
	res1000<-1000
	res600<-600
	res300<-300
}

# un ciclo de 1 hasta MC en el que se lean estos archivos
# los nombres acutalizarlos de acuerdo al contador del ciclo
# conservar todos los archivos, ie hay que haer un arreglo en donde meterlos todos

# string #1
c1_agb<-rep("Temp//2_AGBtx",MC)
c1_nrb<-rep("Temp//2_NRB",MC)
c1_con<-rep("Temp//2_CON_TOT",MC)
c1_con_nrb1<-rep("Temp//2_CON_NRB",MC)

# string #2
s1<-seq(1,MC,1)
if (length(s1)<10) {    # cuando MC es menor de 10
	cmen10<-as.character(s1)
	c0s<-rep("0",length(cmen10)) # construye vector con "0"
	c2<-paste(c0s,cmen10,sep="")
} else {
	smen10<-s1[s1<10] #separa los numeros menores a 10
	smas10<-s1[s1>9] #separa los numeros mayores a 9
	cmas10<-as.character(smas10)
	cmen10<-as.character(smen10)
	c0s<-rep("0",length(smen10)) # construye vector con "0"
	c01<-paste(c0s,cmen10,sep="")
	c2<-c(c01,cmas10)
} # cadena 2

# string #3
c3<-rep(".csv",MC)

# Nombres finales
agb_CadF<-paste(c1_agb,c2,c3,sep="")
nrb_CadF<-paste(c1_nrb,c2,c3,sep="")
con_CadF<-paste(c1_con,c2,c3,sep="")
connrb_CadF<-paste(c1_con_nrb1,c2,c3,sep="")

# Hasta acá construccion automatica de nombres de archivos
# Lectura automatica de archivos y construccion de bases de datos

for (numar in 1:MC) {
 
	Nagbx<-agb_CadF[numar]
	Nnrbx<-nrb_CadF[numar]
	Nconx<-con_CadF[numar]
	Nconnrbx<-connrb_CadF[numar]
  
	Dagbx<-read.csv(Nagbx,colClasses=c("NULL",NA))
	Dnrbx<-read.csv(Nnrbx,colClasses=c("NULL",NA))
	Dconx<-read.csv(Nconx,colClasses=c("NULL",NA))
	Dconnrbx<-read.csv(Nconnrbx,colClasses=c("NULL",NA))

	if (numar==1) {
		DBagb<-Dagbx
		DBnrb<-Dnrbx
		DBcon<-Dconx
		DBconnrb<-Dconnrbx
	} else {
		DBagb<-cbind(DBagb,Dagbx)
		DBnrb<-cbind(DBnrb,Dnrbx)
		DBcon<-cbind(DBcon,Dconx)
		DBconnrb<-cbind(DBconnrb,Dconnrbx)
	}
} # for (numar in 1:MC) ## lectura de archivos ++++++++++++

# Hasta aqui funciona, completar las divisines que estan abajo 5.10.10.2013.
fnrb<-(DBnrb/DBcon)*100
fnrb[is.na(fnrb)] <- 0
fnrb_nrb<-(DBnrb/DBconnrb)*100
fnrb_nrb[is.na(fnrb_nrb)] <- 0
print (fnrb)
print (fnrb_nrb)

# Graphical layout ####
# AGB ####

# 2a modificacion 
# Automoatizar la construccion de complete_agb
# Hacer un ciclo que vaya de 1 hasta MC para que repita cbind con tantas columnas como MC

#This construct a unique data.frame with each mc run ordered by column
#and named as mc_1, mc_2, etc. It saves the table to the HD in case needed, delete the # symbol in "#write.table..."
mc<-paste0("mc_",1:MC)
mc

complete_agb <- DBagb

colnames(DBagb)<-c(mc)

#ejx<-seq(IT,(IT+(ST-1)),(48/IL))
ejx<-seq(IT,(IT+(STdyn)),(IL/48))

# NRB stats ####
# Reads 3 type tables for spatial average stats
NRB <- (read.csv("Temp//3_NRB.csv",colClasses=c("NULL",NA))/ST)
str(NRB)
# The idea here is to load NRB values per year, and then cropping the years off
# Without the cropping both NRB calculations should be exactly the same 
listNRBcsv <- list.files("Temp", pattern = "^2_NRB.+[.]csv$",ignore.case=F)
NRB_list = lapply(paste0("Temp/",listNRBcsv), read.csv)
if (cutoff_yrs == 0){
  NRB_l <- as.data.frame(sapply(NRB_list, function(df) apply(df[,-(3)], 2, mean))) 
} else {
  NRB_l <- as.data.frame(sapply(NRB_list, function(df) apply(df[-(1:cutoff_yrs),-(3)], 2, mean))) 
}
NRB_n <- as.data.frame(t(NRB_l[2,]))
rownames(NRB_n) <- 1:MC
str(NRB_n)
NRB
NRB_n

CON_TOT <- (read.csv("Temp//3_CON_TOT.csv",colClasses=c("NULL",NA))/ST)
listCON_TOTcsv <- list.files("Temp", pattern = "^2_CON_TOT.+[.]csv$",ignore.case=F)
CON_TOT_list = lapply(paste0("Temp/",listCON_TOTcsv), read.csv)
if (cutoff_yrs == 0){
  CON_TOT_l <- as.data.frame(sapply(CON_TOT_list, function(df) apply(df[,-(3)], 2, mean))) 
} else {
  CON_TOT_l <- as.data.frame(sapply(CON_TOT_list, function(df) apply(df[-(1:cutoff_yrs),-(3)], 2, mean))) 
}
CON_TOT_n <- as.data.frame(t(CON_TOT_l[2,]))
rownames(CON_TOT_n) <- 1:MC
str(CON_TOT_n)
CON_TOT
CON_TOT_n

CON_NRB <- (read.csv("Temp//3_CON_NRB.csv",colClasses=c("NULL",NA))/ST)
listCON_NRBcsv <- list.files("Temp", pattern = "^2_CON_NRB.+[.]csv$",ignore.case=F)
CON_NRB_list = lapply(paste0("Temp/",listCON_NRBcsv), read.csv)
if (cutoff_yrs == 0){
  CON_NRB_l <- as.data.frame(sapply(CON_NRB_list, function(df) apply(df[,-(3)], 2, mean))) 
} else {
  CON_NRB_l <- as.data.frame(sapply(CON_NRB_list, function(df) apply(df[-(1:cutoff_yrs),-(3)], 2, mean))) 
}
CON_NRB_n <- as.data.frame(t(CON_NRB_l[2,]))
rownames(CON_NRB_n) <- 1:MC
str(CON_NRB_n)
CON_NRB
CON_NRB_n

fNRB <- (NRB/CON_TOT)*100
fNRB[is.na(fNRB)] <- 0
fNRB_nrb <- (NRB/CON_NRB)*100
fNRB_nrb[is.na(fNRB_nrb)] <- 0

fNRB_n <- (NRB_n/CON_TOT_n)*100
fNRB_n[is.na(fNRB_n)] <- 0
fNRB_nrb_n <- (NRB_n/CON_NRB_n)*100
fNRB_nrb_n[is.na(fNRB_nrb_n)] <- 0

NRB_summary<-summary(NRB)
NRB_sd<-sapply(NRB,sd)
NRB_se<-(sapply(NRB,sd))/(sqrt(nrow(NRB)))
NRB_ci_l<-(sapply(NRB,mean))-(NRB_se*1.96)
NRB_ci_u<-(sapply(NRB,mean))+(NRB_se*1.96)

NRB_summary
NRB_sd
NRB_se
NRB_ci_l
NRB_ci_u

# CON_TOT stats ####
CON_TOT_summary<-summary(CON_TOT)
CON_TOT_sd<-sapply(CON_TOT,sd)
CON_TOT_se<-(sapply(CON_TOT,sd))/(sqrt(nrow(CON_TOT)))
CON_TOT_ci_l<-(sapply(CON_TOT,mean))-(CON_TOT_se*1.96)
CON_TOT_ci_u<-(sapply(CON_TOT,mean))+(CON_TOT_se*1.96)

CON_TOT_summary
CON_TOT_sd
CON_TOT_se
CON_TOT_ci_l
CON_TOT_ci_u


# CON_NRB stats ####
CON_NRB_summary<-summary(CON_NRB)
CON_NRB_sd<-sapply(CON_NRB,sd)
CON_NRB_se<-(sapply(CON_NRB,sd))/(sqrt(nrow(CON_NRB)))
CON_NRB_ci_l<-(sapply(CON_NRB,mean))-(CON_NRB_se*1.96)
CON_NRB_ci_u<-(sapply(CON_NRB,mean))+(CON_NRB_se*1.96)

CON_NRB_summary
CON_NRB_sd
CON_NRB_se
CON_NRB_ci_l
CON_NRB_ci_u


# fNRB stats ####
fNRB_summary<-summary(fNRB)
fNRB_sd<-sapply(fNRB,sd)
fNRB_se<-(sapply(fNRB,sd))/(sqrt(nrow(fNRB)))
fNRB_ci_l<-(sapply(fNRB,mean))-(fNRB_se*1.96)
fNRB_ci_u<-(sapply(fNRB,mean))+(fNRB_se*1.96)

fNRB_summary
fNRB_sd
fNRB_se
fNRB_ci_l
fNRB_ci_u


# fNRB nrb stats ####

fNRB_nrb_summary<-summary(fNRB_nrb)
fNRB_nrb_sd<-sapply(fNRB_nrb,sd)
fNRB_nrb_se<-(sapply(fNRB_nrb,sd))/(sqrt(nrow(fNRB_nrb)))
fNRB_nrb_ci_l<-(sapply(fNRB_nrb,mean))-(fNRB_nrb_se*1.96)
fNRB_nrb_ci_u<-(sapply(fNRB_nrb,mean))+(fNRB_nrb_se*1.96)

fNRB_nrb_summary
fNRB_nrb_sd
fNRB_nrb_se
fNRB_nrb_ci_l
fNRB_nrb_ci_u


# AGB,NRB,fNRN and FW_USE Trajectories ####

#par(mfrow=c(4,1),mar=c(4,4,4,4))
#dev.new(width=15, height=42)
# Size for 4 vertical graphs: width=75,height=190
tiff(filename=paste(OutDir,"//AGB_NRB_fNRB.tif",sep=""),width=75,height=190,units="mm",res=res1000,bg="white",compression=c("lzw"),
	type=c("windows"),pointsize=12,family="",restoreConsole=TRUE)
par(mfrow=c(4,1),oma=c(0,0,10,0),mar=c(5.1,5.1,1.0,2.1)) 

plot(ejx,DBagb[,1],type="l",col="darkgrey",lwd=0.25,ylim=c(0,max(DBagb)),xlab="", ylab="agb (tDM)",cex.lab=1,
     cex.axis=1)
for (lin in 2:MC) {
	lines(ejx,DBagb[,lin],lty="solid",col="darkgrey",lwd=0.25)
}
	lines(ejx,DBagb[,1],lty="solid",col="red",lwd=2)

Main_Title<-paste("Parameters set by user:
			StartUp year =",IT," \n Simulation length =",STdyn,"yr \n MC =",MC,"runs \n Initial Stock =",Ini_st.factor.percentage," \n Initial Stock w/MC =",Ini_st_MC_yesno," \n Iteration length =",IL,"weeks (",IL*0.25,"months ) \n Tree cover map provided?",COVER_MAP_yesno," \n AGB map provided?",AGBmap_yesno," \n Accounting for fuelwood from deforestation?",DEF_FW_yesno) 
title(main=Main_Title,line=NA,outer=TRUE,adj=0.5,
      cex.main = 1,   font.main= 1, col.main= "blue")

plot(ejx,DBnrb[,1],type="l",col="darkgrey",lwd=0.25,ylim=c(0,max(DBnrb)),xlab="",ylab="nrb (tDM)",cex.lab=1,
     cex.axis=1)
  for (lin in 2:MC) {
  lines(ejx,DBnrb[,lin],lty="solid",col="darkgrey",lwd=0.25)
}
  lines(ejx,DBnrb[,1],lty="solid",col="red",lwd=2)

plot(ejx,(fnrb[,1]),type="l",col="darkgrey",lwd=0.25,ylim=c(-0.05,100),xlab="",ylab="fnrb (%)",cex.lab=1,
     cex.axis=1)
  for (lin in 2:MC) {
  lines(ejx,(fnrb[,lin]),lty="solid",col="darkgrey",lwd=0.25)
}
  lines(ejx,(fnrb[,1]),lty="solid",col="red",lwd=2)

# frnb(nrb) turned off
# plot(ejx,(DBnrb[,1]/DBconnrb[,1]),type="l",col="darkgrey",lwd=0.25,ylim=c(0,1),xlab="years",ylab="fnrb (nrb) (%)",cex.lab=1,
#     cex.axis=1)
#  for (lin in 2:MC) {
#  lines(ejx,(DBnrb[,lin]/DBconnrb[,lin]),lty="solid",col="darkgrey",lwd=0.25)
# }
#  lines(ejx,(DBnrb[,1]/DBconnrb[,1]),lty="solid",col="red",lwd=2)


plot(ejx,DBcon[,1],type="l",col="darkgrey",lwd=0.25,ylim=c(0,max(DBcon)),xlab="", ylab="fuelwood use (tDM)",cex.lab=1,
     cex.axis=1)
  for (lin in 2:MC) {
  lines(ejx,DBcon[,lin],lty="solid",col="darkgrey",lwd=0.25)
}
  lines(ejx,DBcon[,1],lty="solid",col="red",lwd=2)

dev.off()


# AGB,NRB,fNRB and FW_USE Trajectories minus n years ####
# Number of years to be cut from starting year
cutoff_yrs_backwards <- cutoff_yrs+1

# par(mfrow=c(4,1),mar=c(4,4,4,4))
# dev.new(width=15, height=42)
# Size for 4 vertical graphs: width=75,height=190
tiff(filename=paste0(OutDir,"//AGB_NRB_fNRB_+",cutoff_yrs,".tif"),width=75,height=190,units="mm",res=res1000,bg="white",compression=c("lzw"),
     type=c("windows"),pointsize=12,family="",restoreConsole=TRUE)
par(mfrow=c(4,1),oma=c(0,0,10,0),mar=c(5.1,5.1,1.0,2.1)) 

# Slice first n years from graph ONLY
ejx_n<-seq((IT+cutoff_yrs),(IT+(STdyn)),(IL/48)) #Esto evita que lea los primeros n años.
DBagb_n <- DBagb[cutoff_yrs_backwards:ST, ]
DBnrb_n <- DBnrb[cutoff_yrs_backwards:ST, ]
fnrb_n <- fnrb[cutoff_yrs_backwards:ST, ]
DBcon_n <- DBcon[cutoff_yrs_backwards:ST, ]

plot(ejx_n,DBagb_n[,1],type="l",col="darkgrey",lwd=0.25,ylim=c(0,max(DBagb_n)),xlab="", ylab="agb (tDM)",cex.lab=1,
     cex.axis=1)
for (lin in 2:MC) {
  lines(ejx_n,DBagb_n[,lin],lty="solid",col="darkgrey",lwd=0.25)
}
lines(ejx_n,DBagb_n[,1],lty="solid",col="red",lwd=2)

Main_Title<-paste("Parameters set by user:
			StartUp year =",(IT+cutoff_yrs)," \n Simulation length =",(STdyn-cutoff_yrs),"yr \n MC =",MC,"runs \n Initial Stock =",Ini_st.factor.percentage," \n Initial Stock w/MC =",Ini_st_MC_yesno," \n Iteration length =",IL,"weeks (",IL*0.25,"months ) \n Tree cover map provided?",COVER_MAP_yesno," \n AGB map provided?",AGBmap_yesno," \n Accounting for fuelwood from deforestation?",DEF_FW_yesno) 
title(main=Main_Title,line=NA,outer=TRUE,adj=0.5,
      cex.main = 1,   font.main= 1, col.main= "blue")

plot(ejx_n,DBnrb_n[,1],type="l",col="darkgrey",lwd=0.25,ylim=c(0,max(DBnrb_n)),xlab="",ylab="nrb (tDM)",cex.lab=1,
     cex.axis=1)
for (lin in 2:MC) {
  lines(ejx_n,DBnrb_n[,lin],lty="solid",col="darkgrey",lwd=0.25)
}
lines(ejx_n,DBnrb_n[,1],lty="solid",col="red",lwd=2)

plot(ejx_n,(fnrb_n[,1]),type="l",col="darkgrey",lwd=0.25,ylim=c(-0.05,100),xlab="",ylab="fnrb (%)",cex.lab=1,
     cex.axis=1)
for (lin in 2:MC) {
  lines(ejx_n,(fnrb_n[,lin]),lty="solid",col="darkgrey",lwd=0.25)
}
lines(ejx_n,(fnrb_n[,1]),lty="solid",col="red",lwd=2)

#frnb(nrb) turned off
#plot(ejx,(DBnrb[,1]/DBconnrb[,1]),type="l",col="darkgrey",lwd=0.25,ylim=c(0,1),xlab="years",ylab="fnrb (nrb) (%)",cex.lab=1,
#     cex.axis=1)
#  for (lin in 2:MC) {
#  lines(ejx,(DBnrb[,lin]/DBconnrb[,lin]),lty="solid",col="darkgrey",lwd=0.25)
#}
#  lines(ejx,(DBnrb[,1]/DBconnrb[,1]),lty="solid",col="red",lwd=2)

plot(ejx_n,DBcon_n[,1],type="l",col="darkgrey",lwd=0.25,ylim=c(0,max(DBcon_n)),xlab="", ylab="fuelwood use (tDM)",cex.lab=1,
     cex.axis=1)
for (lin in 2:MC) {
  lines(ejx_n,DBcon_n[,lin],lty="solid",col="darkgrey",lwd=0.25)
}
lines(ejx_n,DBcon_n[,1],lty="solid",col="red",lwd=2)

dev.off()


# BOXPLOT GRAPH ####
# Add different periods within time window with slices

#dev.new(width=15, height=42)
tiff(filename=paste(OutDir,"//Boxplots.tif",sep=""),width=75,height=190,units="mm",res=res1000,bg="white",compression=c("lzw"),type=c("windows"),pointsize=12,family="",restoreConsole=TRUE)
par(mfrow=c(4,1),oma=c(0,0,10,0),mar=c(5.1,5.1,1.0,2.1)) 

e<-boxplot((NRB), plot=FALSE, gpars=list(xlab="Year",ylab="NRB (tDM)",lwd=0.25,lty=1,col="grey",ylim=c(0,max(NRB))))
bxp(e,pars=list(ylab=expression("         NRB \n (tDM iteration"^{-1}*")"),ylim=c(0,max(NRB))))

h<-boxplot((fNRB), plot=FALSE, gpars=list(xlab="Year",ylab="fNRB (%)",lwd=0.25,lty=1,col="grey",ylim=c(0,100)))
bxp(h,pars=list(ylab=expression("       fNRB \n (% iteration"^{-1}*") "),ylim=c(-0.05,100)))

#k<-boxplot((fNRB_nrb), plot=FALSE, gpars=list(xlab="Year",ylab="fNRB_nrb (%)",lwd=0.25,lty=1,col="grey",ylim=c(0,100)))
#bxp(k,pars=list(ylab=expression("        fNRB_nrb \n (% iteration"^{-1}*") "),ylim=c(0,100)))

e<-boxplot((CON_TOT), plot=FALSE, gpars=list(xlab="Year",ylab="fw use_tot (tDM)",lwd=0.25,lty=1,col="grey",ylim=c(0,max(CON_TOT))))
bxp(e,pars=list(ylab=expression("       fw use \n (tDM iteration"^{-1}*") "),ylim=c(0,max(CON_TOT))))

h<-boxplot((CON_NRB), plot=FALSE, gpars=list(xlab="Year",ylab="fw use_nrb (tDM)",lwd=0.25,lty=1,col="grey",ylim=c(0,max(CON_TOT))))
bxp(h,pars=list(ylab=expression("       fw use_nrb \n    (tDM iteration"^{-1}*") "),ylim=c(0,max(CON_TOT))))

Main_Title<-paste("Parameters set by user:
			StartUp year =",IT," \n Simulation length =",STdyn,"yr \n MC =",MC,"runs \n Initial Stock =",Ini_st.factor.percentage," \n Initial Stock w/MC =",Ini_st_MC_yesno," \n Iteration length =",IL,"weeks (",IL*0.25,"months ) \n Tree cover map provided?",COVER_MAP_yesno," \n AGB map provided?",AGBmap_yesno," \n Accounting for fuelwood from deforestation?",DEF_FW_yesno) 
title(main=Main_Title,line=NA,outer=TRUE,adj=0.5,
      cex.main = 1,   font.main= 1, col.main= "blue")

dev.off()

# Add different periods within time window with slices

#dev.new(width=15, height=42)
tiff(filename=paste0(OutDir,"//Boxplots_+",cutoff_yrs,".tif"),width=75,height=190,units="mm",res=res1000,bg="white",compression=c("lzw"),type=c("windows"),pointsize=12,family="",restoreConsole=TRUE)
par(mfrow=c(4,1),oma=c(0,0,10,0),mar=c(5.1,5.1,1.0,2.1)) 

e<-boxplot((NRB_n), plot=FALSE, gpars=list(xlab="Year",ylab="NRB (tDM)",lwd=0.25,lty=1,col="grey",ylim=c(0,max(NRB_n))))
bxp(e,pars=list(ylab=expression("         NRB \n (tDM iteration"^{-1}*")"),ylim=c(0,max(NRB_n))))

h<-boxplot((fNRB_n), plot=FALSE, gpars=list(xlab="Year",ylab="fNRB (%)",lwd=0.25,lty=1,col="grey",ylim=c(0,100)))
bxp(h,pars=list(ylab=expression("       fNRB \n (% iteration"^{-1}*") "),ylim=c(-0.05,100)))

#k<-boxplot((fNRB_nrb), plot=FALSE, gpars=list(xlab="Year",ylab="fNRB_nrb (%)",lwd=0.25,lty=1,col="grey",ylim=c(0,100)))
#bxp(k,pars=list(ylab=expression("        fNRB_nrb \n (% iteration"^{-1}*") "),ylim=c(0,100)))

e<-boxplot((CON_TOT_n), plot=FALSE, gpars=list(xlab="Year",ylab="fw use_tot (tDM)",lwd=0.25,lty=1,col="grey",ylim=c(0,max(CON_TOT_n))))
bxp(e,pars=list(ylab=expression("       fw use \n (tDM iteration"^{-1}*") "),ylim=c(0,max(CON_TOT_n))))

h<-boxplot((CON_NRB_n), plot=FALSE, gpars=list(xlab="Year",ylab="fw use_nrb (tDM)",lwd=0.25,lty=1,col="grey",ylim=c(0,max(CON_TOT_n))))
bxp(h,pars=list(ylab=expression("       fw use_nrb \n    (tDM iteration"^{-1}*") "),ylim=c(0,max(CON_TOT_n))))

Main_Title<-paste("Parameters set by user:
			StartUp year =",(IT+cutoff_yrs)," \n Simulation length =",(STdyn-cutoff_yrs),"yr \n MC =",MC,"runs \n Initial Stock =",Ini_st.factor.percentage," \n Initial Stock w/MC =",Ini_st_MC_yesno," \n Iteration length =",IL,"weeks (",IL*0.25,"months ) \n Tree cover map provided?",COVER_MAP_yesno," \n AGB map provided?",AGBmap_yesno," \n Accounting for fuelwood from deforestation?",DEF_FW_yesno) 
title(main=Main_Title,line=NA,outer=TRUE,adj=0.5,
      cex.main = 1,   font.main= 1, col.main= "blue")

dev.off()

# END ####
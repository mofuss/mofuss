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
unlink("Debugging//*.*",force=TRUE)
unlink("Temp//*.*",force=TRUE)
unlink("HTML_animation*//*", recursive = TRUE,force=TRUE)
unlink("Out*//*", recursive = TRUE,force=TRUE)
unlink("LaTeX//*.pdf",force=TRUE)
unlink("LaTeX//*.mp4",force=TRUE)
unlink("Summary_Report//*.*",force=TRUE)

Country<-readLines("LULCC/TempTables/Country.txt")

# Set master dir ####
#setwd(paste0(getwd(),"/LULCC"))
master_dir <- getwd()

# Load packages ####
pkcg_list <- readLines("https://gitlab.com/mofuss/mofuss/raw/master/linux/scripts/LULCC/R_pkgs_linux.txt")
lapply(pkcg_list, require, character.only = TRUE)

# INPUT PARAMETERS Read in the arguments listed at the command line in DINAMICA'S "Run external process" ####
args=(commandArgs(TRUE))
# "args" is now a list of character vectors.First check to see if arguments are passed.
# Then cycle through each element of the list and evaluate the expressions.
if(length(args)==0){
  print("No arguments supplied by DINAMICA.")
  #Default values here (to be used when running the script through R directly)
  Scenarios=1
  SceCode="BaU" #"s1a" #BAU s1a, s1b, s1c, s1d, s2a, s2b, s2c, s3
  Model="Fw" # Useless under fuelwood and charcoal integrated into the same model
  first_yr=2010
  last_yr=2050
  Subset_locs=0
}else{
  for(i in 1:length(args)){
    eval(parse(text=args[[i]]))
  }
}

# Read parameters table, checking if its delimiter is comma or semicolon ####
read_csv(glue("{master_dir}/LULCC/SourceData/parameters.csv")) %>% 
  {if(is.null(.$ParCHR[1])) read_csv2(glue("{master_dir}/LULCC/SourceData/parameters.csv")) else .} -> country_parameters
# Append "+"
country_parameters$ParCHR[1] <- str_c("+", country_parameters$ParCHR[1])
country_parameters$ParCHR[2] <- str_c("+", country_parameters$ParCHR[2])
# print(tbl_df(country_parameters), n=100) ####

# Read supply parameters table, checking if its delimiter is comma or semicolon ####
read_csv(glue("{master_dir}/LULCC/TempTables/growth_parameters.csv")) %>% 
  {if(is.null(.$TOF[1])) read_csv2(glue("{master_dir}/LULCC/TempTables/growth_parameters.csv")) else .} -> growth_parameters

res<-read.csv("LULCC/TempTables/Resolution.csv", header=T)
resolution<-res[1,2]
userarea_r<-raster("LULCC/TempRaster/mask_c.tif")

#First one shot
locs_c<-raster("In/DemandScenarios/locs_c.tif")
db_locs <- as.data.frame(getValues(locs_c))
db_locs_f<-db_locs[complete.cases(db_locs),]

if (Scenarios==1) {

	setwd(paste0(getwd(),"/In/DemandScenarios"))
	unlink("fwuse_*.csv",force=TRUE)

	DemSce_semicolon<-read.csv(paste0(SceCode,"_fwch.csv"), sep=";", stringsAsFactors=FALSE)
	DemSce_comma<-read.csv(paste0(SceCode,"_fwch.csv"), sep=",", stringsAsFactors=FALSE)
		if (is.null(DemSce_semicolon$X2027_ch_v[1])) { ## Read in the arguments listed at the command line in DINAMICA'S "Run external process"
			DemSce<-DemSce_comma
		} else {
			DemSce<-DemSce_semicolon
		}
	
	lastrow<- nrow(DemSce) 
	lastcol<- ncol(DemSce) 
	DemSce_s<-DemSce[1:lastrow,1:lastcol]
	DemSce_clean <- DemSce_s[DemSce_s$clump_id %in% db_locs_f, ]
	yrs<-first_yr:last_yr #Calibration+Simulation period
	steps_dif<-(last_yr-first_yr)+1	
	steps<-1:steps_dif #Stdyn+1 e.g. 2027-2003=24->24+1=25

	for (i in steps) {
		if (nchar(i)==1) {
			col_fwv<-paste0("X",yrs[i],"_fw_v")
			db_fwV<-as.data.frame(DemSce_clean[ ,c("clump_id",col_fwv)])
			colnames(db_fwV)<-c("Key","Value")

			col_chv<-paste0("X",yrs[i],"_ch_v")
			db_chV<-as.data.frame(DemSce_clean[ ,c("clump_id",col_chv)])
			colnames(db_chV)<-c("Key","Value")
			db_chV$Value<-(as.numeric(db_chV$Value)+as.numeric(db_fwV$Value))/1000
			write.csv(db_chV,paste0("fwuse_V",0,i,".csv"),row.names = FALSE)

			col_w<-paste0("X",yrs[i],"_fw_w")
			db_W<-as.data.frame(DemSce_clean[ ,c("clump_id",col_w)])
			colnames(db_W)<-c("Key","Value")
			db_W$Value<-as.numeric(db_W$Value)/1000
			write.csv(db_W,paste0("fwuse_W",0,i,".csv"),row.names = FALSE)

		} else {

			col_fwv<-paste0("X",yrs[i],"_fw_v")
			db_fwV<-as.data.frame(DemSce_clean[ ,c("clump_id",col_fwv)])
			colnames(db_fwV)<-c("Key","Value")

			col_chv<-paste0("X",yrs[i],"_ch_v")
			db_chV<-as.data.frame(DemSce_clean[ ,c("clump_id",col_chv)])
			colnames(db_chV)<-c("Key","Value")
			db_chV$Value<-(as.numeric(db_chV$Value)+as.numeric(db_fwV$Value))/1000
			write.csv(db_chV,paste0("fwuse_V",i,".csv"),row.names = FALSE)
			
			col_w<-paste0("X",yrs[i],"_fw_w")
			db_W<-as.data.frame(DemSce_clean[ ,c("clump_id",col_w)])
			colnames(db_W)<-c("Key","Value")
			db_W$Value<-as.numeric(db_W$Value)/1000
			write.csv(db_W,paste0("fwuse_W",i,".csv"),row.names = FALSE)
		}
	}

} else {
	"Do Nothing"
}
Sys.sleep(10)
setwd(master_dir)

CtyPar_semicolon<-read.csv("LULCC/SourceData/parameters.csv", sep=";", header=T)
CtyPar_comma<-read.csv("LULCC/SourceData/parameters.csv",  sep=",", header=T)
if (is.null(CtyPar_semicolon$ParCHR[1])) { ## Read in the arguments listed at the command line in DINAMICA'S "Run external process"
  CtyPar<-CtyPar_comma
} else {
  CtyPar<-CtyPar_semicolon
}
CtyPar[] <- lapply(CtyPar, as.character)

data_semicolon<-read.csv("LULCC/TempTables/growth_parameters.csv", sep=";", header=T)
data_comma<-read.csv("LULCC/TempTables/growth_parameters.csv", sep=",", header=T)
if (is.null(data_semicolon$TOF[1])) { 
  data_all<-data_comma
} else {
  data_all<-data_semicolon
}

# Produce TOF vs FOR categories ####
data_FOR<-subset(data_all, data_all$TOF==0)
data_TOF<-subset(data_all, data_all$TOF==1)
max_tot<-nrow(data_all)

dataTOFvsFOR<-as.data.frame(data_all[ ,7])
colnames(dataTOFvsFOR)<-("x")
dataTOFvsFOR=data.frame(Key=c(1:max_tot),dataTOFvsFOR)
write.csv(dataTOFvsFOR,"LULCC/TempTables//TOFvsFOR_Categories.csv",row.names = FALSE)

# # Locs selection code is currently deprecated, update if selecting villages make sense again (i  think it does)
# if (Subset_locs == 1) {
#   mask<-readShapePoly("LULCC/InVector/Extent_Mask",proj4string=CRS(UTMproj))
#   userarea<-readShapePoly("LULCC/TempVector/Extent_Analysis",proj4string=CRS(UTMproj))
#   analysisshp<-crop(userarea, mask)
#   ext<-extent(analysisshp)
#   DEM_c1<-raster("LULCC/TempRaster/DEM_c.tif")
#   ProjExtent<-projectExtent(DEM_c1, UTMproj)
#   analysis_r_codes<-rasterize(analysisshp, DEM_c1, field=ext_analysis_ID,datatype="INT2S",overwrite=TRUE)
#   analysis_r<-(analysis_r_codes*0)+1
#   Ext_Locs_v<-readShapePoints("LULCC/InVector/Extent_Locs")
#   Ext_Locs_clip_v<-crop(Ext_Locs_v,ext)
#   Ext_Locs_clip_r<-rasterize(Ext_Locs_clip_v, DEM_c1, field=Ext_Locs_name_ID,datatype="INT4S",overwrite=TRUE)
#   Ext_Locs_clip_r_c<-Ext_Locs_clip_r*analysis_r
#   writeRaster(Ext_Locs_clip_r_c, filename="LULCC/TempRaster//Ext_Locs_c.tif", datatype="INT4S", overwrite=TRUE)
# } else {
#   "No subset of localities selected"
# }

# Pending, check how if derived datasets when locs are selected are needed or not.
# CHECK CROP LOCKS IN MOFUSS WIN BACKUP




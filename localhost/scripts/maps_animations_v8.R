# MoFuSS
# Version 8
# Date: Aug 2026
# EGOML dependency bundle: V8

# 2dolist
# Check after fNRB partition tables and vectors how it works for country-based and Google Earth polys
# CtyPar<-read.csv("LULCC/DownloadedDatasets/SourceDataGlobal/parameters.csv", header=T)
# CtyPar[] <- lapply(CtyPar, as.character)

# Internal parameters
videoson <- 1 # 1 creates the MP4 animation; no other animation formats are generated
compilelatex <- 1
fNRB_partition_tables <- 1 # required period-level fNRB tables and vector outputs
mcthreshold <- 30
uncertainty_digits <- 2
copy_old_dinamica_rasters <- 0

# Load packages ----
library(animation)
library(data.table)
library(foreach)
library(jpeg)
library(png)
library(raster)
library(sf)
library(tiff)
library(tidyverse)

# Read in the arguments listed at the command line in Dinamica EGO'S "Run external process" ####
args=(commandArgs(TRUE))

# "args" is now a list of character vectors.
# First check to see if arguments are passed.
# Then cycle through each element of the list and evaluate the expressions.
if(length(args)==0){
  print("No arguments supplied by DINAMICA.")
  ##Supply default values here (to be used when running the script through R directly)
  MC = 30 # MonteCarlo runs
  IT = 2000 # Initial year
  K_MC=1
  TOF_MC=1
  Ini_st_MC=75
  Ini_st.factor.percentage=100
  COVER_MAP=1
  rmax_MC=1
  DEF_FW=0
  IL=48 # Iteration length in week - each year = 48 weeks
  # STdyn=10 # Simulation length set by parameters table, it cycles in the repeat functor is STdyn+1 as 2 cycles are needed for 1 year: 1jan->31dec
  Harv.Pix.W=7000000
  Prune.W=1 
  Harv.Pix.V=7000000
  Prune.V=1
  Harv.Pix_MC=0
  Prune_MC=0
  # Subset_locs=0
  MaxAGB=400 # Maximum K for all LULC classes at any MC
  AGBmap=1
  SumTables=0
  OSType=64
  BaUvsICS="BaU"
  RerunMC=1
  cutoff_yrs=10
  DryRun=0
  
}else{
  for(i in 1:length(args)){
    eval(parse(text=args[[i]]))
  }
}

dinamica_stdyn <- if (exists("STdyn", inherits = FALSE)) {
  suppressWarnings(as.integer(STdyn))
} else {
  NA_integer_
}

MC <- as.integer(MC)
if (length(MC) != 1 || is.na(MC) || MC < 1) {
  stop("MC must be one positive integer giving the number of Monte Carlo runs.")
}

# This script intentionally resolves all data paths relative to countrydir.
if (!file.exists("LULCC/TempTables/Country.csv")) {
  stop(
    "Run maps_animations_v8.R from countrydir. Missing: ",
    file.path(getwd(), "LULCC", "TempTables", "Country.csv")
  )
}

# Read parameters table ----
read.csv("LULCC/TempTables/Country.csv") %>%
  dplyr::filter(Key. == "1") %>%
  pull(Country) -> country_name

if (length(country_name) != 1L || is.na(country_name) || !nzchar(trimws(country_name))) {
  stop("Country.csv must contain exactly one non-empty Key.=1 Country value.")
}

# Specify the directory where the file is located
parameters_directory <- paste0(getwd(), "/LULCC/DownloadedDatasets/SourceData", country_name)

# Use list.files() to find the file that matches the pattern
parameters_name <- list.files(path = parameters_directory, pattern = "^parameters.*\\.csv$", full.names = TRUE)

if (length(parameters_name) != 1L) {
  stop(
    "Expected exactly one parameters*.csv in ", parameters_directory,
    "; found ", length(parameters_name), "."
  )
}

# Detect the separator by checking the first line
first_line <- readLines(parameters_name, n = 1)
sep <- if (grepl(";", first_line)) ";" else ","

# Read the CSV file with the detected separator
country_parameters <- read_delim(parameters_name, delim = sep)

if (!all(c("Var", "ParCHR") %in% names(country_parameters))) {
  stop("The parameter table must contain Var and ParCHR columns.")
}

parameter_value <- function(key) {
  value <- country_parameters$ParCHR[country_parameters$Var == key]
  if (length(value) != 1L || is.na(value) || !nzchar(trimws(value))) {
    stop("Expected exactly one non-empty parameter value for: ", key)
  }
  trimws(as.character(value))
}

aoi_poly <- suppressWarnings(as.integer(parameter_value("aoi_poly")))
if (length(aoi_poly) != 1L || is.na(aoi_poly) || !aoi_poly %in% c(0L, 1L)) {
  stop("The aoi_poly parameter must be 0 (administrative AOI) or 1 (custom polygon).")
}

# Print the tibble (up to 30 rows)
print(as_tibble(country_parameters), n = 30)

country_parameters %>%
  dplyr::filter(Var == "LULCt1map") %>%
  pull(ParCHR) -> LULCt1map

country_parameters %>%
  dplyr::filter(Var == "LULCt2map") %>%
  pull(ParCHR) -> LULCt2map

country_parameters %>%
  dplyr::filter(Var == "LULCt3map") %>%
  pull(ParCHR) -> LULCt3map

country_parameters %>%
  dplyr::filter(Var == "AGB1map") %>%
  pull(ParCHR) -> AGB1map

country_parameters %>%
  dplyr::filter(Var == "AGB2map") %>%
  pull(ParCHR) -> AGB2map

country_parameters %>%
  dplyr::filter(Var == "AGB3map") %>%
  pull(ParCHR) -> AGB3map

country_parameters %>%
  dplyr::filter(Var == "end_year") %>%
  pull(ParCHR) %>%
  as.integer(.) -> end_year
STdyn <- end_year - IT

configured_start <- suppressWarnings(as.integer(parameter_value("start_year")))
configured_mc <- suppressWarnings(as.integer(parameter_value("monte_carlo_runs")))
if (anyNA(c(configured_start, end_year, configured_mc))) {
  stop("start_year, end_year, and monte_carlo_runs must be integers.")
}
if (IT != configured_start) {
  stop("Dinamica IT does not match parameters.csv start_year.")
}
if (MC != configured_mc) {
  stop("Dinamica MC does not match parameters.csv monte_carlo_runs.")
}
if (!is.na(dinamica_stdyn) && dinamica_stdyn != STdyn) {
  stop("Dinamica STdyn does not match end_year - start_year in parameters.csv.")
}
if (!STdyn %in% c(20L, 30L, 35L, 40L, 50L)) {
  stop("Supported simulation lengths are 20, 30, 35, 40, or 50 years.")
}
cutoff_yrs <- suppressWarnings(as.integer(cutoff_yrs))
if (length(cutoff_yrs) != 1L || is.na(cutoff_yrs) || cutoff_yrs != 10L) {
  stop("The supported post-spin-up analysis cutoff is exactly 10 years (2010).")
}
SumTables <- suppressWarnings(as.integer(SumTables))
if (length(SumTables) != 1L || is.na(SumTables) || SumTables != 0L) {
  stop("The legacy annual administrative-summary branch is retired; SumTables must be 0.")
}

DryRun <- if (exists("DryRun", inherits = FALSE)) {
  suppressWarnings(as.integer(DryRun))
} else {
  0L
}
if (length(DryRun) != 1L || is.na(DryRun) || !DryRun %in% c(0L, 1L)) {
  stop("DryRun must be 0 or 1.")
}

# Verify the complete Dinamica raster family before producing any map. This
# catches interrupted MC runs and incorrect MC counts before outputs are mixed.
expected_run_dirs <- paste0("debugging_", seq_len(MC))
missing_run_dirs <- expected_run_dirs[!dir.exists(expected_run_dirs)]
if (length(missing_run_dirs)) {
  stop(
    "Missing ", length(missing_run_dirs),
    " expected Monte Carlo run folder(s). First missing: ", missing_run_dirs[[1]]
  )
}
expected_codes <- sprintf("%02d", seq_len(STdyn + 1L))
required_families <- c("Growth", "Growth_less_harv", "Harvest_tot")
expected_run_files <- unlist(lapply(expected_run_dirs, function(run_dir) {
  unlist(lapply(required_families, function(family) {
    file.path(run_dir, paste0(family, expected_codes, ".tif"))
  }), use.names = FALSE)
}), use.names = FALSE)
missing_run_files <- expected_run_files[!file.exists(expected_run_files)]
if (length(missing_run_files)) {
  stop(
    "Missing ", length(missing_run_files),
    " expected Dinamica raster(s). First missing: ", missing_run_files[[1]]
  )
}

if (DryRun == 1L) {
  cat(
    sprintf(
      "[DRY-RUN] maps_animations8 preflight passed: %d MC runs, %d-%d, codes 01-%02d; legacy annual summaries off; period fNRB tables on.\n",
      MC, IT, end_year, STdyn + 1L
    )
  )
  quit(save = "no", status = 0L, runLast = FALSE)
}

res<- read.csv("LULCC/TempTables/Resolution.csv", header=T)
resolution <- res[1,2]
userarea_r <- raster("LULCC/TempRaster/mask_c.tif")

if (BaUvsICS == "ICS") {
  OutDir<-"Out"
} else {
  OutDir<-"Out"
}

if (OSType == 64) {
  ffmpeg_path<-file.path(getwd(),"ffmpeg64/bin/ffmpeg.exe")
} else {
  ffmpeg_path<-file.path(getwd(),"ffmpeg32/bin/ffmpeg.exe")
}

country_parameters %>%
  dplyr::filter(Var == "mapscale") %>%
  pull(ParCHR) %>%
  as.numeric() -> scalebar_loi
label_scalebar_loi<-paste0(scalebar_loi/1000, "km", sep="")

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
print(DEF_FW)
print(IL)
print(STdyn)
print(ST)
print(Harv.Pix.W)
print(Prune.W)
print(Harv.Pix.V)
print(Prune.V)
print(Harv.Pix_MC)
print(Prune_MC)
print(ffmpeg_path)
#print(Subset_locs)
print(MaxAGB)
print(AGBmap)
print(SumTables)
print(OSType)
print(BaUvsICS)

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
  Ini_st.factor.percentage_L = "Tree cover as fraction of K" #Couldn't pass "%" to LaTeX in "\%" form.
} else {
  COVER_MAP_yesno = "No"
  Ini_st.factor.percentage = paste(Ini_st.factor.percentage,"% of K",sep="")
  Ini_st.factor.percentage_L = paste(Ini_st.factor.percentage,"fraction of K",sep="") #Couldn't pass "%" to LaTeX in "\%" form.
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

# if (Subset_locs == 1) {
# 	Subset_locs_yesno = "Yes"
# } else {
# 	Subset_locs_yesno = "No"
# }

if (AGBmap == 1) {
  AGBmap_yesno = "Yes"
  COVER_MAP_yesno = "Not applicable"
  Ini_st_MC_yesno = "Not applicable"
  Ini_st.factor.percentage = "Using AGB map"
  Ini_st.factor.percentage_L = "Using AGB map"
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

if (RerunMC == 1) {
  RerunMC_yesno = "Yes"
} else {
  RerunMC_yesno = "No"
}

# Save tables in csv with input parameters for BaU and ICS scenarios ####

ParDF = data.frame(c(
  "Spatial resolution",
  #"Operating System Type",
  "Type of scenario",
  #"Sample of localities of interest?",
  "StartUp year",
  "Simulation Length (SL)",
  "Number of MC realizations",
  #"Initial Stock",
  #"Initial stock through MC?",
  "K through MC?",
  "rmax through MC?",
  "TOF through MC?",
  #"Iteration length",
  "Re-run Monte Carlo?",
  #"Tree cover map provided?",
  "Aboveground biomass map provided?",
  "Accounting for fuelwood from deforestation?"
),
value=(c(
  paste(resolution," meters",sep=""),
  #paste(OSType,"-bit",sep=""),
  BaUvsICS,
  #Subset_locs_yesno,
  IT,
  paste(STdyn," years",sep=""),
  paste(MC," runs",sep=""),
  #Ini_st.factor.percentage_L,
  #Ini_st_MC_yesno,
  K_MC_yesno,
  rmax_MC_yesno,
  TOF_MC_yesno,
  #paste(IL," weeks (",IL*0.25," months)",sep=""),
  RerunMC_yesno,
  #COVER_MAP_yesno,
  AGBmap_yesno,
  DEF_FW_yesno
)))							

colnames(ParDF) <- c("Parameter",paste("Value",substr(OutDir,4,6),sep=""))

# if (BaUvsICS == "ICS") {
# write.csv(ParDF, "LULCC/TempTables/InputParaICS.csv", row.names=FALSE)
# } else {
write.csv(ParDF, "LULCC/TempTables/InputParaBaU.csv", row.names=FALSE)
#}

#rBaUt<-file.exists("LULCC/TempTables/InputParaBaU.csv")
#rICSt<-file.exists("LULCC/TempTables/InputParaICS.csv")
#if (rBaUt == "TRUE" & rICSt == "TRUE") {
rBaU<-read.csv("LULCC/TempTables/InputParaBaU.csv")
#rICS<-read.csv("LULCC/TempTables/InputParaICS.csv")
#rBauICS <- merge(rBaU,rICS,by="Parameter", sort=FALSE)

write.csv(rBaU, "LULCC/TempTables/InputPara.csv", row.names=FALSE, quote=FALSE)
#write.csv(rBauICS, "LULCC/TempTables/InputPara.csv", row.names=FALSE, quote=FALSE)
# } else { 
# 	"One out of two scenario table parameters is missing"		
# }

# Area of interest map (projected coordinates) ####
country_parameters %>%
  dplyr::filter(Var == "treecover_name") %>%
  pull(ParCHR) -> treecover_name
aoi<-raster(paste("LULCC/DownloadedDatasets/SourceDataGlobal/InRaster/",treecover_name,sep=""))
#aoi_r<-reclassify(aoi,c(-Inf,-1,NA,101,Inf,NA))
#aoi[aoi > 100] = NA
aoi_IniSt<-raster("Temp//2_IniSt01.tif")
mask <- st_read("LULCC/DownloadedDatasets/SourceDataGlobal/InVector/extent_mask.gpkg")
extent_analysis <- st_read("LULCC/TempVector/ext_analysis.gpkg") #ERROR IN READING GEOPACKAGE
aoi_c<-crop(aoi,mask)

Locs_r<-raster("LULCC//TempRaster//locs_c_w.tif") #Check there is locs_c_v as well!
Locs_p<-as.data.frame(rasterToPoints(Locs_r, fun=NULL, n=4, na.rm=TRUE, digits=12, dissolve=TRUE))
coordinates(Locs_p)=c("x", "y")
proj4string(Locs_p) <- crs(Locs_r)

tiff(filename=paste0(OutDir,"//Area_of_Interest.tif"),
     width=290,height=290,units="mm",res=res300,bg="white",
     compression=c("lzw"),type=c("windows"),
     pointsize=12,family="",restoreConsole=TRUE)
plot(aoi_c, main="Area of Interest: set by user (red polygon)",
     ylab="Projected S-N coordinate",
     xlab="Projected W-E coordinate",
     cex.main=1.5,
     legend=TRUE, legend.width=2.5,  
     legend.args=list(text=bquote(Tree~cover~"(%)"~year~2000),side=4, font=2, line=-1.35, cex=1))
#legend.args=list(text=bquote(Tree~cover~"(%)"~year~ .(IT)),side=4, font=2, line=-1.35, cex=1))
plot(mask, border="black", col="transparent", add=TRUE, lwd = 2)
#plot(locs_figures_GCS, pch=19, cex=(0.25), add=TRUE) #Plotea localidades de interes en el ?rea de an?lisis
plot(extent_analysis, border="red", col="transparent", add=TRUE, lwd = 2.5)

par(new=TRUE, 
    plt=c(0,1,0,1), 
    mar=c(42,4.2,4.2,42), 
    usr=c(0,1,0,1)
)
greypallete<-gray.colors(255, start = 0.15, end = 1, gamma = 2, alpha = 1)
# image((raster(Globe_name)), col=greypallete, axes=FALSE, ann=FALSE) #GLOBE_NAME SHOULD COME FROM MOFUSS GITLAB
dev.off()


# Localities of Interest (projected coordinates) ####

if (raster::isLonLat(aoi_IniSt)) {
  stop("Map units must be projected linear units before converting biomass to t/ha.")
}
Areaadj <- abs(xres(aoi_IniSt) * yres(aoi_IniSt)) / 10000
if (!is.finite(Areaadj) || Areaadj <= 0) stop("Invalid raster cell area.")
MaxAGB_initial<-(cellStats(aoi_IniSt,max)/Areaadj)

# if (Subset_locs == 1) {
# 	locs_figures<-Extent_Locs_p
# } else {
locs_figures<-Locs_p
#}

tiff(filename=paste(OutDir,"//Localities_of_Interest.tif",sep=""),width=290,height=290,units="mm",res=res300,bg="white",compression=c("lzw"),
     type=c("windows"),pointsize=12,family="",restoreConsole=TRUE)
plot((aoi_IniSt/Areaadj), main="Localities of Interest: set by user",ylab="Projected S-N coordinate",xlab="Projected W-E coordinate",cex.main=1.5,
     legend=TRUE, legend.width=2.5, 
     #legend.args=list(text=expression("Aboveground Biomass (t ha"^-1*") circa year 2000"),side=4, font=2, line=-1.35, cex=1),
     legend.args=list(text=bquote("Aboveground Biomass (t ha"^-1*") circa year"~.(IT)),side=4, font=2, line=-1.35, cex=1),
     zlim=c(0,MaxAGB_initial))
plot(locs_figures, pch=19, cex=(0.05), add=TRUE)
scalebar(scalebar_loi,type='line', divs=4, lwd=2.5, label=label_scalebar_loi, cex=1.5)
plot(extent_analysis, border="red", col="transparent", add=TRUE, lwd = 0.25)

#par(new=TRUE, plt=c(0,1,0,1), mar=c(42,4.2,4.2,42), usr=c(0,1,0,1))
#greypallete<-gray.colors(255, start = 0.15, end = 1, gamma = 2, alpha = 1)
#image((raster(HondurasGlobe_name)), col=greypallete, axes=FALSE, ann=FALSE)
dev.off()


# Shared temporal metadata ####
# Last_STdyn is used by static maps as well as the optional MP4.
Last_STdyn <- STdyn
analysis_start_year <- IT + cutoff_yrs
analysis_start_code <- cutoff_yrs + 1L
end_code <- STdyn + 1L
code_text <- function(x) sprintf("%02d", as.integer(x))


# Map AGB ####

runagbmap <- 1

if(runagbmap == 1){
  
  graphics.off()
  tiff(filename=paste(OutDir,"//Map_AGB.tif",sep=""),width=170,height=200,units="mm",res=res600,bg="white",compression=c("lzw"),type=c("windows"),pointsize=12,family="",restoreConsole=TRUE)
  
  par(mfrow = c(3, 2), oma=c(1.5,1.5,0,1), mar=c(3,3,4,1))
  
  # Graphic parameters AGB map
  mainsize <- 1.1
  legwidth <- 2.5
  axissize <- 1
  labelsize <- 0.75
  barline <- (-1.1)
  redline_wd <- 0.5
  # Define a color gradient from white to red
  color_pal <- colorRampPalette(c("white", "orange", "red"))
  # Create a color gradient from white to red
  colors <- color_pal(100)  # Create 100 intermediate colors
  
  start_agb_path <- file.path(
    "debugging_1", paste0("Growth_less_harv", code_text(analysis_start_code), ".tif")
  )
  nrb_baseline_path <- file.path(
    "debugging_1", paste0("Growth", code_text(analysis_start_code), ".tif")
  )
  end_agb_path <- file.path(
    "debugging_1", paste0("Growth_less_harv", code_text(end_code), ".tif")
  )
  required_map_inputs <- c(start_agb_path, nrb_baseline_path, end_agb_path)
  missing_map_inputs <- required_map_inputs[!file.exists(required_map_inputs)]
  if (length(missing_map_inputs)) {
    stop("Missing required static-map raster: ", missing_map_inputs[[1]])
  }

  bal_t0_maxAGB <- cellStats(raster(start_agb_path), max)
  bal_tn_maxAGB <- cellStats(raster(end_agb_path), max)
  MaxAGB_1stMC_bind<-cbind(bal_t0_maxAGB,bal_tn_maxAGB)
  MaxAGB_1stMC <- ((max(MaxAGB_1stMC_bind[1, ], na.rm=TRUE))/Areaadj)
  
  bal_t0 <- raster(start_agb_path)
  plot((bal_t0/Areaadj), main=paste0("Aboveground Biomass ",analysis_start_year),cex.main=mainsize, useRaster=TRUE,
       legend=TRUE, legend.width=legwidth, legend.shrink=1,cex.axis=axissize,
       legend.args=list(text=expression("t ha"^-1*""),side=4, font=2, line=barline, cex=labelsize),zlim=c(0,MaxAGB_1stMC))
  plot(extent_analysis, border="red", col="transparent", add=TRUE, lwd = redline_wd)
  
  bal_tn <- raster(end_agb_path)
  plot((bal_tn/Areaadj), main=paste0("Aboveground Biomass ",(IT+as.numeric(Last_STdyn))),cex.main=mainsize, useRaster=TRUE,
       legend=TRUE, legend.width=legwidth, legend.shrink=1,cex.axis=axissize,
       legend.args=list(text=expression("t ha"^-1*""),
                        side=4, font=2, line=barline, cex=labelsize),zlim=c(0,MaxAGB_1stMC),
       scalebar(scalebar_loi,type='line', divs=4, lwd=1.25, label=label_scalebar_loi, cex=1)
  )
  plot(extent_analysis, border="red", col="transparent", add=TRUE, lwd = redline_wd)
  
  # NRB uses standing biomass immediately before the first included harvest,
  # matching stage-1's v3-compatible 2010-to-end definition.
  nrb_baseline <- raster(nrb_baseline_path)
  agbdiff <- bal_tn - nrb_baseline
  NRBneg <- calc(agbdiff, fun = function(x) { ifelse(x >= 0, 0, x) })
  NRB <- NRBneg*-1
  NRBmax<-(cellStats(NRB,max)/Areaadj)
  if(NRBmax == 0){
    plot((NRB/Areaadj), main=paste0("NRB: period ",analysis_start_year," to ",(IT+as.numeric(Last_STdyn))),
         col=colors,  # Apply the custom color palette
         useRaster=TRUE,
         legend=TRUE, legend.width=legwidth, legend.shrink=1,cex.axis=axissize,
         legend.args=list(text=expression("t ha"^-1*""),
                          side=4, font=2, line=barline, cex=labelsize),
         zlim=c(0,1))
    plot(extent_analysis, border="red", col="transparent", add=TRUE, lwd = redline_wd)
  } else {
    plot((NRB/Areaadj), main=paste0("NRB: period ", analysis_start_year, " to ", (IT + as.numeric(Last_STdyn))),
         col=colors,  # Apply the custom color palette
         useRaster=TRUE,  # Use raster method for plotting if suitable
         legend=TRUE, legend.width=legwidth, legend.shrink=1, cex.axis=axissize,
         legend.args=list(text=expression("t ha"^-1*""), side=4, font=2, line=barline, cex=labelsize),
         zlim=c(0, NRBmax))  # Control the scale of the color gradient
    plot(extent_analysis, border="red", col="transparent", add=TRUE, lwd = redline_wd)
    
  }
  
  # Define the range of your raster files
  file_numbers <- analysis_start_code:end_code
  # Create a vector of file paths
  file_paths <- file.path(
    "debugging_1", paste0("Harvest_tot", code_text(file_numbers), ".tif")
  )
  missing_harvest_inputs <- file_paths[!file.exists(file_paths)]
  if (length(missing_harvest_inputs)) {
    stop("Missing required harvest raster: ", missing_harvest_inputs[[1]])
  }
  # Load all rasters into a stack
  raster_stack <- stack(file_paths)
  # Sum all the rasters in the stack
  summed_harvest <- sum(raster_stack)
  fNRB <- overlay(
    NRB,
    summed_harvest,
    fun = function(nrb, harvest) {
      ifelse(is.na(nrb) | is.na(harvest) | harvest <= 0, NA_real_, 100 * nrb / harvest)
    }
  )
  
  # fNRB<-(raster("Temp//2_fNRB01.tif"))*100 # fNRB for the entire simulation period for 1st MC run
  plot(fNRB, main=paste0("fNRB: period ",analysis_start_year," to ",(IT+as.numeric(Last_STdyn))),
       col=colors,  # Apply the custom color palette
       useRaster=TRUE,  # Use raster method for plotting if suitable
       legend=TRUE, legend.width=legwidth, legend.shrink=1, cex.axis=axissize,
       legend.args=list(text=expression("%"), side=4, font=2, line=barline, cex=labelsize),
       zlim=c(0,100))  # Control the scale of the color gradient
  plot(extent_analysis, border="red", col="transparent", add=TRUE, lwd = redline_wd)
  
  
  # WARNING!!! Fw_def_tot = 2010-2030 ----
  Fw_def_tot<-(raster("Temp//2_FW_DEF01.tif")) # Cumulative fuelwod from deforestation for simulation period for 1st MC
  Fw_defmax<-(cellStats(Fw_def_tot,max)/Areaadj)
  if(Fw_defmax == 0){	
    plot((Fw_def_tot/Areaadj), main=paste0("Fuelwood from deforestation: period ",analysis_start_year," to ",(IT+as.numeric(Last_STdyn))), 
         cex.main=mainsize, 
         useRaster=TRUE,
         legend=TRUE, legend.width=legwidth, legend.shrink=1,cex.axis=axissize,
         legend.args=list(text=expression("t ha"^-1*""), side=4, font=2, line=barline, cex=labelsize),
         zlim=c(0,1))
    plot(extent_analysis, border="red", col="transparent", add=TRUE, lwd = redline_wd)
  } else {
    plot((Fw_def_tot/Areaadj), main=paste0("Fuelwood from deforestation: period ",analysis_start_year," to ",(IT+as.numeric(Last_STdyn))),
         cex.main=mainsize, 
         useRaster=TRUE,
         legend=TRUE, legend.width=legwidth, legend.shrink=1,cex.axis=axissize,
         legend.args=list(text=expression("t ha"^-1*""), side=4, font=2, line=barline, cex=labelsize),zlim=c(0,Fw_defmax))
    plot(extent_analysis, border="red", col="transparent", add=TRUE, lwd = redline_wd)
  }
  
  # Harv_tot <- (raster("Temp//2_CON_TOT01.tif")) # Cumulative fuelwood harvest for simulation period for 1st MC
  Harv_tot <- summed_harvest
  Harv_totmax<-(cellStats(Harv_tot,max)/Areaadj)
  if(Harv_totmax == 0){
    plot((Harv_tot/Areaadj), main=paste0("Harvested fuelwood: period ",analysis_start_year," to ",(IT+as.numeric(Last_STdyn))),
         col=colors,  # Apply the custom color palette 
         useRaster=TRUE,
         legend=TRUE, legend.width=legwidth, legend.shrink=1,cex.axis=axissize,
         legend.args=list(text=expression("t ha"^-1*""), side=4, font=2, line=barline, cex=labelsize),
         zlim=c(0,1))
    # plot(locs_figures, pch=19, cex=(0.10), add=TRUE)
    plot(extent_analysis, border="red", col="transparent", add=TRUE, lwd = redline_wd)
  } else {
    plot((Harv_tot/Areaadj), main=paste0("Harvested fuelwood: period ",analysis_start_year," to ",(IT+as.numeric(Last_STdyn))),
         col=colors,  # Apply the custom color palette
         useRaster=TRUE,
         legend=TRUE, legend.width=legwidth, legend.shrink=1,cex.axis=axissize,
         legend.args=list(text=expression("t ha"^-1*""), side=4, font=2, line=barline, cex=labelsize),
         zlim=c(0,Harv_totmax))
    # plot(locs_figures, pch=19, cex=(0.10), add=TRUE)
    plot(extent_analysis, border="red", col="transparent", add=TRUE, lwd = redline_wd)
  }
  
  title(ylab="Projected S-N coordinate", xlab="Projected W-E coordinate\n",sub="Showing first Monte Carlo run",outer=TRUE, line=0.35)
  dev.off()
}

# Animations ####
if (videoson == 1){
  # MP4-only temporal setup. The video uses the first Monte Carlo run because
  # its source rasters are stored in debugging_1.
  frame_ids <- sprintf("%02d", seq_len(ST))
  bal_names <- file.path("debugging_1", paste0("Growth_less_harv", frame_ids, ".tif"))
  harv_tot_names <- file.path("debugging_1", paste0("Harvest_tot", frame_ids, ".tif"))
  frame_years <- IT - 1 + trunc(seq(1, STdyn + 1, length.out = ST))

  missing_animation_files <- c(bal_names, harv_tot_names)[
    !file.exists(c(bal_names, harv_tot_names))
  ]
  if (length(missing_animation_files) > 0) {
    stop(
      "Cannot create MP4; missing ", length(missing_animation_files),
      " animation raster(s). First missing file: ", missing_animation_files[1]
    )
  }

  harvest_layer_maxima <- cellStats(stack(harv_tot_names), max, na.rm = TRUE)
  HarMAX <- max(harvest_layer_maxima, na.rm = TRUE) / Areaadj
  if (!is.finite(HarMAX) || HarMAX <= 0) {
    HarMAX <- 1
  }

  # MP4 video ####
  if (!file.exists(ffmpeg_path)) {
    stop("Cannot create MP4; ffmpeg was not found at: ", ffmpeg_path)
  }
  Video_path <- file.path(
    getwd(), OutDir, paste0("Growth_Harvest_Ani", OutDir, ".mp4")
  )

  saveVideo(expr = {
    par(mfrow = c(1, 2), oma = c(1, 1, 1, 1), mar = c(4, 4, 2, 1))
    for (i in seq_len(ST)) {
      plot((raster(harv_tot_names[i])/Areaadj),
           main = paste("Annually harvested fuelwood", frame_years[i]),
           cex.main = 1.5,
           col=colors,
           legend=TRUE, legend.width=6, legend.shrink=0.4, cex.axis=1,
           legend.args=list(text=expression("t ha"^-1*"yr"^-1*""),side=4, font=2, line=(-1.35), cex=1.25),
           zlim=c(0,HarMAX))
      scalebar(scalebar_loi,type='line', divs=4, lwd=1.5, label=label_scalebar_loi, cex=1.15)
      plot(extent_analysis, border="red", col="transparent", add=TRUE, lwd = redline_wd)
      title(xlab="Projected W-E coordinate\nShowing first Monte Carlo run",ylab="Projected S-N coordinate", outer=TRUE, line=-0.5)
      
      plot((raster(bal_names[i])/Areaadj),
           main = paste("Aboveground Biomass", frame_years[i]),
           cex.main = 1.5,
           legend=TRUE, legend.width=6,legend.shrink=0.4, cex.axis=1,
           legend.args=list(text=expression("t ha"^-1*""),side=4, font=2, line=(-1.35), cex=1.25),
           zlim=c(0,MaxAGB_1stMC))
      plot(extent_analysis, border="red", col="transparent", add=TRUE, lwd = redline_wd)
    }
  },
  ffmpeg = ffmpeg_path,
  other.opts = "-c:v libx264 -r 30000/1001 -b:v 6M -vf scale=iw*3:ih*3 -pix_fmt yuv420p",
  video.name = Video_path, overwrite = TRUE,
  ani.height = 700, ani.width = 1200, interval = 1, nmax = ST)

  if (!file.exists(Video_path)) {
    stop("MP4 generation finished without creating the expected file: ", Video_path)
  }
  message("MP4 animation written to: ", Video_path)
}


# Summary tables ####

if (SumTables == 1) {
  
  dir.create("Out/webmofuss_results/") 
  
  country_parameters %>%
    dplyr::filter(Var == "ext_analysis_ID") %>%
    pull(ParCHR) -> ext_analysis_ID
  
  country_parameters %>%
    dplyr::filter(Var == "ext_analysis_NAME") %>%
    pull(ParCHR) -> ext_analysis_NAME
  
  country_parameters %>%
    dplyr::filter(Var == "ext_analysis_ID_1") %>%
    pull(ParCHR) -> ext_analysis_ID_1
  
  country_parameters %>%
    dplyr::filter(Var == "ext_analysis_NAME_1") %>%
    pull(ParCHR) -> ext_analysis_NAME_1
  
  country_parameters %>%
    dplyr::filter(Var == "ext_analysis_ID_2") %>%
    pull(ParCHR) -> ext_analysis_ID_2
  
  country_parameters %>%
    dplyr::filter(Var == "ext_analysis_NAME_2") %>%
    pull(ParCHR) -> ext_analysis_NAME_2
  
  userarea_gpkg <- st_read("LULCC/TempVector/userarea.gpkg") 
  userarea_df <- userarea_gpkg %>% st_drop_geometry()
  
  admin <- raster("LULCC/TempRaster//admin_c.tif")
  StackNRB[StackNRB == 0] = NA #This is to sum and average only positive NRB values
  NRBzon_sum <- as.data.frame(zonal(StackNRB, admin, 'sum')) %>%
    as.data.table()
  NRBzon_sum_1mc <- as.data.frame(zonal(StackNRB[[1]], admin, 'sum')) %>%
    as.data.table() %>%
    setnames(.,"sum", "NRB_1MC")
  # zonal(StackNRB, admin, 'mean')
  # zonal(StackNRB, admin, 'sd')
  
  NRBzon_mean <- NRBzon_sum[, list(NRB_MC_mean = rowMeans(.SD)), by = zone]
  NRBzon_sd <- NRBzon_sum[, list(NRB_MC_sd = rowSds(.SD)), by = zone]
  
  NRBzon_sum_m2 <- merge(userarea_df, NRBzon_sum_1mc, by.x = ext_analysis_ID, by.y = "zone") %>%
    merge(NRBzon_mean, by.x = ext_analysis_ID, by.y = "zone") %>%
    merge(NRBzon_sd, by.x = ext_analysis_ID, by.y = "zone")
  
  StackCON_TOT[StackCON_TOT == 0] = NA 
  CON_TOTzon_sum <- as.data.frame(zonal(StackCON_TOT, admin, 'sum')) %>%
    as.data.table()
  CON_TOTzon_sum_1mc <- as.data.frame(zonal(StackCON_TOT[[1]], admin, 'sum')) %>%
    as.data.table() %>%
    setnames(.,"sum", "CON_TOT_1MC")
  #zonal(StackCON_TOT, admin, 'mean')
  #zonal(StackCON_TOT, admin, 'sd')
  
  CON_TOTzon_mean <- CON_TOTzon_sum[, list(CON_TOT_MC_mean = rowMeans(.SD)), by = zone]
  CON_TOTzon_sd <- CON_TOTzon_sum[, list(CON_TOT_MC_sd = rowSds(.SD)), by = zone]
  
  CON_TOTzon_sum_m2 <- merge(userarea_df, CON_TOTzon_sum_1mc, by.x = ext_analysis_ID, by.y = "zone") %>%
    merge(CON_TOTzon_mean, by.x = ext_analysis_ID, by.y = "zone") %>%
    merge(CON_TOTzon_sd, by.x = ext_analysis_ID, by.y = "zone")
  
  StackCON_NRB[StackCON_NRB == 0] = NA
  CON_NRBzon_sum <- as.data.frame(zonal(StackCON_NRB, admin, 'sum')) %>%
    as.data.table()
  CON_NRBzon_sum_1mc <- as.data.frame(zonal(StackCON_NRB[[1]], admin, 'sum')) %>%
    as.data.table() %>%
    setnames(.,"sum", "CON_NRB_1MC")
  #zonal(StackCON_NRB, admin, 'mean')
  #zonal(StackCON_NRB, admin, 'sd')
  
  CON_NRBzon_mean <- CON_NRBzon_sum[, list(CON_NRB_MC_mean = rowMeans(.SD)), by = zone]
  CON_NRBzon_sd <- CON_NRBzon_sum[, list(CON_NRB_MC_sd = rowSds(.SD)), by = zone]
  
  CON_NRBzon_sum_m2 <- merge(userarea_df, CON_NRBzon_sum_1mc, by.x = ext_analysis_ID, by.y = "zone") %>%
    merge(CON_NRBzon_mean, by.x = ext_analysis_ID, by.y = "zone") %>%
    merge(CON_NRBzon_sd, by.x = ext_analysis_ID, by.y = "zone")
  
  NRB_fNRB2x <- merge(NRBzon_sum_m2, CON_TOTzon_sum_m2, by.x = ext_analysis_ID, by.y = ext_analysis_ID) %>%
    merge(CON_NRBzon_sum_m2, by.x = ext_analysis_ID, by.y = ext_analysis_ID) %>%
    dplyr::select(ext_analysis_ID,
                  paste0(ext_analysis_NAME,".x"),
                  "NRB_1MC","NRB_MC_mean","NRB_MC_sd",
                  "CON_TOT_1MC", "CON_TOT_MC_mean", "CON_TOT_MC_sd",
                  "CON_NRB_1MC", "CON_NRB_MC_mean", "CON_NRB_MC_sd") %>%
    dplyr::mutate(fNRB1mc = round(NRB_1MC / CON_TOT_1MC * 100)) %>%
    dplyr::mutate(fNRB = round(NRB_MC_mean / CON_TOT_MC_mean * 100)) %>%
    dplyr::mutate(fNRB_sd = round(sqrt(((NRB_MC_sd/NRB_MC_mean)^2) + ((CON_TOT_MC_sd/CON_TOT_MC_mean)^2))*100)) %>%
    dplyr::mutate(fNRB1mc_nrb = round(NRB_1MC / CON_NRB_1MC * 100)) %>%
    dplyr::mutate(fNRB_nrb = round(NRB_MC_mean / CON_NRB_MC_mean * 100)) %>%
    dplyr::mutate(fNRB_nrb_sd = round(sqrt(((NRB_MC_sd/NRB_MC_mean)^2) + ((CON_NRB_MC_sd/CON_NRB_MC_mean)^2))*100)) %>%
    dplyr::mutate(across(3:11) / 1000) %>%
    dplyr::mutate(across(3:11, round, 0)) %>%
    dplyr::rename(ADM_0 = NAME_0.x) %>%
    dplyr::mutate(across(3:17, as.integer))
  
  str(NRB_fNRB2x)
  names(NRB_fNRB2x)
  
  if (MC < mcthreshold) {
    NRB_fNRB2 <- NRB_fNRB2x %>%
      dplyr::mutate(across(ends_with(c("sd", "se")), ~ NA))
  } else {
    NRB_fNRB2 <- NRB_fNRB2x
  }
  
  
  write.csv(NRB_fNRB2, "LULCC/TempTables/summary_adm0.csv", row.names=FALSE, quote=FALSE)
  write.csv(NRB_fNRB2, "Out/webmofuss_results/summary_adm0.csv", row.names=FALSE, quote=FALSE)
  
  NRB_fNRB2annual <- NRB_fNRB2 %>%
    dplyr::mutate(NRB_yr = round(NRB_MC_mean/ST,0)) %>%
    dplyr::mutate(CON_TOT_yr = round(CON_TOT_MC_mean/ST,0)) %>%
    dplyr::mutate(CON_NRB_yr = round(CON_NRB_MC_mean/ST,0))
  
  write.csv(NRB_fNRB2annual, "LULCC/TempTables/summary_adm0_yr.csv", row.names=FALSE, quote=FALSE)
  write.csv(NRB_fNRB2annual, "Out/webmofuss_results/summary_adm0_yr.csv", row.names=FALSE, quote=FALSE)
  
  # Produce simplified shapefile for webmofuss
  userarea0_simpx <- userarea_gpkg %>%
    inner_join(.,NRB_fNRB2, by="ID") %>%
    # ms_simplify(sys = TRUE) %>%
    dplyr::select(# GID_0,
      ID,
      NAME_0,
      # Subregion,
      # mofuss_reg,
      # NAME_0.x,
      # NAME_1.x,
      # NAME_2.x,
      NRB_1MC,
      NRB_MC_mean,
      NRB_MC_sd,
      CON_TOT_1MC,
      CON_TOT_MC_mean,
      CON_TOT_MC_sd,
      CON_NRB_1MC,
      CON_NRB_MC_mean,
      CON_NRB_MC_sd,
      fNRB1mc,
      fNRB, 
      fNRB_sd, 
      fNRB1mc_nrb,
      fNRB_nrb,
      fNRB_nrb_sd) %>%
    dplyr::rename(ADM0 = NAME_0,
                  # ADM1 = NAME_1.x,
                  # ADM2 = NAME_2.x,
                  NRB1mc = NRB_1MC,
                  NRBm = NRB_MC_mean,
                  NRBsd = NRB_MC_sd,
                  D1mc = CON_TOT_1MC,
                  Dm = CON_TOT_MC_mean,
                  Dsd = CON_TOT_MC_sd,
                  Dnrb_1mc = CON_NRB_1MC,
                  Dnrb_m= CON_NRB_MC_mean,
                  Dnrb_sd = CON_NRB_MC_sd,
                  fNRB2_1mc = fNRB1mc_nrb,
                  fNRB2 = fNRB_nrb, 
                  fNRB2_sd = fNRB_nrb_sd) %>%
    replace(is.na(.), 0)
  
  if (MC < mcthreshold) {
    userarea0_simp <- userarea0_simpx %>%
      dplyr::mutate(across(ends_with(c("sd", "se")), ~ NA))
  } else {
    userarea0_simp <- userarea0_simpx
  }
  
  
  st_write(userarea0_simp, "Out/webmofuss_results/mofuss_regions0.gpkg", delete_layer = TRUE)
  
  if(file.exists("LULCC/TempVector/userarea1.gpkg")){
    userarea_gpkg1 <- st_read("LULCC/TempVector/userarea1.gpkg")
    userarea_df1 <- userarea_gpkg1 %>% st_drop_geometry()
    
    admin1 <- raster("LULCC/TempRaster//admin_c1.tif")
    # StackNRB[StackNRB == 0] = NA #This is to sum and average only positive NRB values
    NRBzon_sum1 <- as.data.frame(zonal(StackNRB, admin1, 'sum')) %>%
      as.data.table()
    NRBzon_sum_1mc1 <- as.data.frame(zonal(StackNRB[[1]], admin1, 'sum')) %>%
      as.data.table() %>%
      setnames(.,"sum", "NRB_1MC")
    # zonal(StackNRB, admin, 'mean')
    # zonal(StackNRB, admin, 'sd')
    
    NRBzon_mean1 <- NRBzon_sum1[, list(NRB_MC_mean = rowMeans(.SD)), by = zone]
    NRBzon_sd1 <- NRBzon_sum1[, list(NRB_MC_sd = rowSds(.SD)), by = zone]
    
    NRBzon_sum_m21 <- merge(userarea_df1, NRBzon_sum_1mc1, by.x = ext_analysis_ID_1, by.y = "zone") %>%
      merge(NRBzon_mean1, by.x = ext_analysis_ID_1, by.y = "zone") %>%
      merge(NRBzon_sd1, by.x = ext_analysis_ID_1, by.y = "zone")
    
    # StackCON_TOT[StackCON_TOT == 0] = NA 
    CON_TOTzon_sum1 <- as.data.frame(zonal(StackCON_TOT, admin1, 'sum')) %>%
      as.data.table()
    CON_TOTzon_sum_1mc1 <- as.data.frame(zonal(StackCON_TOT[[1]], admin1, 'sum')) %>%
      as.data.table() %>%
      setnames(.,"sum", "CON_TOT_1MC")
    #zonal(StackCON_TOT, admin, 'mean')
    #zonal(StackCON_TOT, admin, 'sd')
    
    CON_TOTzon_mean1 <- CON_TOTzon_sum1[, list(CON_TOT_MC_mean = rowMeans(.SD)), by = zone]
    CON_TOTzon_sd1 <- CON_TOTzon_sum1[, list(CON_TOT_MC_sd = rowSds(.SD)), by = zone]
    
    CON_TOTzon_sum_m21 <- merge(userarea_df1, CON_TOTzon_sum_1mc1, by.x = ext_analysis_ID_1, by.y = "zone") %>%
      merge(CON_TOTzon_mean1, by.x = ext_analysis_ID_1, by.y = "zone") %>%
      merge(CON_TOTzon_sd1, by.x = ext_analysis_ID_1, by.y = "zone")
    
    # StackCON_NRB[StackCON_NRB == 0] = NA
    CON_NRBzon_sum1 <- as.data.frame(zonal(StackCON_NRB, admin1, 'sum')) %>%
      as.data.table()
    CON_NRBzon_sum_1mc1 <- as.data.frame(zonal(StackCON_NRB[[1]], admin1, 'sum')) %>%
      as.data.table() %>%
      setnames(.,"sum", "CON_NRB_1MC")
    #zonal(StackCON_NRB, admin, 'mean')
    #zonal(StackCON_NRB, admin, 'sd')
    
    CON_NRBzon_mean1 <- CON_NRBzon_sum1[, list(CON_NRB_MC_mean = rowMeans(.SD)), by = zone]
    CON_NRBzon_sd1 <- CON_NRBzon_sum1[, list(CON_NRB_MC_sd = rowSds(.SD)), by = zone]
    
    CON_NRBzon_sum_m21 <- merge(userarea_df1, CON_NRBzon_sum_1mc1, by.x = ext_analysis_ID_1, by.y = "zone") %>%
      merge(CON_NRBzon_mean1, by.x = ext_analysis_ID_1, by.y = "zone") %>%
      merge(CON_NRBzon_sd1, by.x = ext_analysis_ID_1, by.y = "zone")
    
    NRB_fNRB21x <- merge(NRBzon_sum_m21, CON_TOTzon_sum_m21, by.x = ext_analysis_ID_1, by.y = ext_analysis_ID_1) %>%
      merge(CON_NRBzon_sum_m21, by.x = ext_analysis_ID_1, by.y = ext_analysis_ID_1) %>%
      dplyr::select(ext_analysis_ID_1,
                    paste0(ext_analysis_NAME,".x"),
                    paste0(ext_analysis_NAME_1,".x"),
                    "NRB_1MC","NRB_MC_mean","NRB_MC_sd",
                    "CON_TOT_1MC", "CON_TOT_MC_mean", "CON_TOT_MC_sd",
                    "CON_NRB_1MC", "CON_NRB_MC_mean", "CON_NRB_MC_sd") %>%
      dplyr::mutate(fNRB1mc = round(NRB_1MC / CON_TOT_1MC * 100)) %>%
      dplyr::mutate(fNRB = round(NRB_MC_mean / CON_TOT_MC_mean * 100)) %>%
      dplyr::mutate(fNRB_sd = round(sqrt(((NRB_MC_sd/NRB_MC_mean)^2) + ((CON_TOT_MC_sd/CON_TOT_MC_mean)^2))*100)) %>%
      dplyr::mutate(fNRB1mc_nrb = round(NRB_1MC / CON_NRB_1MC * 100)) %>%
      dplyr::mutate(fNRB_nrb = round(NRB_MC_mean / CON_NRB_MC_mean * 100)) %>%
      dplyr::mutate(fNRB_nrb_sd = round(sqrt(((NRB_MC_sd/NRB_MC_mean)^2) + ((CON_NRB_MC_sd/CON_NRB_MC_mean)^2))*100)) %>%
      dplyr::mutate(across(4:12) / 1000) %>%
      dplyr::mutate(across(4:12, round, 0)) %>%
      dplyr::rename(ADM_0 = NAME_0.x,
                    ADM_1 = NAME_1.x) %>%
      dplyr::mutate(across(all_of(4:18), as.integer))
    
    str(NRB_fNRB21x)
    names(NRB_fNRB21x)
    
    if (MC < mcthreshold) {
      NRB_fNRB21 <- NRB_fNRB21x %>%
        dplyr::mutate(across(ends_with(c("sd", "se")), ~ NA))
    } else {
      NRB_fNRB21 <- NRB_fNRB21x
    }
    
    write.csv(NRB_fNRB21, "LULCC/TempTables/summary_adm1.csv", row.names=FALSE, quote=FALSE)
    write.csv(NRB_fNRB21, "Out/webmofuss_results/summary_adm1.csv", row.names=FALSE, quote=FALSE)
    
    NRB_fNRB21annual <- NRB_fNRB21 %>%
      dplyr::mutate(NRB_yr = round(NRB_MC_mean/ST,0)) %>%
      dplyr::mutate(CON_TOT_yr = round(CON_TOT_MC_mean/ST,0)) %>%
      dplyr::mutate(CON_NRB_yr = round(CON_NRB_MC_mean/ST,0))
    write.csv(NRB_fNRB21annual, "LULCC/TempTables/summary_adm1_yr.csv", row.names=FALSE, quote=FALSE)
    write.csv(NRB_fNRB21annual, "Out/webmofuss_results/summary_adm1_yr.csv", row.names=FALSE, quote=FALSE)
    
    # Produce simplified shapefile for webmofuss
    userarea1_simpx <- userarea_gpkg1 %>%
      inner_join(.,NRB_fNRB21, by="ID") %>%
      # ms_simplify(sys = TRUE) %>%
      dplyr::select(# GID_0,
        ID,
        NAME_0,
        # Subregion,
        # mofuss_reg,
        # NAME_0.x,
        NAME_1,
        # NAME_2.x,
        NRB_1MC,
        NRB_MC_mean,
        NRB_MC_sd,
        CON_TOT_1MC,
        CON_TOT_MC_mean,
        CON_TOT_MC_sd,
        CON_NRB_1MC,
        CON_NRB_MC_mean,
        CON_NRB_MC_sd,
        fNRB1mc,
        fNRB, 
        fNRB_sd, 
        fNRB1mc_nrb,
        fNRB_nrb,
        fNRB_nrb_sd) %>%
      dplyr::rename(ADM0 = NAME_0,
                    ADM1 = NAME_1,
                    # ADM2 = NAME_2.x,
                    NRB1mc = NRB_1MC,
                    NRBm = NRB_MC_mean,
                    NRBsd = NRB_MC_sd,
                    D1mc = CON_TOT_1MC,
                    Dm = CON_TOT_MC_mean,
                    Dsd = CON_TOT_MC_sd,
                    Dnrb_1mc = CON_NRB_1MC,
                    Dnrb_m= CON_NRB_MC_mean,
                    Dnrb_sd = CON_NRB_MC_sd,
                    fNRB2_1mc = fNRB1mc_nrb,
                    fNRB2 = fNRB_nrb, 
                    fNRB2_sd = fNRB_nrb_sd) %>%
      replace(is.na(.), 0)
    
    if (MC < mcthreshold) {
      userarea1_simp <- userarea1_simpx %>%
        dplyr::mutate(across(ends_with(c("sd", "se")), ~ NA))
    } else {
      userarea1_simp <- userarea1_simpx
    }
    
    st_write(userarea1_simp, "Out/webmofuss_results/mofuss_regions1.gpkg", delete_layer = TRUE)
    
    if(file.exists("LULCC/TempVector/userarea2.gpkg")){
      userarea_gpkg2 <- st_read("LULCC/TempVector/userarea2.gpkg")
      userarea_df2 <- userarea_gpkg2 %>% st_drop_geometry()
      
      admin2 <- raster("LULCC/TempRaster//admin_c2.tif")
      # StackNRB[StackNRB == 0] = NA #This is to sum and average only positive NRB values
      NRBzon_sum2 <- as.data.frame(zonal(StackNRB, admin2, 'sum')) %>%
        as.data.table()
      NRBzon_sum_1mc2 <- as.data.frame(zonal(StackNRB[[1]], admin2, 'sum')) %>%
        as.data.table() %>%
        setnames(.,"sum", "NRB_1MC")
      # zonal(StackNRB, admin, 'mean')
      # zonal(StackNRB, admin, 'sd')
      
      NRBzon_mean2 <- NRBzon_sum2[, list(NRB_MC_mean = rowMeans(.SD)), by = zone]
      NRBzon_sd2 <- NRBzon_sum2[, list(NRB_MC_sd = rowSds(.SD)), by = zone]
      
      NRBzon_sum_m22 <- merge(userarea_df2, NRBzon_sum_1mc2, by.x = ext_analysis_ID_2, by.y = "zone") %>%
        merge(NRBzon_mean2, by.x = ext_analysis_ID_2, by.y = "zone") %>%
        merge(NRBzon_sd2, by.x = ext_analysis_ID_2, by.y = "zone")
      
      # StackCON_TOT[StackCON_TOT == 0] = NA 
      CON_TOTzon_sum2 <- as.data.frame(zonal(StackCON_TOT, admin2, 'sum')) %>%
        as.data.table()
      CON_TOTzon_sum_1mc2 <- as.data.frame(zonal(StackCON_TOT[[1]], admin2, 'sum')) %>%
        as.data.table() %>%
        setnames(.,"sum", "CON_TOT_1MC")
      #zonal(StackCON_TOT, admin, 'mean')
      #zonal(StackCON_TOT, admin, 'sd')
      
      CON_TOTzon_mean2 <- CON_TOTzon_sum2[, list(CON_TOT_MC_mean = rowMeans(.SD)), by = zone]
      CON_TOTzon_sd2 <- CON_TOTzon_sum2[, list(CON_TOT_MC_sd = rowSds(.SD)), by = zone]
      
      CON_TOTzon_sum_m22 <- merge(userarea_df2, CON_TOTzon_sum_1mc2, by.x = ext_analysis_ID_2, by.y = "zone") %>%
        merge(CON_TOTzon_mean2, by.x = ext_analysis_ID_2, by.y = "zone") %>%
        merge(CON_TOTzon_sd2, by.x = ext_analysis_ID_2, by.y = "zone")
      
      # StackCON_NRB[StackCON_NRB == 0] = NA
      CON_NRBzon_sum2 <- as.data.frame(zonal(StackCON_NRB, admin2, 'sum')) %>%
        as.data.table()
      CON_NRBzon_sum_1mc2 <- as.data.frame(zonal(StackCON_NRB[[1]], admin2, 'sum')) %>%
        as.data.table() %>%
        setnames(.,"sum", "CON_NRB_1MC")
      #zonal(StackCON_NRB, admin, 'mean')
      #zonal(StackCON_NRB, admin, 'sd')
      
      CON_NRBzon_mean2 <- CON_NRBzon_sum2[, list(CON_NRB_MC_mean = rowMeans(.SD)), by = zone]
      CON_NRBzon_sd2 <- CON_NRBzon_sum2[, list(CON_NRB_MC_sd = rowSds(.SD)), by = zone]
      
      CON_NRBzon_sum_m22 <- merge(userarea_df2, CON_NRBzon_sum_1mc2, by.x = ext_analysis_ID_2, by.y = "zone") %>%
        merge(CON_NRBzon_mean2, by.x = ext_analysis_ID_2, by.y = "zone") %>%
        merge(CON_NRBzon_sd2, by.x = ext_analysis_ID_2, by.y = "zone")
      
      NRB_fNRB22x <- merge(NRBzon_sum_m22, CON_TOTzon_sum_m22, by.x = ext_analysis_ID_2, by.y = ext_analysis_ID_2) %>%
        merge(CON_NRBzon_sum_m22, by.x = ext_analysis_ID_2, by.y = ext_analysis_ID_2) %>%
        dplyr::select(ext_analysis_ID_2,
                      paste0(ext_analysis_NAME,".x"),
                      paste0(ext_analysis_NAME_1,".x"),
                      paste0(ext_analysis_NAME_2,".x"),
                      "NRB_1MC","NRB_MC_mean","NRB_MC_sd",
                      "CON_TOT_1MC", "CON_TOT_MC_mean", "CON_TOT_MC_sd",
                      "CON_NRB_1MC", "CON_NRB_MC_mean", "CON_NRB_MC_sd") %>%
        dplyr::mutate(fNRB1mc = round(NRB_1MC / CON_TOT_1MC * 100)) %>%
        dplyr::mutate(fNRB = round(NRB_MC_mean / CON_TOT_MC_mean * 100)) %>%
        dplyr::mutate(fNRB_sd = round(sqrt(((NRB_MC_sd/NRB_MC_mean)^2) + ((CON_TOT_MC_sd/CON_TOT_MC_mean)^2))*100)) %>%
        dplyr::mutate(fNRB1mc_nrb = round(NRB_1MC / CON_NRB_1MC * 100)) %>%
        dplyr::mutate(fNRB_nrb = round(NRB_MC_mean / CON_NRB_MC_mean * 100)) %>%
        dplyr::mutate(fNRB_nrb_sd = round(sqrt(((NRB_MC_sd/NRB_MC_mean)^2) + ((CON_NRB_MC_sd/CON_NRB_MC_mean)^2))*100)) %>%
        dplyr::mutate(across(5:13) / 1000) %>%
        dplyr::mutate(across(5:13, round, 0)) %>%
        dplyr::rename(ADM_0 = NAME_0.x,
                      ADM_1 = NAME_1.x,
                      ADM_2 = NAME_2.x) %>%
        dplyr::mutate(across(all_of(5:19), as.integer))
      
      str(NRB_fNRB22x)
      names(NRB_fNRB22x)
      
      if (MC < mcthreshold) {
        NRB_fNRB22 <- NRB_fNRB22x %>%
          dplyr::mutate(across(ends_with(c("sd", "se")), ~ NA))
      } else {
        NRB_fNRB22 <- NRB_fNRB22x
      }
      
      write.csv(NRB_fNRB22, "LULCC/TempTables/summary_adm2.csv", row.names=FALSE, quote=FALSE)
      write.csv(NRB_fNRB22, "Out/webmofuss_results/summary_adm2.csv", row.names=FALSE, quote=FALSE)
      
      NRB_fNRB22annual <- NRB_fNRB22 %>%
        dplyr::mutate(NRB_yr = round(NRB_MC_mean/ST,0)) %>%
        dplyr::mutate(CON_TOT_yr = round(CON_TOT_MC_mean/ST,0)) %>%
        dplyr::mutate(CON_NRB_yr = round(CON_NRB_MC_mean/ST,0))
      write.csv(NRB_fNRB22annual, "LULCC/TempTables/summary_adm2_yr.csv", row.names=FALSE, quote=FALSE)
      write.csv(NRB_fNRB22annual, "Out/webmofuss_results/summary_adm2_yr.csv", row.names=FALSE, quote=FALSE)
      
      # Produce simplified shapefile for webmofuss
      userarea2_simpx <- userarea_gpkg2 %>%
        inner_join(.,NRB_fNRB22, by="ID") %>%
        # ms_simplify(sys = TRUE) %>%
        dplyr::select(# GID_0,
          ID,
          NAME_0,
          # Subregion,
          # mofuss_reg,
          # NAME_0.x,
          NAME_1,
          NAME_2,
          NRB_1MC,
          NRB_MC_mean,
          NRB_MC_sd,
          CON_TOT_1MC,
          CON_TOT_MC_mean,
          CON_TOT_MC_sd,
          CON_NRB_1MC,
          CON_NRB_MC_mean,
          CON_NRB_MC_sd,
          fNRB1mc,
          fNRB, 
          fNRB_sd, 
          fNRB1mc_nrb,
          fNRB_nrb,
          fNRB_nrb_sd) %>%
        dplyr::rename(ADM0 = NAME_0,
                      ADM1 = NAME_1,
                      ADM2 = NAME_2,
                      NRB1mc = NRB_1MC,
                      NRBm = NRB_MC_mean,
                      NRBsd = NRB_MC_sd,
                      D1mc = CON_TOT_1MC,
                      Dm = CON_TOT_MC_mean,
                      Dsd = CON_TOT_MC_sd,
                      Dnrb_1mc = CON_NRB_1MC,
                      Dnrb_m= CON_NRB_MC_mean,
                      Dnrb_sd = CON_NRB_MC_sd,
                      fNRB2_1mc = fNRB1mc_nrb,
                      fNRB2 = fNRB_nrb, 
                      fNRB2_sd = fNRB_nrb_sd) %>%
        replace(is.na(.), 0)
      
      
      if (MC < mcthreshold) {
        userarea2_simp <- userarea2_simpx %>%
          dplyr::mutate(across(ends_with(c("sd", "se")), ~ NA))
        userarea2_simp
      } else {
        userarea2_simp <- userarea2_simpx
      }
      
      st_write(userarea2_simp, "Out/webmofuss_results/mofuss_regions2.gpkg", delete_layer = TRUE)
      
    }
  }
  
  ## Saves summary tables in .csv for BaU and ICS SCENARIOS ####
  ## LULCC/TempTables is the canonical location used by the modern PDF report.
  
  writeLines(paste(MC," Monte Carlo runs",sep=""), "LULCC/TempTables/MCruns.txt", useBytes=T)
  writeLines(paste(STdyn," years",sep=""), "LULCC/TempTables/SimLength.txt", useBytes=T)
  
  cols1 <- c(1:3)
  NRBBaUICS <- NRB_fNRB2[,cols1]
  
  cols2 <- c(1, 8:9)
  fNRBBaUICS <- NRB_fNRB2[,cols2]
  
  # if (BaUvsICS == "ICS") {
  # 	colnames(NRB_fNRB2)<- c("Name","NRB","NRBsd","FWuse","FWusesd","FWu2","FWusd2","fNRB","fNRBsd","fNRB2","fNRBsd2")
  # 	write.csv(NRB_fNRB2, "LULCC/TempTables/SumTableICS.csv", row.names=FALSE, quote=FALSE)
  # 	unlink("LaTeX/SumTableICS.csv", force=TRUE)
  # 	file.copy("LULCC/TempTables/SumTableICS.csv", "LaTeX/SumTableICS.csv")
  # 
  # 	colnames(NRBBaUICS)<- c("Name", "NRB.ICS", "NRB.ICS.sd")
  # 	write.csv(NRBBaUICS, "LULCC/TempTables/NRBICS.csv", row.names=FALSE, quote=FALSE)
  # 
  # 	colnames(fNRBBaUICS)<- c("Name", "fNRB.ICS", "fNRB.ICS.sd")
  # 	write.csv(fNRBBaUICS, "LULCC/TempTables/fNRBICS.csv", row.names=FALSE, quote=FALSE)
  # } else {
  colnames(NRB_fNRB2)<- c("Name", "NRB","NRBsd","FWuse","FWusesd","FWu2","FWusd2","fNRB","fNRBsd","fNRB2","fNRBsd2")
  write.csv(NRB_fNRB2, "LULCC/TempTables/SumTableBaU.csv", row.names=FALSE, quote=FALSE)
  
  colnames(NRBBaUICS)<- c("Name", "NRB.BaU", "NRB.BaU.sd")
  write.csv(NRBBaUICS, "LULCC/TempTables/NRBBaU.csv", row.names=FALSE, quote=FALSE)
  
  colnames(fNRBBaUICS)<- c("Name", "fNRB.BaU", "fNRB.BaU.sd")
  write.csv(fNRBBaUICS, "LULCC/TempTables/fNRBBaU.csv", row.names=FALSE, quote=FALSE)
  
  #}
  
  rNRBBaU<-file.exists("LULCC/TempTables/NRBBaU.csv")
  #rNRBICS<-file.exists("LULCC/TempTables/NRBICS.csv")
  rfNRBBaU<-file.exists("LULCC/TempTables/fNRBBaU.csv")
  #rfNRBICS<-file.exists("LULCC/TempTables/fNRBICS.csv")
  rBaUSt<-file.exists("LULCC/TempTables/SumTableBaU.csv")
  #rICSSt<-file.exists("LULCC/TempTables/SumTableICS.csv")
  
  #if (rNRBBaU == "TRUE" & rNRBICS == "TRUE") {
  rNRBBaUt<-read.csv("LULCC/TempTables/NRBBaU.csv")
  #rNRBICSt<-read.csv("LULCC/TempTables/NRBICS.csv")
  #rNRBBauICSt <- merge(rNRBBaUt,rNRBICSt,by="Name", sort=FALSE, all.x=TRUE)
  #colnames(rNRBBauICSt)<- c("Name",
  #	paste("NRB.BaU (n=",MC,")",sep=""),paste("NRB.BaU.sd (n=",MC,")",sep=""),
  #	paste("NRB.ICS (n=",MC,")",sep=""),paste("NRB.ICS.sd (n=",MC,")",sep=""))
  write.csv(rNRBBaUt, "LULCC/TempTables/NRBTable.csv", row.names=FALSE, quote=FALSE)
  #write.csv(rNRBBauICSt, "LULCC/TempTables/NRBTable.csv", row.names=FALSE, quote=FALSE)
  # } else {
  # 	"One out of two scenario table parameters is missing"
  # }
  # 
  # if (rfNRBBaU == "TRUE" & rfNRBICS == "TRUE") {
  rfNRBBaUt<-read.csv("LULCC/TempTables/fNRBBaU.csv")
  # 	rfNRBICSt<-read.csv("LULCC/TempTables/fNRBICS.csv")
  # 	rfNRBBauICSt <- merge(rfNRBBaUt,rfNRBICSt,by="Name", sort=FALSE, all.x=TRUE)
  # 	colnames(rfNRBBauICSt)<- c("Name", 
  # 		paste("fNRB.BaU (n=",MC,")",sep=""),paste("fNRB.BaU.sd (n=",MC,")",sep=""),
  # 		paste("fNRB.ICS (n=",MC,")",sep=""),paste("fNRB.ICS.sd (n=",MC,")",sep=""))
  write.csv(rfNRBBaUt, "LULCC/TempTables/fNRBTable.csv", row.names=FALSE, quote=FALSE)
  # 	write.csv(rfNRBBauICSt, "LULCC/TempTables/fNRBTable.csv", row.names=FALSE, quote=FALSE)
  # } else { 
  # 	"One out of two scenario table parameters is missing"		
  # }
  
  #if (rBaUSt == "TRUE" & rICSSt == "TRUE") {
  rBaUStt<-read.csv("LULCC/TempTables/SumTableBaU.csv")
  #rICSStt<-read.csv("LULCC/TempTables/SumTableICS.csv")
  #rBaUICStt <- merge(rBaUStt,rICSStt,by="Name", sort=FALSE, all.x=TRUE)
  write.csv(rBaUStt, "LULCC/TempTables/SumTable.csv", row.names=FALSE, quote=FALSE)
  #write.csv(rBaUICStt, "LULCC/TempTables/SumTable.csv", row.names=FALSE, quote=FALSE)
  # } else {
  # 	"One out of two scenario table parameters is missing"
  # }
  
  
  # # areal_fNRB - just a test...it won't fly
  # NRB01test <- raster("Temp/2_NRB01.tif")
  # NRB01testna <- reclassify(NRB01test, cbind(-Inf, 0, NA), right=TRUE)
  # NRB01testRtG<- raster("Temp/2_NRB01_RtG.tif")
  # NRB01testRtGna <- reclassify(NRB01testRtG, cbind(-Inf, 0, NA), right=TRUE)
  # 
  # NRBzon_count<-as.data.frame(zonal(NRB01testna, admin, 'count', na.rm = TRUE))
  # NRBzon_countNA<-as.data.frame(zonal(NRB01testna, admin, 'count', na.rm = FALSE))
  # aereal_fNRB <- cbind(NRBzon_count[1],round((NRBzon_count[,2] / NRBzon_countNA[,2] * 100),0))
  # names_zonal <- unique(userarea_DF[,c(ext_analysis_ID, ext_analysis_NAME)])
  # aereal_fNRB_zone <- merge(names_zonal, aereal_fNRB, by.x = ext_analysis_ID, by.y = "zone")
  # colnames(aereal_fNRB_zone) <- c(ext_analysis_ID, ext_analysis_NAME, "areal_fNRB")
  # write.csv(aereal_fNRB_zone, "LULCC/TempTables/areal_fNRB.csv", row.names=FALSE, quote=FALSE)
  # 
  # NRBzon_count_RtG<-as.data.frame(zonal(NRB01testRtGna, admin, 'count', na.rm = TRUE))
  # NRBzon_countNA_RtG<-as.data.frame(zonal(NRB01testRtGna, admin, 'count', na.rm = FALSE))
  # aereal_fNRB_RtG <- cbind(NRBzon_count_RtG[1],round((NRBzon_count_RtG[,2] / NRBzon_countNA_RtG[,2] * 100),0))
  # aereal_fNRB_zone_RtG <- merge(names_zonal, aereal_fNRB_RtG, by.x = ext_analysis_ID, by.y = "zone")
  # colnames(aereal_fNRB_zone_RtG) <- c(ext_analysis_ID, ext_analysis_NAME, "areal_fNRB")
  # write.csv(aereal_fNRB_zone_RtG, "LULCC/TempTables/areal_fNRB_RtG.csv", row.names=FALSE, quote=FALSE)
  
  
  # Chunk code to produce additonal tables using higher level admin units IF THEY EXIST
  if(file.exists("LULCC/TempVector/ext_analysis1.gpkg")){
    NRBBaUICS1<-NRB_fNRB21[,cols1]
    fNRBBaUICS1<-NRB_fNRB21[,cols2]
    
    colnames(NRB_fNRB21)<- c("Name", "NRB","NRBsd","FWuse","FWusesd","FWu2","FWusd2","fNRB","fNRBsd","fNRB2","fNRBsd2")
    write.csv(NRB_fNRB21, "LULCC/TempTables/SumTableBaU1.csv", row.names=FALSE, quote=FALSE)
    
    colnames(NRBBaUICS1)<- c("Name", "NRB.BaU", "NRB.BaU.sd")
    write.csv(NRBBaUICS1, "LULCC/TempTables/NRBBaU1.csv", row.names=FALSE, quote=FALSE)
    
    colnames(fNRBBaUICS1)<- c("Name", "fNRB.BaU", "fNRB.BaU.sd")
    write.csv(fNRBBaUICS1, "LULCC/TempTables/fNRBBaU1.csv", row.names=FALSE, quote=FALSE)
    
    rNRBBaU1<-file.exists("LULCC/TempTables/NRBBaU1.csv")
    #rNRBICS<-file.exists("LULCC/TempTables/NRBICS.csv")
    rfNRBBaU1<-file.exists("LULCC/TempTables/fNRBBaU1.csv")
    #rfNRBICS<-file.exists("LULCC/TempTables/fNRBICS.csv")
    rBaUSt1<-file.exists("LULCC/TempTables/SumTableBaU1.csv")
    #rICSSt<-file.exists("LULCC/TempTables/SumTableICS.csv")
    
    rNRBBaU1<-file.exists("LULCC/TempTables/NRBBaU1.csv")
    #rNRBICS<-file.exists("LULCC/TempTables/NRBICS.csv")
    rfNRBBaU1<-file.exists("LULCC/TempTables/fNRBBaU1.csv")
    #rfNRBICS<-file.exists("LULCC/TempTables/fNRBICS.csv")
    rBaUSt1<-file.exists("LULCC/TempTables/SumTableBaU1.csv")
    #rICSSt<-file.exists("LULCC/TempTables/SumTableICS.csv")
    
    #if (rNRBBaU == "TRUE" & rNRBICS == "TRUE") {
    rNRBBaUt1<-read.csv("LULCC/TempTables/NRBBaU1.csv")
    #rNRBICSt<-read.csv("LULCC/TempTables/NRBICS.csv")
    #rNRBBauICSt <- merge(rNRBBaUt,rNRBICSt,by="Name", sort=FALSE, all.x=TRUE)
    #colnames(rNRBBauICSt)<- c("Name",
    #	paste("NRB.BaU (n=",MC,")",sep=""),paste("NRB.BaU.sd (n=",MC,")",sep=""),
    #	paste("NRB.ICS (n=",MC,")",sep=""),paste("NRB.ICS.sd (n=",MC,")",sep=""))
    write.csv(rNRBBaUt1, "LULCC/TempTables/NRBTable1.csv", row.names=FALSE, quote=FALSE)
    #write.csv(rNRBBauICSt, "LULCC/TempTables/NRBTable.csv", row.names=FALSE, quote=FALSE)
    # } else {
    # 	"One out of two scenario table parameters is missing"
    # }
    # 
    # if (rfNRBBaU == "TRUE" & rfNRBICS == "TRUE") {
    rfNRBBaUt1<-read.csv("LULCC/TempTables/fNRBBaU1.csv")
    # 	rfNRBICSt<-read.csv("LULCC/TempTables/fNRBICS.csv")
    # 	rfNRBBauICSt <- merge(rfNRBBaUt,rfNRBICSt,by="Name", sort=FALSE, all.x=TRUE)
    # 	colnames(rfNRBBauICSt)<- c("Name", 
    # 		paste("fNRB.BaU (n=",MC,")",sep=""),paste("fNRB.BaU.sd (n=",MC,")",sep=""),
    # 		paste("fNRB.ICS (n=",MC,")",sep=""),paste("fNRB.ICS.sd (n=",MC,")",sep=""))
    write.csv(rfNRBBaUt1, "LULCC/TempTables/fNRBTable1.csv", row.names=FALSE, quote=FALSE)
    # 	write.csv(rfNRBBauICSt, "LULCC/TempTables/fNRBTable.csv", row.names=FALSE, quote=FALSE)
    # } else { 
    # 	"One out of two scenario table parameters is missing"		
    # }
    
    #if (rBaUSt == "TRUE" & rICSSt == "TRUE") {
    rBaUStt1<-read.csv("LULCC/TempTables/SumTableBaU1.csv")
    #rICSStt<-read.csv("LULCC/TempTables/SumTableICS.csv")
    #rBaUICStt <- merge(rBaUStt,rICSStt,by="Name", sort=FALSE, all.x=TRUE)
    write.csv(rBaUStt1, "LULCC/TempTables/SumTable1.csv", row.names=FALSE, quote=FALSE)
    #write.csv(rBaUICStt, "LULCC/TempTables/SumTable.csv", row.names=FALSE, quote=FALSE)
    # } else {
    # 	"One out of two scenario table parameters is missing"
    # }
    
  }
  
  
  
  # Summary tables for PDF latex report ####
  
  # dir.create(paste(OutDir,"/summarytables",sep=""))
  
} else {
  SumTables_yesno = "No"
}


# Conversion of TIFF to JPG and PNG for PNG report ----

dir.create(paste(OutDir,"/png",sep=""))
dir.create(paste(OutDir,"/jpg",sep=""))

figlist <- c("Map_AGB", "Area_of_Interest", "Localities_of_Interest",
             "AGB_NRB_fNRB_+10", "Boxplots_+10")
for (i in figlist) {
  img1 <- readTIFF(paste(OutDir,"/",i,".tif",sep=""), native=TRUE)
  writeJPEG(img1, target = paste(OutDir,"/jpg/",i,".jpg",sep=""), quality = 1)
  img2 <- readTIFF(paste(OutDir,"/",i,".tif",sep=""), native=FALSE)
  writePNG(img2, target = paste(OutDir,"/png/",i,".png",sep=""))
}

# Copy key rasters to output folder - 1st MC ----
if (copy_old_dinamica_rasters == 1) {
  rasternames <- c("2_NRB01.tif", "2_CON_TOT01.tif", "2_CON_NRB01.tif",
                   "2_fNRB01.tif", "2_IniSt01.tif", "2_AGBt101.tif")
  for (p in rasternames){
    file.copy(from=paste0("Temp//",p),
              to="Out/webmofuss_results/",
              overwrite = TRUE, recursive = TRUE, copy.mode = TRUE)
  }
}


# Summarise variation among independent Monte Carlo runs. These are absolute
# uncertainties: NRB and harvest retain their source units, while fNRB retains
# percentage points. stats::sd() is the sample SD (denominator n - 1), and the
# standard error is SD / sqrt(n), where n is the number of valid runs.
summarise_mc_uncertainty <- function(data, value_columns, expected_runs,
                                     digits = uncertainty_digits) {
  required_columns <- c("zone", "MC", value_columns)
  missing_columns <- setdiff(required_columns, names(data))
  if (length(missing_columns) > 0) {
    stop(
      "Cannot calculate Monte Carlo uncertainty; missing columns: ",
      paste(missing_columns, collapse = ", ")
    )
  }

  duplicate_runs <- data %>%
    dplyr::count(zone, MC, name = "rows_per_run") %>%
    dplyr::filter(rows_per_run != 1)
  if (nrow(duplicate_runs) > 0) {
    stop("Each zone must have exactly one row per Monte Carlo run.")
  }

  run_counts <- data %>%
    dplyr::group_by(zone) %>%
    dplyr::summarise(MC_n = dplyr::n_distinct(MC), .groups = "drop")
  if (any(run_counts$MC_n != expected_runs)) {
    warning(
      "Expected ", expected_runs, " Monte Carlo runs per zone, but observed: ",
      paste(sort(unique(run_counts$MC_n)), collapse = ", ")
    )
  }

  mc_mean <- function(x) {
    x <- x[is.finite(x)]
    if (length(x) == 0) NA_real_ else mean(x)
  }
  mc_sd <- function(x) {
    x <- x[is.finite(x)]
    if (length(x) < 2) NA_real_ else stats::sd(x)
  }
  mc_se <- function(x) {
    x <- x[is.finite(x)]
    if (length(x) < 2) NA_real_ else stats::sd(x) / sqrt(length(x))
  }

  data %>%
    dplyr::group_by(zone) %>%
    dplyr::summarise(
      dplyr::across(
        dplyr::all_of(value_columns),
        list(mean = mc_mean, sd = mc_sd, se = mc_se),
        .names = "{.col}_{.fn}"
      ),
      MC_n = dplyr::n_distinct(MC),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      dplyr::across(
        tidyselect::matches("_(mean|sd|se)$"),
        \(x) round(x, digits)
      )
    )
}

# Apply presentation rounding only to Monte Carlo result fields. Identifier
# fields (for example zone, ID, and MC_n) are deliberately left unchanged.
round_mc_result_columns <- function(data, digits) {
  data %>%
    dplyr::mutate(
      dplyr::across(
        tidyselect::matches("^(NRB|Harv|fNRB).*_(mean|sd|se|1MC)$"),
        \(x) round(x, digits)
      )
    )
}

if (fNRB_partition_tables == 1) {
  
  dir.create("Out/webmofuss_results/") 
  
  # fNRB partition tables and vectors ####
  if (aoi_poly == 1) {
    admin <- raster("LULCC/TempRaster//admin_c.tif")
    ecoregions <- raster("LULCC/TempRaster/ecoregions_c.tif")
    userarea_gpkg <- st_read("LULCC/TempVector/userarea.gpkg")
    ecoregions_gpkg <- st_read("LULCC/DownloadedDatasets/SourceDataGlobal/InVector/ecoregions.gpkg") # Why not TempVector?
    
    adminlevel <- c(admin, ecoregions)
    admin_name <- c("adm0", "ecoregions")
    
  } else {
    admin <- raster("LULCC/TempRaster/admin_c.tif")
    admin1 <- raster("LULCC/TempRaster/admin_c1.tif")
    admin2 <- raster("LULCC/TempRaster/admin_c2.tif")
    ecoregions <- raster("LULCC/TempRaster/ecoregions_c.tif")
    
    userarea_gpkg <- st_read("LULCC/TempVector/userarea.gpkg")
    userarea_gpkg1 <- st_read("LULCC/TempVector/userarea1.gpkg")
    userarea_gpkg2 <- st_read("LULCC/TempVector/userarea2.gpkg")
    ecoregions_gpkg <- st_read("LULCC/DownloadedDatasets/SourceDataGlobal/InVector/ecoregions.gpkg") # Why not TempVector?

    country_parameters %>%
      dplyr::filter(Var == "ext_analysis_ID") %>%
      pull(ParCHR) -> ext_analysis_ID
    
    country_parameters %>%
      dplyr::filter(Var == "ext_analysis_NAME") %>%
      pull(ParCHR) -> ext_analysis_NAME
    
    country_parameters %>%
      dplyr::filter(Var == "ext_analysis_ID_1") %>%
      pull(ParCHR) -> ext_analysis_ID_1
    
    country_parameters %>%
      dplyr::filter(Var == "ext_analysis_NAME_1") %>%
      pull(ParCHR) -> ext_analysis_NAME_1
    
    country_parameters %>%
      dplyr::filter(Var == "ext_analysis_ID_2") %>%
      pull(ParCHR) -> ext_analysis_ID_2
    
    country_parameters %>%
      dplyr::filter(Var == "ext_analysis_NAME_2") %>%
      pull(ParCHR) -> ext_analysis_NAME_2
    
    country_parameters %>%
      dplyr::filter(Var == "ecoregions_ID") %>%
      pull(ParCHR) -> ecoregions_ID
    
    country_parameters %>%
      dplyr::filter(Var == "ecoregions_NAME") %>%
      pull(ParCHR) -> ecoregions_NAME
    
    adminlevel <- c(admin, admin1, admin2, ecoregions)
    admin_name <- c("adm0", "adm1", "adm2", "ecoregions")
  }
  
  # # Only the following bins are possible from the 3_demand4IDW_v1 script
  # STdyn = 20 # 2020
  # STdyn = 30 # 2030
  # STdyn = 35 # 2035
  # STdyn = 40 # 2040
  # STdyn = 50 # 2050
  
  foreach(admm = adminlevel, admname = admin_name) %do% {
    #admm <- adminlevel[[4]] # Only the first admin level
    #admname <- admin_name[[4]]
    NRBzon_frlist <- list()
    # MC=2
    for (j in 1:MC) {
      #j = 1
      print(j)
      
      # NRB 
      listGlH <- list.files(paste0("debugging_",j), pattern = "^Growth_less_harv.+[.]tif$",ignore.case=F)
      stackGlH <- stack(paste0(paste0("debugging_",j,"/"),listGlH))
      nlay <- nlayers(stackGlH)
      
      listGx <- list.files(paste0("debugging_",j), pattern = "^Growth.+[.]tif$",ignore.case=F)
      listG <- listGx[ !grepl("_less_harv", listGx) ]
      stackG <- stack(paste0(paste0("debugging_",j,"/"),listG))
      nlayers(stackG) #for cross checking pattern
      
      nlay_yr <- nlay+1999
      nrb_name_per <- paste("nrb_sum_bin2010", nlay_yr, sep = "_")
      calculated_nrb_per <- stackG[[11]] - stackGlH[[nlay-1]] # Bin will start in 2010 and end in the final year
      calculated_nrb_per[calculated_nrb_per <= 0] = NA 
      calculated_sum_nrb_per <- as.data.frame(zonal(calculated_nrb_per, admm, 'sum')) %>%
        as.data.table() %>%
        setnames(.,"sum", paste0("NRB_2010_", nlay_yr))
      assign(nrb_name_per, calculated_sum_nrb_per)
      
      if (STdyn != 20){
        nrb_name <- paste("nrb_sum_bin2020", nlay_yr, sep = "_")
        calculated_nrb <- stackG[[21]] - stackGlH[[nlay-1]] # Bin will start in 2020 and end in the final year
        calculated_nrb[calculated_nrb <= 0] = NA 
        calculated_sum_nrb <- as.data.frame(zonal(calculated_nrb, admm, 'sum')) %>%
          as.data.table() %>%
          setnames(.,"sum", paste0("NRB_2020_",nlay_yr)) %>%
          dplyr::select(!zone)
        assign(nrb_name, calculated_sum_nrb)
      }
      
      if (STdyn == 20){
        nrb_bin2010_2020 <- stackG[[11]] - stackGlH[[nlay-1]] # Bin will be 2010-2020
        nrb_bin2010_2020[nrb_bin2010_2020 <= 0] = NA 
        nrb_sum_bin2010_2020 <- as.data.frame(zonal(nrb_bin2010_2020, admm, 'sum')) %>%
          as.data.table() %>%
          setnames(.,"sum", "NRB_2010_2020") #%>%
        #dplyr::select(!zone)
      }
      
      if (STdyn == 30){
        nrb_bin2010_2020 <- stackG[[11]] - stackGlH[[20]] # Bin will be 2010-2020
        nrb_bin2010_2020[nrb_bin2010_2020 <= 0] = NA 
        nrb_sum_bin2010_2020 <- as.data.frame(zonal(nrb_bin2010_2020, admm, 'sum')) %>%
          as.data.table() %>%
          setnames(.,"sum", "NRB_2010_2020") %>%
          dplyr::select(!zone)
        
        nrb_bin2020_2030 <- stackG[[21]] - stackGlH[[nlay-1]] # Bin will be 2020-2030
        nrb_bin2020_2030[nrb_bin2020_2030 <= 0] = NA 
        nrb_sum_bin2020_2030 <- as.data.frame(zonal(nrb_bin2020_2030, admm, 'sum')) %>%
          as.data.table() %>%
          setnames(.,"sum", "NRB_2020_2030") %>%
          dplyr::select(!zone)
      }
      
      if (STdyn == 35){
        nrb_bin2010_2020 <- stackG[[11]] - stackGlH[[20]] # Bin will be 2010-2020
        nrb_bin2010_2020[nrb_bin2010_2020 <= 0] = NA 
        nrb_sum_bin2010_2020 <- as.data.frame(zonal(nrb_bin2010_2020, admm, 'sum')) %>%
          as.data.table() %>%
          setnames(.,"sum", "NRB_2010_2020") %>%
          dplyr::select(!zone)
        
        nrb_bin2020_2035 <- stackG[[21]] - stackGlH[[nlay-1]] # Bin will be 2020-2035
        nrb_bin2020_2035[nrb_bin2020_2035 <= 0] = NA 
        nrb_sum_bin2020_2035 <- as.data.frame(zonal(nrb_bin2020_2035, admm, 'sum')) %>%
          as.data.table() %>%
          setnames(.,"sum", "NRB_2020_2035") %>%
          dplyr::select(!zone)
      }
      
      if (STdyn == 40){ 
        nrb_bin2010_2020 <- stackG[[11]] - stackGlH[[20]] # Bin will be 2010-2020
        nrb_bin2010_2020[nrb_bin2010_2020 <= 0] = NA 
        nrb_sum_bin2010_2020 <- as.data.frame(zonal(nrb_bin2010_2020, admm, 'sum')) %>%
          as.data.table() %>%
          setnames(.,"sum", "NRB_2010_2020") %>%
          dplyr::select(!zone)
        
        nrb_bin2020_2030 <- stackG[[21]] - stackGlH[[30]] # Bin will be 2020-2030
        nrb_bin2020_2030[nrb_bin2020_2030 <= 0] = NA 
        nrb_sum_bin2020_2030 <- as.data.frame(zonal(nrb_bin2020_2030, admm, 'sum')) %>%
          as.data.table() %>%
          setnames(.,"sum", "NRB_2020_2030") %>%
          dplyr::select(!zone)
        
        nrb_bin2030_2040 <- stackG[[31]] - stackGlH[[nlay-1]] # Bin will be 2030-2040
        nrb_bin2030_2040[nrb_bin2030_2040 <= 0] = NA 
        nrb_sum_bin2030_2040 <- as.data.frame(zonal(nrb_bin2030_2040, admm, 'sum')) %>%
          as.data.table() %>%
          setnames(.,"sum", "NRB_2030_2040") %>%
          dplyr::select(!zone)
        
        nrb_bin2020_2040 <- stackG[[21]] - stackGlH[[nlay-1]] # Bin will be 2020-2040
        nrb_bin2020_2040[nrb_bin2020_2040 <= 0] = NA 
        nrb_sum_bin2020_2040 <- as.data.frame(zonal(nrb_bin2020_2040, admm, 'sum')) %>%
          as.data.table() %>%
          setnames(.,"sum", "NRB_2020_2040") %>%
          dplyr::select(!zone)
      } 
      
      if (STdyn == 50){ # STdyn = 40 # 2050
        nrb_bin2010_2020 <- stackG[[11]] - stackGlH[[20]] # Bin will be 2010-2020
        nrb_bin2010_2020[nrb_bin2010_2020 <= 0] = NA 
        nrb_sum_bin2010_2020 <- as.data.frame(zonal(nrb_bin2010_2020, admm, 'sum')) %>%
          as.data.table() %>%
          setnames(.,"sum", "NRB_2010_2020") %>%
          dplyr::select(!zone)
        
        nrb_bin2020_2030 <- stackG[[21]] - stackGlH[[30]] # Bin will be 2020-2030
        nrb_bin2020_2030[nrb_bin2020_2030 <= 0] = NA 
        nrb_sum_bin2020_2030 <- as.data.frame(zonal(nrb_bin2020_2030, admm, 'sum')) %>%
          as.data.table() %>%
          setnames(.,"sum", "NRB_2020_2030") %>%
          dplyr::select(!zone)
        
        nrb_bin2030_2040 <- stackG[[31]] - stackGlH[[40]] # Bin will be 2030-2040
        nrb_bin2030_2040[nrb_bin2030_2040 <= 0] = NA 
        nrb_sum_bin2030_2040 <- as.data.frame(zonal(nrb_bin2030_2040, admm, 'sum')) %>%
          as.data.table() %>%
          setnames(.,"sum", "NRB_2030_2040") %>%
          dplyr::select(!zone)
        
        nrb_bin2040_2050 <- stackG[[41]] - stackGlH[[nlay-1]] # Bin will be 2040-2050
        nrb_bin2040_2050[nrb_bin2040_2050 <= 0] = NA 
        nrb_sum_bin2040_2050 <- as.data.frame(zonal(nrb_bin2040_2050, admm, 'sum')) %>%
          as.data.table() %>%
          setnames(.,"sum", "NRB_2040_2050") %>%
          dplyr::select(!zone)
        
        nrb_bin2020_2050 <- stackG[[21]] - stackGlH[[nlay-1]] # Bin will be 2020-2050
        nrb_bin2020_2050[nrb_bin2020_2050 <= 0] = NA 
        nrb_sum_bin2020_2050 <- as.data.frame(zonal(nrb_bin2020_2050, admm, 'sum')) %>%
          as.data.table() %>%
          setnames(.,"sum", "NRB_2020_2050") %>%
          dplyr::select(!zone)
      }
      
      # Define all potential variable names
      variable_nrb <- c("nrb_sum_bin2010_2050", # Do not use in summary tables
                        "nrb_sum_bin2010_2040", # Do not use in summary tables
                        "nrb_sum_bin2010_2035", # Do not use in summary tables
                        "nrb_sum_bin2010_2030", # Do not use in summary tables
                        "nrb_sum_bin2010_2020", # Do not use in summary tables
                        "nrb_sum_bin2020_2030", # STdyn = 20 # 2030
                        "nrb_sum_bin2020_2035", # STdyn = 25 # 2035
                        "nrb_sum_bin2020_2040", # STdyn = 30 # 2040 + "nrb_sum_bin2020_2030",
                        "nrb_sum_bin2020_2050", # STdyn = 40 # 2050 + "nrb_sum_bin2020_2030", + "nrb_sum_bin2030_2040"
                        "nrb_sum_bin2030_2040", # STdyn = 30 # 2040 + "nrb_sum_bin2020_2030",
                        "nrb_sum_bin2040_2050") # STdyn = 40 # 2050 + "nrb_sum_bin2020_2030", + "nrb_sum_bin2030_2040"
      
      # Use mget to try to get these variables from the global environment,
      # specifying NA for any that don't exist
      existing_nrb_x2 <- mget(variable_nrb, envir = .GlobalEnv, ifnotfound = list(NA))
      
      # Filter out the NAs from the list. Since each NA is actually a list element, we check for it differently
      existing_nrb_x <- Filter(function(x) {
        if (is.numeric(x) || is.character(x)) { # If the element is a vector or single value
          return(!is.na(x))
        } else if (is.data.frame(x) || is.list(x)) { # If the element is a data frame or list
          # Check if any value in the data frame or list is not NA
          return(any(!is.na(unlist(x))))
        } else {
          return(FALSE) # If the element is of a different type, exclude it
        }
      }, existing_nrb_x2)
      
      # Function to remove duplicates while preserving names
      remove_nrb <- function(lst) {
        unique_nrb <- list()  # Initialize an empty list for the unique elements
        seen_nrbhashes <- character()  # Keep track of hashes for seen elements
        
        for (name in names(lst)) {
          element <- lst[[name]]
          # Serialize the element to a raw vector and generate a hash
          element_nrbhash <- digest::digest(element, serialize = TRUE)
          
          if (!element_nrbhash %in% seen_nrbhashes) {
            unique_nrb[[name]] <- element  # Add to unique list with the original name
            seen_nrbhashes <- c(seen_nrbhashes, element_nrbhash)  # Mark this hash as seen
          }
        }
        
        return(unique_nrb)
      }
      
      # Use the function
      existing_nrb <- remove_nrb(existing_nrb_x)
      
      nrb_bind <- (bind_rows(existing_nrb))
      # Function to remove NAs and shift non-NA values upwards
      shift_up <- function(x) {
        # Remove NAs and return the non-NA values
        non_na_values <- x[!is.na(x)]
        # Calculate the number of NAs to pad
        na_pad <- rep(NA, length(x) - length(non_na_values))
        # Combine non-NA values with NA padding
        return(c(non_na_values, na_pad))
      }
      # Apply the function to each column
      nrb_sum_fr_unfil <- as.data.frame(lapply(nrb_bind, shift_up))
      nrb_sum_fr <- nrb_sum_fr_unfil[!is.na(nrb_sum_fr_unfil[[1]]), ]
      nrb_sum_fr
      
      if (STdyn == 20) {
        nrb_sum_fr <- nrb_sum_fr_unfil
      }
      
      # Harvest
      
      listharvx_per <- list.files(paste0("debugging_",j), pattern = "^Harvest_tot.+[.]tif$",ignore.case=F)
      listharv_per <- listharvx_per[ !grepl("_tot_nrb", listharvx_per) ]
      #listharv_per <- listharv_per[!grepl("tot0[1-9]|tot10", listharv_per)] 
      stackhar_per <- stack(paste0(paste0("debugging_",j,"/"),listharv_per[11:length(listharv_per)])) # Bin will start in 2010 and end in the final year
      nlayers(stackhar_per)
      
      harv_name_per <- paste("harv_sum_bin2010", nlay_yr, sep = "_")
      harvest_st_per <- stackApply(stackhar_per, indices=1, fun=sum)
      # harv_mean <- stackApply(stackhar_mc1, indices=1, fun=mean)
      harv_sum_per <- as.data.frame(zonal(harvest_st_per, admm, 'sum')) %>% # Bin will start in 2010 and end in the final year
        as.data.table() %>%
        setnames(.,"sum", paste0("Harv_2010_",nlay_yr))
      assign(harv_name_per, harv_sum_per)
      
      harv_name <- paste("harv_sum_bin2020", nlay_yr, sep = "_")
      listharv <- listharv_per[21:(nlay-1)]
      stackharv <- stack(paste0(paste0("debugging_",j,"/"),listharv))
      nlayers(stackharv)
      
      harvest_st <- stackApply(stackharv, indices=1, fun=sum)
      # harv_mean <- stackApply(stackhar_mc1, indices=1, fun=mean)
      harvest_sum_st <- as.data.frame(zonal(harvest_st, admm, 'sum')) %>%
        as.data.table() %>%
        setnames(.,"sum", paste0("Harv_2010_",nlay_yr)) %>%
        dplyr::select(!zone)
      assign(harv_name, harvest_sum_st)
      
      if (STdyn == 20){
        listharv_bin2010_2020 <- listharv_per[11:(nlay-1)]
        stackharv_bin2010_2020 <- stack(paste0(paste0("debugging_",j,"/"),listharv_bin2010_2020))
        nlayers(stackharv_bin2010_2020)
        
        harvest_st_bin2010_2020 <- stackApply(stackharv_bin2010_2020, indices=1, fun=sum)
        # harv_mean <- stackApply(stackhar_mc1, indices=1, fun=mean)
        harv_sum_bin2010_2020 <- as.data.frame(zonal(harvest_st_bin2010_2020, admm, 'sum')) %>%
          as.data.table() %>%
          setnames(.,"sum", "Harv_2010_2020") #%>%
        #dplyr::select(!zone)
      }
      
      if (STdyn == 30){
        listharv_bin2010_2020 <- listharv_per[11:20]
        stackharv_bin2010_2020 <- stack(paste0(paste0("debugging_",j,"/"),listharv_bin2010_2020))
        nlayers(stackharv_bin2010_2020)
        
        harvest_st_bin2010_2020 <- stackApply(stackharv_bin2010_2020, indices=1, fun=sum)
        # harv_mean <- stackApply(stackhar_mc1, indices=1, fun=mean)
        harv_sum_bin2010_2020 <- as.data.frame(zonal(harvest_st_bin2010_2020, admm, 'sum')) %>%
          as.data.table() %>%
          setnames(.,"sum", "Harv_2010_2020") %>%
          dplyr::select(!zone)
        
        listharv_bin2020_2030 <- listharv_per[21:(nlay-1)]
        stackharv_bin2020_2030 <- stack(paste0(paste0("debugging_",j,"/"),listharv_bin2020_2030))
        nlayers(stackharv_bin2020_2030)
        
        harvest_st_bin2020_2030 <- stackApply(stackharv_bin2020_2030, indices=1, fun=sum)
        # harv_mean <- stackApply(stackhar_mc1, indices=1, fun=mean)
        harv_sum_bin2020_2030<- as.data.frame(zonal(harvest_st_bin2020_2030, admm, 'sum')) %>%
          as.data.table() %>%
          setnames(.,"sum", "Harv_2020_2030") %>%
          dplyr::select(!zone)
      }
      
      if (STdyn == 35){
        listharv_bin2010_2020 <- listharv_per[11:20]
        stackharv_bin2010_2020 <- stack(paste0(paste0("debugging_",j,"/"),listharv_bin2010_2020))
        nlayers(stackharv_bin2010_2020)
        
        harvest_st_bin2010_2020 <- stackApply(stackharv_bin2010_2020, indices=1, fun=sum)
        # harv_mean <- stackApply(stackhar_mc1, indices=1, fun=mean)
        harv_sum_bin2010_2020 <- as.data.frame(zonal(harvest_st_bin2010_2020, admm, 'sum')) %>%
          as.data.table() %>%
          setnames(.,"sum", "Harv_2010_2020") %>%
          dplyr::select(!zone)
        
        listharv_bin2020_2035 <- listharv_per[21:(nlay-1)]
        stackharv_bin2020_2035 <- stack(paste0(paste0("debugging_",j,"/"),listharv_bin2020_2035))
        nlayers(stackharv_bin2020_2035)
        
        harvest_st_bin2020_2035 <- stackApply(stackharv_bin2020_2035, indices=1, fun=sum)
        # harv_mean <- stackApply(stackhar_mc1, indices=1, fun=mean)
        harv_sum_bin2020_2035<- as.data.frame(zonal(harvest_st_bin2020_2035, admm, 'sum')) %>%
          as.data.table() %>%
          setnames(.,"sum", "Harv_2020_2035") %>%
          dplyr::select(!zone)
      }
      
      if (STdyn == 40){
        listharv_bin2010_2020 <- listharv_per[11:20]
        stackharv_bin2010_2020 <- stack(paste0(paste0("debugging_",j,"/"),listharv_bin2010_2020))
        nlayers(stackharv_bin2010_2020)
        
        harvest_st_bin2010_2020 <- stackApply(stackharv_bin2010_2020, indices=1, fun=sum)
        # harv_mean <- stackApply(stackhar_mc1, indices=1, fun=mean)
        harv_sum_bin2010_2020 <- as.data.frame(zonal(harvest_st_bin2010_2020, admm, 'sum')) %>%
          as.data.table() %>%
          setnames(.,"sum", "Harv_2010_2020") %>%
          dplyr::select(!zone)
        
        listharv_bin2020_2030 <- listharv_per[21:30]
        stackharv_bin2020_2030 <- stack(paste0(paste0("debugging_",j,"/"),listharv_bin2020_2030))
        nlayers(stackharv_bin2020_2030)
        
        harvest_st_bin2020_2030 <- stackApply(stackharv_bin2020_2030, indices=1, fun=sum)
        # harv_mean <- stackApply(stackhar_mc1, indices=1, fun=mean)
        harv_sum_bin2020_2030<- as.data.frame(zonal(harvest_st_bin2020_2030, admm, 'sum')) %>%
          as.data.table() %>%
          setnames(.,"sum", "Harv_2020_2030") %>%
          dplyr::select(!zone)
        
        listharv_bin2030_2040 <- listharv_per[31:(nlay-1)]
        stackharv_bin2030_2040 <- stack(paste0(paste0("debugging_",j,"/"),listharv_bin2030_2040))
        nlayers(stackharv_bin2030_2040)
        
        harvest_st_bin2030_2040 <- stackApply(stackharv_bin2030_2040, indices=1, fun=sum)
        # harv_mean <- stackApply(stackhar_mc1, indices=1, fun=mean)
        harv_sum_bin2030_2040 <- as.data.frame(zonal(harvest_st_bin2030_2040, admm, 'sum')) %>%
          as.data.table() %>% 
          setnames(.,"sum", "Harv_2030_2040") %>%
          dplyr::select(!zone)
        
        listharv_bin2020_2040 <- listharv_per[21:(nlay-1)]
        stackharv_bin2020_2040 <- stack(paste0(paste0("debugging_",j,"/"),listharv_bin2020_2040))
        nlayers(stackharv_bin2020_2040)
        
        harvest_st_bin2020_2040 <- stackApply(stackharv_bin2020_2040, indices=1, fun=sum)
        # harv_mean <- stackApply(stackhar_mc1, indices=1, fun=mean)
        harv_sum_bin2020_2040<- as.data.frame(zonal(harvest_st_bin2020_2040, admm, 'sum')) %>%
          as.data.table() %>%
          setnames(.,"sum", "Harv_2020_2040") %>%
          dplyr::select(!zone)
      }
      
      if (STdyn == 50){
        listharv_bin2010_2020 <- listharv_per[11:20]
        stackharv_bin2010_2020 <- stack(paste0(paste0("debugging_",j,"/"),listharv_bin2010_2020))
        nlayers(stackharv_bin2010_2020)
        
        harvest_st_bin2010_2020 <- stackApply(stackharv_bin2010_2020, indices=1, fun=sum)
        # harv_mean <- stackApply(stackhar_mc1, indices=1, fun=mean)
        harv_sum_bin2010_2020 <- as.data.frame(zonal(harvest_st_bin2010_2020, admm, 'sum')) %>%
          as.data.table() %>%
          setnames(.,"sum", "Harv_2010_2020") %>%
          dplyr::select(!zone)
        
        listharv_bin2020_2030 <- listharv_per[21:30]
        stackharv_bin2020_2030 <- stack(paste0(paste0("debugging_",j,"/"),listharv_bin2020_2030))
        nlayers(stackharv_bin2020_2030)
        
        harvest_st_bin2020_2030 <- stackApply(stackharv_bin2020_2030, indices=1, fun=sum)
        # harv_mean <- stackApply(stackhar_mc1, indices=1, fun=mean)
        harv_sum_bin2020_2030<- as.data.frame(zonal(harvest_st_bin2020_2030, admm, 'sum')) %>%
          as.data.table() %>%
          setnames(.,"sum", "Harv_2020_2030") %>%
          dplyr::select(!zone)
        
        listharv_bin2030_2040 <- listharv_per[31:40]
        stackharv_bin2030_2040 <- stack(paste0(paste0("debugging_",j,"/"),listharv_bin2030_2040))
        nlayers(stackharv_bin2030_2040)
        
        harvest_st_bin2030_2040 <- stackApply(stackharv_bin2030_2040, indices=1, fun=sum)
        # harv_mean <- stackApply(stackhar_mc1, indices=1, fun=mean)
        harv_sum_bin2030_2040 <- as.data.frame(zonal(harvest_st_bin2030_2040, admm, 'sum')) %>%
          as.data.table() %>% 
          setnames(.,"sum", "Harv_2030_2040") %>%
          dplyr::select(!zone)
        
        listharv_bin2040_2050 <- listharv_per[41:(nlay-1)]
        stackharv_bin2040_2050 <- stack(paste0(paste0("debugging_",j,"/"),listharv_bin2040_2050))
        nlayers(stackharv_bin2040_2050)
        
        harvest_st_bin2040_2050 <- stackApply(stackharv_bin2040_2050, indices=1, fun=sum)
        # harv_mean <- stackApply(stackhar_mc1, indices=1, fun=mean)
        harv_sum_bin2040_2050 <- as.data.frame(zonal(harvest_st_bin2040_2050, admm, 'sum')) %>%
          as.data.table() %>%
          setnames(.,"sum", "Harv_2040_2050") %>%
          dplyr::select(!zone)
        
        listharv_bin2020_2050 <- listharv_per[21:(nlay-1)]
        stackharv_bin2020_2050 <- stack(paste0(paste0("debugging_",j,"/"),listharv_bin2020_2050))
        nlayers(stackharv_bin2020_2050)
        
        harvest_st_bin2020_2050 <- stackApply(stackharv_bin2020_2050, indices=1, fun=sum)
        # harv_mean <- stackApply(stackhar_mc1, indices=1, fun=mean)
        harv_sum_bin2020_2050 <- as.data.frame(zonal(harvest_st_bin2020_2050, admm, 'sum')) %>%
          as.data.table() %>%
          setnames(.,"sum", "Harv_2020_2050") %>%
          dplyr::select(!zone)
      }
      
      # Define all potential variable names
      variable_harv <- c("harv_sum_bin2010_2050", # Do not use in summary tables
                         "harv_sum_bin2010_2040", # Do not use in summary tables
                         "harv_sum_bin2010_2035", # Do not use in summary tables
                         "harv_sum_bin2010_2030", # Do not use in summary tables
                         "harv_sum_bin2010_2020", # Do not use in summary tables
                         "harv_sum_bin2020_2030", # STdyn = 20 # 2030
                         "harv_sum_bin2020_2035", # STdyn = 25 # 2035
                         "harv_sum_bin2020_2040", # STdyn = 30 # 2040 + "nrb_sum_bin2020_2030",
                         "harv_sum_bin2020_2050", # STdyn = 40 # 2050 + "nrb_sum_bin2020_2030", + "nrb_sum_bin2030_2040"
                         "harv_sum_bin2030_2040", # STdyn = 30 # 2040 + "nrb_sum_bin2020_2030",
                         "harv_sum_bin2040_2050") # STdyn = 40 # 2050 + "nrb_sum_bin2020_2030", + "nrb_sum_bin2030_2040"
      
      # Use mget to try to get these variables from the global environment,
      # specifying NA for any that don't exist
      existing_harv_x2 <- mget(variable_harv, envir = .GlobalEnv, ifnotfound = list(NA))
      
      # Filter out the NAs from the list. Since each NA is actually a list element, we check for it differently
      existing_harv_x <- Filter(function(x) {
        if (is.numeric(x) || is.character(x)) { # If the element is a vector or single value
          return(!is.na(x))
        } else if (is.data.frame(x) || is.list(x)) { # If the element is a data frame or list
          # Check if any value in the data frame or list is not NA
          return(any(!is.na(unlist(x))))
        } else {
          return(FALSE) # If the element is of a different type, exclude it
        }
      }, existing_harv_x2)
      
      # Function to remove duplicates while preserving names
      remove_harv <- function(lst) {
        unique_harv <- list()  # Initialize an empty list for the unique elements
        seen_harvhashes <- character()  # Keep track of hashes for seen elements
        
        for (name in names(lst)) {
          element <- lst[[name]]
          # Serialize the element to a raw vector and generate a hash
          element_harvhash <- digest::digest(element, serialize = TRUE)
          
          if (!element_harvhash %in% seen_harvhashes) {
            unique_harv[[name]] <- element  # Add to unique list with the original name
            seen_harvhashes <- c(seen_harvhashes, element_harvhash)  # Mark this hash as seen
          }
        }
        
        return(unique_harv)
      }
      
      # Use the function
      existing_harv <- remove_harv(existing_harv_x)
      
      harv_bind <- (bind_rows(existing_harv))
      # Function to remove NAs and shift non-NA values upwards
      shift_up <- function(x) {
        # Remove NAs and return the non-NA values
        non_na_values <- x[!is.na(x)]
        # Calculate the number of NAs to pad
        na_pad <- rep(NA, length(x) - length(non_na_values))
        # Combine non-NA values with NA padding
        return(c(non_na_values, na_pad))
      }
      # Apply the function to each column
      harv_sum_fr_unfil <- as.data.frame(lapply(harv_bind, shift_up))
      harv_sum_fr <- harv_sum_fr_unfil[!is.na(harv_sum_fr_unfil[[1]]), ]
      harv_sum_fr
      
      if (STdyn == 20) {
        harv_sum_fr <- harv_sum_fr_unfil
      }
      
      if (STdyn == 20){
        NRBzon_fr <- merge(nrb_sum_fr, harv_sum_fr, by = "zone") %>%
          # dplyr::rename(NRB_2010_2020 = x,
          #               Harv_2010_2020 = y) %>%
          dplyr::mutate(across(everything(), ~as.numeric(trimws(.x)))) %>%
          dplyr::mutate(fNRB_2010_2020 = NRB_2010_2020/Harv_2010_2020*100)
        
        NRBzon_fr$MC <- j  # maybe you want to keep track of which iteration produced it?
        NRBzon_frlist[[j]] <- NRBzon_fr # add it to your list
        
        if (j == 1) {
          NRBzon_frlist1MC <- NRBzon_frlist %>%
            as.data.frame() %>%
            rename_with(.fn = ~ paste0(.x, "_1MC"))
        }  
      } else if (STdyn == 30){
        NRBzon_fr <- merge(nrb_sum_fr, harv_sum_fr, by = "zone") %>%
          dplyr::mutate(across(everything(), ~as.numeric(trimws(.x)))) %>%
          dplyr::mutate(fNRB_2010_2030 = NRB_2010_2030/Harv_2010_2030*100,
                        fNRB_2020_2030 = NRB_2020_2030/Harv_2020_2030*100,
                        fNRB_2010_2020 = NRB_2010_2020/Harv_2010_2020*100)
        
        NRBzon_fr$MC <- j  # maybe you want to keep track of which iteration produced it?
        NRBzon_frlist[[j]] <- NRBzon_fr # add it to your list
        
        if (j == 1) {
          NRBzon_frlist1MC <- NRBzon_frlist %>%
            as.data.frame() %>%
            rename_with(.fn = ~ paste0(.x, "_1MC"))
        }  
        
      } else if (STdyn == 35){
        NRBzon_fr <- merge(nrb_sum_fr, harv_sum_fr, by = "zone") %>%
          dplyr::mutate(across(everything(), ~as.numeric(trimws(.x)))) %>%
          dplyr::mutate(fNRB_2010_2035 = NRB_2010_2035/Harv_2010_2035*100,
                        fNRB_2020_2035 = NRB_2020_2035/Harv_2020_2035*100,
                        fNRB_2010_2020 = NRB_2010_2020/Harv_2010_2020*100)
        
        NRBzon_fr$MC <- j  # maybe you want to keep track of which iteration produced it?
        NRBzon_frlist[[j]] <- NRBzon_fr # add it to your list
        
        if (j == 1) {
          NRBzon_frlist1MC <- NRBzon_frlist %>%
            as.data.frame() %>%
            rename_with(.fn = ~ paste0(.x, "_1MC"))
        }  
      } else if (STdyn == 40){
        NRBzon_fr <- merge(nrb_sum_fr, harv_sum_fr, by = "zone") %>%
          dplyr::mutate(across(everything(), ~as.numeric(trimws(.x)))) %>%
          dplyr::mutate(fNRB_2010_2040 = NRB_2010_2040/Harv_2010_2040*100,
                        fNRB_2020_2040 = NRB_2020_2040/Harv_2020_2040*100,
                        fNRB_2010_2020 = NRB_2010_2020/Harv_2010_2020*100,
                        fNRB_2020_2030 = NRB_2020_2030/Harv_2020_2030*100,
                        fNRB_2030_2040 = NRB_2030_2040/Harv_2030_2040*100)
        
        NRBzon_fr$MC <- j  # maybe you want to keep track of which iteration produced it?
        NRBzon_frlist[[j]] <- NRBzon_fr # add it to your list
        
        if (j == 1) {
          NRBzon_frlist1MC <- NRBzon_frlist %>%
            as.data.frame() %>%
            rename_with(.fn = ~ paste0(.x, "_1MC"))
        }  
      } else if (STdyn == 50){
        NRBzon_fr <- merge(nrb_sum_fr, harv_sum_fr, by = "zone") %>%
          dplyr::mutate(across(everything(), ~as.numeric(trimws(.x)))) %>%
          dplyr::mutate(
            fNRB_2010_2050 = NRB_2010_2050 / Harv_2010_2050 * 100,
            fNRB_2020_2050 = NRB_2020_2050 / Harv_2020_2050 * 100,
            fNRB_2010_2020 = NRB_2010_2020 / Harv_2010_2020 * 100,
            fNRB_2020_2030 = NRB_2020_2030 / Harv_2020_2030 * 100,
            fNRB_2030_2040 = NRB_2030_2040 / Harv_2030_2040 * 100,
            fNRB_2040_2050 = NRB_2040_2050 / Harv_2040_2050 * 100
          )
        
        NRBzon_fr$MC <- j  # maybe you want to keep track of which iteration produced it?
        NRBzon_frlist[[j]] <- NRBzon_fr # add it to your list
        
        if (j == 1) {
          NRBzon_frlist1MC <- NRBzon_frlist %>%
            as.data.frame() %>%
            rename_with(.fn = ~ paste0(.x, "_1MC"))
        } 
      } else {
        print("error with simulation length")  
      }
      
    } # for (j in 1:MC) {
    
    # Integrate tables with all the above datasets ----
    if (STdyn == 20){ # STdyn == 10 summary----
      print(20)
      
      NRBzon_frbind <- dplyr::bind_rows(NRBzon_frlist)
      summarycols <- c("NRB_2010_2020", "Harv_2010_2020", "fNRB_2010_2020")
      
      NRBzonfr_st <- summarise_mc_uncertainty(
        NRBzon_frbind, summarycols, expected_runs = MC
      )
      
      NRBzonfr_stR <- reduce(
        .x = list(
          c('NRB_2010_2020_mean', 'NRB_2010_2020_sd', 'NRB_2010_2020_se'),
          c('Harv_2010_2020_mean', 'Harv_2010_2020_sd', 'Harv_2010_2020_se'),
          c('fNRB_2010_2020_mean', 'fNRB_2010_2020_sd', 'fNRB_2010_2020_se')
        ),
        .f = ~ relocate(.x, .y[2], .after = .y[1]) %>% relocate(.y[3], .after = .y[2]),
        .init = NRBzonfr_st
      )
      NRBzonfr_stR
      names(NRBzonfr_stR)
      
      # fNRB mean, SD, and SE above come directly from the per-run fNRB values.
      # This preserves absolute percentage-point uncertainty and the covariance
      # between numerator and denominator within each Monte Carlo run.
      NRBzonfr_statsx <- NRBzonfr_stR
      
      if (MC < mcthreshold) {
        NRBzonfr_stats <- NRBzonfr_statsx %>%
          dplyr::mutate(across(ends_with(c("sd", "se")), ~ NA))
      } else {
        NRBzonfr_stats <- NRBzonfr_statsx
      }
      
      NRB_fNRB2_fr <- cbind(NRBzonfr_stats,NRBzon_frlist1MC) %>%
        round_mc_result_columns(uncertainty_digits)
      NRB_fNRB2_fr
      names(NRB_fNRB2_fr)
      
      if (admname == "adm0") {
        NRB_fNRB2_frcompl_madm0 <- userarea_gpkg %>%
          st_drop_geometry() %>%
          merge(., NRB_fNRB2_fr, by.x = ext_analysis_ID, by.y = "zone") %>%
          replace(is.na(.), 0)
        write.csv(NRB_fNRB2_frcompl_madm0, "LULCC/TempTables/summary_adm0_frcompl.csv", row.names=FALSE, quote=FALSE)
        write.csv(NRB_fNRB2_frcompl_madm0, "Out/webmofuss_results/summary_adm0_frcompl.csv", row.names=FALSE, quote=FALSE)
        
        NRB_fNRB3_fr_madm0 <- NRB_fNRB2_frcompl_madm0 %>%
          round_mc_result_columns(0)
        write.csv(NRB_fNRB3_fr_madm0, "LULCC/TempTables/summary_adm0_fr.csv", row.names=FALSE, quote=FALSE)
        write.csv(NRB_fNRB3_fr_madm0, "Out/webmofuss_results/summary_adm0_fr.csv", row.names=FALSE, quote=FALSE)
        
        print(paste0(admname," finished for tables"))
        
        if (aoi_poly == 1) {
          userarea_simpx_fr0 <- userarea_gpkg %>%
            inner_join(.,NRB_fNRB3_fr_madm0, by="ID") %>%
            dplyr::select(-NAME_0.y) %>%
            dplyr::rename(NAME_0 = NAME_0.x) %>%
            replace(is.na(.), 0)
          st_write(userarea_simpx_fr0, "Out/webmofuss_results/mofuss_adm0_fr.gpkg", delete_layer = TRUE)
          print(paste0(admname," finished for vector layers"))
        } else {
          userarea_simpx_fr0 <- userarea_gpkg %>%
            inner_join(.,NRB_fNRB3_fr_madm0, by="ID") %>%
            dplyr::select(-NAME_0.y, -Subregion.y, -mofuss_reg.y, -GID_0.y) %>%
            dplyr::rename(GID_0 = GID_0.x,
                          NAME_0 = NAME_0.x,
                          Subregion = Subregion.x,
                          mofuss_reg = mofuss_reg.x) %>%
            replace(is.na(.), 0)
          st_write(userarea_simpx_fr0, "Out/webmofuss_results/mofuss_adm0_fr.gpkg", delete_layer = TRUE)
          print(paste0(admname," finished for vector layers"))
        }
        
      } else if (admname == "adm1") {
        NRB_fNRB2_frcompl_madm1 <- userarea_gpkg1 %>%
          st_drop_geometry() %>%
          merge(., NRB_fNRB2_fr, by.x = ext_analysis_ID, by.y = "zone") %>%
          dplyr::select(-GID_0, -GID_1) %>%
          replace(is.na(.), 0)
        write.csv(NRB_fNRB2_frcompl_madm1, "LULCC/TempTables/summary_adm1_frcompl.csv", row.names=FALSE, quote=FALSE)
        write.csv(NRB_fNRB2_frcompl_madm1, "Out/webmofuss_results/summary_adm1_frcompl.csv", row.names=FALSE, quote=FALSE)
        
        NRB_fNRB3_fr_madm1 <- NRB_fNRB2_frcompl_madm1 %>%
          round_mc_result_columns(0)
        write.csv(NRB_fNRB3_fr_madm1, "LULCC/TempTables/summary_adm1_fr.csv", row.names=FALSE, quote=FALSE)
        write.csv(NRB_fNRB3_fr_madm1, "Out/webmofuss_results/summary_adm1_fr.csv", row.names=FALSE, quote=FALSE)
        
        print(paste0(admname," finished for tables"))
        
        userarea_simpx_fr1 <- userarea_gpkg1 %>%
          inner_join(.,NRB_fNRB3_fr_madm1, by="ID") %>%
          dplyr::select(-NAME_0.y, -NAME_1.y) %>%
          dplyr::rename(NAME_0 = NAME_0.x,
                        NAME_1 = NAME_1.x) %>%
          replace(is.na(.), 0)
        st_write(userarea_simpx_fr1, "Out/webmofuss_results/mofuss_adm1_fr.gpkg", delete_layer = TRUE)
        print(paste0(admname," finished for vector layers"))
        
      } else if (admname == "adm2") {
        NRB_fNRB2_frcompl_madm2 <- userarea_gpkg2 %>%
          st_drop_geometry() %>%
          merge(., NRB_fNRB2_fr, by.x = ext_analysis_ID, by.y = "zone") %>%
          dplyr::select(-GID_0, -GID_1, -GID_2) %>%
          replace(is.na(.), 0)
        write.csv(NRB_fNRB2_frcompl_madm2, "LULCC/TempTables/summary_adm2_frcompl.csv", row.names=FALSE, quote=FALSE)
        write.csv(NRB_fNRB2_frcompl_madm2, "Out/webmofuss_results/summary_adm2_frcompl.csv", row.names=FALSE, quote=FALSE)
        
        NRB_fNRB3_fr_madm2 <- NRB_fNRB2_frcompl_madm2 %>%
          round_mc_result_columns(0)
        write.csv(NRB_fNRB3_fr_madm2, "LULCC/TempTables/summary_adm2_fr.csv", row.names=FALSE, quote=FALSE)
        write.csv(NRB_fNRB3_fr_madm2, "Out/webmofuss_results/summary_adm2_fr.csv", row.names=FALSE, quote=FALSE)
        
        print(paste0(admname," finished for tables"))
        
        userarea_simpx_fr2 <- userarea_gpkg2 %>%
          inner_join(.,NRB_fNRB3_fr_madm2, by="ID") %>%
          dplyr::select(-NAME_0.y, -NAME_1.y, -NAME_2.y) %>%
          dplyr::rename(NAME_0 = NAME_0.x,
                        NAME_1 = NAME_1.x,
                        NAME_2 = NAME_2.x,) %>%
          replace(is.na(.), 0)
        st_write(userarea_simpx_fr2, "Out/webmofuss_results/mofuss_adm2_fr.gpkg", delete_layer = TRUE)
        print(paste0(admname," finished for vector layers"))
        
      } else if (admname == "ecoregions") {
        NRB_fNRB2_frcompl_meco2 <- ecoregions_gpkg %>%
          st_drop_geometry() %>%
          merge(., NRB_fNRB2_fr, by.x = ecoregions_ID, by.y = "zone") %>%
          replace(is.na(.), 0)
        write.csv(NRB_fNRB2_frcompl_meco2, "LULCC/TempTables/summary_ecoregions_frcompl.csv", row.names=FALSE, quote=FALSE)
        write.csv(NRB_fNRB2_frcompl_meco2, "Out/webmofuss_results/summary_ecoregions_frcompl.csv", row.names=FALSE, quote=FALSE)
        
        NRB_fNRB3_fr_meco2 <- NRB_fNRB2_frcompl_meco2 %>%
          round_mc_result_columns(0)
        write.csv(NRB_fNRB3_fr_meco2, "LULCC/TempTables/summary_ecoregions_fr.csv", row.names=FALSE, quote=FALSE)
        write.csv(NRB_fNRB3_fr_meco2, "Out/webmofuss_results/summary_ecoregions_fr.csv", row.names=FALSE, quote=FALSE)
        
        print(paste0(admname," finished for tables"))
        
        userarea_simpx_fr2 <- ecoregions_gpkg %>%
          inner_join(.,NRB_fNRB3_fr_meco2, by="ECO_ID") %>%
          dplyr::select(-ECO_NAME.y, -NNH_NAME.y, -GID_0.y, -NAME_0.y, -Subregion.y, -mofuss_reg.y, -ID.y) %>%
          dplyr::rename(ECO_NAME = ECO_NAME.x,
                        NNH_NAME = NNH_NAME.x,
                        GID_0 = GID_0.x,
                        NAME_0 = NAME_0.x,
                        Subregion = Subregion.x,
                        ID = ID.x,
                        mofuss_reg = mofuss_reg.x) %>%
          replace(is.na(.), 0)
        st_write(userarea_simpx_fr2, "Out/webmofuss_results/mofuss_ecoregions_fr.gpkg", delete_layer = TRUE)
        print(paste0(admname," finished for vector layers"))
      }
      
      
      
    } else if (STdyn == 30){ # STdyn == 30 summary----
      # print(20)
      
      NRBzon_frbind <- dplyr::bind_rows(NRBzon_frlist)
      summarycols <- c("NRB_2010_2030", "NRB_2010_2020", "NRB_2020_2030", 
                       "Harv_2010_2030", "Harv_2010_2020", "Harv_2020_2030",
                       "fNRB_2010_2030", "fNRB_2010_2020", "fNRB_2020_2030")
      
      NRBzonfr_st <- summarise_mc_uncertainty(
        NRBzon_frbind, summarycols, expected_runs = MC
      )
      
      NRBzonfr_stR <- reduce(
        .x = list(
          c('NRB_2010_2030_mean', 'NRB_2010_2030_sd', 'NRB_2010_2030_se'),
          c('NRB_2010_2020_mean', 'NRB_2010_2020_sd', 'NRB_2010_2020_se'),
          c('NRB_2020_2030_mean', 'NRB_2020_2030_sd', 'NRB_2020_2030_se'),
          c('Harv_2010_2030_mean', 'Harv_2010_2030_sd', 'Harv_2010_2030_se'),
          c('Harv_2010_2020_mean', 'Harv_2010_2020_sd', 'Harv_2010_2020_se'),
          c('Harv_2020_2030_mean', 'Harv_2020_2030_sd', 'Harv_2020_2030_se'),
          c('fNRB_2010_2030_mean', 'fNRB_2010_2030_sd', 'fNRB_2010_2030_se'),
          c('fNRB_2010_2020_mean', 'fNRB_2010_2020_sd', 'fNRB_2010_2020_se'),
          c('fNRB_2020_2030_mean', 'fNRB_2020_2030_sd', 'fNRB_2020_2030_se')
        ),
        .f = ~ relocate(.x, .y[2], .after = .y[1]) %>% relocate(.y[3], .after = .y[2]),
        .init = NRBzonfr_st
      )
      NRBzonfr_stR
      names(NRBzonfr_stR)
      
      NRBzonfr_statsx <- NRBzonfr_stR
      
      if (MC < mcthreshold) {
        NRBzonfr_stats <- NRBzonfr_statsx %>%
          dplyr::mutate(across(ends_with(c("sd", "se")), ~ NA))
      } else {
        NRBzonfr_stats <- NRBzonfr_statsx
      }
      
      NRB_fNRB2_fr <- cbind(NRBzonfr_stats,NRBzon_frlist1MC) %>%
        round_mc_result_columns(uncertainty_digits)
      NRB_fNRB2_fr
      names(NRB_fNRB2_fr)
      
      if (admname == "adm0") {
        NRB_fNRB2_frcompl_madm0 <- userarea_gpkg %>%
          st_drop_geometry() %>%
          merge(., NRB_fNRB2_fr, by.x = ext_analysis_ID, by.y = "zone") %>%
          replace(is.na(.), 0)
        write.csv(NRB_fNRB2_frcompl_madm0, "LULCC/TempTables/summary_adm0_frcompl.csv", row.names=FALSE, quote=FALSE)
        write.csv(NRB_fNRB2_frcompl_madm0, "Out/webmofuss_results/summary_adm0_frcompl.csv", row.names=FALSE, quote=FALSE)
        
        NRB_fNRB3_fr_madm0 <- NRB_fNRB2_frcompl_madm0 %>%
          dplyr::select(-matches("_2010_2030|_2010_2020")) %>%
          dplyr::select(-ends_with("_1MC")) %>%
          round_mc_result_columns(0)
        write.csv(NRB_fNRB3_fr_madm0, "LULCC/TempTables/summary_adm0_fr.csv", row.names=FALSE, quote=FALSE)
        write.csv(NRB_fNRB3_fr_madm0, "Out/webmofuss_results/summary_adm0_fr.csv", row.names=FALSE, quote=FALSE)
        
        print(paste0(admname," finished for tables"))
        
        if (aoi_poly == 1) {
          userarea_simpx_fr0 <- userarea_gpkg %>%
            inner_join(.,NRB_fNRB3_fr_madm0, by="ID") %>%
            dplyr::select(-NAME_0.y) %>%
            dplyr::rename(NAME_0 = NAME_0.x) %>%
            replace(is.na(.), 0)
          st_write(userarea_simpx_fr0, "Out/webmofuss_results/mofuss_adm0_fr.gpkg", delete_layer = TRUE)
          print(paste0(admname," finished for vector layers"))
        } else {
          userarea_simpx_fr0 <- userarea_gpkg %>%
            inner_join(.,NRB_fNRB3_fr_madm0, by="ID") %>%
            dplyr::select(-NAME_0.y, -Subregion.y, -mofuss_reg.y, -GID_0.y) %>%
            dplyr::rename(GID_0 = GID_0.x,
                          NAME_0 = NAME_0.x,
                          Subregion = Subregion.x,
                          mofuss_reg = mofuss_reg.x) %>%
            replace(is.na(.), 0)
          st_write(userarea_simpx_fr0, "Out/webmofuss_results/mofuss_adm0_fr.gpkg", delete_layer = TRUE)
          print(paste0(admname," finished for vector layers"))
        }
        
      } else if (admname == "adm1") {
        NRB_fNRB2_frcompl_madm1 <- userarea_gpkg1 %>%
          st_drop_geometry() %>%
          merge(., NRB_fNRB2_fr, by.x = ext_analysis_ID, by.y = "zone") %>%
          dplyr::select(-GID_0, -GID_1) %>%
          replace(is.na(.), 0)
        write.csv(NRB_fNRB2_frcompl_madm1, "LULCC/TempTables/summary_adm1_frcompl.csv", row.names=FALSE, quote=FALSE)
        write.csv(NRB_fNRB2_frcompl_madm1, "Out/webmofuss_results/summary_adm1_frcompl.csv", row.names=FALSE, quote=FALSE)
        
        NRB_fNRB3_fr_madm1 <- NRB_fNRB2_frcompl_madm1 %>%
          dplyr::select(-matches("_2010_2030|_2010_2020")) %>%
          dplyr::select(-ends_with("_1MC")) %>%
          round_mc_result_columns(0)
        write.csv(NRB_fNRB3_fr_madm1, "LULCC/TempTables/summary_adm1_fr.csv", row.names=FALSE, quote=FALSE)
        write.csv(NRB_fNRB3_fr_madm1, "Out/webmofuss_results/summary_adm1_fr.csv", row.names=FALSE, quote=FALSE)
        
        print(paste0(admname," finished for tables"))
        
        userarea_simpx_fr1 <- userarea_gpkg1 %>%
          inner_join(.,NRB_fNRB3_fr_madm1, by="ID") %>%
          dplyr::select(-NAME_0.y, -NAME_1.y) %>%
          dplyr::rename(NAME_0 = NAME_0.x,
                        NAME_1 = NAME_1.x) %>%
          replace(is.na(.), 0)
        st_write(userarea_simpx_fr1, "Out/webmofuss_results/mofuss_adm1_fr.gpkg", delete_layer = TRUE)
        print(paste0(admname," finished for vector layers"))
        
      } else if (admname == "adm2") {
        NRB_fNRB2_frcompl_madm2 <- userarea_gpkg2 %>%
          st_drop_geometry() %>%
          merge(., NRB_fNRB2_fr, by.x = ext_analysis_ID, by.y = "zone") %>%
          dplyr::select(-GID_0, -GID_1, -GID_2) %>%
          replace(is.na(.), 0)
        write.csv(NRB_fNRB2_frcompl_madm2, "LULCC/TempTables/summary_adm2_frcompl.csv", row.names=FALSE, quote=FALSE)
        write.csv(NRB_fNRB2_frcompl_madm2, "Out/webmofuss_results/summary_adm2_frcompl.csv", row.names=FALSE, quote=FALSE)
        
        NRB_fNRB3_fr_madm2 <- NRB_fNRB2_frcompl_madm2 %>%
          dplyr::select(-matches("_2010_2030|_2010_2020")) %>%
          dplyr::select(-ends_with("_1MC")) %>%
          round_mc_result_columns(0)
        write.csv(NRB_fNRB3_fr_madm2, "LULCC/TempTables/summary_adm2_fr.csv", row.names=FALSE, quote=FALSE)
        write.csv(NRB_fNRB3_fr_madm2, "Out/webmofuss_results/summary_adm2_fr.csv", row.names=FALSE, quote=FALSE)
        
        print(paste0(admname," finished for tables"))
        
        userarea_simpx_fr2 <- userarea_gpkg2 %>%
          inner_join(.,NRB_fNRB3_fr_madm2, by="ID") %>%
          dplyr::select(-NAME_0.y, -NAME_1.y, -NAME_2.y) %>%
          dplyr::rename(NAME_0 = NAME_0.x,
                        NAME_1 = NAME_1.x,
                        NAME_2 = NAME_2.x,) %>%
          replace(is.na(.), 0)
        st_write(userarea_simpx_fr2, "Out/webmofuss_results/mofuss_adm2_fr.gpkg", delete_layer = TRUE)
        print(paste0(admname," finished for vector layers"))
        
      } else if (admname == "ecoregions") {
        NRB_fNRB2_frcompl_meco2 <- ecoregions_gpkg %>%
          st_drop_geometry() %>%
          merge(., NRB_fNRB2_fr, by.x = ecoregions_ID, by.y = "zone") %>%
          replace(is.na(.), 0)
        write.csv(NRB_fNRB2_frcompl_meco2, "LULCC/TempTables/summary_ecoregions_frcompl.csv", row.names=FALSE, quote=FALSE)
        write.csv(NRB_fNRB2_frcompl_meco2, "Out/webmofuss_results/summary_ecoregions_frcompl.csv", row.names=FALSE, quote=FALSE)
        
        NRB_fNRB3_fr_meco2 <- NRB_fNRB2_frcompl_meco2 %>%
          dplyr::select(-matches("_2010_2030|_2010_2020")) %>%
          dplyr::select(-ends_with("_1MC")) %>%
          round_mc_result_columns(0)
        write.csv(NRB_fNRB3_fr_meco2, "LULCC/TempTables/summary_ecoregions_fr.csv", row.names=FALSE, quote=FALSE)
        write.csv(NRB_fNRB3_fr_meco2, "Out/webmofuss_results/summary_ecoregions_fr.csv", row.names=FALSE, quote=FALSE)
        
        print(paste0(admname," finished for tables"))
        
        userarea_simpx_fr2 <- ecoregions_gpkg %>%
          inner_join(.,NRB_fNRB3_fr_meco2, by="ECO_ID") %>%
          dplyr::select(-ECO_NAME.y, -NNH_NAME.y, -GID_0.y, -NAME_0.y, -Subregion.y, -mofuss_reg.y, -ID.y) %>%
          dplyr::rename(ECO_NAME = ECO_NAME.x,
                        NNH_NAME = NNH_NAME.x,
                        GID_0 = GID_0.x,
                        NAME_0 = NAME_0.x,
                        Subregion = Subregion.x,
                        ID = ID.x,
                        mofuss_reg = mofuss_reg.x) %>%
          replace(is.na(.), 0)
        st_write(userarea_simpx_fr2, "Out/webmofuss_results/mofuss_ecoregions_fr.gpkg", delete_layer = TRUE)
        print(paste0(admname," finished for vector layers"))
      }
      
      
    } else if (STdyn == 35){ # STdyn == 35 summary----
      print(35)
      
      NRBzon_frbind <- dplyr::bind_rows(NRBzon_frlist)
      summarycols <- c("NRB_2010_2035", "NRB_2020_2035", "NRB_2010_2020", 
                       "Harv_2010_2035", "Harv_2020_2035", "Harv_2010_2020",
                       "fNRB_2010_2035", "fNRB_2020_2035", "fNRB_2010_2020")
      
      NRBzonfr_st <- summarise_mc_uncertainty(
        NRBzon_frbind, summarycols, expected_runs = MC
      )
      
      NRBzonfr_stR <- reduce(
        .x = list(
          c('NRB_2010_2035_mean', 'NRB_2010_2035_sd', 'NRB_2010_2035_se'),
          c('NRB_2020_2035_mean', 'NRB_2020_2035_sd', 'NRB_2020_2035_se'),
          c('NRB_2010_2020_mean', 'NRB_2010_2020_sd', 'NRB_2010_2020_se'),
          c('Harv_2010_2035_mean', 'Harv_2010_2035_sd', 'Harv_2010_2035_se'),
          c('Harv_2020_2035_mean', 'Harv_2020_2035_sd', 'Harv_2020_2035_se'),
          c('Harv_2010_2020_mean', 'Harv_2010_2020_sd', 'Harv_2010_2020_se'),
          c('fNRB_2010_2035_mean', 'fNRB_2010_2035_sd', 'fNRB_2010_2035_se'),
          c('fNRB_2020_2035_mean', 'fNRB_2020_2035_sd', 'fNRB_2020_2035_se'),
          c('fNRB_2010_2020_mean', 'fNRB_2010_2020_sd', 'fNRB_2010_2020_se')
        ),
        .f = ~ relocate(.x, .y[2], .after = .y[1]) %>% relocate(.y[3], .after = .y[2]),
        .init = NRBzonfr_st
      )
      NRBzonfr_stR
      names(NRBzonfr_stR)
      
      NRBzonfr_statsx <- NRBzonfr_stR
      
      if (MC < mcthreshold) {
        NRBzonfr_stats <- NRBzonfr_statsx %>%
          dplyr::mutate(across(ends_with(c("sd", "se")), ~ NA))
      } else {
        NRBzonfr_stats <- NRBzonfr_statsx
      }
      
      NRB_fNRB2_fr <- cbind(NRBzonfr_stats,NRBzon_frlist1MC) %>%
        round_mc_result_columns(uncertainty_digits)
      NRB_fNRB2_fr
      names(NRB_fNRB2_fr)
      
      if (admname == "adm0") {
        NRB_fNRB2_frcompl_madm0 <- userarea_gpkg %>%
          st_drop_geometry() %>%
          merge(., NRB_fNRB2_fr, by.x = ext_analysis_ID, by.y = "zone") %>%
          replace(is.na(.), 0)
        write.csv(NRB_fNRB2_frcompl_madm0, "LULCC/TempTables/summary_adm0_frcompl.csv", row.names=FALSE, quote=FALSE)
        write.csv(NRB_fNRB2_frcompl_madm0, "Out/webmofuss_results/summary_adm0_frcompl.csv", row.names=FALSE, quote=FALSE)
        
        NRB_fNRB3_fr_madm0 <- NRB_fNRB2_frcompl_madm0 %>%
          dplyr::select(-matches("_2010_2035|_2010_2020")) %>%
          dplyr::relocate(NRB_2020_2035_1MC, .after = zone_1MC) %>%
          dplyr::relocate(Harv_2020_2035_1MC, .after = NRB_2020_2035_1MC) %>%
          dplyr::select(-ends_with("_1MC")) %>%
          round_mc_result_columns(0)
        write.csv(NRB_fNRB3_fr_madm0, "LULCC/TempTables/summary_adm0_fr.csv", row.names=FALSE, quote=FALSE)
        write.csv(NRB_fNRB3_fr_madm0, "Out/webmofuss_results/summary_adm0_fr.csv", row.names=FALSE, quote=FALSE)
        
        print(paste0(admname," finished for tables"))
        
        if (aoi_poly == 1) {
          userarea_simpx_fr0 <- userarea_gpkg %>%
            inner_join(.,NRB_fNRB3_fr_madm0, by="ID") %>%
            dplyr::select(-NAME_0.y) %>%
            dplyr::rename(NAME_0 = NAME_0.x) %>%
            replace(is.na(.), 0)
          st_write(userarea_simpx_fr0, "Out/webmofuss_results/mofuss_adm0_fr.gpkg", delete_layer = TRUE)
          print(paste0(admname," finished for vector layers"))
        } else {
          userarea_simpx_fr0 <- userarea_gpkg %>%
            inner_join(.,NRB_fNRB3_fr_madm0, by="ID") %>%
            dplyr::select(-NAME_0.y, -Subregion.y, -mofuss_reg.y, -GID_0.y) %>%
            dplyr::rename(GID_0 = GID_0.x,
                          NAME_0 = NAME_0.x,
                          Subregion = Subregion.x,
                          mofuss_reg = mofuss_reg.x) %>%
            replace(is.na(.), 0)
          st_write(userarea_simpx_fr0, "Out/webmofuss_results/mofuss_adm0_fr.gpkg", delete_layer = TRUE)
          print(paste0(admname," finished for vector layers"))
        }
        
      } else if (admname == "adm1") {
        NRB_fNRB2_frcompl_madm1 <- userarea_gpkg1 %>%
          st_drop_geometry() %>%
          merge(., NRB_fNRB2_fr, by.x = ext_analysis_ID, by.y = "zone") %>%
          dplyr::select(-GID_0, -GID_1) %>%
          replace(is.na(.), 0)
        write.csv(NRB_fNRB2_frcompl_madm1, "LULCC/TempTables/summary_adm1_frcompl.csv", row.names=FALSE, quote=FALSE)
        write.csv(NRB_fNRB2_frcompl_madm1, "Out/webmofuss_results/summary_adm1_frcompl.csv", row.names=FALSE, quote=FALSE)
        
        NRB_fNRB3_fr_madm1 <- NRB_fNRB2_frcompl_madm1 %>%
          dplyr::select(-matches("_2010_2035|_2010_2020")) %>%
          dplyr::relocate(NRB_2020_2035_1MC, .after = zone_1MC) %>%
          dplyr::relocate(Harv_2020_2035_1MC, .after = NRB_2020_2035_1MC) %>%
          dplyr::select(-ends_with("_1MC")) %>%
          round_mc_result_columns(0)
        write.csv(NRB_fNRB3_fr_madm1, "LULCC/TempTables/summary_adm1_fr.csv", row.names=FALSE, quote=FALSE)
        write.csv(NRB_fNRB3_fr_madm1, "Out/webmofuss_results/summary_adm1_fr.csv", row.names=FALSE, quote=FALSE)
        
        print(paste0(admname," finished for tables"))
        
        userarea_simpx_fr1 <- userarea_gpkg1 %>%
          inner_join(.,NRB_fNRB3_fr_madm1, by="ID") %>%
          dplyr::select(-NAME_0.y, -NAME_1.y) %>%
          dplyr::rename(NAME_0 = NAME_0.x,
                        NAME_1 = NAME_1.x) %>%
          replace(is.na(.), 0)
        st_write(userarea_simpx_fr1, "Out/webmofuss_results/mofuss_adm1_fr.gpkg", delete_layer = TRUE)
        print(paste0(admname," finished for vector layers"))
        
      } else if (admname == "adm2") {
        NRB_fNRB2_frcompl_madm2 <- userarea_gpkg2 %>%
          st_drop_geometry() %>%
          merge(., NRB_fNRB2_fr, by.x = ext_analysis_ID, by.y = "zone") %>%
          dplyr::select(-GID_0, -GID_1, -GID_2) %>%
          replace(is.na(.), 0)
        write.csv(NRB_fNRB2_frcompl_madm2, "LULCC/TempTables/summary_adm2_frcompl.csv", row.names=FALSE, quote=FALSE)
        write.csv(NRB_fNRB2_frcompl_madm2, "Out/webmofuss_results/summary_adm2_frcompl.csv", row.names=FALSE, quote=FALSE)
        
        NRB_fNRB3_fr_madm2 <- NRB_fNRB2_frcompl_madm2 %>%
          dplyr::select(-matches("_2010_2035|_2010_2020")) %>%
          dplyr::relocate(NRB_2020_2035_1MC, .after = zone_1MC) %>%
          dplyr::relocate(Harv_2020_2035_1MC, .after = NRB_2020_2035_1MC) %>%
          dplyr::select(-ends_with("_1MC")) %>%
          round_mc_result_columns(0)
        write.csv(NRB_fNRB3_fr_madm2, "LULCC/TempTables/summary_adm2_fr.csv", row.names=FALSE, quote=FALSE)
        write.csv(NRB_fNRB3_fr_madm2, "Out/webmofuss_results/summary_adm2_fr.csv", row.names=FALSE, quote=FALSE)
        
        print(paste0(admname," finished for tables"))
        
        userarea_simpx_fr2 <- userarea_gpkg2 %>%
          inner_join(.,NRB_fNRB3_fr_madm2, by="ID") %>%
          dplyr::select(-NAME_0.y, -NAME_1.y, -NAME_2.y) %>%
          dplyr::rename(NAME_0 = NAME_0.x,
                        NAME_1 = NAME_1.x,
                        NAME_2 = NAME_2.x,) %>%
          replace(is.na(.), 0)
        st_write(userarea_simpx_fr2, "Out/webmofuss_results/mofuss_adm2_fr.gpkg", delete_layer = TRUE)
        print(paste0(admname," finished for vector layers"))
        
      } else if (admname == "ecoregions") {
        NRB_fNRB2_frcompl_meco2 <- ecoregions_gpkg %>%
          st_drop_geometry() %>%
          merge(., NRB_fNRB2_fr, by.x = ecoregions_ID, by.y = "zone") %>%
          replace(is.na(.), 0)
        write.csv(NRB_fNRB2_frcompl_meco2, "LULCC/TempTables/summary_ecoregions_frcompl.csv", row.names=FALSE, quote=FALSE)
        write.csv(NRB_fNRB2_frcompl_meco2, "Out/webmofuss_results/summary_ecoregions_frcompl.csv", row.names=FALSE, quote=FALSE)
        
        NRB_fNRB3_fr_meco2 <- NRB_fNRB2_frcompl_meco2 %>%
          dplyr::select(-matches("_2010_2035|_2010_2020")) %>%
          dplyr::relocate(NRB_2020_2035_1MC, .after = zone_1MC) %>%
          dplyr::relocate(Harv_2020_2035_1MC, .after = NRB_2020_2035_1MC) %>%
          dplyr::select(-ends_with("_1MC")) %>%
          round_mc_result_columns(0)
        write.csv(NRB_fNRB3_fr_meco2, "LULCC/TempTables/summary_ecoregions_fr.csv", row.names=FALSE, quote=FALSE)
        write.csv(NRB_fNRB3_fr_meco2, "Out/webmofuss_results/summary_ecoregions_fr.csv", row.names=FALSE, quote=FALSE)
        
        print(paste0(admname," finished for tables"))
        
        userarea_simpx_fr2 <- ecoregions_gpkg %>%
          inner_join(.,NRB_fNRB3_fr_meco2, by="ECO_ID") %>%
          dplyr::select(-ECO_NAME.y, -NNH_NAME.y, -GID_0.y, -NAME_0.y, -Subregion.y, -mofuss_reg.y, -ID.y) %>%
          dplyr::rename(ECO_NAME = ECO_NAME.x,
                        NNH_NAME = NNH_NAME.x,
                        GID_0 = GID_0.x,
                        NAME_0 = NAME_0.x,
                        Subregion = Subregion.x,
                        ID = ID.x,
                        mofuss_reg = mofuss_reg.x) %>%
          replace(is.na(.), 0)
        st_write(userarea_simpx_fr2, "Out/webmofuss_results/mofuss_ecoregions_fr.gpkg", delete_layer = TRUE)
        print(paste0(admname," finished for vector layers"))
      }
      
      
      
    } else if (STdyn == 40){ # STdyn == 40 summary----
      print(40)
      
      NRBzon_frbind <- dplyr::bind_rows(NRBzon_frlist)
      summarycols <- c("NRB_2010_2040", "NRB_2020_2040", "NRB_2010_2020", "NRB_2020_2030", "NRB_2030_2040",
                       "Harv_2010_2040", "Harv_2020_2040", "Harv_2010_2020", "Harv_2020_2030",  "Harv_2030_2040", 
                       "fNRB_2010_2040", "fNRB_2020_2040", "fNRB_2010_2020", "fNRB_2020_2030", "fNRB_2030_2040")
      
      NRBzonfr_st <- summarise_mc_uncertainty(
        NRBzon_frbind, summarycols, expected_runs = MC
      )
      
      NRBzonfr_stR <- reduce(
        .x = list(
          c('NRB_2010_2040_mean', 'NRB_2010_2040_sd', 'NRB_2010_2040_se'),
          c('NRB_2020_2040_mean', 'NRB_2020_2040_sd', 'NRB_2020_2040_se'),
          c('NRB_2010_2020_mean', 'NRB_2010_2020_sd', 'NRB_2010_2020_se'),
          c('NRB_2020_2030_mean', 'NRB_2020_2030_sd', 'NRB_2020_2030_se'),
          c('NRB_2030_2040_mean', 'NRB_2030_2040_sd', 'NRB_2030_2040_se'),
          c('Harv_2010_2040_mean', 'Harv_2010_2040_sd', 'Harv_2010_2040_se'),
          c('Harv_2020_2040_mean', 'Harv_2020_2040_sd', 'Harv_2020_2040_se'),
          c('Harv_2010_2020_mean', 'Harv_2010_2020_sd', 'Harv_2010_2020_se'),
          c('Harv_2020_2030_mean', 'Harv_2020_2030_sd', 'Harv_2020_2030_se'),
          c('Harv_2030_2040_mean', 'Harv_2030_2040_sd', 'Harv_2030_2040_se'),
          c('fNRB_2010_2040_mean', 'fNRB_2010_2040_sd', 'fNRB_2010_2040_se'),
          c('fNRB_2020_2040_mean', 'fNRB_2020_2040_sd', 'fNRB_2020_2040_se'),
          c('fNRB_2010_2020_mean', 'fNRB_2010_2020_sd', 'fNRB_2010_2020_se'),
          c('fNRB_2020_2030_mean', 'fNRB_2020_2030_sd', 'fNRB_2020_2030_se'),
          c('fNRB_2030_2040_mean', 'fNRB_2030_2040_sd', 'fNRB_2030_2040_se')
        ),
        .f = ~ relocate(.x, .y[2], .after = .y[1]) %>% relocate(.y[3], .after = .y[2]),
        .init = NRBzonfr_st
      )
      NRBzonfr_stR
      names(NRBzonfr_stR)
      
      NRBzonfr_statsx <- NRBzonfr_stR
      
      if (MC < mcthreshold) {
        NRBzonfr_stats <- NRBzonfr_statsx %>%
          dplyr::mutate(across(ends_with(c("sd", "se")), ~ NA))
      } else {
        NRBzonfr_stats <- NRBzonfr_statsx
      }
      
      NRB_fNRB2_fr <- cbind(NRBzonfr_stats,NRBzon_frlist1MC) %>%
        round_mc_result_columns(uncertainty_digits)
      NRB_fNRB2_fr
      names(NRB_fNRB2_fr)
      
      if (admname == "adm0") {
        NRB_fNRB2_frcompl_madm0 <- userarea_gpkg %>%
          st_drop_geometry() %>%
          merge(., NRB_fNRB2_fr, by.x = ext_analysis_ID, by.y = "zone") %>%
          replace(is.na(.), 0)
        write.csv(NRB_fNRB2_frcompl_madm0, "LULCC/TempTables/summary_adm0_frcompl.csv", row.names=FALSE, quote=FALSE)
        write.csv(NRB_fNRB2_frcompl_madm0, "Out/webmofuss_results/summary_adm0_frcompl.csv", row.names=FALSE, quote=FALSE)
        
        NRB_fNRB3_fr_madm0 <- NRB_fNRB2_frcompl_madm0 %>%
          dplyr::select(-matches("_2010_2040|_2010_2020")) %>%
          dplyr::relocate(NRB_2020_2040_1MC, .after = zone_1MC) %>%
          dplyr::relocate(Harv_2020_2040_1MC, .after = NRB_2030_2040_1MC) %>%
          dplyr::select(-ends_with("_1MC")) %>%
          round_mc_result_columns(0)
        write.csv(NRB_fNRB3_fr_madm0, "LULCC/TempTables/summary_adm0_fr.csv", row.names=FALSE, quote=FALSE)
        write.csv(NRB_fNRB3_fr_madm0, "Out/webmofuss_results/summary_adm0_fr.csv", row.names=FALSE, quote=FALSE)
        
        print(paste0(admname," finished for tables"))
        
        if (aoi_poly == 1) {
          userarea_simpx_fr0 <- userarea_gpkg %>%
            inner_join(.,NRB_fNRB3_fr_madm0, by="ID") %>%
            dplyr::select(-NAME_0.y) %>%
            dplyr::rename(NAME_0 = NAME_0.x) %>%
            replace(is.na(.), 0)
          st_write(userarea_simpx_fr0, "Out/webmofuss_results/mofuss_adm0_fr.gpkg", delete_layer = TRUE)
          print(paste0(admname," finished for vector layers"))
        } else {
          userarea_simpx_fr0 <- userarea_gpkg %>%
            inner_join(.,NRB_fNRB3_fr_madm0, by="ID") %>%
            dplyr::select(-NAME_0.y, -Subregion.y, -mofuss_reg.y, -GID_0.y) %>%
            dplyr::rename(GID_0 = GID_0.x,
                          NAME_0 = NAME_0.x,
                          Subregion = Subregion.x,
                          mofuss_reg = mofuss_reg.x) %>%
            replace(is.na(.), 0)
          st_write(userarea_simpx_fr0, "Out/webmofuss_results/mofuss_adm0_fr.gpkg", delete_layer = TRUE)
          print(paste0(admname," finished for vector layers"))
        }
        
      } else if (admname == "adm1") {
        NRB_fNRB2_frcompl_madm1 <- userarea_gpkg1 %>%
          st_drop_geometry() %>%
          merge(., NRB_fNRB2_fr, by.x = ext_analysis_ID, by.y = "zone") %>%
          dplyr::select(-GID_0, -GID_1) %>%
          replace(is.na(.), 0)
        write.csv(NRB_fNRB2_frcompl_madm1, "LULCC/TempTables/summary_adm1_frcompl.csv", row.names=FALSE, quote=FALSE)
        write.csv(NRB_fNRB2_frcompl_madm1, "Out/webmofuss_results/summary_adm1_frcompl.csv", row.names=FALSE, quote=FALSE)
        
        NRB_fNRB3_fr_madm1 <- NRB_fNRB2_frcompl_madm1 %>%
          dplyr::select(-matches("_2010_2040|_2010_2020")) %>%
          dplyr::relocate(NRB_2020_2040_1MC, .after = zone_1MC) %>%
          dplyr::relocate(Harv_2020_2040_1MC, .after = NRB_2030_2040_1MC) %>%
          dplyr::select(-ends_with("_1MC")) %>%
          round_mc_result_columns(0)
        write.csv(NRB_fNRB3_fr_madm1, "LULCC/TempTables/summary_adm1_fr.csv", row.names=FALSE, quote=FALSE)
        write.csv(NRB_fNRB3_fr_madm1, "Out/webmofuss_results/summary_adm1_fr.csv", row.names=FALSE, quote=FALSE)
        
        print(paste0(admname," finished for tables"))
        
        userarea_simpx_fr1 <- userarea_gpkg1 %>%
          inner_join(.,NRB_fNRB3_fr_madm1, by="ID") %>%
          dplyr::select(-NAME_0.y, -NAME_1.y) %>%
          dplyr::rename(NAME_0 = NAME_0.x,
                        NAME_1 = NAME_1.x) %>%
          replace(is.na(.), 0)
        st_write(userarea_simpx_fr1, "Out/webmofuss_results/mofuss_adm1_fr.gpkg", delete_layer = TRUE)
        print(paste0(admname," finished for vector layers"))
        
      } else if (admname == "adm2") {
        NRB_fNRB2_frcompl_madm2 <- userarea_gpkg2 %>%
          st_drop_geometry() %>%
          merge(., NRB_fNRB2_fr, by.x = ext_analysis_ID, by.y = "zone") %>%
          dplyr::select(-GID_0, -GID_1, -GID_2) %>%
          replace(is.na(.), 0)
        write.csv(NRB_fNRB2_frcompl_madm2, "LULCC/TempTables/summary_adm2_frcompl.csv", row.names=FALSE, quote=FALSE)
        write.csv(NRB_fNRB2_frcompl_madm2, "Out/webmofuss_results/summary_adm2_frcompl.csv", row.names=FALSE, quote=FALSE)
        
        NRB_fNRB3_fr_madm2 <- NRB_fNRB2_frcompl_madm2 %>%
          dplyr::select(-matches("_2010_2040|_2010_2020")) %>%
          dplyr::relocate(NRB_2020_2040_1MC, .after = zone_1MC) %>%
          dplyr::relocate(Harv_2020_2040_1MC, .after = NRB_2030_2040_1MC) %>%
          dplyr::select(-ends_with("_1MC")) %>%
          round_mc_result_columns(0)
        write.csv(NRB_fNRB3_fr_madm2, "LULCC/TempTables/summary_adm2_fr.csv", row.names=FALSE, quote=FALSE)
        write.csv(NRB_fNRB3_fr_madm2, "Out/webmofuss_results/summary_adm2_fr.csv", row.names=FALSE, quote=FALSE)
        
        print(paste0(admname," finished for tables"))
        
        userarea_simpx_fr2 <- userarea_gpkg2 %>%
          inner_join(.,NRB_fNRB3_fr_madm2, by="ID") %>%
          dplyr::select(-NAME_0.y, -NAME_1.y, -NAME_2.y) %>%
          dplyr::rename(NAME_0 = NAME_0.x,
                        NAME_1 = NAME_1.x,
                        NAME_2 = NAME_2.x,) %>%
          replace(is.na(.), 0)
        st_write(userarea_simpx_fr2, "Out/webmofuss_results/mofuss_adm2_fr.gpkg", delete_layer = TRUE)
        print(paste0(admname," finished for vector layers"))
        
      } else if (admname == "ecoregions") {
        NRB_fNRB2_frcompl_meco2 <- ecoregions_gpkg %>%
          st_drop_geometry() %>%
          merge(., NRB_fNRB2_fr, by.x = ecoregions_ID, by.y = "zone") %>%
          replace(is.na(.), 0)
        write.csv(NRB_fNRB2_frcompl_meco2, "LULCC/TempTables/summary_ecoregions_frcompl.csv", row.names=FALSE, quote=FALSE)
        write.csv(NRB_fNRB2_frcompl_meco2, "Out/webmofuss_results/summary_ecoregions_frcompl.csv", row.names=FALSE, quote=FALSE)
        
        NRB_fNRB3_fr_meco2 <- NRB_fNRB2_frcompl_meco2 %>%
          dplyr::select(-matches("_2010_2040|_2010_2020")) %>%
          dplyr::relocate(NRB_2020_2040_1MC, .after = zone_1MC) %>%
          dplyr::relocate(Harv_2020_2040_1MC, .after = NRB_2030_2040_1MC) %>%
          dplyr::select(-ends_with("_1MC")) %>%
          round_mc_result_columns(0)
        write.csv(NRB_fNRB3_fr_meco2, "LULCC/TempTables/summary_ecoregions_fr.csv", row.names=FALSE, quote=FALSE)
        write.csv(NRB_fNRB3_fr_meco2, "Out/webmofuss_results/summary_ecoregions_fr.csv", row.names=FALSE, quote=FALSE)
        
        print(paste0(admname," finished for tables"))
        
        userarea_simpx_fr2 <- ecoregions_gpkg %>%
          inner_join(.,NRB_fNRB3_fr_meco2, by="ECO_ID") %>%
          dplyr::select(-ECO_NAME.y, -NNH_NAME.y, -GID_0.y, -NAME_0.y, -Subregion.y, -mofuss_reg.y, -ID.y) %>%
          dplyr::rename(ECO_NAME = ECO_NAME.x,
                        NNH_NAME = NNH_NAME.x,
                        GID_0 = GID_0.x,
                        NAME_0 = NAME_0.x,
                        Subregion = Subregion.x,
                        ID = ID.x,
                        mofuss_reg = mofuss_reg.x) %>%
          replace(is.na(.), 0)
        st_write(userarea_simpx_fr2, "Out/webmofuss_results/mofuss_ecoregions_fr.gpkg", delete_layer = TRUE)
        print(paste0(admname," finished for vector layers"))
      }
      
      
      
    } else if (STdyn == 50){ # STdyn == 50 summary----
      print(50)
      
      NRBzon_frbind <- dplyr::bind_rows(NRBzon_frlist)
      summarycols <- c("NRB_2010_2050", "NRB_2020_2050", "NRB_2010_2020", "NRB_2020_2030", "NRB_2030_2040", "NRB_2040_2050",
                       "Harv_2010_2050", "Harv_2020_2050", "Harv_2010_2020", "Harv_2020_2030",  "Harv_2030_2040", "Harv_2040_2050", 
                       "fNRB_2010_2050", "fNRB_2020_2050", "fNRB_2010_2020", "fNRB_2020_2030", "fNRB_2030_2040", "fNRB_2040_2050")
      
      NRBzonfr_st <- summarise_mc_uncertainty(
        NRBzon_frbind, summarycols, expected_runs = MC
      )
      
      NRBzonfr_stR <- reduce(
        .x = list(
          c('NRB_2010_2050_mean', 'NRB_2010_2050_sd', 'NRB_2010_2050_se'),
          c('NRB_2020_2050_mean', 'NRB_2020_2050_sd', 'NRB_2020_2050_se'),
          c('NRB_2010_2020_mean', 'NRB_2010_2020_sd', 'NRB_2010_2020_se'),
          c('NRB_2020_2030_mean', 'NRB_2020_2030_sd', 'NRB_2020_2030_se'),
          c('NRB_2030_2040_mean', 'NRB_2030_2040_sd', 'NRB_2030_2040_se'),
          c('NRB_2040_2050_mean', 'NRB_2040_2050_sd', 'NRB_2040_2050_se'),
          c('Harv_2010_2050_mean', 'Harv_2010_2050_sd', 'Harv_2010_2050_se'),
          c('Harv_2020_2050_mean', 'Harv_2020_2050_sd', 'Harv_2020_2050_se'),
          c('Harv_2010_2020_mean', 'Harv_2010_2020_sd', 'Harv_2010_2020_se'),
          c('Harv_2020_2030_mean', 'Harv_2020_2030_sd', 'Harv_2020_2030_se'),
          c('Harv_2030_2040_mean', 'Harv_2030_2040_sd', 'Harv_2030_2040_se'),
          c('Harv_2040_2050_mean', 'Harv_2040_2050_sd', 'Harv_2040_2050_se'),
          c('fNRB_2010_2050_mean', 'fNRB_2010_2050_sd', 'fNRB_2010_2050_se'),
          c('fNRB_2020_2050_mean', 'fNRB_2020_2050_sd', 'fNRB_2020_2050_se'),
          c('fNRB_2010_2020_mean', 'fNRB_2010_2020_sd', 'fNRB_2010_2020_se'),
          c('fNRB_2020_2030_mean', 'fNRB_2020_2030_sd', 'fNRB_2020_2030_se'),
          c('fNRB_2030_2040_mean', 'fNRB_2030_2040_sd', 'fNRB_2030_2040_se'),
          c('fNRB_2040_2050_mean', 'fNRB_2040_2050_sd', 'fNRB_2040_2050_se')
        ),
        .f = ~ relocate(.x, .y[2], .after = .y[1]) %>% relocate(.y[3], .after = .y[2]),
        .init = NRBzonfr_st
      )
      NRBzonfr_stR
      names(NRBzonfr_stR)
      
      NRBzonfr_statsx <- NRBzonfr_stR
      names(NRBzonfr_statsx)
      
      if (MC < mcthreshold) {
        NRBzonfr_stats <- NRBzonfr_statsx %>%
          dplyr::mutate(across(ends_with(c("sd", "se")), ~ NA))
      } else {
        NRBzonfr_stats <- NRBzonfr_statsx
      }
      
      NRB_fNRB2_fr <- cbind(NRBzonfr_stats,NRBzon_frlist1MC) %>%
        round_mc_result_columns(uncertainty_digits)
      NRB_fNRB2_fr
      names(NRB_fNRB2_fr)
      
      if (admname == "adm0") {
        NRB_fNRB2_frcompl_madm0 <- userarea_gpkg %>%
          st_drop_geometry() %>%
          merge(., NRB_fNRB2_fr, by.x = ext_analysis_ID, by.y = "zone") %>%
          replace(is.na(.), 0)
        write.csv(NRB_fNRB2_frcompl_madm0, "LULCC/TempTables/summary_adm0_frcompl.csv", row.names=FALSE, quote=FALSE)
        write.csv(NRB_fNRB2_frcompl_madm0, "Out/webmofuss_results/summary_adm0_frcompl.csv", row.names=FALSE, quote=FALSE)
        
        NRB_fNRB3_fr_madm0 <- NRB_fNRB2_frcompl_madm0 %>%
          dplyr::select(-matches("_2010_2050|_2010_2020")) %>%
          dplyr::relocate(NRB_2020_2050_1MC, .after = zone_1MC) %>%
          dplyr::relocate(Harv_2020_2050_1MC, .after = NRB_2040_2050_1MC) %>%
          dplyr::select(-ends_with("_1MC")) %>%
          round_mc_result_columns(0)
        write.csv(NRB_fNRB3_fr_madm0, "LULCC/TempTables/summary_adm0_fr.csv", row.names=FALSE, quote=FALSE)
        write.csv(NRB_fNRB3_fr_madm0, "Out/webmofuss_results/summary_adm0_fr.csv", row.names=FALSE, quote=FALSE)
        
        print(paste0(admname," finished for tables"))
        
        if (aoi_poly == 1) {
          userarea_simpx_fr0 <- userarea_gpkg %>%
            inner_join(.,NRB_fNRB3_fr_madm0, by="ID") %>%
            dplyr::select(-NAME_0.y) %>%
            dplyr::rename(NAME_0 = NAME_0.x) %>%
            replace(is.na(.), 0)
          st_write(userarea_simpx_fr0, "Out/webmofuss_results/mofuss_adm0_fr.gpkg", delete_layer = TRUE)
          print(paste0(admname," finished for vector layers"))
        } else {
          userarea_simpx_fr0 <- userarea_gpkg %>%
            inner_join(.,NRB_fNRB3_fr_madm0, by="ID") %>%
            dplyr::select(-NAME_0.y, -Subregion.y, -mofuss_reg.y, -GID_0.y) %>%
            dplyr::rename(GID_0 = GID_0.x,
                          NAME_0 = NAME_0.x,
                          Subregion = Subregion.x,
                          mofuss_reg = mofuss_reg.x) %>%
            replace(is.na(.), 0)
          st_write(userarea_simpx_fr0, "Out/webmofuss_results/mofuss_adm0_fr.gpkg", delete_layer = TRUE)
          print(paste0(admname," finished for vector layers"))
        }
        
      } else if (admname == "adm1") {
        NRB_fNRB2_frcompl_madm1 <- userarea_gpkg1 %>%
          st_drop_geometry() %>%
          merge(., NRB_fNRB2_fr, by.x = ext_analysis_ID, by.y = "zone") %>%
          dplyr::select(-GID_0, -GID_1) %>%
          replace(is.na(.), 0)
        write.csv(NRB_fNRB2_frcompl_madm1, "LULCC/TempTables/summary_adm1_frcompl.csv", row.names=FALSE, quote=FALSE)
        write.csv(NRB_fNRB2_frcompl_madm1, "Out/webmofuss_results/summary_adm1_frcompl.csv", row.names=FALSE, quote=FALSE)
        
        NRB_fNRB3_fr_madm1 <- NRB_fNRB2_frcompl_madm1 %>%
          dplyr::select(-matches("_2010_2050|_2010_2020")) %>%
          dplyr::relocate(NRB_2020_2050_1MC, .after = zone_1MC) %>%
          dplyr::relocate(Harv_2020_2050_1MC, .after = NRB_2040_2050_1MC) %>%
          dplyr::select(-ends_with("_1MC")) %>%
          round_mc_result_columns(0)
        write.csv(NRB_fNRB3_fr_madm1, "LULCC/TempTables/summary_adm1_fr.csv", row.names=FALSE, quote=FALSE)
        write.csv(NRB_fNRB3_fr_madm1, "Out/webmofuss_results/summary_adm1_fr.csv", row.names=FALSE, quote=FALSE)
        
        print(paste0(admname," finished for tables"))
        
        userarea_simpx_fr1 <- userarea_gpkg1 %>%
          inner_join(.,NRB_fNRB3_fr_madm1, by="ID") %>%
          dplyr::select(-NAME_0.y, -NAME_1.y) %>%
          dplyr::rename(NAME_0 = NAME_0.x,
                        NAME_1 = NAME_1.x) %>%
          replace(is.na(.), 0)
        st_write(userarea_simpx_fr1, "Out/webmofuss_results/mofuss_adm1_fr.gpkg", delete_layer = TRUE)
        print(paste0(admname," finished for vector layers"))
        
      } else if (admname == "adm2") {
        NRB_fNRB2_frcompl_madm2 <- userarea_gpkg2 %>%
          st_drop_geometry() %>%
          merge(., NRB_fNRB2_fr, by.x = ext_analysis_ID, by.y = "zone") %>%
          dplyr::select(-GID_0, -GID_1, -GID_2) %>%
          replace(is.na(.), 0)
        write.csv(NRB_fNRB2_frcompl_madm2, "LULCC/TempTables/summary_adm2_frcompl.csv", row.names=FALSE, quote=FALSE)
        write.csv(NRB_fNRB2_frcompl_madm2, "Out/webmofuss_results/summary_adm2_frcompl.csv", row.names=FALSE, quote=FALSE)
        
        NRB_fNRB3_fr_madm2 <- NRB_fNRB2_frcompl_madm2 %>%
          dplyr::select(-matches("_2010_2050|_2010_2020")) %>%
          dplyr::relocate(NRB_2020_2050_1MC, .after = zone_1MC) %>%
          dplyr::relocate(Harv_2020_2050_1MC, .after = NRB_2040_2050_1MC) %>%
          dplyr::select(-ends_with("_1MC")) %>%
          round_mc_result_columns(0)
        write.csv(NRB_fNRB3_fr_madm2, "LULCC/TempTables/summary_adm2_fr.csv", row.names=FALSE, quote=FALSE)
        write.csv(NRB_fNRB3_fr_madm2, "Out/webmofuss_results/summary_adm2_fr.csv", row.names=FALSE, quote=FALSE)
        
        print(paste0(admname," finished for tables"))
        
        userarea_simpx_fr2 <- userarea_gpkg2 %>%
          inner_join(.,NRB_fNRB3_fr_madm2, by="ID") %>%
          dplyr::select(-NAME_0.y, -NAME_1.y, -NAME_2.y) %>%
          dplyr::rename(NAME_0 = NAME_0.x,
                        NAME_1 = NAME_1.x,
                        NAME_2 = NAME_2.x,) %>%
          replace(is.na(.), 0)
        st_write(userarea_simpx_fr2, "Out/webmofuss_results/mofuss_adm2_fr.gpkg", delete_layer = TRUE)
        print(paste0(admname," finished for vector layers"))
        
      } else if (admname == "ecoregions") {
        NRB_fNRB2_frcompl_meco2 <- ecoregions_gpkg %>%
          st_drop_geometry() %>%
          merge(., NRB_fNRB2_fr, by.x = ecoregions_ID, by.y = "zone") %>%
          replace(is.na(.), 0)
        write.csv(NRB_fNRB2_frcompl_meco2, "LULCC/TempTables/summary_ecoregions_frcompl.csv", row.names=FALSE, quote=FALSE)
        write.csv(NRB_fNRB2_frcompl_meco2, "Out/webmofuss_results/summary_ecoregions_frcompl.csv", row.names=FALSE, quote=FALSE)

        NRB_fNRB3_fr_meco2 <- NRB_fNRB2_frcompl_meco2 %>%
          dplyr::select(-matches("_2010_2050|_2010_2020")) %>%
          dplyr::relocate(NRB_2020_2050_1MC, .after = zone_1MC) %>%
          dplyr::relocate(Harv_2020_2050_1MC, .after = NRB_2040_2050_1MC) %>%
          dplyr::select(-ends_with("_1MC")) %>%
          round_mc_result_columns(0)
        write.csv(NRB_fNRB3_fr_meco2, "LULCC/TempTables/summary_ecoregions_fr.csv", row.names=FALSE, quote=FALSE)
        write.csv(NRB_fNRB3_fr_meco2, "Out/webmofuss_results/summary_ecoregions_fr.csv", row.names=FALSE, quote=FALSE)
        
        print(paste0(admname," finished for tables"))
        
        userarea_simpx_fr2 <- ecoregions_gpkg %>%
          inner_join(.,NRB_fNRB3_fr_meco2, by="ECO_ID") %>%
          dplyr::select(-ECO_NAME.y, -NNH_NAME.y, -GID_0.y, -NAME_0.y, -Subregion.y, -mofuss_reg.y, -ID.y) %>%
          dplyr::rename(ECO_NAME = ECO_NAME.x,
                        NNH_NAME = NNH_NAME.x,
                        GID_0 = GID_0.x,
                        NAME_0 = NAME_0.x,
                        Subregion = Subregion.x,
                        ID = ID.x,
                        mofuss_reg = mofuss_reg.x) %>%
          replace(is.na(.), 0)
        st_write(userarea_simpx_fr2, "Out/webmofuss_results/mofuss_ecoregions_fr.gpkg", delete_layer = TRUE)
        print(paste0(admname," finished for vector layers"))
      }
      
    } else {
      print("error with simulation length")  
    }
    
  } # foreach(admm = adminlevel, admname = admin_name) %do% {
  
} # if (fNRB_partition_tables == 1) {

# Compile LaTeX file into PDF report ####
if (compilelatex == 1) {
  
  # ---- Modern summary report (auto-generated; replaces legacy texi2dvi block) ----
  # Reads current MoFuSS outputs, auto-detects available data and figures,
  # compiles the modern template with MiKTeX, and writes:
  #     Summary_Report/MoFuSS_Summary_Report_<scenario>_scenario.pdf
  # The original legacy block is preserved in maps_animations7_backup_premodern.R
  print("Run LaTeX and compile modern summary report (pdf)")
  source(file.path(getwd(), "LaTeX", "generate_modern_report_v8.R"))
   report_out <- generate_modern_report(
     base_dir = getwd(),
     output_dir = OutDir,
     mc_threshold = mcthreshold
   )
  print(paste("Modern report written:", report_out))
  
}


# END ----

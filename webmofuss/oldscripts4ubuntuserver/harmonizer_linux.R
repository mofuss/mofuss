# MoFuSS
# Windows version
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
unlink("Debugging//*.*", force=TRUE)
unlink("Temp//*.*", force=TRUE)
unlink("HTML_animation*//*", recursive = TRUE,force=TRUE)
unlink("*.Rout",force=TRUE)
#unlink("*.txt",force=TRUE) # set to only erase specific txts
unlink("*.log",force=TRUE)
unlink("Mofuss_Summary_Report.aux",force=TRUE)
unlink("Mofuss_Summary_Report.lof",force=TRUE)
unlink("Mofuss_Summary_Report.lot",force=TRUE)
unlink("Mofuss_Summary_Report.out",force=TRUE)
unlink("Mofuss_Summary_Report.toc",force=TRUE)
unlink("In//*.*",force=TRUE)
unlink("In//IDW_boost//*.*",force=TRUE)
unlink("Out*//*", recursive = TRUE,force=TRUE)
unlink("LaTeX//*.pdf",force=TRUE)
unlink("LaTeX//*.mp4",force=TRUE)
unlink("LaTeX//*.csv",force=TRUE)
unlink("LaTeX//SimLength.txt",force=TRUE)
unlink("LaTeX//MCruns.txt",force=TRUE)
unlink("Summary_Report//*.*",force=TRUE)
unlink("Logs//*.*",force=TRUE)
unlink("LULCC//Out_lulcc//*.*",force=TRUE)
unlink("LULCC//TempRaster//*.*",force=TRUE)
unlink("LULCC//TempTables//Friction_drivingoverroads.csv",force=TRUE)
unlink("LULCC//TempTables//Friction_drivingoverroads_calcdist.csv",force=TRUE)
unlink("LULCC//TempTables//Friction_rivers_reclass.csv",force=TRUE)
unlink("LULCC//TempTables//Friction_rivers_reclass_calcdist.csv",force=TRUE)
unlink("LULCC//TempTables//Friction_walkingcrosscountry.csv",force=TRUE)
unlink("LULCC//TempTables//Friction_walkingoverroads.csv",force=TRUE)
unlink("LULCC//TempTables//Supply_parameters.csv",force=TRUE)
unlink("LULCC//TempTables//Resolution.csv",force=TRUE)
unlink("LULCC//TempVector//*.*",force=TRUE)
unlink("LULCC//TempVector_GCS//*.*",force=TRUE)
#unlink("LULCC//*.txt",force=TRUE) #erase specifics txt
unlink("LULCC//*.csv",force=TRUE)	
unlink("LULCC//InTables//fwuse*.*",force=TRUE)
unlink("LULCC//CroppingCountrySpecific.R",force=TRUE)
unlink("LULCC//CroppingCountrySpecific.Rout",force=TRUE)

Country<-readLines("LULCC/TempTables/Country.txt")

flist <- list.files("LULCC/SourceData/InTables")
file.copy(paste0("LULCC/SourceData/InTables/",flist), "LULCC/TempTables")

# Set master dir ####
setwd(paste0(getwd(),"/LULCC"))
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
  resolution=100 #in meters
  GEpoly=0
  nameuser="Juan de los Palotes"		
  ads="Norwegian University of Life Sciences, Department of Environmental Anthropology"
  ads_ctry="in the city of As, Norway"
}else{
  for(i in 1:length(args)){
    eval(parse(text=args[[i]]))
  }
}

# Read parameters table, checking if its delimiter is comma or semicolon ####
read_csv(glue("{master_dir}/SourceData/parameters.csv")) %>% 
  {if(is.null(.$ParCHR[1])) read_csv2(glue("{master_dir}/SourceData/parameters.csv")) else .} -> country_parameters
# Append "+"
country_parameters$ParCHR[1] <- str_c("+", country_parameters$ParCHR[1])
country_parameters$ParCHR[2] <- str_c("+", country_parameters$ParCHR[2])
# print(tbl_df(country_parameters), n=100) ####

# Read supply parameters table, checking if its delimiter is comma or semicolon ####
read_csv(glue("{master_dir}/TempTables/growth_parameters.csv")) %>% 
  {if(is.null(.$TOF[1])) read_csv2(glue("{master_dir}/TempTables/growth_parameters.csv")) else .} -> growth_parameters

#save resolution as csv for down stream uses
write.csv(resolution, "TempTables/Resolution.csv")

#save user data as txt for down stream uses
writeLines(paste0(gsub("_"," ",nameuser),",from ",
                  gsub("_"," ",ads)," in ", 
                  gsub("_"," ",ads_ctry),","),"TempTables/UserData.txt", useBytes=T)
readLines("TempTables/UserData.txt")


# Select Area: Google Earth .kml or ESRI .shp ####
 country_parameters %>%
   filter(Var == "ProjUTM") %>%
   pull(ParCHR) -> UTMproj
 st_crs(UTMproj) %>% 
   .$epsg -> epsg_utm
 
 country_parameters %>%
   filter(Var == "ProjGCS") %>%
   pull(ParCHR) -> GCSproj
 st_crs(GCSproj) %>% 
   .$epsg -> epsg_gcs
 
 if (GEpoly == 1) {
   if (length(list.files("SourceData/InVector_GCS", ".kml")) == 0) {
     #file.show(file.path(getwd(), "Wizard_imgs/NoKMLs.pdf"))
   } else {
     if (length(list.files("SourceData/InVector_GCS", ".kml")) > 1) {
       #file.show(file.path(getwd(), "Wizard_imgs/ManyKMLs.pdf"))
       thisdir<-getwd()
       setwd(file.path(getwd(), "SourceData/InVector_GCS/"))
       nrbpoly<-tk_choose.files(caption = "Choose a Google Earth kml file as the analysis area",
                                filters=matrix(c("", ".kml"),
                                               1, 2, byrow = TRUE))
       setwd(thisdir)                 
     } else { 
       nrbpoly<-paste0("SourceData/InVector_GCS/",list.files("SourceData/InVector_GCS", ".kml"))
     }
   }
   polykml = st_read(nrbpoly) 
   dfx =  data.frame(1,"GoogleEarthPoly")
   colnames(dfx) <- c(country_parameters %>%
                         filter(Var == "ext_analysis_ID") %>%
                         pull(ParCHR),
                       country_parameters %>%
                         filter(Var == "ext_analysis_NAME") %>%
                         pull(ParCHR))
   bind_cols (polykml, dfx) %>%
     st_zm() %>%
     subset(select=-c(Name,description)) -> userarea_GCS
   userarea <- st_transform(userarea_GCS, epsg_utm)
 }else{
   if (length(list.files("SourceData/InVector", ".gpkg")) == 0) {
     #file.show(file.path(getwd(), "Wizard_imgs/NoKMLs.pdf"))
   } else {
     if (length(list.files("SourceData/InVector", ".gpkg")) > 2) {
       #file.show(file.path(getwd(), "Wizard_imgs/ManySHPs.pdf"))
       unlink("SourceData//InVector//*.shp.xml",force=TRUE)
       thisdir<-getwd()
       setwd(file.path(getwd(), "SourceData/InVector/"))
       nrbpolyshp <- tk_choose.files(caption = "Choose a geopackage file as the analysis area",
                                   filters=matrix(c("", ".gpkg"),
                                                  1, 2, byrow = TRUE))
       setwd(thisdir)
       userarea <- st_read(nrbpolyshp)
       #   st_set_crs(country_parameters %>%
       #                filter(Var == "ProjUTM") %>% 
       #                pull(ParCHR)) -> userarea
       st_transform(userarea, epsg_gcs) -> userarea_GCS
     } else { 
       st_read("SourceData/InVector/ext_analysis.gpkg") -> userarea
       st_transform(userarea, epsg_gcs) -> userarea_GCS		
     }
   }
 }

st_write(userarea_GCS, "TempVector_GCS/userarea_GCS.gpkg", delete_layer=TRUE)
st_write(userarea, "TempVector/userarea.gpkg", delete_layer=TRUE)

# Vector masks and extents 
mask <- st_read("SourceData/InVector/mask.gpkg")
if (GEpoly == 1) {
  analysisshp <- st_intersection(mask, userarea)
  analysisshp_GCS <- st_transform(analysisshp, epsg_gcs)
} else {
  analysisshp <- userarea
  analysisshp_GCS <- st_transform(userarea, epsg_gcs)	
}
# For future use in figures and IDW_Boost  ####
mask_GCS<-st_transform(mask,epsg_gcs)
st_write (analysisshp,"TempVector/ext_analysis.gpkg", delete_layer=TRUE)
st_write (mask_GCS,"TempVector_GCS/mask_gcs.gpkg", delete_layer=TRUE)
st_write (analysisshp_GCS,"TempVector_GCS/ext_analysis_gcs.gpkg", delete_layer=TRUE)

# Create a raster mask of a certain size size, extent, resolution and projection as a provided raster template for the follwoing operations. ####
userarea_ras <- raster(userarea, res=resolution)
userarea_ras <- fasterize(userarea, userarea_ras,
                          field = country_parameters %>%
                            filter(Var == "ext_analysis_ID") %>%
                            pull(ParCHR))
userarea_r_m <- crop(userarea_ras, extent(mask)) %>%
  mask(mask)
userarea_r <- (userarea_r_m*0)+1
writeRaster(userarea_r, filename="TempRaster//mask_c.tif", datatype="INT2S", overwrite=TRUE)
mask_r <- fasterize(mask, userarea_r,
                    field = country_parameters %>%
                      filter(Var == "ext_analysis_ID") %>%
                      pull(ParCHR))
mask_r_m <- crop(mask_r, extent(userarea_r)) %>%
  mask(userarea_r)
writeRaster(mask_r_m, filename="TempRaster//admin_c.tif", datatype="INT2S", overwrite=TRUE)

# userarea_r_GCS <- raster(userarea_GCS, res=0.0008333333)
# userarea_r_codes_GCS <- fasterize(userarea_GCS, userarea_r_codes_GCS, field = ext_analysis_ID)
# userarea_r_GCS<-(userarea_r_codes_GCS*0)+1

# Mask for grass purposes ####
# Extent
analysisshp %>% 
  st_bbox() -> ext

# Create grass database ####
unlink("grass", recursive= TRUE, force=TRUE)
glue("grass -e -c EPSG:{epsg_utm} {master_dir}/grass/utm") %>% 
  system()

"grass --config path" %>% system(intern = T) -> pth

# Initialize grass
initGRASS(gisBase = pth,
          gisDbase = glue("{master_dir}/grass/"),
          location = "utm",
          mapset = "PERMANENT",
          override = T)

# Set region
execGRASS("g.region",
          parameters = list(w = as.character(ext[1]),
                            s = as.character(ext[2]),
                            e = as.character(ext[3]),
                            n = as.character(ext[4]),
                            res = as.character(resolution)),
          flags = "a")

# Import polygon to crop
st_read("TempVector/ext_analysis.gpkg") %>%
  writeVECT("analysisshp_vgr", v.in.ogr_flags="overwrite")

# process DEM ####
# Import dtem
tic()
glue("{master_dir}/SourceData/InRaster/DTEM.tif") %>% 
  {execGRASS("r.in.gdal",
             parameters = list(input = .,
                               output = "dtem"),
             flags = "overwrite")}
# Set mask
execGRASS("r.mask",
          parameters = list(vector = "analysisshp_vgr"))

# Resample
execGRASS("r.resamp.interp",
          parameters = list(input = "dtem",
                            output = "dtem",
                            method = "bilinear"),
          flags = "overwrite")
# # Remove mask
# execGRASS("r.mask",
#           flags = "r")
# Export
execGRASS("r.out.gdal",
          parameters = list(input = "dtem",
                            output = glue("{master_dir}/TempRaster/DEM_c.tif"),
                            nodata=-9999,
                            type="Int16"),
          flags = c("overwrite", "f"))
toc()

# # win chunk:
# tic()
# raster("SourceData/InRaster/DTEM.tif") %>%
#   crop(extent(userarea_r)) %>%
#   resample(userarea_r, "bilinear") %>%
#   mask(userarea_r) -> DEM_r_m 
# writeRaster(DEM_r_m, filename="TempRaster//DEM_c.tif", datatype="INT2S", overwrite=TRUE)
# toc()

# tree cover, forest loss and forest gain ####
tic()
# Import treecover
country_parameters %>% 
  filter(Var == "treecover_name") %>% 
  pull(ParCHR) %>% 
  {glue("{master_dir}/SourceData/InRaster/{.}")} %>% 
  {execGRASS("r.in.gdal",
             parameters = list(input = .,
                               output = "tc2000"),
             flags = "overwrite")}

# Import gain
country_parameters %>% 
  filter(Var == "gain_name") %>% 
  pull(ParCHR) %>% 
  {glue("{master_dir}/SourceData/InRaster/{.}")} %>% 
  {execGRASS("r.in.gdal",
             parameters = list(input = .,
                               output = "gain"),
             flags = "overwrite")}

# Import loss year
country_parameters %>% 
  filter(Var == "lossyear_name") %>% 
  pull(ParCHR) %>% 
  {glue("{master_dir}/SourceData/InRaster/{.}")} %>% 
  {execGRASS("r.in.gdal",
             parameters = list(input = .,
                               output = "lossyear"),
             flags = "overwrite")}

# # Set mask
# execGRASS("r.mask",
#           parameters = list(vector = "analysisshp_vgr"))

# Resample
execGRASS("r.resamp.interp",
          parameters = list(input = "tc2000",
                            output = "tc2000",
                            method = "bilinear"),
          flags = "overwrite")

execGRASS("r.resamp.interp",
          parameters = list(input = "gain",
                            output = "gain",
                            method = "nearest"),
          flags = "overwrite")

execGRASS("r.resamp.interp",
          parameters = list(input = "lossyear",
                            output = "lossyear",
                            method = "nearest"),
          flags = "overwrite")

# # Remove mask
# execGRASS("r.mask",
#           flags = "r")

# Export
execGRASS("r.out.gdal",
          parameters = list(input = "tc2000",
                            output = glue("{master_dir}/TempRaster/tc2000_c.tif"),
                            nodata = -9999),
          flags = "overwrite")

execGRASS("r.out.gdal",
          parameters = list(input = "gain",
                            output = glue("{master_dir}/TempRaster/gain_c.tif"),
                            nodata = -9999,
                            type="Int16"),
          flags = c("overwrite", "f"))


execGRASS("r.out.gdal",
          parameters = list(input = "lossyear",
                            output = glue("{master_dir}/TempRaster/lossyear_c.tif"),
                            nodata = -9999),
          flags = "overwrite")
toc()

# # win chunks
# tic()
# country_parameters %>%
#   filter(Var == "treecover_name") %>%
#   pull(ParCHR) -> treecover_name
# raster(paste0("SourceData/InRaster/",treecover_name)) %>%
#   crop(extent(userarea_r)) %>%
#   resample(userarea_r, "bilinear") %>%
#   mask(userarea_r) -> tc2000_r_m 
# writeRaster(tc2000_r_m, filename="TempRaster//tc2000_c.tif", datatype="INT2S", overwrite=TRUE)
# 
# country_parameters %>%
#   filter(Var == "gain_name") %>%
#   pull(ParCHR) -> gain_name
# raster(paste0("SourceData/InRaster/",gain_name)) %>%
#   crop(extent(userarea_r)) %>%
#   resample(userarea_r, "ngb") %>%
#   mask(userarea_r) -> gain_r_m 
# writeRaster(gain_r_m, filename="TempRaster//gain_c.tif", datatype="INT2S", overwrite=TRUE)
# 
# country_parameters %>%
#   filter(Var == "lossyear_name") %>%
#   pull(ParCHR) -> lossyear_name
# raster(paste0("SourceData/InRaster/",lossyear_name)) %>%
#   crop(extent(userarea_r)) %>%
#   resample(userarea_r, "ngb") %>%
#   mask(userarea_r) -> lossyear_r_m 
# writeRaster(lossyear_r_m, filename="TempRaster//lossyear_c.tif", datatype="INT2S", overwrite=TRUE)
# toc()

# Loss maps annualizations for LULCC modeling ####
raster("TempRaster/lossyear_c.tif") %>%
  {.} %in% 1:12 %>% 
  writeRaster(filename="TempRaster//lossyear01_12_c.tif", datatype="INT2S", overwrite=TRUE)

raster("TempRaster/lossyear_c.tif") %>%
  {.} %in% 1:6 %>% 
  writeRaster(filename="TempRaster//lossyear01_06_c.tif", datatype="INT2S", overwrite=TRUE)

# Annual losses using apply or map ####
tic()
lossyear_r_m <- raster("TempRaster/lossyear_c.tif")
seq(1:12) %>% 
  walk(function(i){
    lossyear_r_m[lossyear_r_m == i] <- 1
    lossyear_r_m[lossyear_r_m != i] <- 0
    #lossyear_r_m[lossyear_r_m != i | is.na(lossyear_r_m)] <- 0
    lossyear_r_m %>%
      writeRaster(str_c("TempRaster/AnnLoss", str_pad(i, width = 2, pad = "0"),".tif"), datatype = "INT2S",overwrite = T)
    })
toc()

# #Annual losses using apply or map PARALLELIZED --- try in linux ####
# tic()
# lossyear_r_m <- raster("TempRaster/lossyear_c.tif")
# plan(multisession, workers = 8)
# seq(1:12) %>%
#   future_map(function(i){
#     lossyear_r_m[lossyear_r_m == i] <- 1
#     lossyear_r_m[lossyear_r_m != i] <- 0
#     #lossyear_r_m[lossyear_r_m != i | is.na(lossyear_r_m)] <- 0
#     lossyear_r_m %>%
#       writeRaster(str_c("TempRaster/AnnLoss", str_pad(i, width = 2, pad = "0"),".tif"), datatype = "INT2S",overwrite = T)
#   })
# toc()

# LULC maps ####
# Import map
country_parameters %>% 
  filter(Var == "LULCt1map_name") %>%
  pull(ParCHR) %>% 
  {glue("{master_dir}/SourceData/InRaster/{.}")} %>% 
  {execGRASS("r.in.gdal",
             parameters = list(input = .,
                               output = "lulc_t1"),
             flags = "overwrite")}

# # Set mask
# execGRASS("r.mask",
#           parameters = list(vector = "analysisshp_vgr"))

# Resample
execGRASS("r.resamp.interp",
          parameters = list(input = "lulc_t1",
                            output = "lulc_t1",
                            method = "nearest"),
          flags = "overwrite")

# # Remove mask
# execGRASS("r.mask",
#           flags = "r")

# Reclassify with TOF
growth_parameters %>% 
  rename(Value = 1) %>%
  mutate(TOF_c = str_c(Value, " = ", TOF)) %>%
  dplyr::select(TOF_c) %>%
  write_csv(glue("{master_dir}/TempRaster/rules.txt"), col_names = F)

execGRASS("r.reclass",
          parameters = list(input = "lulc_t1",
                            output = "lulc_t1_tof",
                            rules = glue("{master_dir}/TempRaster/rules.txt")),
          flags = "overwrite")

unlink(glue("{master_dir}/TempRaster/rules.txt"))

# Export
execGRASS("r.out.gdal",
          parameters = list(input = "lulc_t1",
                            output = glue("{master_dir}/TempRaster/lulc_t1_c.tif"),
                            nodata=-9999,
                            type="Int16"),
          flags = c("overwrite", "f"))

execGRASS("r.out.gdal",
          parameters = list(input = "lulc_t1_tof",
                            output = glue("{master_dir}/TempRaster/lulc_t1_tof_c.tif")),
          flags = "overwrite")

# Pending: check existence of t2 and t3

# *****************

# Demand pending

# *****************

# IDW pending

# # win chunks
# country_parameters %>%
#   filter(Var == "LULCt1map") %>%
#   pull(ParCHR) -> LULCt1map
# country_parameters %>%
#   filter(Var == "LULCt1map_name") %>%
#   pull(ParCHR) -> LULCt1map_name
# #if (identical(LULCt1map, character(0))) {
# if (identical(LULCt1map, NA_character_)) {
#   print("No LULCt1 map available")
#   } else if (LULCt1map == "YES"){
#     raster(paste0("SourceData/InRaster/",LULCt1map_name)) %>%
#       crop(extent(userarea_r)) %>%
#       resample(userarea_r, "ngb") %>%
#       mask(userarea_r) -> LULCt1_r_m
#     writeRaster(LULCt1_r_m, filename="TempRaster//LULCt1_c.tif", datatype="INT2S", overwrite=TRUE)
#     } else {
#       "No LULCt1 map available"
#     }
# 
# country_parameters %>%
#   filter(Var == "LULCt2map") %>%
#   pull(ParCHR) -> LULCt2map
# country_parameters %>%
#   filter(Var == "LULCt2map_name") %>%
#   pull(ParCHR) -> LULCt2map_name
# #if (identical(LULCt2map, character(0))) {
# if (identical(LULCt2map, NA_character_)) {
#   "No LULCt2 map available"
# } else if (LULCt2map == "YES"){
#   raster(paste0("SourceData/InRaster/",LULCt2map_name)) %>%
#     crop(extent(userarea_r)) %>%
#     resample(userarea_r, "ngb") %>%
#     mask(userarea_r) -> LULCt2_r_m
#   writeRaster(LULCt2_r_m, filename="TempRaster//LULCt2_c.tif", datatype="INT2S", overwrite=TRUE)
# } else {
#   "No LULCt2 map available"
# }
# 
# country_parameters %>%
#   filter(Var == "LULCt3map") %>%
#   pull(ParCHR) -> LULCt3map
# country_parameters %>%
#   filter(Var == "LULCt3map_name") %>%
#   pull(ParCHR) -> LULCt3map_name
# #if (identical(LULCt3map, character(0))) {
# if (identical(LULCt3map, NA_character_)) {
#   "No LULCt3 map available"
# } else if (LULCt3map == "YES"){
#   raster(paste0("SourceData/InRaster/",LULCt3map_name)) %>%
#     crop(extent(userarea_r)) %>%
#     resample(userarea_r, "ngb") %>%
#     mask(userarea_r) -> LULCt3_r_m
#   writeRaster(LULCt3_r_m, filename="TempRaster//LULCt3_c.tif", datatype="INT2S", overwrite=TRUE)
# } else {
#   "No LULCt3 map available"
# }

# #Reclassify LULC map into TOF (1) and FOR (0) binary map as mask for LULCC analysis mask --- REVISE --- #### 
# TOFvsFOR_matrix<-as.matrix(growth_parameters[,c("Key*","TOF")])
# 
# #if (identical(LULCt1map, character(0))) {
# if (identical(LULCt1map, NA_character_)) {
#   "No LULCt1 map available"
# } else if (LULCt1map == "YES"){
#   reclassify(LULCt1_r_m, TOFvsFOR_matrix,
#              filename="TempRaster//TOFvsFOR_mask.tif",
#              datatype="INT2S", overwrite=TRUE)
# } else {
#   "No LULCt1 map available"
# }
# 
# #if (identical(LULCt2map, character(0))) {
# if (identical(LULCt2map, NA_character_)) {
#   "No LULCt2 map available"
# } else if (LULCt2map == "YES"){
#   reclassify(LULCt2_r_m, TOFvsFOR_matrix,
#              filename="TempRaster//TOFvsFOR_mask.tif",
#              datatype="INT2S", overwrite=TRUE)
# } else {
#   "No LULCt2 map available"
# }
# 
# #if (identical(LULCt3map, character(0))) {
# if (identical(LULCt2map, NA_character_)) {
#   "No LULCt3 map available"
# } else if (LULCt3map == "YES"){
#   reclassify(LULCt3_r_m, TOFvsFOR_matrix,
#              filename="TempRaster//TOFvsFOR_mask.tif",
#              datatype="INT2S", overwrite=TRUE)
# } else {
#   "No LULCt3 map available"
# }
  
# AGB map linux chunk  ####
# Import agb
tic()
if (identical(country_parameters %>%
              filter(Var == "AGBmap") %>%
              pull(ParCHR), NA_character_)) {
  print("No AGB map available")
} else if (country_parameters %>%
           filter(Var == "AGBmap") %>%
           pull(ParCHR) == "YES"){
  glue("{master_dir}/SourceData/InRaster/",country_parameters %>%
         filter(Var == "AGB_name") %>%
         pull(ParCHR)) %>% 
    {execGRASS("r.in.gdal",
               parameters = list(input = .,
                                 output = "agb"),
               flags = "overwrite")}
  # Resample
  execGRASS("r.resamp.interp",
            parameters = list(input = "agb",
                              output = "agb",
                              method = "bilinear"),
            flags = "overwrite")
  # Export
  execGRASS("r.out.gdal",
            parameters = list(input = "agb",
                              output = glue("{master_dir}/TempRaster/agb_c.tif"),
                              nodata=-9999),
            flags = "overwrite")
} else {
  "No AGB map available"
}
toc()

# AGB map win chunk  ####
# Import agb
tic()
if (identical(country_parameters %>%
              filter(Var == "AGBmap") %>%
              pull(ParCHR), NA_character_)) {
  print("No AGB map available")
} else if (country_parameters %>%
           filter(Var == "AGBmap") %>%
           pull(ParCHR) == "YES"){
  raster(paste0("SourceData/InRaster/",country_parameters %>%
                  filter(Var == "AGB_name") %>%
                  pull(ParCHR))) %>%
    crop(extent(userarea_r)) %>%
    resample(userarea_r, "bilinear") %>%
    mask(userarea_r) %>%
    writeRaster(filename="TempRaster//agb_c_win.tif", datatype="INT4S", overwrite=TRUE)
} else {
  "No AGB map available"
}
toc()

# NPA with Fasterize ####
if (identical(country_parameters %>%
              filter(Var == "NPAmap") %>%
              pull(ParCHR), NA_character_)) {
  print("No NPA map available")
} else if (country_parameters %>%
           filter(Var == "NPAmap") %>%
           pull(ParCHR)  == "YES"){
  glue("SourceData/InVector/",country_parameters %>%
         filter(Var == "NPA_name") %>%
         pull(ParCHR)) %>%
    st_read() %>%
    fasterize(userarea_r, field=country_parameters %>%
                filter(Var == "NPA_name_ID") %>%
                pull(ParCHR)) %>%
    {. * userarea_r} %>%
    writeRaster(filename="TempRaster/NPA_c.tif", datatype="INT2S", overwrite=TRUE)
} else {
  "No NPA map available"
}

# Rivers with GRASS - linux chunk reading gpkg ####
tic()
st_read(glue("SourceData/InVector/",country_parameters %>%
               filter(Var == "rivers_name") %>%
               pull(ParCHR) )) %>%
  writeVECT("rivers", v.in.ogr_flags="overwrite")

# # Set mask
# execGRASS("r.mask",
#           parameters = list(vector = "analysisshp_vgr"))
# Rasterize
execGRASS("v.to.rast",
          parameters = list(input = "rivers",
                            output = "rivers",
                            use = "attr",
                            attribute_column = country_parameters %>%
                              filter(Var == "rivers_name_ID") %>%
                              .$ParCHR),
          flags = "overwrite")
                                      
# Export rivers
execGRASS("r.out.gdal",
          parameters = list(input = "rivers",
                            output = glue("{master_dir}/TempRaster/rivers_c.tif"),
                            nodata=0),
          flags = "overwrite")
toc()


# # Rivers with Velox - win chunk reading shapefiles ####
# tic()
# country_parameters %>%
#   filter(Var == "rivers_name") %>%
#   pull(ParCHR) -> rivers_name
# country_parameters %>%
#   filter(Var == "rivers_name_ID") %>%
#   pull(ParCHR) -> rivers_name_ID
# rivers<-st_read(paste("SourceData/InVector/",rivers_name,sep=""))
# userarea_r_vx<-velox(userarea_r)
# userarea_r_vx$rasterize(spdf=rivers, field=rivers_name_ID, background = NA)
# rivers_c1<- userarea_r_vx$as.RasterLayer(band = 1)
# rivers_c<-rivers_c1*userarea_r
# writeRaster(rivers_c, filename="TempRaster//rivers_c_win.tif", datatype="INT2S", overwrite=TRUE)
# toc()

# Roads with GRASS - linux chunk reading gpkg ####
tic()
st_read(glue("SourceData/InVector/",country_parameters %>%
               filter(Var == "roads_name") %>%
               pull(ParCHR))) %>%
  writeVECT("roads", v.in.ogr_flags="overwrite")

# # Set mask
# execGRASS("r.mask",
#           parameters = list(vector = "analysisshp_vgr"))

# # Rebuild vector topology
# execGRASS("v.build",
#           map="roads", 
#           option="build")

# Rasterize
execGRASS("v.to.rast",
          parameters = list(input = "roads",
                            output = "roads",
                            use = "attr",
                            attribute_column = country_parameters %>%
                              filter(Var == "roads_name_ID") %>%
                              .$ParCHR),
          flags = "overwrite")

# Export roads
execGRASS("r.out.gdal",
          parameters = list(input = "roads",
                            output = glue("{master_dir}/TempRaster/roads_c.tif"),
                            nodata=0),
          flags = "overwrite")
toc()

# # Roads with Velox - win chunk reading shapefiles ####
# country_parameters %>%
#   filter(Var == "roads_name") %>%
#   pull(ParCHR) -> roads_name
# country_parameters %>%
#   filter(Var == "roads_name_ID") %>%
#   pull(ParCHR) -> roads_name_ID
# tic()
# roads<-st_read(paste("SourceData/InVector/",roads_name,sep=""))
# userarea_r_vx<-velox(userarea_r)
# userarea_r_vx$rasterize(spdf=roads, field=roads_name_ID, background = NA)
# roads_c1<- userarea_r_vx$as.RasterLayer(band = 1)
# roads_c<-roads_c1 * userarea_r
# writeRaster(roads_c, filename="TempRaster//roads_c.tif", datatype="INT2S", overwrite=TRUE)
# toc()


# Demand modeling --- REVISE CAREFULLY AND UPDATE ALL CODES --- ####
setwd("..")

# Localities with Rasterize ---couldn't find out how to use Fasterize or Velox or Stars####
# Rasterize ALL locs, for debugging
tic()
mask_ras <- raster(mask, res=resolution)
mask_ras <- fasterize(mask, mask_ras, field = country_parameters %>%
                        filter(Var == "ext_analysis_ID") %>%
                        pull(ParCHR))
st_read(glue("In/DemandScenarios/",country_parameters %>%
               filter(Var == "Locs_name") %>%
               pull(ParCHR))) %>%
  rasterize(mask_ras, field = country_parameters %>%
              filter(Var == "Locs_name_ID") %>%
              pull(ParCHR),
            background=NA) %>% 
  writeRaster(filename="In/DemandScenarios/locs_all.tif", datatype="INT2S", overwrite=TRUE)
toc()

# Rasterize SELECTED locs, for debugging codes
tic()
st_read(glue("In/DemandScenarios/",country_parameters %>%
               filter(Var == "Locs_name") %>%
               pull(ParCHR))) %>%
  st_crop(userarea) %>%
  rasterize(userarea_r, field = country_parameters %>%
              filter(Var == "Locs_name_ID") %>%
              pull(ParCHR),
            background=NA) %>%
  writeRaster(filename="In/DemandScenarios/locs_c.tif", datatype="INT2S", overwrite=TRUE)
toc()

# Erase after debug previous code
# tic()
# raster("In/DemandScenarios/locs_all.tif") %>%
#   crop(extent(userarea_r)) %>%
#   resample(userarea_r, "ngb") %>%
#   mask(userarea_r) -> locs_r_m 
# writeRaster(locs_r_m, filename="LULCC/TempRaster/locs_c.tif", datatype="INT2S", overwrite=TRUE)
# writeRaster(locs_r_m, filename="In/DemandScenarios/locs_c.tif", datatype="INT2S", overwrite=TRUE)
# toc()

# Localities with Grass####
tic()
st_read(glue("In/DemandScenarios/", country_parameters %>%
               filter(Var == "Locs_name") %>%
               pull(ParCHR))) %>%
  writeVECT("locs", v.in.ogr_flags="overwrite")

# Rasterize locs selection
execGRASS("v.to.rast",
          parameters = list(input = "locs",
                            output = "locs",
                            use = "attr",
                            attribute_column = country_parameters %>%
                              filter(Var == "Locs_name_ID") %>%
                              .$ParCHR),
          flags = "overwrite")

# Export locs
execGRASS("r.out.gdal",
          parameters = list(input = "locs",
                            output = "In/DemandScenarios/locs_cg.tif",
                            nodata=0),
          flags = "overwrite")
toc()

# Rasterize ALL locs with GRASS
# Remove mask ####
execGRASS("r.mask",
          flags = "r")

st_read(glue("In/DemandScenarios/", country_parameters %>%
               filter(Var == "Locs_name") %>%
               pull(ParCHR))) %>%
  writeVECT("locs_all", v.in.ogr_flags="overwrite")

execGRASS("v.to.rast",
          parameters = list(input = "locs_all",
                            output = "locs_all",
                            use = "attr",
                            attribute_column = country_parameters %>%
                              filter(Var == "Locs_name_ID") %>%
                              .$ParCHR),
          flags = "overwrite")

# Export locs
execGRASS("r.out.gdal",
          parameters = list(input = "locs_all",
                            output = "In/DemandScenarios/locs_allg.tif",
                            nodata=0),
          flags = "overwrite")
toc()

if (country_parameters %>%
    filter(Var == "Demand_Sc") %>%
    pull(ParCHR) == "YES") {
  
  "Do Nothing"
  
} else {
  st_read(glue("In/DemandScenarios/",country_parameters %>%
                 filter(Var == "Locs_name") %>%
                 pull(ParCHR))) -> locs
  
  list(country_parameters %>%
         filter(Var == "Locs_name_W") %>%
         pull(ParCHR), country_parameters %>%
         filter(Var == "Locs_name_V") %>%
         pull(ParCHR)) %>% 
    walk(function(i){
      print(i)
      locs %>%
        select(country_parameters %>%
                 filter(Var == "Locs_name_ID") %>%
                 pull(ParCHR), i) %>%
        st_set_geometry(NULL) %>%
        setNames(c("Key","Value")) %>%
        write.csv(glue("LULCC/TempTables/",i,"_all.csv"),row.names = FALSE)
    })
  
  st_read(glue("In/DemandScenarios/",country_parameters %>%
                 filter(Var == "Locs_name") %>%
                 pull(ParCHR))) %>%
    st_crop(userarea) -> locs_c
  
  list(country_parameters %>%
         filter(Var == "Locs_name_W") %>%
         pull(ParCHR), country_parameters %>%
         filter(Var == "Locs_name_V") %>%
         pull(ParCHR)) %>% 
    walk(function(i){
      print(i)
      locs_c %>%
        select(country_parameters %>%
                 filter(Var == "Locs_name_ID") %>%
                 pull(ParCHR), i) %>%
        st_set_geometry(NULL) %>%
        setNames(c("Key","Value")) %>%
        write.csv(glue("LULCC/TempTables/",i,".csv"),row.names = FALSE)
    })   
 
}

### For IDW optimization purposes only.

# if (IDW_op == "NO") {
# 	"Do nothing"
# } else {
# 	AllLocs_v<-readShapePoints(paste("SourceData/InVector/",Locs_name,sep=""))
# 	Locs_clip_v<-crop(AllLocs_v,ext)
# 	grps<-sort(Locs_clip_v$SS_GROUP)
# 	unique_grps<-as.data.frame(unique(grps))
# 	colnames(unique_grps)<-"A"
# 	unique_grps$B<-unique_grps$A
# 	write.csv(unique_grps, "TempTables/unique_grps.csv", row.names=FALSE)
# 
# 	setwd("../")
# 	for (i in unique_grps$A) {
# 		spSubset <- Locs_clip_v[Locs_clip_v$SS_GROUP==i,]
# 		Locs_clip_r<-rasterize(spSubset, DEM_c1, field=Locs_name_ID,datatype="INT4S",overwrite=TRUE)
# 		Locs_clip_r_c<-Locs_clip_r*userarea_r
# 		writeRaster(Locs_clip_r_c, filename=paste0("In//IDW_boost//Locs_c",i,".tif"), datatype="INT4S", overwrite=TRUE)
# 	}
# }

# # Demand in Raster ####
# country_parameters %>%
#   filter(Var == "Demand_in_Raster") %>%
#   pull(ParCHR) -> Demand_in_Raster
# if (Demand_in_Raster == "YES") {
#   dir<-getwd()
#   for (i in c("C:/Program Files/Dinamica EGO","C:/Program Files","C:/")) {
#     setwd(i)
#     DCpath<-list.files(path = ".", pattern = "DinamicaConsole.exe", all.files = FALSE,
#                        full.names = FALSE, recursive = TRUE,
#                        ignore.case = FALSE, include.dirs = TRUE)
#     if (length(DCpath)> 0) break
#   }
#   DCpath2<-paste(getwd(),"/",DCpath,sep="")
#   setwd(dir)
#   if (length(DCpath)== 0) {
#     shell.exec(file.path(getwd(), "Wizard_imgs/ManyKMLs.pdf"))
#   }
#   cmdDin<-paste0('"',DCpath2,'"', ' 0_Demand_points.egoml')
#   #cmdDin<-paste('"c:/Program Files/Dinamica EGO/DinamicaConsole.exe"', '0_Demand_points.egoml')
#   system(cmdDin, wait = TRUE)
# } else {
#   "Do nothing"
# }

# Land Use Land Cover Module ####
#setwd(paste0(getwd(),"/LULCC"))
list("~/mofuss/linux/scripts/LULCC/1_Matrix_gain_linux5.egoml",
     "~/mofuss/linux/scripts/LULCC/1_Matrix_loss_linux5.egoml",
     "~/mofuss/linux/scripts/LULCC/2_Distance_calc_linux5.egoml",
     "~/mofuss/linux/scripts/LULCC/3_Ranges_gain_linux5.egoml",
     "~/mofuss/linux/scripts/LULCC/3_Ranges_loss_linux5.egoml",
     "~/mofuss/linux/scripts/LULCC/4_Weights_gain_linux5.egoml",
     "~/mofuss/linux/scripts/LULCC/4_Weights_loss_linux5.egoml",
     "~/mofuss/linux/scripts/LULCC/5_Correlation_gain_linux5.egoml",
     "~/mofuss/linux/scripts/LULCC/5_Correlation_loss_linux5.egoml",
     "~/mofuss/linux/scripts/LULCC/6_Probability_gain_linux5.egoml",
     "~/mofuss/linux/scripts/LULCC/6_Probability_loss_linux5.egoml",
     "~/mofuss/linux/scripts/LULCC/7_Simulation_gain_linux5.egoml",
     "~/mofuss/linux/scripts/LULCC/7_Simulation_loss_linux5.egoml",
     "~/mofuss/linux/scripts/LULCC/8_Validation_gain_linux5.egoml",
     "~/mofuss/linux/scripts/LULCC/8_Validation_loss_linux5.egoml") %>% 
  walk(function(i){
    system(glue("~/Dinamica/Dinamica.AppImage ",i))
  })

# End ####

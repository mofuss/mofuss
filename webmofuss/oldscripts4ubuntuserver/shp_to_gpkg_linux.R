# MoFuSS
# Version 2
# Date: Nov 2019

rm(list=ls(all=TRUE))

flist <- list.files("SourceData/InTables")
file.copy(paste0("SourceData/InTables/",flist), "TempTables")

# Load packages ####
pkcg_list <- readLines("R_pkgs.txt")
lapply(pkcg_list, require, character.only = TRUE)

# Set master dir ####
master_dir <- getwd()

# Read parameters table, checking if its delimiter is comma or semicolon ####
read_csv(glue("{master_dir}/SourceData/Parameters.csv")) %>% 
  {if(is.null(.$ParCHR[1])) read_csv2(glue("{master_dir}/SourceData/Parameters.csv")) else .} -> country_parameters

# Append "+"
country_parameters$ParCHR[1] <- str_c("+", country_parameters$ParCHR[1])
country_parameters$ParCHR[2] <- str_c("+", country_parameters$ParCHR[2])
# print(tbl_df(country_parameters), n=100) ####


# Convert from ESRI shapefil to Geopackage --Once in a lifetime procedure
rivers_gcs <- st_read("DownloadedDatasets/SourceDataPeru/InVector_GCS/rivers_mod_dobler.gpkg")
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

rivers_utm <- st_transform(rivers_gcs, epsg_utm)
st_write(rivers_utm, "DownloadedDatasets/SourceDataPeru/InVector/rivers.gpkg", delete_layer=TRUE)

roads_gcs <- st_read("DownloadedDatasets/SourceDataPeru/InVector_GCS/roads_mod_dobler.gpkg")
roads_utm <- st_transform(roads_gcs, epsg_utm)
st_write(roads_utm, "DownloadedDatasets/SourceDataPeru/InVector/roads.gpkg", delete_layer=TRUE)

npa_shp <- st_read("DownloadedDatasets/SourceDataZambia/InVector/wdpa_ZMB.shp")
st_write(npa_shp, "DownloadedDatasets/SourceDataZambia/InVector/npa.gpkg", delete_layer=TRUE)

mask_shp <- st_read("SourceData/InVector/Extent_Mask.shp")
st_write(mask_shp, "SourceData/InVector/mask.gpkg", delete_layer=TRUE)

ana_shp <- st_read("SourceData/InVector/Extent_Analysis.shp")
st_write(ana_shp, "SourceData/InVector/ext_analysis.gpkg", delete_layer=TRUE)

rivers_shp <- st_read("DownloadedDatasets/SourceDataPeru/InVector/rivers.shp")
st_write(rivers_shp, "DownloadedDatasets/SourceDataPeru/InVector/rivers.gpkg", delete_layer=TRUE)
country_parameters %>%
  filter(Var == "rivers_name") %>%
  pull(ParCHR) -> rivers_name
country_parameters %>%
  filter(Var == "rivers_name_ID") %>%
  pull(ParCHR) -> rivers_name_ID
rivers_gpkg <- st_read(glue("DownloadedDatasets/SourceDataPeru/InVector/",rivers_name))

roads_shp <- st_read("DownloadedDatasets/SourceDataPeru/InVector/roads.shp")
st_write(roads_shp, "DownloadedDatasets/SourceDataPeru/InVector/roads.gpkg", delete_layer=TRUE)
country_parameters %>%
  filter(Var == "roads_name") %>%
  pull(ParCHR) -> roads_name
country_parameters %>%
  filter(Var == "roads_name_ID") %>%
  pull(ParCHR) -> roads_name_ID
roads_gpkg <- st_read(glue("DownloadedDatasets/SourceDataPeru/InVector/",roads_name))

locs_shp <- st_read("DownloadedDatasets/SourceDataPeru/InVector/Locs.shp")
st_write(locs_shp, "DownloadedDatasets/SourceDataPeru/InVector/locs.gpkg", delete_layer=TRUE)
country_parameters %>%
  filter(Var == "Locs_name") %>%
  pull(ParCHR) -> locs_name
country_parameters %>%
  filter(Var == "Locs_name_ID") %>%
  pull(ParCHR) -> locs_name_ID
locs_gpkg <- st_read(glue("DownloadedDatasets/SourceDataPeru/InVector/",locs_name))

# End ####
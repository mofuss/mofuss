# Load packages ----
library(sf)
library(tictoc)
#library(mapview)
library(tidyverse)
library(readxl)
library(hacksaw)
library(rmapshaper)
library(svDialogs)

getwd()
setwd <- "C:/Users/aghil/Downloads"

cambodialaosattractor <- st_read("cambodialaosattractor_pcs.shp")
plot(cambodialaosattractor)
st_write(cambodialaosattractor, "cambodialaosattraction.gpkg", delete_layer = TRUE)



myanmarattractor <- st_read("myanmarattractor_pcs.shp")
plot(myanmarattractor)
st_write(myanmarattractor, "myanmarattraction.gpkg", delete_layer = TRUE)

congoattractor <- st_read("congoattractor_pcs.shp")
plot(congoattractor)
st_write(congoattractor, "congoattraction.gpkg", delete_layer = TRUE)

cafattractor <- st_read("cafattractor_pcs.shp")
plot(cafattractor)
st_write(cafattractor, "cafattraction.gpkg", delete_layer = TRUE)

caattractor <- st_read("caattractor.shp")
plot(caattractor)
st_write(caattractor, "caattraction.gpkg", delete_layer = TRUE)

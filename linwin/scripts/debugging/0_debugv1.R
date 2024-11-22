# MoFuSS
# Windows version
# Date: August 2023


library(exactextractr)

# Demand within 2nd MC run

analysis <- st_read("LULCC/SourceData/InVector/extent_analysis.gpkg") 
mask_d <- st_read("LULCC/SourceData/InVector/extent_mask.gpkg") %>%
  st_union() %>%
  st_as_sf()
# %>%
#  vect()

mask_d_gcs <- st_transform(mask_d, epsg_gcs)



Cum_exp_harv2010 <- raster("Debugging/Cum_exp_harv01.tif")
Cum_exp_harv2050 <- raster("Debugging/Cum_exp_harv41.tif")
exact_extract(Cum_exp_harv2010, mask_d, 'sum')
exact_extract(Cum_exp_harv2050, mask_d, 'sum')
plot(Cum_exp_harv2010)


mask_drr <- raster::rasterize(mask_d,Cum_exp_harv2010, fun='sum')
mask_dr <- raster(mask_drr)
plot(mask_dr)

terra::zonal(Cum_exp_harv2010, mask_dr, 'sum')
?rasterize

Harvest_tot2010 <- raster("Debugging/Harvest_tot01.tif")
Harvest_tot2050 <- raster("Debugging/Harvest_tot41.tif")
exact_extract(Harvest_tot2010, mask_d, 'sum')
exact_extract(Harvest_tot2050, mask_d, 'sum')


Expect_harv_tot2010 <- raster("Debugging/Expect_harv_tot01.tif")
Expect_harv_tot2050 <- raster("Debugging/Expect_harv_tot41.tif")
exact_extract(Expect_harv_tot2010, mask_d, 'sum')
exact_extract(Expect_harv_tot2050, mask_d, 'sum')

BaU_fwch_v <- read_csv("In/DemandScenarios/BaU_fwch_v.csv") %>% 
  {if(is.null(.$ID[1])) read_csv2("In/DemandScenarios/BaU_fwch_v.csv") else .}

BaU_fwch_w <- read_csv("In/DemandScenarios/BaU_fwch_w.csv") %>% 
  {if(is.null(.$ID[1])) read_csv2("In/DemandScenarios/BaU_fwch_w.csv") else .}

woodfueluseMWI2010 <- as.numeric(colSums(BaU_fwch_w[,2], na.rm=TRUE) + colSums(BaU_fwch_v[,2], na.rm=TRUE))
woodfueluseMWI2010
woodfueluseMWI2050 <- as.numeric(colSums(BaU_fwch_w[,42], na.rm=TRUE) + colSums(BaU_fwch_v[,42], na.rm=TRUE))
woodfueluseMWI2050

Growth01 <- raster("Debugging/Growth01.tif")
Growth_less_harv01 <- raster("Debugging/Growth_less_harv01.tif")
agb_c <- raster("LULCC/TempRaster/agb_c.tif")
agb_c1 <- raster("LULCC/TempRaster/agb_c1.tif")
zonal(Growth01, mask_d)

exact_extract(Growth01, mask_d, 'sum')
exact_extract(Growth_less_harv01, mask_d, 'sum')
exact_extract(agb_c, mask_d, 'sum')
exact_extract(agb_c1, mask_d, 'sum')

# Stacks
# Check pixel-based temporal trends iteratively over 40 year stacks, ideally showing different variables in the same graph
pop2010_bio <- raster("D:/Demand/population_out/HSRL_bio_users_2010.tif")
exact_extract(pop2010_bio, mask_d_gcs, 'sum')

pop2010_cha <- raster("D:/Demand/population_out/HSRL_cha_users_2010.tif")
exact_extract(pop2010_cha, mask_d_gcs, 'sum')


HSRL_bio_demand_2010 <- raster("D:/Demand/demand_out/HSRL_bio_demand_2010.tif")
exact_extract(HSRL_bio_demand_2010, mask_d_gcs, 'sum')

HSRL_cha_demand_2010 <- raster("D:/Demand/demand_out/HSRL_cha_demand_2010.tif")
exact_extract(HSRL_cha_demand_2010, mask_d_gcs, 'sum')


mask_dr_gcs <- raster::rasterize(mask_d_gcs,HSRL_bio_demand_2010, fun='sum')
plot(mask_dr_gcs)
raster::zonal(HSRL_bio_demand_2010,mask_dr_gcs,'sum')
raster::zonal(HSRL_cha_demand_2010,mask_dr_gcs,'sum')
4863829+2112935

HSRL_wftons_v_2010 <- raster("D:/Demand/demand_out/HSRL_wftons_v_2010.tif")
HSRL_wftons_w_2010 <- raster("D:/Demand/demand_out/HSRL_wftons_w_2010.tif")
raster::zonal(HSRL_wftons_v_2010,mask_dr_gcs,'sum')
raster::zonal(HSRL_wftons_w_2010,mask_dr_gcs,'sum')
2449023+4527742

wf_w_st_2010_db <- raster("D:/Demand/demand_out/wf_w_st_2010_db.tif")
raster::zonal(wf_w_st_2010_db,mask_dr,'sum')


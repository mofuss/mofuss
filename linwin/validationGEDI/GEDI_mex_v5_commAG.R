# Solo usar V, porque vamos a aumir cambios por venta


# GEDI tests 4 Ivan1

library(tidyverse)
library(sf)
library(terra)
library(mapview)
library(pROC)
library(png)
library(viridis)
library(ggspatial)
library(ggpubr)
library(rasterVis)
library(tidyterra)
library(raster)
# library(ggsn)

# Set ROC_country working directories
# setwd("D:/ROC_mex") # Ivan Desktop
setwd("C:/ROC_mex") # Adrian laptop

unlink("GEDI_temp", recursive = TRUE)
dir.create("GEDI_temp")

# read GEDI poiint data with Standard Error
geditest <- st_read("GEDI_in/GEDI_AGB_MX_RepeatedMeasures_qualFlag_agbd_agbd_se.shp")

geditest_p <- geditest %>% st_transform(3395)

write.csv(geditest_p, 'GEDI_data.csv')


#Histogram
geditest_hist_19<-ggplot(geditest_p, aes(x=firstAGB)) + 
  geom_histogram(color="white", fill="black", binwidth = 10)+
  labs(y = "Frequency",
       x = "AGBD 2019")+
  theme_classic()+
  xlim(0,500)+ 
  ylim(0,100000)

geditest_hist_23<-ggplot(geditest_p, aes(x=lastAGB)) + 
  geom_histogram(color="white", fill="black", binwidth = 10)+
  labs(y = "Frequency",
       x = "AGBD 2023")+
  theme_classic()+
  xlim(0,500)+ 
  ylim(0,100000)

geditest_hist<- ggarrange(geditest_hist_19, geditest_hist_23)

ggsave("GEDI_temp/AGBD_hist.pdf", width = 8, height = 2.5)
#ggsave("GEDI_temp/AGBD_hist.png", width = 8, height = 2.5)

# read Mexico raster probability layers
idw_v_mex <- rast("GEDI_in/IDW_C++_fw_v13.tif")
idw_w_mex <- rast("GEDI_in/IDW_C++_fw_w13.tif")

# rasterize over IDW 1km raster layer using a function mean plus se propagation (missing)
# classify based on number of points threshold
ptsthres <- 0 # threshold
m <- c(0, ptsthres, NA,
       ptsthres, Inf, 1)
rclmat <- matrix(m, ncol=3, byrow=TRUE)

# firstAGB
firstAGB_rast_ptsx <- terra::rasterize(geditest_p, idw_v_mex, field="firstAGB", fun = length)
terra::writeRaster(firstAGB_rast_ptsx, "GEDI_temp/firstAGB_rast_ptsx.tif",
                   filetype = "GTiff", overwrite = T)
firstAGB_rast_ptsthres <- firstAGB_rast_ptsx %>%
  terra::classify(rclmat, include.lowest=TRUE) 
# terra::writeRaster(firstAGB_rast_ptsthres, "GEDI_temp/ptsthres.tif",filetype = "GTiff", overwrite = T)
firstAGB_rast_mt <- terra::rasterize(geditest_p, idw_v_mex, field="firstAGB", fun = mean)
firstAGB_rast_m <-  firstAGB_rast_mt * firstAGB_rast_ptsthres
terra::writeRaster(firstAGB_rast_m, "GEDI_temp/firstAGB_rast_m.tif",
                   filetype = "GTiff", overwrite = T)
firstAGB_rast_sdt <- terra::rasterize(geditest_p, idw_v_mex, field="firstAGB", fun = sd)
firstAGB_rast_sd <-  firstAGB_rast_sdt * firstAGB_rast_ptsthres
terra::writeRaster(firstAGB_rast_sd, "GEDI_temp/firstAGB_rast_sd.tif",
                   filetype = "GTiff", overwrite = T)
firstAGB_rast_pts <- firstAGB_rast_ptsx * firstAGB_rast_ptsthres
firstAGB_rast_se <- firstAGB_rast_sd / sqrt(firstAGB_rast_pts)
terra::writeRaster(firstAGB_rast_se, "GEDI_temp/firstAGB_rast_se.tif",
                   filetype = "GTiff", overwrite = T)

# lastAGB
lastAGB_rast_ptsx <- terra::rasterize(geditest_p, idw_v_mex, field="lastAGB", fun = length)
terra::writeRaster(lastAGB_rast_ptsx, "GEDI_temp/lastAGB_rast_ptsx.tif",
                   filetype = "GTiff", overwrite = T)
lastAGB_rast_ptsthres <- lastAGB_rast_ptsx %>%
  terra::classify(rclmat, include.lowest=TRUE)
# terra::writeRaster(lastAGB_rast_ptsthres, "GEDI_temp/ptsthres.tif",filetype = "GTiff", overwrite = T)
lastAGB_rast_mt <- terra::rasterize(geditest_p, idw_v_mex, field="lastAGB", fun = mean)
lastAGB_rast_m <-  lastAGB_rast_mt * lastAGB_rast_ptsthres
terra::writeRaster(lastAGB_rast_m, "GEDI_temp/lastAGB_rast_m.tif",
                   filetype = "GTiff", overwrite = T)
lastAGB_rast_sdt <- terra::rasterize(geditest_p, idw_v_mex, field="lastAGB", fun = sd)
lastAGB_rast_sd <-  lastAGB_rast_sdt * lastAGB_rast_ptsthres
terra::writeRaster(lastAGB_rast_sd, "GEDI_temp/lastAGB_rast_sd.tif",
                   filetype = "GTiff", overwrite = T)
lastAGB_rast_pts <- lastAGB_rast_ptsx * lastAGB_rast_ptsthres
lastAGB_rast_se <- lastAGB_rast_sd / sqrt(lastAGB_rast_pts)
terra::writeRaster(lastAGB_rast_se, "GEDI_temp/lastAGB_rast_se.tif",
                   filetype = "GTiff", overwrite = T)

dAGB_m <- (lastAGB_rast_m - firstAGB_rast_m)
dAGB_sd <- sqrt((lastAGB_rast_sd^2) + (firstAGB_rast_sd^2))
dAGB_se <- sqrt((lastAGB_rast_se^2) + (firstAGB_rast_se^2))

terra::writeRaster(dAGB_m, "GEDI_temp/dAGB_m.tif",
                   filetype = "GTiff", overwrite = T)
terra::writeRaster(dAGB_sd, "GEDI_temp/dAGB_sd.tif",
                   filetype = "GTiff", overwrite = T)
terra::writeRaster(dAGB_se, "GEDI_temp/dAGB_se.tif",
                   filetype = "GTiff", overwrite = T)

# Convert rasters to dataframes for ROC (no es mejor exportar un stack como data frame?)
dAGB_m_df<-as.data.frame(dAGB_m, xy=TRUE) %>% 
  st_as_sf(coords=1:2)
dAGB_sd_df<-as.data.frame(dAGB_sd, xy=TRUE) %>% 
  st_as_sf(coords=1:2)
dAGB_se_df<-as.data.frame(dAGB_se, xy=TRUE) %>% 
  st_as_sf(coords=1:2)

idw_v_mex_df <-as.data.frame(idw_v_mex, xy=TRUE) %>% # OJO!!! USO ESTE PARCHE PARA MOVERLE ENTRE V Y W
  st_as_sf(coords=1:2)
idw_w_mex_df <-as.data.frame(idw_w_mex, xy=TRUE) %>% 
  st_as_sf(coords=1:2)

# Join dAGB and Mofuss probability
filtering <- "range" # range
if (filtering == "se"){
  GEDI_Mofuss_loss <- idw_v_mex_df %>% 
    st_join(dAGB_m_df, by="geometry") %>% 
    st_join(dAGB_sd_df, by="geometry") %>% 
    st_join(dAGB_se_df, by="geometry") %>% 
     dplyr::rename(probability_mofuss="IDW_C++_fw_v13",
                  AGB_delta="mean",
                  AGB_sd="lastAGB_sd.x",
                  AGB_se="lastAGB_sd.y") %>%
    drop_na(AGB_se) %>%
    # dplyr::filter(abs(AGB_delta)>=AGB_se) %>%
    transform(., Change = ifelse(abs(AGB_delta)>(AGB_se) & AGB_delta<0, 1, 0)) %>%
    drop_na(probability_mofuss) %>%
    drop_na(AGB_delta) %>% 
    drop_na(Change)
} else {
  GEDI_Mofuss <- idw_v_mex_df %>% 
    st_join(dAGB_m_df, by="geometry") %>% 
    dplyr::rename(AGB_delta="mean", probability_mofuss="IDW_C++_fw_v13") %>% 
    drop_na(probability_mofuss) %>% 
    drop_na(AGB_delta) %>% 
    filter(AGB_delta>-4000 & AGB_delta<60) %>% 
    st_set_crs(3395)
  
  # Create datasets for Gain and Loss ROC curves
  GEDI_Mofuss_gain <- GEDI_Mofuss %>% 
    mutate(Change = case_when(AGB_delta>-4000 & AGB_delta<=(10)~ 'no_gain',
                              AGB_delta>10 & AGB_delta<60~ 'gain'))   %>% 
    mutate(probability_mofuss=as.numeric(probability_mofuss)) %>%
    mutate(Change=as.character(Change)) %>% 
    drop_na(Change) 
  
  GEDI_Mofuss_loss <- GEDI_Mofuss %>% 
    mutate(Change= case_when(AGB_delta>-4000 & AGB_delta<=(-10) ~ 'loss',
                             AGB_delta>-10 & AGB_delta<60 ~ 'no_loss'))   %>%
    mutate(probability_mofuss=as.numeric(probability_mofuss)) %>%
    mutate(Change=as.character(Change)) %>% 
    drop_na(Change) 
}


#plot changes AGBD
Mex_sil <- st_read("GEDI_in/contdv1mgw/contdv1mgw.shp") %>% 
   st_transform(3395) 

GEDI_Mofuss_class <- GEDI_Mofuss %>%
  mutate("AGBD_Change"= case_when (AGB_delta>-4000 & AGB_delta <=-10 ~ 'Loss',
                            AGB_delta>-10 & AGB_delta < 10 ~ 'No change',
                            AGB_delta>=10 & AGB_delta <=60 ~ 'Gain'))

AGBD_delta_plot<- ggplot() +  
  geom_sf(data=GEDI_Mofuss_class, aes(color = AGBD_Change), 
          size = 0.00001) +
  geom_sf(Mex_sil, mapping= aes(color = AREA), color = "gray2", fill=NA, size=0.25) +
  ggtitle("AGBD change in México", 
          subtitle= "2019-2023")+
  xlab("Longitude") + ylab("Latitude") +
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  #scale_color_continuous(name = "delta_AGB")+
  labs(color= "AGBD Change")+
  scale_color_manual (values = c("Loss" = "goldenrod",
                                 "No change" = "azure3",
                                 "Gain" = "seagreen4")) +
  annotation_scale() +
  annotation_north_arrow(location='tr', 
                         height = unit(1, "cm"),
                         width = unit(0.7, "cm"))

AGBD_delta_plot
Sys.sleep(25)
ggsave("GEDI_temp/AGBD_delta_plot.pdf", width = 7, height = 5)
#ggsave("GEDI_temp/AGBD_delta_plot.png", width = 7, height = 5)

#plot Mofuss maps

idw_v_mex_df_WGS84 <-project(idw_v_mex, "epsg:4326") %>% 
  as.data.frame(xy=TRUE)
idw_w_mex_df_WGS84 <-project(idw_w_mex, "epsg:4326") %>% 
  as.data.frame(xy=TRUE)

idw_v_mex_plot <- ggplot() +  
  geom_tile(data=idw_v_mex_df_WGS84, aes(x=x, y=y, fill=`IDW_C++_fw_v13`), alpha=0.8) + 
  theme_bw()+
  scale_fill_viridis(name="IDW C++ fw v13", option = "D") +
  coord_equal() +
  xlab("Longitude") + ylab("Latitude") +
  theme(legend.position="right") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.key = element_blank()) + 
  #annotation_scale() +
  annotation_north_arrow(location='tr', 
                         height = unit(1, "cm"),
                         width = unit(0.7, "cm"))
idw_v_mex_plot

Sys.sleep(35)
ggsave("GEDI_temp/idw_v_mex_plot.pdf", width = 7, height = 5)
#ggsave("GEDI_temp/idw_v_mex_plot.png", width = 7, height = 5)

idw_w_mex_plot <- ggplot() +  
  geom_tile(data=idw_w_mex_df_WGS84, aes(x=x, y=y, fill=`IDW_C++_fw_w13`), alpha=0.8) + 
  theme_bw()+
  scale_fill_viridis(name="IDW C++ fw w13", option = "D") +
  coord_equal() +
  xlab("Longitude") + ylab("Latitude") +
  theme(legend.position="right") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.key = element_blank()) + 
  #annotation_scale() +
  annotation_north_arrow(location='tr', 
                         height = unit(1, "cm"),
                         width = unit(0.7, "cm"))
idw_w_mex_plot

#Sys.sleep(35)
ggsave("GEDI_temp/idw_w_mex_plot.pdf", width = 7, height = 5)
#ggsave("GEDI_temp/idw_w_mex_plot.png", width = 7, height = 5)

# ROC Curves 

# Random model for ROC test
Change<-rbinom(360889, size = 1, prob = 0.5) #Modify depending on the number of values in the final grid with AGB delta
probability_mofuss<-runif(360889, min=0, max=1)
GEDI_Mofuss_random<-data.frame(Change, probability_mofuss) %>% 
  mutate(Change = case_when(Change==0 ~ 'yes',
                            Change>0 ~ 'no'))   
# ROC curves
roc_loss_v <- roc(GEDI_Mofuss_loss$Change, GEDI_Mofuss_loss$probability_mofuss)
roc_gain_v <- roc(GEDI_Mofuss_gain$Change, GEDI_Mofuss_gain$probability_mofuss)
roc_random_v <- roc(GEDI_Mofuss_random$Change, GEDI_Mofuss_random$probability_mofuss)

roc_loss_v$auc #AUC value
roc_gain_v$auc  #AUC value
roc_random_v$auc  #AUC value

# Control vs cases...
GEDI_Mofuss_loss2 <- GEDI_Mofuss_loss %>%
  dplyr::mutate(Change = case_when(
    Change == "no_loss" ~ 0,
    Change == "loss" ~ 1,
    TRUE ~ NA_real_ 
  ))

GEDI_Mofuss_gain2 <- GEDI_Mofuss_gain %>%
  dplyr::mutate(Change = case_when(
    Change == "no_gain" ~ 0,
    Change == "gain" ~ 1,
    TRUE ~ NA_real_ 
  ))

roc_loss_v2 <- roc(GEDI_Mofuss_loss2$Change, GEDI_Mofuss_loss$probability_mofuss)
roc_gain_v2 <- roc(GEDI_Mofuss_gain2$Change, GEDI_Mofuss_gain$probability_mofuss)
roc_random_v2 <- roc(GEDI_Mofuss_random$Change, GEDI_Mofuss_random$probability_mofuss)

roc_loss_v2$auc #AUC value
roc_gain_v2$auc  #AUC value
roc_random_v2$auc  #AUC value

roclist_v <- list("AGBD Loss, AUC = 0.591" = roc_loss_v, #Modify AUC value manually with previous values in lines above
                  "AGBD Gain, AUC = 0.577" = roc_gain_v, #Modify AUC value manually with previous values in lines above
                  "AGBD Random, AUC = 0.501" = roc_random_v) #Modify AUC value manually with previous values in lines above

roc_plot_v <- ggroc(roclist_v, legacy.axes = TRUE) +
  geom_abline() +
  theme_classic() +
  ggtitle("ROC Curves V") +
  labs(x = "1 - Specificity",
       y = "Sensitivity",
       col = "ROC Curves")+
  scale_fill_manual(values=c("red4", "#006633","#330066"))

roc_plot_v
ggsave("GEDI_temp/roc_plot_v.pdf", width = 7, height = 5)
#ggsave("GEDI_temp/roc_plot_v.png", width = 7, height = 5)


# #Test for two ROC curves
# test_loss<-pROC::roc.test(roc_loss_v, roc_random_v, method=c("bootstrap"), boot.n=1000)
# test_gain<-pROC::roc.test(roc_gain_v, roc_random_v, method=c("bootstrap"), boot.n=1000)

##NOW FOR W
# read Mexico raster probability layers
idw_w_mex <- rast("GEDI_in/IDW_C++_fw_w13.tif") #Replace v for w

# rasterize over IDW 1km raster layer using a function mean plus se propagation (missing)
# classify based on number of points threshold
ptsthres <- 0 # threshold
m <- c(0, ptsthres, NA,
       ptsthres, Inf, 1)
rclmat <- matrix(m, ncol=3, byrow=TRUE)

# firstAGB
firstAGB_rast_ptsx <- terra::rasterize(geditest_p, idw_w_mex, field="firstAGB", fun = length)
terra::writeRaster(firstAGB_rast_ptsx, "GEDI_temp/firstAGB_rast_ptsx.tif",
                   filetype = "GTiff", overwrite = T)
firstAGB_rast_ptsthres <- firstAGB_rast_ptsx %>%
  terra::classify(rclmat, include.lowest=TRUE) 
# terra::writeRaster(firstAGB_rast_ptsthres, "GEDI_temp/ptsthres.tif",filetype = "GTiff", overwrite = T)
firstAGB_rast_mt <- terra::rasterize(geditest_p, idw_w_mex, field="firstAGB", fun = mean)
firstAGB_rast_m <-  firstAGB_rast_mt * firstAGB_rast_ptsthres
terra::writeRaster(firstAGB_rast_m, "GEDI_temp/firstAGB_rast_m.tif",
                   filetype = "GTiff", overwrite = T)
firstAGB_rast_sdt <- terra::rasterize(geditest_p, idw_w_mex, field="firstAGB", fun = sd)
firstAGB_rast_sd <-  firstAGB_rast_sdt * firstAGB_rast_ptsthres
terra::writeRaster(firstAGB_rast_sd, "GEDI_temp/firstAGB_rast_sd.tif",
                   filetype = "GTiff", overwrite = T)
firstAGB_rast_pts <- firstAGB_rast_ptsx * firstAGB_rast_ptsthres
firstAGB_rast_se <- firstAGB_rast_sd / sqrt(firstAGB_rast_pts)
terra::writeRaster(firstAGB_rast_se, "GEDI_temp/firstAGB_rast_se.tif",
                   filetype = "GTiff", overwrite = T)

# lastAGB
lastAGB_rast_ptsx <- terra::rasterize(geditest_p, idw_w_mex, field="lastAGB", fun = length)
terra::writeRaster(lastAGB_rast_ptsx, "GEDI_temp/lastAGB_rast_ptsx.tif",
                   filetype = "GTiff", overwrite = T)
lastAGB_rast_ptsthres <- lastAGB_rast_ptsx %>%
  terra::classify(rclmat, include.lowest=TRUE)
# terra::writeRaster(lastAGB_rast_ptsthres, "GEDI_temp/ptsthres.tif",filetype = "GTiff", overwrite = T)
lastAGB_rast_mt <- terra::rasterize(geditest_p, idw_w_mex, field="lastAGB", fun = mean)
lastAGB_rast_m <-  lastAGB_rast_mt * lastAGB_rast_ptsthres
terra::writeRaster(lastAGB_rast_m, "GEDI_temp/lastAGB_rast_m.tif",
                   filetype = "GTiff", overwrite = T)
lastAGB_rast_sdt <- terra::rasterize(geditest_p, idw_w_mex, field="lastAGB", fun = sd)
lastAGB_rast_sd <-  lastAGB_rast_sdt * lastAGB_rast_ptsthres
terra::writeRaster(lastAGB_rast_sd, "GEDI_temp/lastAGB_rast_sd.tif",
                   filetype = "GTiff", overwrite = T)
lastAGB_rast_pts <- lastAGB_rast_ptsx * lastAGB_rast_ptsthres
lastAGB_rast_se <- lastAGB_rast_sd / sqrt(lastAGB_rast_pts)
terra::writeRaster(lastAGB_rast_se, "GEDI_temp/lastAGB_rast_se.tif",
                   filetype = "GTiff", overwrite = T)

dAGB_m <- (lastAGB_rast_m - firstAGB_rast_m)
dAGB_sd <- sqrt((lastAGB_rast_sd^2) + (firstAGB_rast_sd^2))
dAGB_se <- sqrt((lastAGB_rast_se^2) + (firstAGB_rast_se^2))

terra::writeRaster(dAGB_m, "GEDI_temp/dAGB_m.tif",
                   filetype = "GTiff", overwrite = T)
terra::writeRaster(dAGB_sd, "GEDI_temp/dAGB_sd.tif",
                   filetype = "GTiff", overwrite = T)
terra::writeRaster(dAGB_se, "GEDI_temp/dAGB_se.tif",
                   filetype = "GTiff", overwrite = T)

# stack_v <- c(idw_w_mex,dAGB_m,dAGB_sd,dAGB_se)
# terra::writeRaster(stack_v, "GEDI_temp/stack_v.tif",
#                    filetype = "GTiff", overwrite = T)

# Convert rasters to dataframes for ROC (no es mejor exportar un stack como data frame?)
dAGB_m_df<-as.data.frame(dAGB_m, xy=TRUE) %>% 
  st_as_sf(coords=1:2)
dAGB_sd_df<-as.data.frame(dAGB_sd, xy=TRUE) %>% 
  st_as_sf(coords=1:2)
dAGB_se_df<-as.data.frame(dAGB_se, xy=TRUE) %>% 
  st_as_sf(coords=1:2)

idw_w_mex_df <-as.data.frame(idw_w_mex, xy=TRUE) %>% # OJO!!! USO ESTE PARCHE PARA MOVERLE ENTRE V Y W
  st_as_sf(coords=1:2)
idw_w_mex_df <-as.data.frame(idw_w_mex, xy=TRUE) %>% 
  st_as_sf(coords=1:2)

# Join dAGB and Mofuss probability
filtering <- "range" # range
if (filtering == "se"){
  GEDI_Mofuss_loss <- idw_w_mex_df %>% 
    st_join(dAGB_m_df, by="geometry") %>% 
    st_join(dAGB_sd_df, by="geometry") %>% 
    st_join(dAGB_se_df, by="geometry") %>% 
    dplyr::rename(probability_mofuss="IDW_C++_fw_w13",
                  AGB_delta="mean",
                  AGB_sd="lastAGB_sd.x",
                  AGB_se="lastAGB_sd.y") %>%
    drop_na(AGB_se) %>%
    # dplyr::filter(abs(AGB_delta)>=AGB_se) %>%
    transform(., Change = ifelse(abs(AGB_delta)>(AGB_se) & AGB_delta<0, 1, 0)) %>%
    drop_na(probability_mofuss) %>%
    drop_na(AGB_delta) %>% 
    drop_na(Change)
} else {
  GEDI_Mofuss_w <- idw_w_mex_df %>% 
    st_join(dAGB_m_df, by="geometry") %>% 
    dplyr::rename(AGB_delta="mean", probability_mofuss="IDW_C++_fw_w13") %>% 
    drop_na(probability_mofuss) %>% 
    drop_na(AGB_delta) %>% 
    filter(AGB_delta>-4000 & AGB_delta<60) %>% 
    st_set_crs(3395)
  
  # Create datasets for Gain and Loss ROC curves
  GEDI_Mofuss_gain_w <- GEDI_Mofuss_w %>% 
    mutate(Change = case_when(AGB_delta>-4000 & AGB_delta<=(10)~ 'no',
                              AGB_delta>10 & AGB_delta<60~ 'yes'))   %>% 
    mutate(probability_mofuss=as.numeric(probability_mofuss)) %>%
    mutate(Change=as.character(Change)) %>% 
    drop_na(Change) 
  
  GEDI_Mofuss_loss_w <- GEDI_Mofuss_w %>% 
    mutate(Change= case_when(AGB_delta>-4000 & AGB_delta<=(-10) ~ 'yes',
                             AGB_delta>-10 & AGB_delta<60 ~ 'no'))   %>%
    mutate(probability_mofuss=as.numeric(probability_mofuss)) %>%
    mutate(Change=as.character(Change)) %>% 
    drop_na(Change) 
}
GEDI_Mofuss_loss_w

# ROC Curves 

# Random model for ROC test
Change<-rbinom(360889, size = 1, prob = 0.5) #Modify depending on the number of values in the final grid with AGB delta
probability_mofuss<-runif(360889, min=0, max=1)
GEDI_Mofuss_random_w<-data.frame(Change, probability_mofuss) %>% 
  mutate(Change = case_when(Change==0 ~ 'yes',
                            Change>0 ~ 'no'))   
# ROC curves
roc_loss_w <- roc(GEDI_Mofuss_loss_w$Change, GEDI_Mofuss_loss_w$probability_mofuss)
roc_gain_w <- roc(GEDI_Mofuss_gain_w$Change, GEDI_Mofuss_gain_w$probability_mofuss)
roc_random_w <- roc(GEDI_Mofuss_random_w$Change, GEDI_Mofuss_random_w$probability_mofuss)

roc_loss_w$auc
roc_gain_w$auc
roc_random_w$auc

roclist_w <- list("AGBD Loss, AUC = 0.637 " = roc_loss_w,
                "AGBD Gain, AUC = 0.626 " = roc_gain_w,
                "AGBD Random, AUC = 0.501 " = roc_random_w)

roc_plot_w <- ggroc(roclist_w, legacy.axes = TRUE) +
  geom_abline() +
  theme_classic() +
  ggtitle("ROC Curves W") +
  labs(x = "1 - Specificity",
       y = "Sensitivity",
       col = "ROC Curves")+
  scale_fill_manual(values=c("red4", "#006633","#330066"))

roc_plot_w
Sys.sleep(15)
ggsave("GEDI_temp/roc_plot_w.pdf", width = 7, height = 5)
#ggsave("GEDI_temp/roc_plot_w.png", width = 7, height = 5)

# #Test for two ROC curves
# test_loss<-pROC::roc.test(roc_loss_v, roc_random_v, method=c("bootstrap"), boot.n=1000)
# test_gain<-pROC::roc.test(roc_gain_v, roc_random_v, method=c("bootstrap"), boot.n=1000)

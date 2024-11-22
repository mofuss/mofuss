# GEDI tests 4 Ivan1
# Este es el nuevo scrip hay que leer el shape e ir viendo...

library(tidyverse)
library(sf)
library(terra)
library(mapview)
library(pROC)
library(png)

# Set ROC_country working directories
setwd("D:/ROC_mex") # Ivan Desktop
# setwd("C:/ROC_mex") # Adrian laptop

unlink("GEDI_temp", recursive = TRUE)
dir.create("GEDI_temp")

# read GEDI poiint data with Standard Error
geditest <- st_read("GEDI_in/GEDI_AGB_MX_RepeatedMeasures_qualFlag_agbd_agbd_se.shp")

geditest_p <- geditest %>% st_transform(3395)

st_write(geditest_p, "GEDI_temp/geditest_p.shp", delete_layer = TRUE)

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

# lastAGB
lastAGB_rast_ptsx <- terra::rasterize(geditest_p, idw_v_mex, field="lastAGB", fun = length)
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

dAGB_m <- lastAGB_rast_m - firstAGB_rast_m
dAGB_sd <- sqrt((lastAGB_rast_sd^2) + (firstAGB_rast_sd^2))
dAGB_se <- sqrt((lastAGB_rast_se^2) + (firstAGB_rast_se^2))

terra::writeRaster(dAGB_m, "GEDI_temp/dAGB_m.tif",
                   filetype = "GTiff", overwrite = T)
terra::writeRaster(dAGB_sd, "GEDI_temp/dAGB_sd.tif",
                   filetype = "GTiff", overwrite = T)
terra::writeRaster(dAGB_se, "GEDI_temp/dAGB_se.tif",
                   filetype = "GTiff", overwrite = T)


# Convert rasters to dataframes for ROC
dAGB_m_df<-as.data.frame(dAGB_m, xy=TRUE) %>% 
  st_as_sf(coords=1:2)
dAGB_se_df<-as.data.frame(dAGB_se, xy=TRUE) %>% 
  st_as_sf(coords=1:2)

idw_v_mex_df <-as.data.frame(idw_v_mex, xy=TRUE) %>% 
  st_as_sf(coords=1:2)
idw_w_mex_df <-as.data.frame(idw_w_mex, xy=TRUE) %>% 
  st_as_sf(coords=1:2)

#Option 1: Join dAGB and Mofuss probability without filtering with SE
GEDI_Mofuss <- idw_v_mex_df %>% 
  st_join(dAGB_m_df, by="geometry") %>% 
  dplyr::rename(AGB_delta=mean, probability_mofuss="IDW_C++_fw_v13") %>% 
  drop_na(probability_mofuss) %>% 
  drop_na(AGB_delta)

# Create datasets for Gain and Loss ROC curves with option 1
GEDI_Mofuss_gain <- GEDI_Mofuss %>% 
  mutate(Change = case_when(AGB_delta>-400 & AGB_delta<=(-20)~ '0',
                            AGB_delta>20 & AGB_delta<60~ '1'))   %>% 
  mutate(probability_mofuss=as.numeric(probability_mofuss)) %>%
  mutate(Change=as.numeric(Change)) %>% 
  drop_na(Change)  

GEDI_Mofuss_loss <- GEDI_Mofuss %>% 
  mutate(Change= case_when(AGB_delta>-400 & AGB_delta<=(-20)~ '1',
                           AGB_delta>-20 & AGB_delta<60~ '0'))   %>%
  mutate(probability_mofuss=as.numeric(probability_mofuss)) %>%
  mutate(Change=as.numeric(Change)) %>% 
  drop_na(Change) 

#Option 2: Join dAGB and Mofuss probability and filter with SE
GEDI_Mofuss <- idw_v_mex_df %>% 
  st_join(dAGB_m_df, by="geometry") %>% 
  st_join(dAGB_se_df, by="geometry") %>% #For filtering with SE
  dplyr::rename(AGB_se=lastAGB_sd) %>%  # For filtering with SE
  dplyr::rename(AGB_delta=mean, probability_mofuss="IDW_C++_fw_v13") %>% 
  dplyr::filter(abs(AGB_delta)>AGB_se) %>% # For filtering with SE
  drop_na(probability_mofuss) %>% 
  drop_na(AGB_delta)

# Create datasets for Gain and Loss ROC curves with option 2
GEDI_Mofuss_gain <- GEDI_Mofuss %>% 
  mutate(Change = case_when(AGB_delta>-5000 & AGB_delta<=(0)~ '0',
                            AGB_delta>0 & AGB_delta<50000~ '1'))   %>% 
  mutate(probability_mofuss=as.numeric(probability_mofuss)) %>%
  mutate(Change=as.numeric(Change)) %>% 
  drop_na(Change) 

GEDI_Mofuss_loss <- GEDI_Mofuss %>% 
  mutate(Change= case_when(AGB_delta>-5000 & AGB_delta<=(0)~ '1',
                           AGB_delta>0 & AGB_delta<5000~ '0'))   %>%
  mutate(probability_mofuss=as.numeric(probability_mofuss)) %>%
  mutate(Change=as.numeric(Change)) %>% 
  drop_na(Change) 

#ROC Curves 

## plot the data
#plot(x=GEDI_Mofuss_loss$probability_mofuss, 
 #    y=GEDI_Mofuss_loss$Change, main="AGB Loss")

## fit a logistic regression to the data...
glm.fit_gain=glm(GEDI_Mofuss_gain$Change ~ GEDI_Mofuss_gain$probability_mofuss, family=binomial)
glm.fit_loss=glm(GEDI_Mofuss_loss$Change ~ GEDI_Mofuss_loss$probability_mofuss, family=binomial)

summary_gain<-summary(glm.fit_gain)
summary_gain_df <-summary.glm(glm.fit_gain)$coefficients

summary_loss<-summary(glm.fit_loss)
summary_loss_df <-summary.glm(glm.fit_loss)$coefficients

#Random model for ROC test
x<-rbinom(360889, size = 1, prob = 0.5) #Modify depending on the number of values in the final grid with AGB delta
y<-runif(360889, min=0, max=1)
glm.ran=glm(x ~ y, family=binomial)

#ROC curves
roc_loss_v <- roc(GEDI_Mofuss_loss$Change, glm.fit_loss$fitted.values, plot=TRUE,show.thres=TRUE, legacy.axes=TRUE, percent=TRUE, xlab="False Positive Percentage", ylab="True Postive Percentage", main="Loss IDW_V_MEX",  col="#377eb8", lwd=4, print.auc=TRUE)
Sys.sleep(7)
dev.copy(png,'GEDI_temp/roc_loss_v.png')
dev.off()
roc_gainn_v <- roc(GEDI_Mofuss_gain$Change, glm.fit_gain$fitted.values, plot=TRUE,show.thres=TRUE, legacy.axes=TRUE, percent=TRUE, xlab="False Positive Percentage", ylab="True Postive Percentage", main="Gain IDW_V_MEX",  col="#377eb8", lwd=4, print.auc=TRUE)
Sys.sleep(7)
dev.copy(png,'GEDI_temp/roc_gain_v.png')
dev.off()
roc_45_v <- roc(x, glm.ran$fitted.values, plot=TRUE,show.thres=TRUE, legacy.axes=TRUE, percent=TRUE, xlab="False Positive Percentage", ylab="True Postive Percentage", main="Random IDW_V_MEX", col="#377eb8", lwd=4, print.auc=TRUE)
Sys.sleep(7)
dev.copy(png,'GEDI_temp/roc_random_v.png')
dev.off()

#Test for two ROC curves
#test_loss<-pROC::roc.test(roc_loss_v, roc_45_v, method=c("bootstrap"), boot.n=1000)
#test_gain<-pROC::roc.test(roc_gainn_v, roc_45_v, method=c("bootstrap"), boot.n=1000)



###NOW FOR W ####

# read Mexico raster probability layers
idw_w_mex <- rast("GEDI_in/IDW_C++_fw_w13.tif")

# rasterize over IDW 1km raster layer using a function mean plus se propagation (missing)
# classify based on number of points threshold
ptsthres <- 0 # threshold
m <- c(0, ptsthres, NA,
       ptsthres, Inf, 1)
rclmat <- matrix(m, ncol=3, byrow=TRUE)

# firstAGB
firstAGB_rast_ptsx <- terra::rasterize(geditest_p, idw_w_mex, field="firstAGB", fun = length)
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

# lastAGB
lastAGB_rast_ptsx <- terra::rasterize(geditest_p, idw_w_mex, field="lastAGB", fun = length)
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

dAGB_m <- lastAGB_rast_m - firstAGB_rast_m
dAGB_sd <- sqrt((lastAGB_rast_sd^2) + (firstAGB_rast_sd^2))
dAGB_se <- sqrt((lastAGB_rast_se^2) + (firstAGB_rast_se^2))

terra::writeRaster(dAGB_m, "GEDI_temp/dAGB_m.tif",
                   filetype = "GTiff", overwrite = T)
terra::writeRaster(dAGB_sd, "GEDI_temp/dAGB_sd.tif",
                   filetype = "GTiff", overwrite = T)
terra::writeRaster(dAGB_se, "GEDI_temp/dAGB_se.tif",
                   filetype = "GTiff", overwrite = T)


# Convert rasters to dataframes for ROC
dAGB_m_df<-as.data.frame(dAGB_m, xy=TRUE) %>% 
  st_as_sf(coords=1:2)
dAGB_se_df<-as.data.frame(dAGB_se, xy=TRUE) %>% 
  st_as_sf(coords=1:2)

idw_w_mex_df <-as.data.frame(idw_w_mex, xy=TRUE) %>% 
  st_as_sf(coords=1:2)

# Join dAGB and Mofuss probability
GEDI_Mofuss_w <- idw_w_mex_df %>% 
  st_join(dAGB_m_df, by="geometry") %>% 
  #st_join(dAGB_se_df, by="geometry") %>% #For filtering with SE
  #dplyr::rename(AGB_se=lastAGB_sd) %>%  # For filtering with SE
  dplyr::rename(AGB_delta=mean, probability_mofuss="IDW_C++_fw_w13") %>% 
  #dplyr::filter(abs(AGB_delta)>AGB_se) %>% # For filtering with SE
  drop_na(probability_mofuss) %>% 
  drop_na(AGB_delta)

# Create datasets for Gain and Loss ROC curves
GEDI_Mofuss_gain <- GEDI_Mofuss_w %>% 
  mutate(Change = case_when(AGB_delta>-400 & AGB_delta<=(0)~ '0',
                            AGB_delta>0 & AGB_delta<60~ '1'))   %>% 
  mutate(probability_mofuss=as.numeric(probability_mofuss)) %>%
  mutate(Change=as.numeric(Change)) %>% 
  drop_na(Change) 

GEDI_Mofuss_loss <- GEDI_Mofuss_w %>% 
  mutate(Change= case_when(AGB_delta>-400 & AGB_delta<=(-20)~ '1',
                           AGB_delta>-20 & AGB_delta<60~ '0'))   %>%
  mutate(probability_mofuss=as.numeric(probability_mofuss)) %>%
  mutate(Change=as.numeric(Change)) %>% 
  drop_na(Change) 

#ROC Curves 

## plot the data
#plot(x=GEDI_Mofuss_loss$probability_mofuss, 
#    y=GEDI_Mofuss_loss$Change, main="AGB Loss")

## fit a logistic regression to the data...
glm.fit_gain=glm(GEDI_Mofuss_gain$Change ~ GEDI_Mofuss_gain$probability_mofuss, family=binomial)
glm.fit_loss=glm(GEDI_Mofuss_loss$Change ~ GEDI_Mofuss_loss$probability_mofuss, family=binomial)

summary_gain<-summary(glm.fit_gain)
summary_gain_df <-summary.glm(glm.fit_gain)$coefficients

summary_loss<-summary(glm.fit_loss)
summary_loss_df <-summary.glm(glm.fit_loss)$coefficients

#Random model for ROC test
x<-rbinom(360889, size = 1, prob = 0.5) #Modify depending on the number of values in the final grid with AGB delta
y<-runif(360889, min=0, max=1)
glm.ran=glm(x ~ y, family=binomial)

#ROC curves
roc_loss_w <- roc(GEDI_Mofuss_loss$Change, glm.fit_loss$fitted.values, plot=TRUE,show.thres=TRUE, legacy.axes=TRUE, percent=TRUE, xlab="False Positive Percentage", ylab="True Postive Percentage", main="Loss IDW_W_MEX", col="#377eb8", lwd=3, print.auc=TRUE)
Sys.sleep(7)
dev.copy(png,'GEDI_temp/roc_loss_w.png')
dev.off()
roc_gainn_w <- roc(GEDI_Mofuss_gain$Change, glm.fit_gain$fitted.values, plot=TRUE,show.thres=TRUE, legacy.axes=TRUE, percent=TRUE, xlab="False Positive Percentage", ylab="True Postive Percentage", main="Gain IDW_W_MEX", col="#377eb8", lwd=4, print.auc=TRUE)
Sys.sleep(7)
dev.copy(png,'GEDI_temp/roc_gain_w.png')
dev.off()
roc_45_w <- roc(x, glm.ran$fitted.values, plot=TRUE,show.thres=TRUE, legacy.axes=TRUE, percent=TRUE, xlab="False Positive Percentage", ylab="True Postive Percentage", main="Random IDW_W_MEX", col="#377eb8", lwd=4, print.auc=TRUE)
Sys.sleep(7)
dev.copy(png,'GEDI_temp/roc_random_w.png')
dev.off()


#Test for two ROC curves
#test_loss<-pROC::roc.test(roc_loss_w, roc_45_w, method=c("bootstrap"), boot.n=1000)
#test_gain<-pROC::roc.test(roc_gainn_w, roc_45_w, method=c("bootstrap"), boot.n=1000)


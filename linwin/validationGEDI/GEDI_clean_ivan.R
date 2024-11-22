# Load libraries ####
library(dplyr)
library(sf)
library(raster)
library(rgdal)
library(progress)
library(fasterize)
library(ggplot2)
library(scales)
library(tidyr)
library(tidyverse)
library(stringr)
library(spatialEco)
library (pROC)

#Load GEDI data ####
setwd("C:/Users/ihern/OneDrive/Documentos/Posdoc/GEDIAGBchangeMX/MX_GEDI_AGB_rep")
AGB<- st_read("GEDI_AGB_MX_RepeatedMeasures_qualFlag_agbd_agbd_se.shp")
AGB_df<-as.data.frame(AGB) %>% 
  mutate(se_perc_firstAGB=firstAGBse/firstAGB*100) %>% #calculate standard error of firstAGB
  mutate(se_perc_lastAGB=lastAGBse/lastAGB*100) %>% #calculate standard error of lastAGB
  dplyr::filter(se_perc_lastAGB<25 & se_perc_lastAGB<25) #filter values <25% standard error

#Load grid 1km ####
rejilla<-st_read("Rejilla_Mexico_01.shp") %>% 
  dplyr::select(id, geometry)
st_crs(rejilla)<-4326 # asignar proyección a la rejilla

#Intersect GEDI points and grid 1km ####
#AGB_rej<-point.in.poly(AGB_df, rejilla, duplicate = F) #This is the process that takes 7-12h and needs to be improved

#Calculate delta in each grid cell####
AGB_rej_delta<-as.data.frame(AGB_rej) %>%  
  dplyr::group_by(pid1) %>% 
  dplyr::summarize(firstAGB=mean(firstAGB),
                   lastAGB=mean(lastAGB),
                   coords.x1=mean(coords.x1),
                   coords.x2=mean(coords.x2)) %>% 
  mutate(delta=lastAGB-firstAGB)

#Assign Mofuss values (raster) to AGB_rej_delta. This was done in QGIS.####

#Import new layer of AGB_rej_delta + Mofuss values.####
GEDI_Mofuss<- st_read("GEDI_Mofuss_sd.shp") 

#Create two datasets: Gain and Loss ####
GEDI_Mofuss_prep<-GEDI_Mofuss %>% as.data.frame() %>% 
  dplyr::select(val_bio,SAMPLE_1, geometry) %>% 
  dplyr::rename(probability_mofuss=SAMPLE_1, delta=val_bio) 

GEDI_Mofuss_gain <- GEDI_Mofuss_prep %>% 
  mutate(Change = case_when(delta>-400 & delta<=(-20)~ '0',
                            delta>0 & delta<60~ '1',
                            delta>(-20) & delta<=(0)~ '0'))   %>% 
  mutate(Change=as.numeric(Change)) %>% 
  drop_na(probability_mofuss) %>% 
  drop_na(Change)

GEDI_Mofuss_loss <- GEDI_Mofuss_prep %>% 
  mutate(Change= case_when(delta>-400 & delta<=(-20)~ '1',
                           delta>0 & delta<60~ '0',
                           delta>(-20) & delta<=(0)~ '0'))   %>%
  mutate(probability_mofuss=as.numeric(probability_mofuss)) %>%
  mutate(Change=as.numeric(Change)) %>% 
  drop_na(probability_mofuss) %>% 
  drop_na(Change)


##ROC ####

## plot the data
plot(x=GEDI_Mofuss_gain$probability_mofuss, 
     y=GEDI_Mofuss_gain$Change)

## fit a logistic regression to the data...
glm.fit_gain=glm(GEDI_Mofuss_gain$Change ~ GEDI_Mofuss_gain$probability_mofuss, family=binomial)
glm.fit_loss=glm(GEDI_Mofuss_loss$Change ~ GEDI_Mofuss_loss$probability_mofuss, family=binomial)

summary_gain<-summary(glm.fit_gain)
summary_gain_df <-summary.glm(glm.fit_gain)$coefficients

summary_loss<-summary(glm.fit_loss)
summary_loss_df <-summary.glm(glm.fit_loss)$coefficients

#Random model for ROC test
x<-rbinom(359496, size = 1, prob = 0.5) #Modify depending on the number of values in the final grid with AGB delta
y<-runif(359496, min=0, max=1)
glm.ran=glm(x ~ y, family=binomial)

#ROC curves
roc_mofus_loss <- roc(GEDI_Mofuss_loss$Change, glm.fit_loss$fitted.values, plot=TRUE,show.thres=TRUE, legacy.axes=TRUE, percent=TRUE, xlab="False Positive Percentage", ylab="True Postive Percentage", col="#377eb8", lwd=3, print.auc=TRUE)
roc_mofus_gainn <- roc(GEDI_Mofuss_gain$Change, glm.fit_gain$fitted.values, plot=TRUE,show.thres=TRUE, legacy.axes=TRUE, percent=TRUE, xlab="False Positive Percentage", ylab="True Postive Percentage", col="#377eb8", lwd=4, print.auc=TRUE)
roc_45 <- roc(x, glm.ran$fitted.values, plot=TRUE,show.thres=TRUE, legacy.axes=TRUE, percent=TRUE, xlab="False Positive Percentage", ylab="True Postive Percentage", col="#377eb8", lwd=4, print.auc=TRUE)

#Test for two ROC curves
test_loss<-pROC::roc.test(roc_mofus_loss, roc_45, method=c("bootstrap"), boot.n=1000)
test_gain<-pROC::roc.test(roc_mofus_gainn, roc_45, method=c("bootstrap"), boot.n=1000)
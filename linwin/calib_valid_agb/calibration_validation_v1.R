# MoFuSS
# Version 3
# Date: Jul 2024

# 2dolist
# replace missing values with 2015 when using 100m
# finish 1km validation
# replace 2010 AGB map at 100m
# calibrate growth and TOF availability + automated mofuss curves with ctrees

# Internal parameters
agbthreshold <- 1
crop_using_mapview <- 1
ctrees_calval100m <- 0
ctrees_calval1km <- 0
# chloris_calval <- 0
esacci_calval100m <- 0
esacci_calval1000m <- 1

# Load packages ----
library(terra)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)
library(tictoc)
library(fs)
library(tcltk)
library(ggspatial)
library(prettymapr)
library(ggmap)
library(pacman)
library(sf)
library(shiny)
library(leaflet)
library(leaflet.extras)
library(raster)
library(rnaturalearth)
# library(rgdal)
# library(rasterVis)
library(svDialogs)

# Define directories ----

# Define MoFuSS the directory 100m
mofuss_dir100m <- "D:/MoFuSS_Malawi_100m_Mar2024"
# mofuss_dir <- "D:/MoFuSS_Nepal_100m_May2024_4Chloris"
# mofuss_dir <- "D:/MoFuSS_Nepal_100m_May2024"

# Define MoFuSS the directory 1km
mofuss_dir1km <- "D:/MoFuSS_Malawi_100m_Mar2024"

# Define CTress directory 100m
# ctrees_dir <- "D:/CTrees_Malawi_2010-2020_1km"
ctrees_dir100m <- "D:/CTrees_Malawi_2010-2020_100m"

# Define CTress directory 1km
ctrees_dir1km <- "D:/CTrees_Malawi_2010-2020_1km"

# # Define Chloris Geospatial directory
# chloris_dir <- "D:/Chloris_Malawi_30m"
# # chloris_dir <- "D:/Chloris_Nepal_30m"

if (ctrees_calval100m == 1){
  
  # CTrees validations 100m ----
  setwd(ctrees_dir100m)
  
  #mofuss
  
  nrbmofuss10_20x <- terra::rast(paste0(mofuss_dir100m,"/OutBaU/webmofuss_results/nrb_10_20_mean.tif"))
  nrbmofuss10_20 <- terra::classify(nrbmofuss10_20x, cbind(-Inf, agbthreshold, NA))
  
  nrbmofuss10_20_sdx <- terra::rast(paste0(mofuss_dir100m,"/OutBaU/webmofuss_results/nrb_10_20_sd.tif"))
  nrbmofuss10_20_sd <- terra::classify(nrbmofuss10_20_sdx, cbind(-Inf, agbthreshold, NA))
  
  harv10_20x <- terra::rast(paste0(mofuss_dir100m,"/OutBaU/webmofuss_results/harv_10_20_mean.tif"))
  harv10_20 <- terra::classify(harv10_20x, cbind(-Inf, agbthreshold, NA))
  harv10_20_sdx <- terra::rast(paste0(mofuss_dir100m,"/OutBaU/webmofuss_results/harv_10_20_sd.tif"))
  harv10_20_sd <- terra::classify(harv10_20_sdx, cbind(-Inf, agbthreshold, NA))
  
  # ctrees
  
  agb10ctrees_x <- terra::rast(paste0(ctrees_dir100m,"/in/MWI_AGC_100m_2010.tif"))
  agb10ctrees <- agb10ctrees_x * (12/44) * 2
  agb10ctrees[agb10ctrees < 0] <- NA
  #agb10ctrees_se_x <- terra::rast(paste0(ctrees_dir100m,"/in/se_30m_2010.tif"))
  agb20ctrees_x <- terra::rast(paste0(ctrees_dir100m,"/in/MWI_AGC_100m_2020.tif"))
  agb20ctrees <- agb20ctrees_x * (12/44) * 2
  agb20ctrees[agb20ctrees < 0] <- NA
  #agb20ctrees_se_x <- terra::rast(paste0(ctrees_dir100m,"/in/se_30m_2020.tif"))
  
  nrbctrees10_20 <- agb10ctrees-agb20ctrees
  plot(agb10ctrees)
  plot(agb20ctrees)
  # plot(nrbctrees10_20) #This looks weird 
  
  # Project nrbctrees10_20 to the projection of nrbmofuss10_20
  nrbctrees10_20_proj <- project(nrbctrees10_20, crs(nrbmofuss10_20))
  
  if (crop_using_mapview == 1) {
    
    # Select country and get  boundaries
    ne_countries_names <- ne_countries() %>%
      dplyr::select(sovereignt) %>%
      st_drop_geometry() %>%
      dplyr::arrange(.$sovereignt)
    
    ne_countries.input <- dlgList(ne_countries_names$sovereignt, 
                                  preselect = "Malawi",
                                  multiple = FALSE,
                                  title = "Select a country for zooming the leaflet map",
                                  gui = .GUI
    )
    ne_country2 <- ne_countries.input$res
    ne_country <- ne_countries(country = ne_country2, returnclass = "sf")
    
    # Get bounding box of the country
    bounds <- st_bbox(ne_country)
    
    # Define UI
    ui <- fluidPage(
      leafletOutput("map"),
      textOutput("selectedCoords"),
      actionButton("closeBtn", "Close")
    )
    
    # Define server logic
    server <- function(input, output, session) {
      rv <- reactiveValues(drawnExtent = NULL)
      
      output$map <- renderLeaflet({
        leaflet() %>% 
          addProviderTiles(providers$Esri.NatGeoWorldMap) %>% 
          fitBounds(lng1 = as.numeric(bounds$xmin), 
                    lat1 = as.numeric(bounds$ymin), 
                    lng2 = as.numeric(bounds$xmax), 
                    lat2 = as.numeric(bounds$ymax)) %>%
          addDrawToolbar(
            targetGroup = 'draw',
            rectangleOptions = drawRectangleOptions(),
            editOptions = editToolbarOptions()
          )
      })
      
      observeEvent(input$map_draw_new_feature, {
        feature <- input$map_draw_new_feature
        bbox <- feature$geometry$coordinates[[1]]
        xmin <- min(sapply(bbox, function(coord) coord[[1]]))
        xmax <- max(sapply(bbox, function(coord) coord[[1]]))
        ymin <- min(sapply(bbox, function(coord) coord[[2]]))
        ymax <- max(sapply(bbox, function(coord) coord[[2]]))
        rv$drawnExtent <- extent(xmin, xmax, ymin, ymax)
        
        # Save the extent to the global environment
        assign("drawnExtent", rv$drawnExtent, envir = .GlobalEnv)
      })
      
      output$selectedCoords <- renderText({
        if (is.null(rv$drawnExtent)) {
          "Draw a rectangle on the map to select an area."
        } else {
          paste("Selected extent - xmin:", rv$drawnExtent@xmin, 
                "ymin:", rv$drawnExtent@ymin, 
                "xmax:", rv$drawnExtent@xmax, 
                "ymax:", rv$drawnExtent@ymax)
        }
      })
      
      observeEvent(input$closeBtn, {
        stopApp()
      })
    }
    
    stop("Run the shiny app")
    
    # Run the application 
    shinyApp(ui = ui, server = server) # SHINNY APP HERE! ----
    
    {
      print(drawnExtent)
      
      # The code here should execute after the app is closed
      print("The Shiny app has been closed. Continuing with the rest of the script.")
      
      # Convert extent to SpatialPolygons
      ext_poly <- as(drawnExtent, "SpatialPolygons")
      proj4string(ext_poly) <- CRS("+proj=longlat +datum=WGS84")  # Define the original CR
      
      # Define the target CRS (for example, UTM Zone 36S)
      target_crs <- CRS(projection(raster(nrbmofuss10_20)))
      
      # Reproject the SpatialPolygons
      ext_poly_proj <- spTransform(ext_poly, target_crs)
      
      # Get the new extent
      new_extent <- extent(ext_poly_proj)
      
      # Print the new extent
      print(new_extent)
      
      # Crop nrbmofuss10_20 by drawing a polygon
      nrbmofuss10_20_cropped <- crop(nrbmofuss10_20, ext(new_extent))
      harv10_20_cropped <- crop(harv10_20, ext(new_extent))
      
      # }
      
      if (crop_using_mapview != 1) {
        
        # Crop nrbmofuss10_20 by the extent of nrbctrees10_20_proj
        nrbmofuss10_20_cropped <- crop(nrbmofuss10_20, ext(nrbctrees10_20_proj))
        harv10_20_cropped <- crop(harv10_20, ext(nrbctrees10_20_proj))
        
      }
      
      # Resample nrbctrees10_20_proj to match the resolution and extent of nrbmofuss10_20_cropped
      nrbctrees10_20_resampled <- resample(nrbctrees10_20_proj, nrbmofuss10_20_cropped)
      
      # Mask nrbmofuss10_20_cropped by nrbctrees10_20_resampled
      nrbmofuss10_20_masked <- mask(nrbmofuss10_20_cropped, nrbctrees10_20_resampled)
      harv10_20_masked <- mask(harv10_20_cropped, nrbctrees10_20_resampled)
      
      # Plot to visualize
      plot(nrbmofuss10_20_masked)
      plot(nrbctrees10_20_resampled)
      plot(harv10_20_masked)
      
      nrbmofuss10_20_4comp <- terra::classify(nrbmofuss10_20_masked, cbind(-Inf, agbthreshold, NA)) %>%
        round()
      nrbctrees10_20_4comp <- terra::classify(nrbctrees10_20_resampled, cbind(-Inf, agbthreshold, NA)) %>%
        round()
      harv10_20_4comp <- terra::classify(harv10_20_masked, cbind(-Inf, agbthreshold, NA)) %>%
        round()
      gainsctrees10_20_4comp <- terra::classify(nrbctrees10_20_resampled, cbind(0, Inf, NA)) %>%
        round() %>%
        (function(x) x * -1) 
      
      # nrbmofuss10_20_cropped <- aggregate(nrbmofuss10_20_cropped, fact = 10, fun = sum)  # For a 100m raster to 1km
      # nrbctrees10_20_cropped <- aggregate(nrbctrees10_20_cropped, fact = 10, fun = sum)  # For a 100m raster to 1km
      # harv10_20_cropped <- aggregate(harv10_20_cropped, fact = 10, fun = sum)  # For a 100m raster to 1km
      
      # Calculate correlation
      correlation <- cor(values(nrbctrees10_20_4comp), values(nrbmofuss10_20_4comp), use="complete.obs")
      print(correlation)
      
      
      # Scatter plot for comparison
      plot(values(nrbctrees10_20_4comp), values(nrbmofuss10_20_4comp),
           xlab="Observed Biomass Change",
           ylab="Modeled Biomass Change",
           main="Scatter plot of Observed vs. Modeled Biomass Change")
      abline(0, 1, col="red") # 1:1 line
      
      # Calculate RMSE
      rmse <- sqrt(mean((values(nrbctrees10_20_4comp) - values(nrbmofuss10_20_4comp))^2, na.rm=TRUE))
      print(rmse)
      
      # Calculate MAE
      mae <- mean(abs(values(nrbctrees10_20_4comp) - values(nrbmofuss10_20_4comp)), na.rm=TRUE)
      print(mae)
      
      # Observed fNRB
      observednrb <- global(nrbctrees10_20_4comp, fun = "sum", na.rm = TRUE)
      modelednrb <- global(nrbmofuss10_20_4comp, fun = "sum", na.rm = TRUE)
      harvestmofuss <- global(harv10_20_4comp, fun = "sum", na.rm = TRUE)
      fNRB_obs <- round((observednrb / harvestmofuss),2)
      fNRB_mofuss <- round((modelednrb / harvestmofuss),2)
      
      observednrb
      modelednrb
      harvestmofuss
      fNRB_obs # fNRB_obs vs fNRB_mofuss----
      fNRB_mofuss
      print(correlation)
      
      # Define a color gradient from white to red
      color_pal <- colorRampPalette(c("white", "orange", "red"))
      # Create a color gradient from white to red
      colors <- color_pal(100)  # Create 100 intermediate colors
      
      # Find the range of values across both datasets
      min_value <- min(c(minmax(nrbctrees10_20_4comp), minmax(nrbmofuss10_20_4comp), minmax(harv10_20_4comp)))
      max_value <- max(c(minmax(nrbctrees10_20_4comp), minmax(nrbmofuss10_20_4comp), minmax(harv10_20_4comp)))
      
      # Define the common color scale range
      common_range <- c(min_value, max_value)
      
      par(mfrow=c(2, 2))
      
      # Plot the observed data with the common scale
      plot(nrbctrees10_20_4comp, main="Observed NRB (2010-2020)", col=colors, range=common_range)
      
      # Plot the modeled data with the common scale
      plot(nrbmofuss10_20_4comp, main="Modeled NRB (2010-2020)", col=colors, range=common_range)
      
      # Plot the gains data
      plot(gainsctrees10_20_4comp, main="Observed AGB gains (2010-2020)")
      
      # Plot the gains data
      plot(harv10_20_4comp, main="Modeled harvest (2010-2020)", col=colors, range=common_range)
      
      # dev.off()
      
    }
  }
}

if (ctrees_calval1km == 1){
  
  # CTrees validations 1km ----
  setwd(ctrees_dir1km)
  
  #mofuss
  
  nrbmofuss10_20x <- terra::rast(paste0(mofuss_dir,"/OutBaU/webmofuss_results/nrb_10_20_mean.tif"))
  nrbmofuss10_20 <- terra::classify(nrbmofuss10_20x, cbind(-Inf, agbthreshold, NA))
  
  nrbmofuss10_20_sdx <- terra::rast(paste0(mofuss_dir,"/OutBaU/webmofuss_results/nrb_10_20_sd.tif"))
  nrbmofuss10_20_sd <- terra::classify(nrbmofuss10_20_sdx, cbind(-Inf, agbthreshold, NA))
  
  harv10_20x <- terra::rast(paste0(mofuss_dir,"/OutBaU/webmofuss_results/harv_10_20_mean.tif"))
  harv10_20 <- terra::classify(harv10_20x, cbind(-Inf, agbthreshold, NA))
  harv10_20_sdx <- terra::rast(paste0(mofuss_dir,"/OutBaU/webmofuss_results/harv_10_20_sd.tif"))
  harv10_20_sd <- terra::classify(harv10_20_sdx, cbind(-Inf, agbthreshold, NA))
  
  # ctrees
  
  agb10ctrees_x <- terra::rast(paste0(ctrees_dir1km,"/in/ctrees_global_2010_AGC_MWI.tif"))
  agb10ctrees <- agb10ctrees_x * (12/44) * 2
  agb10ctrees[agb10ctrees < 0] <- NA
  #agb10ctrees_se_x <- terra::rast(paste0(ctrees_dir1km,"/in/se_30m_2010.tif"))
  agb20ctrees_x <- terra::rast(paste0(ctrees_dir1km,"/in/ctrees_global_2010_AGC_MWI.tif"))
  agb20ctrees <- agb20ctrees_x * (12/44) * 2
  agb20ctrees[agb20ctrees < 0] <- NA
  #agb20ctrees_se_x <- terra::rast(paste0(ctrees_dir1km,"/in/se_30m_2020.tif"))
  
  nrbctrees10_20 <- agb10ctrees-agb20ctrees
  plot(agb10ctrees)
  plot(agb20ctrees)
  plot(nrbctrees10_20)
  
  # Project nrbctrees10_20 to the projection of nrbmofuss10_20
  nrbctrees10_20_proj <- project(nrbctrees10_20, crs(nrbmofuss10_20))
  
  # Crop nrbmofuss10_20 by the extent of nrbctrees10_20_proj
  nrbmofuss10_20_cropped <- crop(nrbmofuss10_20, ext(nrbctrees10_20_proj))
  harv10_20_cropped <- crop(harv10_20, ext(nrbctrees10_20_proj))
  
  # Resample nrbctrees10_20_proj to match the resolution and extent of nrbmofuss10_20_cropped
  nrbctrees10_20_resampled <- resample(nrbctrees10_20_proj, nrbmofuss10_20_cropped)
  
  # Mask nrbmofuss10_20_cropped by nrbctrees10_20_resampled
  nrbmofuss10_20_masked <- mask(nrbmofuss10_20_cropped, nrbctrees10_20_resampled)
  harv10_20_masked <- mask(harv10_20_cropped, nrbctrees10_20_resampled)
  
  # Plot to visualize
  plot(nrbmofuss10_20_masked)
  plot(nrbctrees10_20_resampled)
  plot(harv10_20_masked)
  
  nrbmofuss10_20_4comp <- terra::classify(nrbmofuss10_20_masked, cbind(-Inf, agbthreshold, NA)) %>%
    round()
  nrbctrees10_20_4comp <- terra::classify(nrbctrees10_20_resampled, cbind(-Inf, agbthreshold, NA)) %>%
    round()
  harv10_20_4comp <- terra::classify(harv10_20_masked, cbind(-Inf, agbthreshold, NA)) %>%
    round()
  gainsctrees10_20_4comp <- terra::classify(nrbctrees10_20_resampled, cbind(0, Inf, NA)) %>%
    round() %>%
    (function(x) x * -1) 
  
  # nrbmofuss10_20_cropped <- aggregate(nrbmofuss10_20_cropped, fact = 10, fun = sum)  # For a 100m raster to 1km
  # nrbctrees10_20_cropped <- aggregate(nrbctrees10_20_cropped, fact = 10, fun = sum)  # For a 100m raster to 1km
  # harv10_20_cropped <- aggregate(harv10_20_cropped, fact = 10, fun = sum)  # For a 100m raster to 1km
  
  # Calculate correlation
  correlation <- cor(values(nrbctrees10_20_4comp), values(nrbmofuss10_20_4comp), use="complete.obs")
  print(correlation)
  
  
  # Scatter plot for comparison
  plot(values(nrbctrees10_20_4comp), values(nrbmofuss10_20_4comp),
       xlab="Observed Biomass Change",
       ylab="Modeled Biomass Change",
       main="Scatter plot of Observed vs. Modeled Biomass Change")
  abline(0, 1, col="red") # 1:1 line
  
  # Calculate RMSE
  rmse <- sqrt(mean((values(nrbctrees10_20_4comp) - values(nrbmofuss10_20_4comp))^2, na.rm=TRUE))
  print(rmse)
  
  # Calculate MAE
  mae <- mean(abs(values(nrbctrees10_20_4comp) - values(nrbmofuss10_20_4comp)), na.rm=TRUE)
  print(mae)
  
  # Observed fNRB
  observednrb <- global(nrbctrees10_20_4comp, fun = "sum", na.rm = TRUE)
  modelednrb <- global(nrbmofuss10_20_4comp, fun = "sum", na.rm = TRUE)
  harvestmofuss <- global(harv10_20_4comp, fun = "sum", na.rm = TRUE)
  fNRB_obs <- round((observednrb / harvestmofuss),2)
  fNRB_mofuss <- round((modelednrb / harvestmofuss),2)
  
  observednrb
  modelednrb
  harvestmofuss
  fNRB_obs
  fNRB_mofuss
  print(correlation)
  
  # Define a color gradient from white to red
  color_pal <- colorRampPalette(c("white", "orange", "red"))
  # Create a color gradient from white to red
  colors <- color_pal(100)  # Create 100 intermediate colors
  
  # Find the range of values across both datasets
  min_value <- min(c(minmax(nrbctrees10_20_4comp), minmax(nrbmofuss10_20_4comp), minmax(harv10_20_4comp)))
  max_value <- max(c(minmax(nrbctrees10_20_4comp), minmax(nrbmofuss10_20_4comp), minmax(harv10_20_4comp)))
  
  # Define the common color scale range
  common_range <- c(min_value, max_value)
  
  par(mfrow=c(2, 2))
  
  # Plot the observed data with the common scale
  plot(nrbctrees10_20_4comp, main="Observed NRB (2010-2020)", col=colors, range=common_range)
  
  # Plot the modeled data with the common scale
  plot(nrbmofuss10_20_4comp, main="Modeled NRB (2010-2020)", col=colors, range=common_range)
  
  # Plot the gains data
  plot(gainsctrees10_20_4comp, main="Observed AGB gains (2010-2020)")
  
  # Plot the gains data
  plot(harv10_20_4comp, main="Modeled harvest (2010-2020)", col=colors, range=common_range)
  
  # dev.off()
  
}


# if (chloris_calval == 1){
#   
#   
#   # Chloris validations ----
#   setwd(chloris_dir)
#   
#   #mofuss
#   
#   nrbmofuss10_20x <- terra::rast(paste0(mofuss_dir,"/OutBaU/webmofuss_results/nrb_10_20_mean.tif"))
#   nrbmofuss10_20 <- terra::classify(nrbmofuss10_20x, cbind(-Inf, agbthreshold, NA))
#   
#   nrbmofuss10_20_sdx <- terra::rast(paste0(mofuss_dir,"/OutBaU/webmofuss_results/nrb_10_20_sd.tif"))
#   nrbmofuss10_20_sd <- terra::classify(nrbmofuss10_20_sdx, cbind(-Inf, agbthreshold, NA))
#   
#   harv10_20x <- terra::rast(paste0(mofuss_dir,"/OutBaU/webmofuss_results/harv_10_20_mean.tif"))
#   harv10_20 <- terra::classify(harv10_20x, cbind(-Inf, agbthreshold, NA))
#   harv10_20_sdx <- terra::rast(paste0(mofuss_dir,"/OutBaU/webmofuss_results/harv_10_20_sd.tif"))
#   harv10_20_sd <- terra::classify(harv10_20_sdx, cbind(-Inf, agbthreshold, NA))
#   
#   # chloris
#   
#   agb10chloris <- terra::rast(paste0(chloris_dir,"/in/stock_30m_2010.tif"))
#   agb10chloris_se <- terra::rast(paste0(chloris_dir,"/in/se_30m_2010.tif"))
#   agb20chloris <- terra::rast(paste0(chloris_dir,"/in/stock_30m_2020.tif"))
#   agb20chloris_se <- terra::rast(paste0(chloris_dir,"/in/se_30m_2020.tif"))
#   
#   nrbchloris10_20 <- agb10chloris-agb20chloris
#   plot(nrbchloris10_20)
#   
#   # Project nrbchloris10_20 to the projection of nrbmofuss10_20
#   nrbchloris10_20_proj <- project(nrbchloris10_20, crs(nrbmofuss10_20))
#   
#   # Crop nrbmofuss10_20 by the extent of nrbchloris10_20_proj
#   nrbmofuss10_20_cropped <- crop(nrbmofuss10_20, ext(nrbchloris10_20_proj))
#   harv10_20_cropped <- crop(harv10_20, ext(nrbchloris10_20_proj))
#   
#   # Resample nrbchloris10_20_proj to match the resolution and extent of nrbmofuss10_20_cropped
#   nrbchloris10_20_resampled <- resample(nrbchloris10_20_proj, nrbmofuss10_20_cropped)
#   
#   # Mask nrbmofuss10_20_cropped by nrbchloris10_20_resampled
#   nrbmofuss10_20_masked <- mask(nrbmofuss10_20_cropped, nrbchloris10_20_resampled)
#   harv10_20_masked <- mask(harv10_20_cropped, nrbchloris10_20_resampled)
#   
#   # Plot to visualize
#   plot(nrbmofuss10_20_masked)
#   plot(nrbchloris10_20_resampled)
#   plot(harv10_20_masked)
#   
#   nrbmofuss10_20_4comp <- terra::classify(nrbmofuss10_20_masked, cbind(-Inf, agbthreshold, NA)) %>%
#     round()
#   nrbchloris10_20_4comp <- terra::classify(nrbchloris10_20_resampled, cbind(-Inf, agbthreshold, NA)) %>%
#     round()
#   harv10_20_4comp <- terra::classify(harv10_20_masked, cbind(-Inf, agbthreshold, NA)) %>%
#     round()
#   gainschloris10_20_4comp <- terra::classify(nrbchloris10_20_resampled, cbind(0, Inf, NA)) %>%
#     round() %>%
#     (function(x) x * -1) 
#   
#   # nrbmofuss10_20_cropped <- aggregate(nrbmofuss10_20_cropped, fact = 10, fun = sum)  # For a 100m raster to 1km
#   # nrbchloris10_20_cropped <- aggregate(nrbchloris10_20_cropped, fact = 10, fun = sum)  # For a 100m raster to 1km
#   # harv10_20_cropped <- aggregate(harv10_20_cropped, fact = 10, fun = sum)  # For a 100m raster to 1km
#   
#   # Calculate correlation
#   correlation <- cor(values(nrbchloris10_20_4comp), values(nrbmofuss10_20_4comp), use="complete.obs")
#   print(correlation)
#   
#   
#   # Scatter plot for comparison
#   plot(values(nrbchloris10_20_4comp), values(nrbmofuss10_20_4comp),
#        xlab="Observed Biomass Change",
#        ylab="Modeled Biomass Change",
#        main="Scatter plot of Observed vs. Modeled Biomass Change")
#   abline(0, 1, col="red") # 1:1 line
#   
#   # Calculate RMSE
#   rmse <- sqrt(mean((values(nrbchloris10_20_4comp) - values(nrbmofuss10_20_4comp))^2, na.rm=TRUE))
#   print(rmse)
#   
#   # Calculate MAE
#   mae <- mean(abs(values(nrbchloris10_20_4comp) - values(nrbmofuss10_20_4comp)), na.rm=TRUE)
#   print(mae)
#   
#   # Observed fNRB
#   observednrb <- global(nrbchloris10_20_4comp, fun = "sum", na.rm = TRUE)
#   modelednrb <- global(nrbmofuss10_20_4comp, fun = "sum", na.rm = TRUE)
#   harvestmofuss <- global(harv10_20_4comp, fun = "sum", na.rm = TRUE)
#   fNRB_obs <- round((observednrb / harvestmofuss),2)
#   fNRB_mofuss <- round((modelednrb / harvestmofuss),2)
#   
#   observednrb
#   modelednrb
#   harvestmofuss
#   fNRB_obs
#   fNRB_mofuss
#   print(correlation)
#   
#   # Define a color gradient from white to red
#   color_pal <- colorRampPalette(c("white", "orange", "red"))
#   # Create a color gradient from white to red
#   colors <- color_pal(100)  # Create 100 intermediate colors
#   
#   # Find the range of values across both datasets
#   min_value <- min(c(minmax(nrbchloris10_20_4comp), minmax(nrbmofuss10_20_4comp), minmax(harv10_20_4comp)))
#   max_value <- max(c(minmax(nrbchloris10_20_4comp), minmax(nrbmofuss10_20_4comp), minmax(harv10_20_4comp)))
#   
#   # Define the common color scale range
#   common_range <- c(min_value, max_value)
#   
#   par(mfrow=c(2, 2))
#   
#   # Plot the observed data with the common scale
#   plot(nrbchloris10_20_4comp, main="Observed NRB (2010-2020)", col=colors, range=common_range)
#   
#   # Plot the modeled data with the common scale
#   plot(nrbmofuss10_20_4comp, main="Modeled NRB (2010-2020)", col=colors, range=common_range)
#   
#   # Plot the gains data
#   plot(gainschloris10_20_4comp, main="Observed AGB gains (2010-2020)")
#   
#   # Plot the gains data
#   plot(harv10_20_4comp, main="Modeled harvest (2010-2020)", col=colors, range=common_range)
#   
#   # dev.off()
#   
#   
# }

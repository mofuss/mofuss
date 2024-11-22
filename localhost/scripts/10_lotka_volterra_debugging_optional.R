# MoFuSS
# Version 1
# Date: Feb 2024

library(raster)
library(terra)
library(ggplot2)
library(stringr)
library(DescTools)
library(mapview)
library(shiny)
library(leaflet)
library(sf)
library(rnaturalearth)
library(rgdal)
library(rasterVis)
library(svDialogs)

setwd(countrydir)
luccategory <- rast("LULCC/TempRaster/LULCt1_c.tif")
growth_parameters1 <- read.csv("LULCC/TempTables/growth_parameters1.csv") %>%
  dplyr::rename(LULCt1_c = Key.)

path2rasters <- "debugging_1"
file_list <- list.files(path2rasters, pattern = "^(Harvest_tot[0-9]+\\.tif$|Growth_less_harv[0-9]+\\.tif$)", full.names = TRUE)

# Assuming terra package, for raster replace with raster::stack()
rasters <- lapply(file_list, raster)

# Select country and get  boundaries
ne_countries_names <- ne_countries() %>%
  dplyr::select(sovereignt) %>%
  st_drop_geometry() %>%
  dplyr::arrange(.$sovereignt)

ne_countries.input <- dlgList(ne_countries_names$sovereignt, 
                              preselect = "Nepal",
                              multiple = FALSE,
                              title = "Select a country for zooming the leaflet map",
                              gui = .GUI
)
ne_country2 <- ne_countries.input$res
ne_country <- ne_countries(country = ne_country2, returnclass = "sf")

# Get bounding box of the country
bounds <- st_bbox(ne_country)

ui <- fluidPage(
  titlePanel("Interactive Raster Point Selection"),
  leafletOutput("map"),
  textOutput("selectedCoords"),
  actionButton("closeBtn", "Close App")
)

server <- function(input, output, session) {
  rv <- reactiveValues(selectedPoint = NULL)
  
  output$map <- renderLeaflet({
    leaflet() %>% 
      # addProviderTiles(providers$OpenStreetMap) %>% # https://leaflet-extras.github.io/leaflet-providers/preview/index.html
      addProviderTiles(providers$Esri.NatGeoWorldMap) %>% 
      # addProviderTiles(providers$Esri.WorldImagery) %>%
      # addRasterImage(rasters[[1]], opacity = 0.8) %>%
      # addTiles() %>%
      fitBounds(lng1 = as.numeric(bounds$xmin), 
                lat1 = as.numeric(bounds$ymin), 
                lng2 = as.numeric(bounds$xmax), 
                lat2 = as.numeric(bounds$ymax))
    
  })
  
  observeEvent(input$map_click, {
    rv$selectedPoint <- input$map_click
  })
  
  output$selectedCoords <- renderText({
    if (is.null(rv$selectedPoint)) {
      "Click on the map to select a point."
    } else {
      paste("Selected coordinates - Latitude:", rv$selectedPoint$lat, "Longitude:", rv$selectedPoint$lng)
    }
  })
  
  observeEvent(input$map_click, {
    rv$selectedPoint <- input$map_click
    write.csv(data.frame(latitude = rv$selectedPoint$lat, longitude = rv$selectedPoint$lng),
              "LULCC/TempTables/selected_coordinates.csv", row.names = FALSE)
  })
  
  observeEvent(input$closeBtn, {
    # This will stop the app when the button is clicked
    stopApp()
  })
}

print("Launching Shiny app...")
shinyApp(ui, server)

# The code here should execute after the app is closed
print("The Shiny app has been closed. Continuing with the rest of the script.")

selected_coords_gcs <- read.csv("LULCC/TempTables/selected_coordinates.csv")
selected_coords_gcs_sp <- SpatialPoints(coords=selected_coords_gcs[,c('longitude', 'latitude')],  
                                   proj4string = CRS('+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'))

# Extract CRS from raster
raster_crs <- projection(rasters[[1]])
# Transform SpatialPoints to match raster CRS
selected_coords_pcs <- spTransform(selected_coords_gcs_sp, CRS(raster_crs))

sf_coords_pcs <- sf::st_as_sf(selected_coords_pcs) %>%
  st_write("LULCC/TempVector/coords.shp", delete_layer = TRUE)

x_coord <- as.numeric(as.data.frame(selected_coords_pcs)$longitude)
y_coord <- as.numeric(as.data.frame(selected_coords_pcs)$latitude)
cbind(x_coord, y_coord)

# Initialize an empty data frame with specific column names
lv_dyn <- data.frame(Year = integer(0), Variable = character(0), Value = numeric(0), stringsAsFactors = FALSE)

for (file_path in file_list) {

  # Function to calculate the year
  calculate_year <- function(file_name) {
    # Use a more precise regex to ensure correct number extraction
    # The regex now ensures it captures the full number correctly, including cases with leading zeros or multiple digits
    number <- as.integer(gsub(".*[^\\d](\\d{2}).tif$", "\\1", file_name))
    year <- 2009 + number
    
    return(year)
  }
  
  # Function to extract the string chunk
  extract_string_chunk <- function(file_name) {
    # This regex removes the directory, the numbers, and the file extension, leaving the descriptive part
    string_chunk <- sub("debugging_1/(.*)[^\\d]\\d+.tif", "\\1", file_name)
    
    return(string_chunk)
  }
  
  # Apply the functions to the file names
  years_attr <- sapply(file_path, calculate_year)
  variable_att <- sapply(file_path, extract_string_chunk)
  years <- StripAttr(years_attr)
  variable <- StripAttr(variable_att)
  years
  variable
  
  # Assuming 'rast' is used to read the raster
  raster_data <- rast(file_path)
  
  # Extract value at specific location
  value <- terra::extract(raster_data, cbind(x_coord, y_coord))[1]

  # Append to the data frame, explicitly naming columns to match
  new_row <- data.frame(Year = years, Variable = variable, Value = as.numeric(value)/100, stringsAsFactors = FALSE)
  lv_dyn <- rbind(lv_dyn, new_row)

}

# Correct for any potential NA values or cleanup as needed
# lv_dyn <- na.omit(lv_dyn)
head(lv_dyn)

lv_dyn2 <- lv_dyn %>%
  dplyr::mutate(Variable = str_replace_all(Variable, c('Growth_less_harv' = 'AGB', 'Harvest_tot' = 'Harvest')))
lv_dyn2$Variable <- factor(lv_dyn2$Variable, levels = c("AGB", "Harvest"))

luctype <- terra::extract(luccategory, cbind(x_coord, y_coord))[1] %>%
  dplyr::inner_join(growth_parameters1, by="LULCt1_c") %>%
  dplyr::select(LULC) %>%
  unname()

toftype <- terra::extract(luccategory, cbind(x_coord, y_coord))[1] %>%
  dplyr::inner_join(growth_parameters1, by="LULCt1_c") %>%
  dplyr::select(TOF) %>% 
  unname()

ggplot(lv_dyn2, aes(x = Year, y = Value, color = Variable)) +
  geom_line(size = 0.75) + # Makes the line thicker
  scale_color_manual(values = c("AGB" = "green", "Harvest" = "black")) + # Explicit color mapping
  labs(y = "AGB (Mg/ha)", x = "Year", title = "AGB and Harvest Trajectories",
       subtitle = paste0(luctype," (TOF= ",toftype,")"), color = "") +
  theme_minimal(base_size = 13) +
  theme(legend.title.align = 0.5,
        legend.position = "top",
        plot.title = element_text(size = 20),
        plot.subtitle = element_text(size = 13),
        axis.title.x = element_text(size = 13),
        axis.title.y = element_text(size = 13),
        legend.background = element_rect(colour = "darkgreen", size = 0.5, linetype = "solid"),
        legend.text = element_text(size = 13))


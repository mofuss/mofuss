

if (webmofuss == 1){
  setwd("/home/rrangel/common")
  rTempdir_fnrbobs = "RUTA EN WEBMOFUSS" # Roberto: necesitas crea rTempdir_fnrbobs (DEBE SER DIFERENTE A rTempdir -de los scripts de mofuss- porque no pueden estar siendo usadas al mismo tiempo)
  agbpath = ""
  demandpath = ""
} else if (webmofuss == 0){
  # ONLY WORKS IN NRBV1 NODE as localhost"
  rTempdir_fnrbobs <- "D:/rTempdir_fnrbobs/"
  #agbpath = "E:/agb3rdparties/"
  agbpath = "G:/Mi unidad/webpages/2026_MoFuSSGlobal_Datasets/fnrb_obs_data/1km_agco2_2000_2025/"
  demandpath = "G:/Mi unidad/webpages/2026_MoFuSSGlobal_Datasets/fnrb_obs_data/"
}

# 2dolist ----

# Internal parameters ----

temdirdefined = 1 

# Load packages ----
library(terra)
# terraOptions(steps = 55)
if (temdirdefined == 1) {
  terraOptions(tempdir = rTempdir_fnrbobs)
  # List all files and directories inside the folder
  contents <- list.files(rTempdir_fnrbobs, full.names = TRUE, recursive = TRUE)
  # Delete the contents but keep the folder
  # unlink(contents, recursive = TRUE, force = TRUE)
}
library(dplyr)
library(leaflet)
library(readr)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(shiny)
# library(shinythemes)
# library(shinycssloaders)

# Define the server logic
shinyServer(function(input, output, session) {
  world <- ne_countries(scale = "medium", returnclass = "sf")
  
  # Initialize the results data frame with proper columns
  initial_results <- data.frame(
    Country = character(),
    Start.Year = integer(),
    End.Year = integer(),
    "Demand.Mg.period" = numeric(),
    "AGB.losses.Mg.period" = numeric(),
    "fNRB.%  " = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Store selected countries and results
  selected_countries <- reactiveVal(character())  # Initialize as empty vector
  results <- reactiveVal(initial_results)  # Store results with year and country
  period <- reactiveVal("")  # Store the period
  
  # Render leaflet map centered on Africa
  output$world_map <- renderLeaflet({
    leaflet(world) %>%
      addTiles() %>%
      setView(lng = 20, lat = 0, zoom = 3) %>%
      addPolygons(layerId = ~iso_a3, popup = ~name, fillOpacity = 0.2, weight = 1, color = "#444444")
  })
  
  # Update selected countries based on clicks on the map
  observeEvent(input$world_map_shape_click, {
    country_code <- input$world_map_shape_click$id
    current_selection <- selected_countries()
    
    if (!is.null(country_code)) {  # Ensure a valid country is clicked
      # Toggle country selection (add or remove)
      if (country_code %in% current_selection) {
        current_selection <- setdiff(current_selection, country_code)
        # Remove deselected country from results
        current_results <- results()
        updated_results <- current_results %>% filter(Country != country_code)
        results(updated_results)
      } else if (length(current_selection) < 16) {
        current_selection <- c(current_selection, country_code)
      }
      
      # Update selected countries
      selected_countries(current_selection)
      
      # Re-highlight selected countries on the map
      leafletProxy("world_map") %>%
        clearShapes() %>%
        addPolygons(data = world, layerId = ~iso_a3, popup = ~name, fillOpacity = 0.2, weight = 1, color = "#444444") %>%
        addPolygons(data = world %>% filter(iso_a3 %in% current_selection),
                    fillColor = "blue", fillOpacity = 0.5, weight = 2, color = "#0000FF")
    }
  })
  
  # Clear selection button
  observeEvent(input$clear_selection, {
    selected_countries(character())  # Reset the country selection
    results(initial_results)  # Clear the results table
    period("")  # Clear the period
    leafletProxy("world_map") %>%
      clearShapes() %>%
      addPolygons(data = world, layerId = ~iso_a3, popup = ~name, fillOpacity = 0.2, weight = 1, color = "#444444")
  })
  
  # Store the period when the end year changes
  observeEvent(input$endyr, {
    period(paste0("2010-", input$endyr))  # <- no longer needed
  })
  
  
  # Calculate results when "Calculate" button is clicked
  observeEvent(input$calculate, {
    # Show spinner after pressing "Calculate"
    showModal(modalDialog("Calculating, please wait...", footer = NULL, easyClose = FALSE))
    
    endyr <- 2022
    countries <- selected_countries()
    
    # If no countries selected, just close the modal and return
    if (length(countries) == 0) {
      removeModal()
      return()
    }
    
    current_results <- results()  # Get current table
    
    # Find countries that haven't been calculated for the selected end year
    new_countries <- setdiff(countries, current_results %>% filter(End.Year == endyr) %>% pull(Country))
    
    # Perform calculation only for new countries for the selected end year
    new_results <- lapply(new_countries, function(country_code) {
      selected_polygon <- world %>% filter(iso_a3 == country_code)
      selected_polygon_vect <- vect(selected_polygon)
      
      # Load demand data
      data_wf <- read_csv(paste0(demandpath,"demand_bau1_v2.csv"))
      demand_sum <- data_wf %>%
        filter(iso3 == country_code, year >= 2010, year <= 2022,
               (fuel == "biomass" & area %in% c("Rural", "Urban")) | 
                 (fuel == "charcoal" & area %in% c("Rural", "Urban"))) %>%
        summarise(total_value = sum(fuel_tons3, na.rm = TRUE)) %>%
        pull(total_value) %>%
        round(., 0)
      
      ### -- CTREES --
      agb2010CO2 <- rast(paste0(agbpath,"ctrees_global_2007_AGC.tif"))
      agb2022CO2 <- rast(paste0(agbpath,"Pantropical_AGC_ctrees/ctrees_global_2022_AGC_pantropic_1km_MgC02_ha.tif"))
      agb2010_ctrees <- agb2010CO2 * 12/44 / 0.47
      agb2022_ctrees <- agb2022CO2 * 12/44 / 0.47
      
      agb2010_ctrees <- mask(crop(agb2010_ctrees, selected_polygon_vect), selected_polygon_vect)
      agb2022_ctrees <- mask(crop(agb2022_ctrees, selected_polygon_vect), selected_polygon_vect)
      
      agb2010_ctrees_area <- agb2010_ctrees * cellSize(agb2010_ctrees, unit = "m") / 10000
      agb2022_ctrees_area <- agb2022_ctrees * cellSize(agb2022_ctrees, unit = "m") / 10000
      agbloss_ctrees <- agb2010_ctrees_area - agb2022_ctrees_area
      agbloss_ctrees[agbloss_ctrees <= 0] <- NA
      agbloss_ctrees_sum <- round(global(agbloss_ctrees, "sum", na.rm = TRUE)[1, 1], 0)
      
      fNRB_ctrees <- round(agbloss_ctrees_sum / demand_sum * 100, 0)
      
      ### -- ESA --
      agb2010_esa <- rast(paste0(agbpath,"esa_1km_global/agbd_2010_1000m.tif"))
      agb2022_esa <- rast(paste0(agbpath,"esa_1km_global/agbd_2022_1000m.tif"))
      
      agb2010_esa <- mask(crop(agb2010_esa, selected_polygon_vect), selected_polygon_vect)
      agb2022_esa <- mask(crop(agb2022_esa, selected_polygon_vect), selected_polygon_vect)
      
      agb2010_esa_area <- agb2010_esa * cellSize(agb2010_esa, unit = "m") / 10000
      agb2022_esa_area <- agb2022_esa * cellSize(agb2022_esa, unit = "m") / 10000
      agbloss_esa <- agb2010_esa_area - agb2022_esa_area
      agbloss_esa[agbloss_esa <= 0] <- NA
      agbloss_esa_sum <- round(global(agbloss_esa, "sum", na.rm = TRUE)[1, 1], 0)
      
      fNRB_esa <- round(agbloss_esa_sum / demand_sum * 100, 0)
      
      return(rbind(
        data.frame(Country = country_code, Source = "Ctrees", Start.Year = 2010, End.Year = 2022,
                   `Demand.Mg.period` = demand_sum, `AGB.losses.Mg.period` = agbloss_ctrees_sum,
                   `fNRB.%  ` = fNRB_ctrees),
        data.frame(Country = country_code, Source = "ESA", Start.Year = 2010, End.Year = 2022,
                   `Demand.Mg.period` = demand_sum, `AGB.losses.Mg.period` = agbloss_esa_sum,
                   `fNRB.%  ` = fNRB_esa)
      ))
      
    })
    
    # If any new results, update the table
    if (length(new_results) > 0) {
      new_results_df <- do.call(rbind, new_results)
      updated_results <- rbind(current_results, new_results_df)
      results(updated_results)
    }
    
    # Close modal and update the table
    removeModal()
    output$results_table <- renderTable({
      format(results(), big.mark = ",", digits = NULL)  # Display integers with thousand separators
    })
  })
})

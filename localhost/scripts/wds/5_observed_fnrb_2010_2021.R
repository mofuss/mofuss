# Run the shiny app in a background R process
setwd("C:/Users/aghil/Documents/wf_demand_shinny")

library("shiny")
library("ggplot2")
library("dplyr")
library("readr")
library("leaflet")
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")
library("terra")

# In the UI section, replace plotOutput with tableOutput
ui <- fluidPage(
  tags$style(HTML("
    body {
      background-color: #2E2E2E;
      color: #FFFFFF;
    }
    .well {
      background-color: #3E3E3E;
      border: none;
    }
    .btn {
      background-color: #4B4B4B;
      color: #FFFFFF;
    }
    h1, h2, h3, h4, h5 {
      color: #FFFFFF;
    }
    .sidebarPanel {
      background-color: #3E3E3E;
    }
    .mainPanel {
      background-color: #2E2E2E;
    }
  ")),
  
  titlePanel("Select up to 3 countries by clicking on the map - just click once and wait for 5-10 seconds"),
  
  sidebarLayout(
    sidebarPanel(
      textOutput("selected_countries_text"),
      actionButton("clear_button", "Clear Selection"),
      # actionButton("same_yscale_button", "Same Y-scale"),  # New button
      # Additional UI elements as needed
    ),
    mainPanel(
      leafletOutput("worldMap"),
      tableOutput("fuelTable")  # Replace plot with a table
    )
  )
)


server <- function(input, output, session) {
  crop_by_country <- function(raster, selected_countries, world) {
    selected_boundaries <- world %>%
      filter(adm0_a3 %in% selected_countries)
    
    if (nrow(selected_boundaries) == 0) {
      stop("No valid boundaries found for the selected countries.")
    }
    
    selected_boundaries_vect <- vect(selected_boundaries)
    cropped_raster <- crop(raster, selected_boundaries_vect)
    masked_raster <- mask(cropped_raster, selected_boundaries_vect)
    
    return(cropped_raster)
  }
  
  file_path <- "cons_fuels_years.csv"
  data <- read_csv(file_path)
  world <- ne_countries(scale = "medium", returnclass = "sf")
  
  selected_countries <- reactiveVal(character(0))
  # same_yscale <- reactiveVal(FALSE)
  
  output$worldMap <- renderLeaflet({
    leaflet(world) %>%
      addTiles() %>%
      setView(lng = 0, lat = 20, zoom = 2) %>%
      addPolygons(layerId = ~adm0_a3, fillColor = "white", fillOpacity = 0.7, 
                  color = "black", weight = 1,
                  highlight = highlightOptions(weight = 2, color = "#666", fillOpacity = 0.7),
                  label = ~adm0_a3)
  })
  
  observeEvent(input$worldMap_shape_click, {
    clicked_country <- input$worldMap_shape_click$id
    current_selection <- selected_countries()
    proxy <- leafletProxy("worldMap")	
    proxy %>% removeShape(layerId = clicked_country)
    polygon <- world %>% filter(adm0_a3 == clicked_country)
    fillColorT <- if (clicked_country %in% current_selection) "white" else "blue"
    proxy %>% addPolygons(data = polygon, fillColor = fillColorT, fillOpacity = 0.7, 
                          color = "black", weight = 1, label = polygon$adm0_a3,
                          layerId = polygon$adm0_a3)
    
    new_selection <- if (clicked_country %in% current_selection) {
      setdiff(current_selection, clicked_country)
    } else {
      c(current_selection, clicked_country)
    }
    selected_countries(new_selection)
    
    output$selected_countries_text <- renderText({
      paste("Selected countries:", paste(new_selection, collapse = ", "))
    })
  })
  
  observeEvent(input$clear_button, {
    selected_countries(character(0))
    leafletProxy("worldMap") %>%
      clearShapes() %>%
      addPolygons(data = world, layerId = ~iso_a3, fillColor = "white", 
                  fillOpacity = 0.7, color = "black", weight = 1,
                  label = ~name)
    output$selected_countries_text <- renderText({"Selected countries: None"})
  })
  
  # Reactive expression to generate the plot data and computed values
  filtered_data <- reactive({
    req(selected_countries())
    
    if (length(selected_countries()) < 1 | length(selected_countries()) > 3) {
      showNotification("Please select up to 3 countries.", type = "error")
      return(NULL)
    }
    
    # Create an empty list to store the results
    results_list <- list()
    
    # Loop through each selected country and calculate values individually
    for (country55 in selected_countries()) {

      # Filter the data for the current country
      country_data <- data %>%
        filter(iso3 %in% country55,
               year >= 2010, year <= 2021,
               (fuel == "Biomass" & area %in% c("Rural", "Urban")) |
                 (fuel == "Charcoal" & area %in% c("Rural", "Urban")))
      
      # Debugging step: Print the filtered data for the current country
      print(paste("Debug: Data for", country55))
      print(country_data)  # Check if the filtered data is as expected

      # Check if the column 'fuel_tons3' exists and calculate summed_value
      if (nrow(country_data) > 0 && "fuel_tons3" %in% colnames(country_data)) {
        summed_value <- country_data %>%
          summarise(total_value = sum(fuel_tons3, na.rm = TRUE)) %>%
          pull(total_value) %>%
          round(.,0)
        print(paste("Summed value for", country55, ":", summed_value))  # Debugging step
      } else {
        summed_value <- 0
        print(paste("Warning: No fuel_tons3 data found for", country55))
      }
      
      # Step 2: Load and crop the rasters for the current country
      agb2010 <- rast("esacciagb_2010_2020_pcs_1km/out_gcs_2010/world_agb_2010_c.tif")
      agb2021 <- rast("esacciagb_2010_2020_pcs_1km/out_gcs_2021/world_agb_2021_c.tif")
      
      agb2010_cropped <- crop_by_country(agb2010, country55, world)
      agb2021_cropped <- crop_by_country(agb2021, country55, world)
      
      # Ensure the cropped rasters are valid
      if (is.null(agb2010_cropped) || is.null(agb2021_cropped)) {
        stop("Cropped rasters are NULL.")
      }
      
      # Compute agblosses10_21 for the current country
      agb2010_cropped <- agb2010_cropped * 100
      agb2021_cropped <- agb2021_cropped * 100
      agblosses10_21 <- agb2010_cropped - agb2021_cropped
      agblosses10_21[agblosses10_21 < 0] <- NA
      
      # Sum all positive pixel values and extract the numeric value from the data frame
      total_agblosses10_21_df <- global(agblosses10_21, "sum", na.rm = TRUE)
      total_agblosses10_21 <- round(total_agblosses10_21_df[1, 1],0)  # Extract the first value
      
      # Store the results in the list for each country
      results_list[[country55]] <- list(
        Country = country55,
        SummedValue = summed_value,  # Make sure SummedValue is assigned
        AGBLosses = total_agblosses10_21
      )
    }
    
    # Convert the list of results to a data frame
    results_df <- do.call(rbind, lapply(results_list, as.data.frame))
    
    # Debugging step - print out results_df to confirm the structure
    print("Debug: Results data frame")
    print(results_df)
    
    return(results_df)
  })
  
  # Render the table with calculated values
  output$fuelTable <- renderTable({
    results_df <- filtered_data()
    req(results_df)  # Ensure the data is valid
    
    # Calculate fNRB as (AGBLosses / SummedValue) * 100
    results_df$fNRB <- with(results_df, ifelse(SummedValue == 0, 0, 
                                               pmin(100, round((AGBLosses / SummedValue) * 100))))
    
    
    # Return the data frame as a table
    results_df
  })
  
  session$onSessionEnded(function() {
    stopApp()
  })
}

shiny::runApp(list(ui = ui, server = server), port = 6134, launch.browser = TRUE)


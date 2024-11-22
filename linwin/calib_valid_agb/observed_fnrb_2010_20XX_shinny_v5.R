library(shiny)
library(sf)
library(terra)
library(dplyr)
library(leaflet)
library(readr)
library(rnaturalearth)
library(rnaturalearthdata)
library(shinythemes)
library(shinycssloaders)

# Define the UI
ui <- fluidPage(
  theme = shinytheme("cyborg"),  # Use a dark theme
  tags$style(HTML("
    .prototype-text {
      color: lightgray;
    }
    .email-text {
      color: yellow;
    }
    .instruction-text {
      color: lightgray;
    }
    .highlight-text {
      color: white;  /* Change this to any color you prefer for 'Clear Selection' */
      font-weight: bold;  /* Optional: makes it bold for more emphasis */
    }
  ")),  # Custom CSS for the paragraphs and email highlight
  titlePanel("Aboveground Biomass Losses vs Woodfuel Demand"),
  sidebarLayout(
    sidebarPanel(
      selectInput("endyr", "Start Year is 2010. Select End Year:", choices = c(2017, 2020, 2022)),
      actionButton("calculate", "Calculate"),
      actionButton("clear_selection", "Clear Selection"),  # Add clear selection button
      tags$hr(),
      p("This tool is a prototype, it is slow and could be buggy.
        For an extended description on what the tool does and how to interpret its results, follow this link."),
      p(HTML("Send questions and suggestions to <span class='email-text'>mofussfreeware@gmail.com</span>. 
        A proper tool with similar functionalities is being built as part of MoFuSS-US.")),
      h4("Instructions"),
      p(class = "instruction-text", "1.- Select end year of analysis."),
      p(class = "instruction-text", "2.- Select up to 16 countries, by clicking or tapping on the map, and waiting 1-2 seconds in each case for the selected country to be highlighted."),
      p(class = "instruction-text", HTML("3.- Press <span class='highlight-text'>Calculate</span> and wait for the results table to appear. Depending on the number of countries and their area,
        it can take up to 5-7 minutes. Do not close your browser.")),
      p(class = "instruction-text", HTML("4.- Press <span class='highlight-text'>Clear Selection</span> to start from scratch.")),
      p(class = "instruction-text", HTML("5.- You can add countries to the list or change the end year and recalculate, 
      but you can't deselect countries from the list (bug), just press <span class='highlight-text'>Clear Selection</span> and start again.")),
      fluidRow(
        column(6, img(src = "ctrees.png", height = "100px")),
        column(6, img(src = "mofuss.png", height = "100px"))
      ),
      p(style = 'margin-top: 20px; font-size: 14px; color: #FFFFFF;', 
        HTML("<a href='https://www.mofuss-sandbox.unam.mx' style='color: #12b974;' target='_blank'>MoFuSS main webpage</a>")
      )  # Inserted link below the logos
    ),
    mainPanel(
      leafletOutput("world_map"),  # Map for interactive country selection
      tableOutput("results_table") 
    )
  )
)

server <- function(input, output, session) {
  # Load country polygons
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
    period(paste0("2010-", input$endyr))  # Store the period as "2010-endyr"
  })
  
  # Calculate results when "Calculate" button is clicked
  observeEvent(input$calculate, {
    # Show spinner after pressing "Calculate"
    showModal(modalDialog("Calculating, please wait...", footer = NULL, easyClose = FALSE))
    
    endyr <- input$endyr
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
      
      # Load and crop rasters
      agb2010CO2 <- rast("E:/agb3rdparties/Pantropical_AGC/ctrees_global_2010_AGC_pantropic_1km_MgC02_ha.tif")
      agb20XXCO2 <- rast(paste0("E:/agb3rdparties/Pantropical_AGC/ctrees_global_", endyr, "_AGC_pantropic_1km_MgC02_ha.tif"))
      agb2010 <- agb2010CO2 * 12/44 / 0.47
      agb20XX <- agb20XXCO2 * 12/44 / 0.47
      
      agb2010_cropped <- terra::crop(agb2010, selected_polygon_vect)
      agb20XX_cropped <- terra::crop(agb20XX, selected_polygon_vect)
      
      agb2010_masked <- terra::mask(agb2010_cropped, selected_polygon_vect)
      agb20XX_masked <- terra::mask(agb20XX_cropped, selected_polygon_vect)
      
      agb2010_masked_pixel_area_ha <- cellSize(agb2010_masked, unit = "m") / 10000
      agb20XX_masked_pixel_area_ha <- cellSize(agb20XX_masked, unit = "m") / 10000
      
      agb2010_masked2 <- agb2010_masked * agb2010_masked_pixel_area_ha
      agb20XX_masked2 <- agb20XX_masked * agb20XX_masked_pixel_area_ha
      
      agblosses10_XX <- agb2010_masked2 - agb20XX_masked2
      agblosses10_XX[agblosses10_XX <= 0] <- NA
      
      total_agblosses10_XX_df <- global(agblosses10_XX, "sum", na.rm = TRUE)
      total_agblosses10_XX <- round(total_agblosses10_XX_df[1, 1], 0)
      
      # Load demand data
      data_wf <- read_csv("D:/demand/demand_in/cons_fuels_years.csv")
      demand_sum <- data_wf %>%
        filter(iso3 == country_code, year >= 2010, year <= endyr,
               (fuel == "Biomass" & area %in% c("Rural", "Urban")) | 
                 (fuel == "Charcoal" & area %in% c("Rural", "Urban"))) %>%
        summarise(total_value = sum(fuel_tons3, na.rm = TRUE)) %>%
        pull(total_value) %>%
        round(., 0)
      
      fNRB_obs <- round(total_agblosses10_XX / demand_sum * 100, 0)
      
      return(data.frame(
        Country = country_code,
        Start.Year = "2010",  # Add the Start.Year column as 2010
        End.Year = endyr,
        "Demand.Mg.period" = demand_sum,
        "AGB.losses.Mg.period" = total_agblosses10_XX,
        "fNRB.%  " = fNRB_obs
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
}



# Open the app in the browser
options(shiny.launch.browser = TRUE)

# Run the application
shinyApp(ui = ui, server = server)

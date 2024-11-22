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

# Define the UI for the shiny app
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
  
  titlePanel("Select between one and eight countries by clicking on the map"),
  sidebarLayout(
    sidebarPanel(
      textOutput("selected_countries_text"),
      actionButton("clear_button", "Clear Selection"),
      actionButton("same_yscale_button", "Same Y-scale"),  # New button
      
      # Explanatory text
      HTML("
<p style='margin-top: 20px; font-size: 20px; color: #FFFFFF;'>Woodfuel Demand Scenarios 2050</p>

<p style='margin-top: 20px; font-size: 14px; color: #FFFFFF;'>
This tool is designed to explore country-specific temporal trajectories of woodfuel demand from 2000 to 2050, 
broken down by fuel type (fuelwood and charcoal) and area type (rural and urban). 
The data used is ingested by MoFuSS at an early stage and represents pre-built scenarios, 
not the result of geoprocessing, but informed in part by the literature cited below.
</p>

<p style='margin-top: 20px; margin-bottom: 40px; font-size: 14px; color: #FFFFFF;'>
Use the 'Same Y-scale' button on and off, to compare absolute magnitudes across countries, 
or to explore each country's trends in greater detail.</p>

<p style='margin-top: 20px; font-size: 20px; color: #FFD700;'>Key References</p>

<p style='margin-top: 10px; font-size: 14px; color: #FFFFFF;'>
    <a href='https://www.nature.com/articles/s41467-021-26036-x' style='color: #FFD700;' target='_blank'>1.- Household cooking fuel estimates at global and country level for 1990 to 2030</a>
</p>

<p style='margin-top: 10px; font-size: 14px; color: #FFFFFF;'>
    <a href='https://www.who.int/data/gho/data/themes/air-pollution' style='color: #FFD700;' target='_blank'>2.- WHO Household air pollution data portal</a>
</p>

<p style='margin-top: 10px; font-size: 14px; color: #FFFFFF;'>
    <a href='https://cdm.unfccc.int/Sunset_CMS_ControlledSlots/public_inputs/sunsetcms/storage/contents/stored-file-20240624161613578/Report_on_Updated_fNRB_Values_20%20June%202024.pdf' style='color: #FFD700;' target='_blank'>3.- Updated fNRB Values for Woodfuel
Interventions</a>
</p>

<p style='margin-top: 10px; margin-bottom: 40px; font-size: 14px; color: #FFFFFF;'>
    <a href='https://www.nature.com/articles/s41893-022-01039-8' style='color: #FFD700;' target='_blank'>4.- A geospatial approach to understanding clean cooking challenges in sub-Saharan Africa</a>.
</p>

<p style='margin-top: 20px; font-size: 14px; color: #FFFFFF;'>
    <a href='https://www.mofuss.unam.mx' style='color: #12b974;' target='_blank'>MoFuSS main webpage</a>
</p>

")
    ),
    mainPanel(
      leafletOutput("worldMap"),
      plotOutput("fuelPlot")
    )
  )
)

# Define the server logic for the shiny app
server <- function(input, output, session) {
  # Load the data from the CSV file
  file_path <- "cons_fuels_years.csv"
  data <- read_csv(file_path)

  # Load the lighter world map
  world <- ne_countries(scale = "medium", returnclass = "sf")
  
  # Initialize selected countries list
  selected_countries <- reactiveVal(character(0))
  
  # Initialize same Y-scale setting
  same_yscale <- reactiveVal(FALSE)
  
  # Render the world map
  output$worldMap <- renderLeaflet({
    leaflet(world) %>%
      addTiles() %>%
      setView(lng = 0, lat = 20, zoom = 2) %>%
      addPolygons(layerId = ~adm0_a3,
                  fillColor = "white", 
                  fillOpacity = 0.7, 
                  color = "black", 
                  weight = 1,
                  highlight = highlightOptions(weight = 2, color = "#666", fillOpacity = 0.7),
                  label = ~adm0_a3)
  })
  
  # Observe map clicks and update selected countries
  observeEvent(input$worldMap_shape_click, {
    
    if(is.null(input$worldMap_shape_click))
      return() 
    
    clicked_country <- input$worldMap_shape_click$id
    current_selection <- selected_countries()
    
    proxy <- leafletProxy("worldMap")	
    proxy %>% removeShape(layerId = clicked_country)
    polygon <- world %>% filter(adm0_a3==clicked_country)
    fillColorT <- "white"
    if (clicked_country %in% current_selection) {
      new_selection <- setdiff(current_selection, clicked_country)
    } else {
      new_selection <- c(current_selection, clicked_country)
      fillColorT <- "blue"
    }
    
    proxy %>% addPolygons(data = polygon, 
                          fillColor = fillColorT,
                          fillOpacity = 0.7, 
                          color = "black", 
                          weight = 1,
                          highlight = highlightOptions(weight = 2, color = "#666", fillOpacity = 0.7),
                          label = polygon$adm0_a3,
                          layerId = polygon$adm0_a3)
    
    selected_countries(new_selection)
    
    # Display selected countries
    output$selected_countries_text <- renderText({
      paste("Selected countries:", paste(new_selection, collapse = ", "))
    })
  })
  
  # Observe "Clear Selection" button click
  observeEvent(input$clear_button, {
    selected_countries(character(0))
    
    leafletProxy("worldMap") %>%
      clearShapes() %>%
      addPolygons(data = world, 
                  layerId = ~iso_a3, 
                  fillColor = "white", 
                  fillOpacity = 0.7, 
                  color = "black", 
                  weight = 1,
                  highlight = highlightOptions(weight = 2, color = "#666", fillOpacity = 0.7),
                  label = ~name)
    
    output$selected_countries_text <- renderText({
      "Selected countries: None"
    })
  })
  
  # Observe "Same Y-scale" button click
  observeEvent(input$same_yscale_button, {
    same_yscale(!same_yscale())  # Toggle the Y-scale setting
  })
  
  # Reactive expression to generate the plot whenever selection changes
  filtered_data <- reactive({
    req(selected_countries())
    if (length(selected_countries()) < 1 || length(selected_countries()) > 8) {
      showNotification("Please select between one and eight countries.", type = "error")
      return(NULL)
    }
    
    data %>%
      filter(iso3 %in% selected_countries(),
             year >= 2000, year <= 2050,
             (fuel == "Biomass" & area %in% c("Rural", "Urban")) |
               (fuel == "Charcoal" & area %in% c("Rural", "Urban")))
  })
  
  # Render the plot automatically based on the filtered data
  output$fuelPlot <- renderPlot({
    plot_data <- filtered_data()
    
    req(plot_data)
    
    # Calculate the maximum y-value if same Y-scale is selected
    if (same_yscale()) {
      y_max <- max(plot_data$fuel_tons3, na.rm = TRUE)
      ggplot(plot_data, aes(x = year, y = fuel_tons3, color = interaction(fuel, area))) +
        geom_line(linewidth = 1) +
        labs(title = "Temporal Trajectories of Woodfuel Demand (2000-2050)",
             x = "Year",
             y = "Tonnes of air-dried wood equivalent per year",
             color = "Legend:") +
        theme_minimal() +
        theme(
          plot.background = element_rect(fill = "lightgrey", color = NA),
          panel.background = element_rect(fill = "lightgrey", color = NA),
          plot.title = element_text(hjust = 0.5),
          legend.position = "bottom"
        ) +
        facet_wrap(~country, ncol = ifelse(length(selected_countries()) > 4, 4, length(selected_countries())), 
                   scales = "fixed") +  # Apply fixed Y-scale
        coord_cartesian(ylim = c(0, y_max))  # Apply the same Y-scale
    } else {
      ggplot(plot_data, aes(x = year, y = fuel_tons3, color = interaction(fuel, area))) +
        geom_line(linewidth = 1) +
        labs(title = "Temporal Trajectories of Woodfuel Demand (2000-2050)",
             x = "Year",
             y = "Tonnes of air-dried wood equivalent per year",
             color = "Legend:") +
        theme_minimal() +
        theme(
          plot.background = element_rect(fill = "lightgrey", color = NA),
          panel.background = element_rect(fill = "lightgrey", color = NA),
          plot.title = element_text(hjust = 0.5),
          legend.position = "bottom"
        ) +
        facet_wrap(~country, ncol = ifelse(length(selected_countries()) > 4, 4, length(selected_countries())), 
                   scales = "free_y")  # Each plot has its own Y-scale
    }
  })
  
  # Automatically stop the app when the browser session ends
  session$onSessionEnded(function() {
    stopApp()
  })
}

# Run the shiny app on a fixed port (e.g., 6134) and open the browser
shiny::runApp(list(ui = ui, server = server), port = 6134, launch.browser = TRUE)

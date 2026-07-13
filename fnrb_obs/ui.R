# MoFuSS
# Version 2
# Date: Jul 2026

# 2dolist ----

# Internal parameters ----

# Load packages ----
library(shiny)
library(leaflet)
library(shinythemes)
library(shinycssloaders)


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
      selectInput("endyr", "Start Year is 2010. Select End Year:", choices = c(2011:2025)),
      actionButton("calculate", "Calculate"),
      actionButton("clear_selection", "Clear Selection"),  # Add clear selection button
      tags$hr(),
      p(
        "This tool compares gross aboveground biomass (AGB) losses between two years, 
  regardless of the driver of change. These losses may therefore include deforestation, 
  natural and human-induced fires, logging, agricultural expansion, woodfuel harvesting, 
  and other disturbances. Woodfuel-related biomass losses are included within this total 
  rather than identified separately. The tool then estimates the fraction of non-renewable 
  biomass (fNRB) by comparing total gross AGB losses with total baseline woodfuel demand 
  over the same period. Because only a fraction of all AGB losses are caused by woodfuel 
  harvesting, the resulting estimate should be interpreted as an upper threshold: ",
        
        span(
          "the true fNRB must be substantially lower.",
          style = "color: white;"
        )
      ),
      p(HTML("Send questions and suggestions to <span class='email-text'>mofussfreeware@gmail.com</span>.")),
      h4("Instructions"),
      p(class = "instruction-text", "1.- Select end year of analysis."),
      p(class = "instruction-text", "2.- Select up to 16 countries, by clicking or tapping on the map, and waiting 1-2 seconds in each case for the selected country to be highlighted."),
      p(class = "instruction-text", HTML("3.- Press <span class='highlight-text'>Calculate</span> and wait for the results table to appear. Depending on the number of countries and their area,
        it can take up to 5-7 minutes. Do not close your browser.")),
      p(class = "instruction-text", HTML("4.- Press <span class='highlight-text'>Clear Selection</span> to start from scratch.")),
      p(class = "instruction-text", HTML("5.- You can add or remove countries to the list or change the end year and recalculate.")),
      fluidRow(
        column(6, img(src = "ctrees.png", height = "100px")),
        column(6, img(src = "mofuss.png", height = "100px"))
      ),
      p(style = 'margin-top: 20px; font-size: 14px; color: #FFFFFF;',
        HTML("<a href='https://www.mofuss.unam.mx' style='color: #12b974;' target='_blank'>MoFuSS main webpage</a>")
      )  # Inserted link below the logos
    ),
    mainPanel(
      leafletOutput("world_map"),  # Map for interactive country selection
      tableOutput("results_table")
    )
  )
)

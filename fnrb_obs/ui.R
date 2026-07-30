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
  titlePanel("Observed Gross and Net AGB Losses vs Woodfuel Demand"),
  sidebarLayout(
    sidebarPanel(
      selectInput("endyr", "Start Year is 2010. Select End Year:", choices = c(2011:2025)),
      actionButton("calculate", "Calculate"),
      actionButton("clear_selection", "Clear Selection"),  # Add clear selection button
      tags$hr(),
      p(
        "This tool compares observed aboveground biomass (AGB) in 2010 with a selected end year. ",
        strong("Gross NRB"),
        " is the sum of losses in pixels whose endpoint AGB decreased; gains elsewhere do not offset it. ",
        strong("Net NRB"),
        " is the country-level endpoint loss after gains offset losses, and is set to zero when the country has a net AGB gain. ",
        "Gross and net fNRB are calculated by dividing the corresponding NRB value by cumulative baseline rural/urban fuelwood-plus-charcoal demand from 2010 through the selected end year (inclusive). ",
        "Observed AGB losses include all drivers, such as deforestation, fires, logging, agricultural expansion, and woodfuel harvesting; they do not attribute losses specifically to woodfuel. Therefore these observed fNRB ratios should be interpreted as diagnostic upper thresholds: ",
        
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

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

shinyUI(fluidPage(
					theme = shinytheme("cyborg"),
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
					    color: white;
					    font-weight: bold;
					  }
					")),
			        
			        titlePanel("Woodfuel Demand Scenarios 2050"),
			        sidebarLayout(
					          sidebarPanel(
							             textOutput("selected_countries_text"),
							             tags$label(
							               class = "control-label",
							               "Select between one and eight countries by clicking on the map"
							             ),
								           actionButton("clear_button", "Clear Selection"),
								           actionButton("same_yscale_button", "Same Y-scale"),  # New button
								           tags$hr(),
									         
									         # Explanatory text
									         HTML("
									      <p style='margin-top: 20px;'>
										      This tool is designed to explore country-specific temporal trajectories of woodfuel demand from 2000 to 2050, 
										      broken down by fuel type (fuelwood and charcoal) and area type (rural and urban). 
										      The data used is ingested by MoFuSS at an early stage and represents pre-built scenarios, 
										      not the result of geoprocessing, but informed in part by the literature cited below.
										      </p>

									      <p>Send questions and suggestions to <span class='email-text'>mofussfreeware@gmail.com</span>.</p>

									      <h4>Instructions</h4>
									      <p class='instruction-text'>Use the 'Same Y-scale' button on and off, to compare absolute magnitudes across countries, or to explore each country's trends in greater detail.</p>

									      <h4 style='margin-top: 20px;'>Key References</h4>

									      <p style='margin-top: 10px;'>
									          <a href='https://www.nature.com/articles/s41467-021-26036-x' style='color: #12b974;' target='_blank'>1.- Household cooking fuel estimates at global and country level for 1990 to 2030</a>
									      </p>

									      <p style='margin-top: 10px;'>
									          <a href='https://www.who.int/data/gho/data/themes/air-pollution' style='color: #12b974;' target='_blank'>2.- WHO Household air pollution data portal</a>
									      </p>

									      <p style='margin-top: 10px;'>
									          <a href='https://cdm.unfccc.int/Sunset_CMS_ControlledSlots/public_inputs/sunsetcms/storage/contents/stored-file-20240624161613578/Report_on_Updated_fNRB_Values_20%20June%202024.pdf' style='color: #12b974;' target='_blank'>3.- Updated fNRB Values for Woodfuel
									      Interventions</a>
									      </p>

									      <p style='margin-top: 10px; margin-bottom: 40px;'>
									          <a href='https://www.nature.com/articles/s41893-022-01039-8' style='color: #12b974;' target='_blank'>4.- A geospatial approach to understanding clean cooking challenges in sub-Saharan Africa</a>.
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
		  ))

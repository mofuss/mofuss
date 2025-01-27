library(shiny)
library(leaflet)

shinyUI(fluidPage(
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
		  ))

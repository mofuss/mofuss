library(shiny)
library(leaflet)
library(shinythemes)
library(shinycssloaders)

# Define the UI
shinyUI(fluidPage(
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
																							       ))

#install.packages("devtools") # I guess you also need this
#devtools::install_github("ropensci/rnaturalearthhires")

if (webmofuss == 1){
  setwd("/home/rrangel/common")
  demandpath = ""
} else if (webmofuss == 0){
  # ONLY WORKS IN NRBV1 NODE as localhost"
  demandpath = "D:/demand/demand_in/"
}

install_and_load <- function(packages) {
  for (package in packages) {
#    if (!require(package, character.only = TRUE)) {
#      install.packages(package, dependencies = TRUE)
      library(package, character.only = TRUE)
#    }
  }
}

required_packages <- c("shiny", "ggplot2", "dplyr", "readr", "leaflet", "sf", "rnaturalearth", "rnaturalearthdata")

install_and_load(required_packages)

shinyServer(function(input, output) {
# Load the data from the CSV file
		      file_path <- paste0(demandpath,"cons_fuels_years.csv")
		        proj_file_path <- paste0(demandpath,"cons_fuels_years_proj.csv")
		        data <- read_csv(file_path)
			  proj_data <- read_csv(proj_file_path)

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
					          
					          # Filter original data
					          data_filtered <- data %>%
							        filter(iso3 %in% selected_countries(),
								                    year >= 2000, year <= 2050,
										                 (fuel == "Biomass" & area %in% c("Rural", "Urban")) |
													                (fuel == "Charcoal" & area %in% c("Rural", "Urban"))) %>%
						        mutate(dataset = "Business-as-Usual")
						    
						    # Filter projected data
						    proj_data_filtered <- proj_data %>%
							          filter(iso3 %in% selected_countries(),
									              year >= 2000, year <= 2050,
										                   (fuel == "Biomass" & area %in% c("Rural", "Urban")) |
													                  (fuel == "Charcoal" & area %in% c("Rural", "Urban"))) %>%
						          mutate(dataset = "Intervention Scenario")
						      
						      # Combine both filtered datasets
						      combined_data <- bind_rows(data_filtered, proj_data_filtered)
						          combined_data
						        })
				  
				  # Render the plot automatically based on the filtered data
				  output$fuelPlot <- renderPlot({
					      plot_data <- filtered_data()
					          req(plot_data)
					          
					          # Define color palette for original and projected data
					          colors <- c("Biomass.Rural" = "darkgreen", "Biomass.Urban" = "darkorange",
							                      "Charcoal.Rural" = "darkred", "Charcoal.Urban" = "black")
						      
						      colors_proj <- c("Biomass.Rural" = "#90EE90",   # Light green
								                            "Biomass.Urban" = "#FFDAB9",   # Peachpuff (light orange)
											                         "Charcoal.Rural" = "#F08080",  # Light coral
											                         "Charcoal.Urban" = "#A9A9A9")  # Grey
						      
						      # Calculate the maximum y-value if same Y-scale is selected
						      if (same_yscale()) {
							            y_max <- max(plot_data$fuel_tons3, na.rm = TRUE)
						            ggplot(plot_data, aes(x = year, y = fuel_tons3, color = interaction(fuel, area), linetype = dataset)) +
								            geom_line(data = subset(plot_data, dataset == "Business-as-Usual"), linewidth = 1) +
									            geom_line(data = subset(plot_data, dataset == "Intervention Scenario"), linewidth = 0.8, alpha = 0.6) +
										            scale_color_manual(values = c(colors, colors_proj)) +
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
										                      scales = "fixed") +
								        coord_cartesian(ylim = c(0, y_max))
								    } else {
									          ggplot(plot_data, aes(x = year, y = fuel_tons3, color = interaction(fuel, area), linetype = dataset)) +
											          geom_line(data = subset(plot_data, dataset == "Business-as-Usual"), linewidth = 1) +
												          geom_line(data = subset(plot_data, dataset == "Intervention Scenario"), linewidth = 0.8, alpha = 0.6) +
													          scale_color_manual(values = c(colors, colors_proj)) +
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
										                      scales = "free_y")
									    }
						        })
})

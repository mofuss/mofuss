# MoFuSS
# Version 2
# Date: Jul 2026

# 2dolist ----

# Internal parameters ----
temdirdefined = 1
# options(shiny.launch.browser = TRUE)

webmofuss <- if (file.exists(".env")) 1 else 0

if (webmofuss == 1){
  setwd("/mnt/storage/apps/fnrb_obs_data/")
  rTempdir_fnrbobs = "/mnt/storage/apps/fnrb_obs_data/rTempdir_fnrbobs/"
  agbpath = "/mnt/storage/apps/fnrb_obs_data/1km_agco2_2000_2025/"
  demandpath = "/mnt/storage/apps/fnrb_obs_data/"
} else if (webmofuss == 0){
  # ONLY WORKS IN NRBV1 NODE as localhost"
  rTempdir_fnrbobs <- "C:/Users/aghil/Documents/MoFuSS_localhost/rTempdir_fnrbobs/"
  agbpath = "G:/Mi unidad/webpages/2026_MoFuSSGlobal_Datasets/fnrb_obs_data/1km_agco2_2000_2025/"
  demandpath = "G:/Mi unidad/webpages/2026_MoFuSSGlobal_Datasets/fnrb_obs_data/"
}

START_YEAR <- 2010L
CARBON_FRACTION <- 0.47
CO2_TO_DM <- (12 / 44) / CARBON_FRACTION
DEMAND_FUELS <- c("fuelwood", "charcoal")
DEMAND_AREAS <- c("rural", "urban")

# Load packages ----
library(terra)
# terraOptions(steps = 55)
if (temdirdefined == 1) {
  terraOptions(tempdir = rTempdir_fnrbobs)
  # List all files and directories inside the folder
  # contents <- list.files(rTempdir_fnrbobs, full.names = TRUE, recursive = TRUE)
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

# Load immutable data once and share it across Shiny sessions
world <- ne_countries(scale = "medium", returnclass = "sf")

# Initialize the results data frame with proper columns
initial_results <- data.frame(
  Country = character(),
  Start.Year = integer(),
  End.Year = integer(),
  "Demand (Mg, period)" = numeric(),
  "Gross NRB (Mg, period)" = numeric(),
  "Net NRB (Mg, period)" = numeric(),
  "Gross fNRB (%)" = numeric(),
  "Net fNRB (%)" = numeric(),
  stringsAsFactors = FALSE,
  check.names = FALSE
)

raster_sum <- function(r) {
  out <- as.numeric(terra::global(r, "sum", na.rm = TRUE)[1, 1])
  if (!is.finite(out)) stop("Raster sum is not finite; check endpoint data coverage.")
  out
}

calculate_nrb <- function(agb_start_mgha, agb_end_mgha, pixel_area_ha) {
  ## Signed endpoint difference in Mg/cell. Raster arithmetic automatically
  ## restricts both metrics to cells valid at BOTH endpoints.
  change_mg <- (agb_start_mgha - agb_end_mgha) * pixel_area_ha
  n_common <- as.numeric(terra::global(!is.na(change_mg), "sum", na.rm = TRUE)[1, 1])
  if (!is.finite(n_common) || n_common == 0) stop("No common valid AGB cells at the two endpoints.")

  gross_loss_mg <- raster_sum(terra::ifel(change_mg > 0, change_mg, 0))
  national_balance_mg <- raster_sum(change_mg) # positive = net loss; negative = net gain
  net_loss_mg <- max(0, national_balance_mg)

  list(gross_mg = gross_loss_mg,
       net_mg = net_loss_mg,
       balance_mg = national_balance_mg,
       common_cells = n_common)
}

extract_period_demand <- function(data_wf, country_code, endyr) {
  required <- c("iso3", "year", "fuel", "area", "fuel_cons_tons")
  missing <- setdiff(required, names(data_wf))
  if (length(missing)) stop("Demand table is missing required column(s): ", paste(missing, collapse = ", "))

  selected <- data_wf %>%
    dplyr::filter(iso3 == country_code,
                  year >= START_YEAR, year <= endyr,
                  fuel %in% DEMAND_FUELS,
                  area %in% DEMAND_AREAS)

  if (!nrow(selected)) stop("No baseline woodfuel-demand records for ", country_code,
                            " in ", START_YEAR, "-", endyr, ".")
  if (any(!is.finite(selected$fuel_cons_tons)) || any(selected$fuel_cons_tons < 0))
    stop("Demand contains missing, non-finite, or negative fuel_cons_tons values for ", country_code, ".")

  expected_years <- START_YEAR:endyr
  missing_years <- setdiff(expected_years, unique(selected$year))
  if (length(missing_years)) stop("Demand is missing year(s) for ", country_code, ": ",
                                  paste(missing_years, collapse = ", "))
  keys <- selected %>% dplyr::count(year, fuel, area, name = "n")
  if (any(keys$n != 1L) || nrow(keys) != length(expected_years) * length(DEMAND_FUELS) * length(DEMAND_AREAS))
    stop("Demand must contain exactly one rural/urban fuelwood/charcoal record per year for ", country_code, ".")

  ## Deliberately preserves the established inclusive extraction: 2010:endyr.
  sum(selected$fuel_cons_tons)
}

fnrb_percent <- function(nrb_mg, demand_mg) {
  if (!is.finite(demand_mg) || demand_mg <= 0) stop("Period demand must be finite and greater than zero.")
  100 * nrb_mg / demand_mg
}

format_results_table <- function(x) {
  mass_cols <- c("Demand (Mg, period)", "Gross NRB (Mg, period)", "Net NRB (Mg, period)")
  pct_cols <- c("Gross fNRB (%)", "Net fNRB (%)")
  x[mass_cols] <- lapply(x[mass_cols], function(v) formatC(round(v), format = "f", digits = 0, big.mark = ","))
  # Display fNRB as whole percentages without capping values above 100%.
  # Values above 100% indicate that observed AGB losses from all causes exceed
  # cumulative woodfuel demand. Reactive results retain full precision.
  x[pct_cols] <- lapply(x[pct_cols], function(v) {
    formatC(round(v), format = "f", digits = 0, big.mark = ",")
  })
  x
}

shinyServer(function(input, output, session) {

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

  # Keep the base map in the browser and redraw only the small selection layer
  update_country_highlights <- function(country_codes) {
    map_proxy <- leafletProxy("world_map") %>%
      clearGroup("selected_countries")

    if (length(country_codes) > 0) {
      map_proxy %>%
        addPolygons(
          data = world[world$iso_a3 %in% country_codes, ],
          group = "selected_countries",
          fillColor = "blue",
          fillOpacity = 0.5,
          weight = 2,
          color = "#0000FF",
          options = pathOptions(interactive = FALSE)
        )
    }
  }

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
        updated_results <- current_results %>% dplyr::filter(Country != country_code)
        results(updated_results)
      } else if (length(current_selection) < 16) {
        current_selection <- c(current_selection, country_code)
      }

      # Update selected countries
      selected_countries(current_selection)

      # Re-highlight selected countries on the map
      update_country_highlights(current_selection)
    }
  })

  # Clear selection button
  observeEvent(input$clear_selection, {
    selected_countries(character())  # Reset the country selection
    results(initial_results)  # Clear the results table
    period("")  # Clear the period
    update_country_highlights(character())
  })

  # Store the period when the end year changes
  observeEvent(input$endyr, {
    period(paste0(START_YEAR, "-", input$endyr))
  })

  # Calculate results when "Calculate" button is clicked
  observeEvent(input$calculate, {
    # Show spinner after pressing "Calculate"
    showModal(modalDialog("Calculating, please wait...", footer = NULL, easyClose = FALSE))
    on.exit(removeModal(), add = TRUE)

    endyr <- as.integer(input$endyr)
    if (!is.finite(endyr) || endyr < 2011L || endyr > 2025L)
      stop("End year must be between 2011 and 2025.")
    countries <- selected_countries()

    # If no countries selected, just close the modal and return
    if (length(countries) == 0) {
      return()
    }

    current_results <- results()  # Get current table

    # Find countries that haven't been calculated for the selected end year
    new_countries <- setdiff(countries, current_results %>% dplyr::filter(End.Year == endyr) %>% pull(Country))

    new_results <- list()

    if (length(new_countries) > 0) {
      # Open each shared input once per calculation, not once per country
      agb2010CO2 <- rast(paste0(agbpath, "ctrees_global_2010_AGC.tif"))
      agb20XXCO2 <- rast(paste0(agbpath, "ctrees_global_", endyr, "_AGC.tif"))
      agbCO2 <- c(agb2010CO2, agb20XXCO2)
      data_wf <- read_csv(paste0(demandpath, "demand_bau1_v2.csv"), show_col_types = FALSE)

      # Perform calculation only for new countries for the selected end year
      new_results <- lapply(new_countries, function(country_code) {
        selected_polygon <- world[which(world$iso_a3 == country_code), ]
        if (nrow(selected_polygon) != 1L) stop("Could not resolve a unique country polygon for: ", country_code)
        selected_polygon_vect <- vect(selected_polygon)
        if (!terra::same.crs(selected_polygon_vect, agbCO2))
          selected_polygon_vect <- terra::project(selected_polygon_vect, terra::crs(agbCO2))

        # Crop the source values before doing raster arithmetic. This preserves
        # the calculation but avoids processing two complete global rasters for
        # every selected country. Mask both aligned layers in a single operation.
        agb_cropped <- terra::crop(agbCO2, selected_polygon_vect)
        agb_masked <- terra::mask(agb_cropped, selected_polygon_vect)
        agb_masked[agb_masked < 0] <- NA

        agb2010_masked <- agb_masked[[1]] * CO2_TO_DM
        agb20XX_masked <- agb_masked[[2]] * CO2_TO_DM

        # CTrees is MgCO2/ha. cellSize returns cell-specific geodesic hectares.
        masked_pixel_area_ha <- terra::cellSize(agb2010_masked, unit = "ha")
        nrb <- calculate_nrb(agb2010_masked, agb20XX_masked, masked_pixel_area_ha)

        # Established demand extraction: inclusive 2010:endyr sum of rural and
        # urban fuelwood + charcoal, using fuel_cons_tons (Mg/year).
        demand_sum <- extract_period_demand(data_wf, country_code, endyr)

        gross_fnrb <- fnrb_percent(nrb$gross_mg, demand_sum)
        net_fnrb <- fnrb_percent(nrb$net_mg, demand_sum)

        data.frame(
          Country = country_code,
          Start.Year = START_YEAR,
          End.Year = endyr,
          "Demand (Mg, period)" = demand_sum,
          "Gross NRB (Mg, period)" = nrb$gross_mg,
          "Net NRB (Mg, period)" = nrb$net_mg,
          "Gross fNRB (%)" = gross_fnrb,
          "Net fNRB (%)" = net_fnrb,
          check.names = FALSE
        )
      })
    }

    # If any new results, update the table
    if (length(new_results) > 0) {
      new_results_df <- do.call(rbind, new_results)
      updated_results <- rbind(current_results, new_results_df)
      results(updated_results)
    }

  })

  output$results_table <- renderTable({
    format_results_table(results())
  })
})

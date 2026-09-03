# SPDX-License-Identifier: Apache-2.0
#
# Copyright 2025-2027 Universidad Nacional Autónoma de México
# and Stockholm Environment Institute
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# https://www.apache.org/licenses/LICENSE-2.0
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# MoFuSS ----
# Script: 2c_demand_tables_v5.R
# Version: 5
# Date: Sep 2026
# Execution: Source from RStudio; Dinamica EGO does not invoke this script directly.
#
# Purpose: Validate, transform, summarize and visualize scenario demand tables
# for Regional, Country and user-polygon analysis areas, and produce a global
# BaU1-versus-ICS3 woodfuel comparison from the complete demand tables.
# Inputs: parameters.csv, scenario demand CSV files and inherited workspace paths.
# Outputs: Processed demand tables, diagnostics and demand-summary figures.
# Side effects: Changes working directory, clears selected output folders and
# overwrites generated CSV and figure files.

# 2dolist ----
# Fix split version of the tables
# Turn off when AOI poly is on

# Internal parameters ----

# Load libraries ----
library(conflicted)

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(ggplot2)
  library(rlang); library(readr); library(scales); library(forcats)
})

# Print parameters table ----
country_parameters <- read_delim(parameters_file_path, delim = delimiter)
print(tibble::as_tibble(country_parameters), n=100)

country_parameters %>%
  dplyr::filter(Var == "region2BprocessedCtry_iso") %>%
  pull(ParCHR) -> region2BprocessedCtry_iso

country_parameters %>%
  dplyr::filter(Var == "subcountry") %>%
  pull(ParCHR) -> subcountry

country_parameters %>%
  dplyr::filter(Var == "scenario_ver") %>%
  pull(ParCHR) -> scenario_ver

country_parameters %>%
  dplyr::filter(Var == "demand_col") %>%
  pull(ParCHR) -> demand_col

country_parameters %>%
  dplyr::filter(Var == "demand_tuning") %>%
  pull(ParCHR) %>%
  as.integer(.) -> demand_tuning

country_parameters %>%
  dplyr::filter(Var == "efchratio") %>%
  pull(ParCHR) %>%
  as.integer(.) -> efchratio

country_parameters %>%
  dplyr::filter(Var == "start_year") %>%
  pull(ParCHR) %>%
  as.integer(.) -> start_year

country_parameters %>%
  dplyr::filter(Var == "end_year") %>%
  pull(ParCHR) %>%
  as.integer(.) -> end_year

country_parameters %>%
  dplyr::filter(Var == "aoi_poly") %>%
  pull(ParCHR) %>%
  as.integer(.) -> aoi_poly

country_parameters %>%
  dplyr::filter(Var == "byregion") %>%
  pull(ParCHR) -> byregion

parameter_chr <- function(parameter_name, required = TRUE) {
  values <- country_parameters %>%
    dplyr::filter(Var == parameter_name) %>%
    dplyr::pull(ParCHR) %>%
    as.character() %>%
    trimws()
  values <- unique(values[!is.na(values) & nzchar(values)])

  if (required && length(values) != 1L) {
    stop("parameters.csv must contain exactly one non-empty ", parameter_name, ".")
  }
  if (!required && length(values) == 0L) {
    return(NA_character_)
  }
  if (length(values) != 1L) {
    stop("parameters.csv contains multiple values for ", parameter_name, ".")
  }
  values[[1]]
}

region2BprocessedReg <- parameter_chr("region2BprocessedReg", required = FALSE)
aoi_poly_file <- parameter_chr("aoi_poly_file", required = FALSE)

if (length(start_year) != 1L || length(end_year) != 1L ||
    is.na(start_year) || is.na(end_year) || end_year < start_year) {
  stop("Invalid start_year/end_year in parameters.csv.")
}

demand_input_dir <- file.path(demanddir, "demand_in")
regions_file <- file.path(demand_input_dir, "mofuss_regions0.gpkg")

read_region_index <- function() {
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("Package 'sf' is required to select Regional or own-polygon demand diagnostics.")
  }
  if (!file.exists(regions_file)) {
    stop("Country-region index not found: ", regions_file)
  }
  region_index <- sf::st_read(regions_file, quiet = TRUE)
  required_columns <- c("GID_0", "NAME_0", "mofuss_reg")
  missing_columns <- setdiff(required_columns, names(region_index))
  if (length(missing_columns) > 0L) {
    stop(
      "Country-region index is missing column(s): ",
      paste(missing_columns, collapse = ", ")
    )
  }
  region_index
}

if (aoi_poly == 1L) {
  if (is.na(aoi_poly_file)) {
    stop("aoi_poly is 1, but aoi_poly_file is empty.")
  }

  region_index <- read_region_index()
  polygon_pattern <- file.path(
    countrydir,
    "LULCC/DownloadedDatasets/SourceDataGlobal/InVector_GCS",
    aoi_poly_file
  )
  polygon_files <- Sys.glob(polygon_pattern)
  if (length(polygon_files) != 1L) {
    stop(
      "Expected exactly one own-polygon file matching ", polygon_pattern,
      "; found ", length(polygon_files), "."
    )
  }

  polygon_aoi <- sf::st_read(polygon_files[[1]], quiet = TRUE)
  if (is.na(sf::st_crs(polygon_aoi)) || is.na(sf::st_crs(region_index))) {
    stop("The own polygon and country-region index must both have defined CRSs.")
  }
  # Use a projected global equal-area CRS for robust positive-area overlap
  # tests. This avoids S2 failures caused by invalid rings in legacy GADM
  # geometries and excludes countries that merely touch the polygon boundary.
  overlap_crs <- 6933
  region_index_overlap <- sf::st_make_valid(sf::st_transform(region_index, overlap_crs))
  polygon_aoi_overlap <- sf::st_make_valid(sf::st_transform(polygon_aoi, overlap_crs))
  polygon_union <- sf::st_union(polygon_aoi_overlap)
  candidate_rows <- lengths(sf::st_intersects(
    region_index_overlap,
    polygon_union
  )) > 0L
  overlap <- suppressWarnings(sf::st_intersection(
    region_index_overlap[candidate_rows, ],
    polygon_union
  ))
  overlap <- overlap[as.numeric(sf::st_area(overlap)) > 0, ]
  selected_iso3 <- sort(unique(as.character(overlap$GID_0)))
  analysis_area_kind <- "OwnPolygon"
  analysis_area_label <- paste0("Own polygon: ", tools::file_path_sans_ext(basename(polygon_files[[1]])))
} else if (identical(byregion, "Regional")) {
  if (is.na(region2BprocessedReg)) {
    stop("Regional diagnostics require region2BprocessedReg in parameters.csv.")
  }
  region_index <- read_region_index()
  selected_iso3 <- sort(unique(as.character(
    region_index$GID_0[region_index$mofuss_reg == region2BprocessedReg]
  )))
  analysis_area_kind <- "Regional"
  analysis_area_label <- region2BprocessedReg
} else if (identical(byregion, "Country")) {
  selected_iso3 <- region2BprocessedCtry_iso
  analysis_area_kind <- "Country"
  analysis_area_label <- region2BprocessedCtry_iso
} else {
  stop(
    "Unsupported analysis-area configuration. Use byregion='Regional', ",
    "byregion='Country', or set aoi_poly=1."
  )
}

selected_iso3 <- unique(selected_iso3[!is.na(selected_iso3) & nzchar(selected_iso3)])
if (length(selected_iso3) == 0L) {
  stop("The configured analysis area did not select any countries.")
}
analysis_area_slug <- gsub("[^A-Za-z0-9_-]+", "_", analysis_area_label)
analysis_area_slug <- gsub("^_+|_+$", "", analysis_area_slug)

setwd(demanddir)
    
    read_wfdb <- function(file) {
      if (!file.exists(file)) {
        stop("Demand table not found: ", file)
      }

      # Some personal ICS exports contain scenario metadata before the actual
      # demand-table header. Locate the real header and infer its delimiter
      # instead of assuming that the first line contains the column names.
      header_lines <- readLines(file, n = 100, warn = FALSE)
      required_columns <- unique(c(
        "iso3",
        "area",
        "fuel",
        "year",
        "num_fuel_users_thousands",
        demand_col
      ))
      header_row <- Inf
      delimiter <- NULL

      for (candidate_delimiter in c(",", ";")) {
        candidate_rows <- which(vapply(
          header_lines,
          function(header_line) {
            header_fields <- strsplit(
              sub("^\ufeff", "", header_line),
              candidate_delimiter,
              fixed = TRUE
            )[[1]]
            header_fields <- tolower(trimws(gsub(
              "\"",
              "",
              header_fields,
              fixed = TRUE
            )))
            all(tolower(required_columns) %in% header_fields)
          },
          logical(1)
        ))

        if (length(candidate_rows) > 0 && candidate_rows[[1]] < header_row) {
          header_row <- candidate_rows[[1]]
          delimiter <- candidate_delimiter
        }
      }

      if (is.null(delimiter)) {
        stop(
          "Could not find a valid demand-table header in ", file,
          ". Required columns: ", paste(required_columns, collapse = ", ")
        )
      }

      demand_table <- readr::read_delim(
        file,
        delim = delimiter,
        skip = header_row - 1,
        show_col_types = FALSE,
        progress = FALSE,
        trim_ws = TRUE
      )
      names(demand_table) <- trimws(sub("^\ufeff", "", names(demand_table)))

      missing_columns <- setdiff(required_columns, names(demand_table))
      if (length(missing_columns) > 0) {
        stop(
          "Demand table ", file, " is missing required column(s): ",
          paste(missing_columns, collapse = ", ")
        )
      }

      demand_table
    }
    
    # Define scenarios ----
    if (scenario_ver %in% c("BaU1_v2", "BaU2_v2", "BaU3_v2",
                            "ICS1_v2", "ICS2_v2", "ICS3_v2")) {
      
      wfdb <- read_wfdb(
        paste0(
          "demand_in/demand_",
          tolower(scenario_ver),
          ".csv"
        )
      )
      
    } else {
      
      stop(paste0("Invalid scenario_ver: ", scenario_ver))
      
    }

    # For local ICS runs (webmofuss == 0), load the BaU1 table used as the
    # common baseline by the global ICS constructor. Web-MoFuSS runs do not
    # have this BaU table, so their plots contain only the selected ICS scenario.
    # The CSV outputs remain selected-scenario-only in both cases.
    is_ics_scenario <- scenario_ver %in% c("ICS1_v2", "ICS2_v2", "ICS3_v2")
    include_bau_comparison <- is_ics_scenario && webmofuss == 0
    bau_comparison_ver <- "BaU1_v2"
    wfdb_bau <- NULL

    if (include_bau_comparison) {
      bau_comparison_path <- file.path(
        "demand_in",
        paste0("demand_", tolower(bau_comparison_ver), ".csv")
      )

      if (!file.exists(bau_comparison_path)) {
        stop(paste0(
          "ICS comparison requested, but the BaU file is missing: ",
          bau_comparison_path
        ))
      }

      wfdb_bau <- read_wfdb(bau_comparison_path)
    }

    scenario_levels <- if (include_bau_comparison) {
      c(bau_comparison_ver, scenario_ver)
    } else {
      scenario_ver
    }

    scenario_comparison_text <- if (include_bau_comparison) {
      paste0(bau_comparison_ver, " vs ", scenario_ver)
    } else {
      scenario_ver
    }

    comparison_plot_height <- if (include_bau_comparison) 11 else 7
    comparison_grid_height <- if (include_bau_comparison) 14 else 9
    
    unique(wfdb$fuel)
    head(wfdb)
    print(scenario_ver) # save as text to recover later down the river
    
    outdir <- "demand_atlas"
    full_path <- file.path(countrydir, outdir)
    unlink(file.path(full_path, "*"), recursive = TRUE)
    
    if (!dir.exists(full_path)) {
      dir.create(full_path, recursive = TRUE)
    }
    
    # Optional: verify
    stopifnot(dir.exists(full_path))

    missing_scope_iso3 <- setdiff(selected_iso3, unique(as.character(wfdb$iso3)))
    if (length(missing_scope_iso3) > 0L) {
      stop(
        "The selected scenario table is missing analysis-area country code(s): ",
        paste(missing_scope_iso3, collapse = ", ")
      )
    }

    scope_members <- wfdb %>%
      dplyr::filter(iso3 %in% selected_iso3) %>%
      dplyr::distinct(iso3, country) %>%
      dplyr::arrange(iso3) %>%
      dplyr::mutate(
        analysis_area_kind = analysis_area_kind,
        analysis_area_label = analysis_area_label,
        .before = 1
      )
    readr::write_csv(
      scope_members,
      file.path(full_path, paste0("demand_scope_", analysis_area_slug, ".csv"))
    )

    # Global BaU1-versus-ICS3 validation and graph ----
    # This always uses the complete country tables, independently of the
    # configured Regional, Country or own-polygon display scope.
    global_bau_path <- file.path(demand_input_dir, "demand_bau1_v2.csv")
    global_ics3_path <- file.path(demand_input_dir, "demand_ics3_v2.csv")

    if (file.exists(global_bau_path) && file.exists(global_ics3_path)) {
      global_bau <- read_wfdb(global_bau_path)
      global_ics3 <- read_wfdb(global_ics3_path)
      comparison_keys <- c("iso3", "area", "fuel", "year")

      invalid_global_values <- function(source_df) {
        demand_values <- source_df[[demand_col]]
        user_values <- source_df$num_fuel_users_thousands
        anyNA(demand_values) || anyNA(user_values) ||
          any(!is.finite(demand_values)) || any(!is.finite(user_values)) ||
          any(demand_values < 0) || any(user_values < 0)
      }
      if (invalid_global_values(global_bau) || invalid_global_values(global_ics3)) {
        stop("BaU1/ICS3 contain missing, non-finite, or negative demand/user values.")
      }

      duplicate_bau <- global_bau %>%
        dplyr::count(dplyr::across(dplyr::all_of(comparison_keys))) %>%
        dplyr::filter(n != 1L)
      duplicate_ics3 <- global_ics3 %>%
        dplyr::count(dplyr::across(dplyr::all_of(comparison_keys))) %>%
        dplyr::filter(n != 1L)
      if (nrow(duplicate_bau) > 0L || nrow(duplicate_ics3) > 0L) {
        stop("BaU1/ICS3 demand tables contain duplicate comparison keys.")
      }

      bau_keys <- global_bau %>% dplyr::distinct(dplyr::across(dplyr::all_of(comparison_keys)))
      ics3_keys <- global_ics3 %>% dplyr::distinct(dplyr::across(dplyr::all_of(comparison_keys)))
      if (nrow(dplyr::anti_join(bau_keys, ics3_keys, by = comparison_keys)) > 0L ||
          nrow(dplyr::anti_join(ics3_keys, bau_keys, by = comparison_keys)) > 0L) {
        stop("BaU1 and ICS3 do not contain the same country-area-fuel-year grid.")
      }
      required_global_years <- 2000L:2050L
      missing_global_years <- setdiff(
        required_global_years,
        base::intersect(unique(global_bau$year), unique(global_ics3$year))
      )
      if (length(missing_global_years) > 0L) {
        stop(
          "The complete global BaU1/ICS3 comparison requires every year from ",
          "2000 through 2050. Missing: ", paste(missing_global_years, collapse = ", ")
        )
      }

      global_comparison <- global_bau %>%
        dplyr::transmute(
          iso3, area, fuel, year,
          users_bau = num_fuel_users_thousands,
          demand_bau = .data[[demand_col]]
        ) %>%
        dplyr::inner_join(
          global_ics3 %>%
            dplyr::transmute(
              iso3, area, fuel, year,
              users_ics3 = num_fuel_users_thousands,
              demand_ics3 = .data[[demand_col]]
            ),
          by = comparison_keys,
          relationship = "one-to-one"
        )

      historical_check <- global_comparison %>%
        dplyr::filter(year <= 2025L) %>%
        dplyr::summarise(
          max_users_difference = max(abs(users_ics3 - users_bau), na.rm = TRUE),
          max_demand_difference = max(abs(demand_ics3 - demand_bau), na.rm = TRUE)
        )
      if (historical_check$max_users_difference != 0 ||
          historical_check$max_demand_difference != 0) {
        stop("ICS3 does not match BaU1 exactly through 2025.")
      }

      wood_fuels <- c("fuelwood", "charcoal", "imp_fuelwood", "imp_charcoal")
      wood_phaseout_check <- global_comparison %>%
        dplyr::filter(
          area %in% c("rural", "urban"),
          fuel %in% wood_fuels,
          year >= 2026L,
          year <= 2050L
        ) %>%
        dplyr::mutate(
          phaseout_progress = (year - 2025L) / (2050L - 2025L),
          expected_ics3_demand = demand_bau * (1 - phaseout_progress),
          difference = demand_ics3 - expected_ics3_demand,
          # CSV serialization can introduce sub-kilogram differences in rows
          # containing tens of millions of tonnes; accept at most 0.001 t or
          # 1e-10 relative error, whichever is larger.
          tolerance = pmax(1e-3, abs(expected_ics3_demand) * 1e-10)
        )
      max_phaseout_difference <- max(abs(wood_phaseout_check$difference), na.rm = TRUE)
      phaseout_violations <- wood_phaseout_check %>%
        dplyr::filter(abs(difference) > tolerance)
      if (!is.finite(max_phaseout_difference) || nrow(phaseout_violations) > 0L) {
        stop(
          "ICS3 woodfuel demand does not follow the configured annual BaU-relative phase-out. ",
          "Maximum absolute difference: ", format(max_phaseout_difference, scientific = TRUE)
        )
      }
      if (any(abs(wood_phaseout_check$demand_ics3[
        wood_phaseout_check$year == 2050L
      ]) > 1e-8)) {
        stop("ICS3 woodfuel demand is not exactly zero in 2050.")
      }

      build_global_channels <- function(source_df, scenario_label) {
        source_df %>%
          dplyr::filter(
            area %in% c("rural", "urban"),
            fuel %in% wood_fuels
          ) %>%
          dplyr::mutate(
            channel = dplyr::if_else(
              area == "rural" & fuel %in% c("fuelwood", "imp_fuelwood"),
              "W: rural fuelwood",
              "V: charcoal + urban fuelwood"
            )
          ) %>%
          dplyr::group_by(year, channel) %>%
          dplyr::summarise(
            demand_wood_equivalent_t = sum(.data[[demand_col]], na.rm = TRUE),
            .groups = "drop"
          ) %>%
          dplyr::mutate(scenario = scenario_label)
      }

      global_channels <- dplyr::bind_rows(
        build_global_channels(global_bau, "BaU1"),
        build_global_channels(global_ics3, "ICS3")
      )
      global_total <- global_channels %>%
        dplyr::group_by(scenario, year) %>%
        dplyr::summarise(
          demand_wood_equivalent_t = sum(demand_wood_equivalent_t),
          .groups = "drop"
        ) %>%
        dplyr::mutate(channel = "Total W + V")
      global_channels <- dplyr::bind_rows(global_channels, global_total) %>%
        dplyr::mutate(
          scenario = factor(scenario, levels = c("BaU1", "ICS3")),
          channel = factor(
            channel,
            levels = c(
              "W: rural fuelwood",
              "V: charcoal + urban fuelwood",
              "Total W + V"
            )
          )
        ) %>%
        dplyr::arrange(channel, scenario, year)

      global_year_min <- min(global_channels$year)
      global_year_max <- max(global_channels$year)
      global_country_count <- dplyr::n_distinct(global_bau$iso3)
      global_csv <- file.path(
        full_path,
        sprintf(
          "global_bau1_vs_ics3_woodfuel_channels_%d_%d.csv",
          global_year_min, global_year_max
        )
      )
      readr::write_csv(global_channels, global_csv)

      global_plot <- ggplot(
        global_channels,
        aes(
          x = year,
          y = demand_wood_equivalent_t / 1e6,
          color = scenario,
          linetype = scenario
        )
      ) +
        geom_line(linewidth = 1.05) +
        facet_wrap(vars(channel), scales = "free_y", ncol = 1) +
        scale_color_manual(values = c("BaU1" = "#333333", "ICS3" = "#0072B2")) +
        scale_linetype_manual(values = c("BaU1" = "solid", "ICS3" = "longdash")) +
        scale_x_continuous(breaks = seq(global_year_min, global_year_max, by = 5)) +
        scale_y_continuous(
          labels = scales::label_number(accuracy = 1),
          expand = expansion(mult = c(0, 0.05))
        ) +
        labs(
          title = "Global MoFuSS woodfuel demand: BaU1 versus ICS3",
          subtitle = paste0(
            "Complete ", global_country_count,
            "-country demand tables; ICS3 equals BaU1 through 2025 and ",
            "phases woodfuel out linearly to zero in 2050"
          ),
          x = NULL,
          y = "Wood-equivalent demand (million tonnes)",
          color = NULL,
          linetype = NULL,
          caption = paste0("Demand field: ", demand_col)
        ) +
        theme_bw(base_size = 12) +
        theme(
          panel.grid.minor = element_blank(),
          legend.position = "bottom",
          plot.title.position = "plot"
        )

      global_png <- file.path(
        full_path,
        sprintf(
          "global_bau1_vs_ics3_woodfuel_channels_%d_%d.png",
          global_year_min, global_year_max
        )
      )
      ggsave(global_png, global_plot, width = 10, height = 12, dpi = 300, bg = "white")
      cat(
        "\033[32m[OK] Global BaU1/ICS3 validation passed; wrote:\n",
        global_csv, "\n", global_png, "\033[0m\n",
        sep = ""
      )
    } else {
      warning(
        "Global BaU1-versus-ICS3 graph was skipped because one or both complete ",
        "demand tables are missing: ", global_bau_path, "; ", global_ics3_path
      )
    }
    
    setwd(countrydir)
    
    # helper: consistent area ordering if present
    order_area <- function(x) {
      lv <- c("rural","urban","overall")
      x <- factor(x, levels = lv[lv %in% unique(x)])
      if (any(is.na(x))) x <- factor(as.character(x)) # fallback if different labels exist
      x
    }
    
    # ─────────────────────────────────────────────────────────────────────────────
    # 1) POPULATION BY FUEL
    #    - If subcountry == 1, create split = country (e.g., Lusaka / NotLusaka)
    #    - Otherwise, no split
    # ─────────────────────────────────────────────────────────────────────────────
    
    # --- helper: safe ymax from a chosen area level ---
    .ymax_from_area <- function(df, area_level = "overall", value_col = pop) {
      value_col <- rlang::enquo(value_col)
      df %>%
        dplyr::filter(area == area_level) %>%
        dplyr::group_by(scenario_panel, year) %>%
        dplyr::summarise(total = sum(!!value_col, na.rm = TRUE), .groups = "drop") %>%
        dplyr::summarise(ymax = max(total, na.rm = TRUE), .groups = "drop") %>%
        dplyr::pull(ymax)
    }
    
    # --- helper: fuel colors (your existing palette) ---
    fuel_palette <- c(
      # Impact / implied fuels → lighter tints
      "imp_fuelwood" = "#C9A27D",  # light brown (tint of Fuelwood)
      "imp_charcoal" = "#7A7A7A",  # mid grey (tint of Charcoal)
      
      # Real fuels → strong anchors
      "fuelwood"     = "#8B4513",  # brown → wood
      "charcoal"     = "#2B2B2B",  # near-black → carbon
      
      "coal"         = "#4B4B4B",
      "kerosene"     = "#E69F00",
      "gas"          = "#56B4E9",
      "electric"     = "#F0E442",
      "biogas"       = "#A65628",
      "pellets"      = "#999999",
      "ethanol"      = "#CC79A7"
    )
    
    # --- 1A) Build ONE clean population table, with optional split column ---
    if (subcountry != 1) {
      
      popdb_clean <- wfdb %>%
        dplyr::filter(
          iso3 %in% selected_iso3,
          year >= start_year, year <= end_year
        ) %>%
        dplyr::mutate(
          pop  = num_fuel_users_thousands * 1000,
          area = order_area(area)
        ) %>%
        dplyr::group_by(area, fuel, year) %>%
        dplyr::summarise(pop = sum(pop, na.rm = TRUE), .groups = "drop") %>%
        dplyr::mutate(
          iso3 = analysis_area_slug,
          split = NA_character_,
          .before = 1
        ) %>%
        # dplyr::select(iso3, region, split, area, fuel, year, pop) %>%
        dplyr::select(iso3, split, area, fuel, year, pop) %>%
        dplyr::arrange(year, area, fuel)
      
      pop_prefix <- "wfdb_v2"
      
    } else {
      
      # IMPORTANT: in your subcountry files, 'country' is the split label (Lusaka/NotLusaka)
      popdb_clean <- wfdb %>%
        dplyr::filter(
          iso3 %in% selected_iso3,
          year >= start_year, year <= end_year
        ) %>%
        dplyr::mutate(
          pop   = num_fuel_users_thousands * 1000,
          area  = order_area(area),
          split = if (length(selected_iso3) == 1L) {
            as.character(country)
          } else {
            paste(iso3, country, sep = ": ")
          }
        ) %>%
        #dplyr::select(iso3, region, split, area, fuel, year, pop) %>%
        dplyr::select(iso3, split, area, fuel, year, pop) %>%
        dplyr::arrange(split, year, area, fuel)
      
      pop_prefix <- "robdb"
    }

    # Plotting copy. For local ICS runs, prepend the equivalent BaU1 rows and
    # retain an explicit scenario factor so facet_grid() produces vertically
    # aligned BaU and ICS panels with shared axes and colors.
    popdb_plot <- popdb_clean %>%
      mutate(scenario_panel = scenario_ver)

    if (include_bau_comparison) {
      popdb_bau_plot <- wfdb_bau %>%
        dplyr::filter(
          iso3 %in% selected_iso3,
          year >= start_year, year <= end_year
        ) %>%
        dplyr::mutate(
          pop = num_fuel_users_thousands * 1000,
          area = order_area(area),
          split = if (subcountry == 1) {
            if (length(selected_iso3) == 1L) as.character(country) else paste(iso3, country, sep = ": ")
          } else {
            NA_character_
          },
          scenario_panel = bau_comparison_ver
        ) %>%
        dplyr::select(iso3, split, area, fuel, year, pop, scenario_panel) %>%
        {
          if (subcountry == 1) {
            .
          } else {
            dplyr::group_by(., area, fuel, year, scenario_panel) %>%
              dplyr::summarise(pop = sum(pop, na.rm = TRUE), .groups = "drop") %>%
              dplyr::mutate(
                iso3 = analysis_area_slug,
                split = NA_character_,
                .before = 1
              )
          }
        }

      popdb_plot <- dplyr::bind_rows(popdb_bau_plot, popdb_plot)
    }

    popdb_plot <- popdb_plot %>%
      mutate(
        scenario_panel = factor(scenario_panel, levels = scenario_levels)
      ) %>%
      arrange(scenario_panel, split, year, area, fuel)
    
    popdb_clean %>% count(split, area, fuel, year) %>% dplyr::filter(n > 1)
    
    # --- 1B) Write LONG table (always) ---
    write_csv(
      popdb_clean %>% dplyr::select(year, area, fuel, pop, split),
      file.path(outdir, sprintf("%s_pop_long_%s_%s_%s.csv",
                                pop_prefix, analysis_area_slug, start_year, end_year))
    )
    
    # --- 1C) Write WIDE table (key depends on split existence) ---
    # If split is all NA => widen by (year, area)
    # Else => widen by (split, year, area)
    wide_keys <- if (all(is.na(popdb_clean$split))) c("year","area") else c("split","year","area")
    
    popdb_wide <- popdb_clean %>%
      dplyr::mutate(colname = paste0(fuel, " [num_fuel_users_thousands]")) %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(c(wide_keys, "colname")))) %>%
      dplyr::summarise(pop = sum(pop, na.rm = TRUE), .groups = "drop") %>%  # <- major fix: no list-cols
      tidyr::pivot_wider(names_from = colname, values_from = pop) %>%
      dplyr::arrange(dplyr::across(dplyr::all_of(wide_keys)))
    
    write_csv(
      popdb_wide,
      file.path(outdir, sprintf("%s_pop_wide_%s_%s_%s.csv",
                                pop_prefix, analysis_area_slug, start_year, end_year))
    )
    
    # --- 1D) Plotting function (reusable for each split) ---
    plot_pop_stack <- function(df, title_suffix = NULL, out_png) {
      
      # use Overall if present; otherwise fallback to max over all areas
      has_overall <- any(df$area == "overall")
      if (has_overall) {
        ymax <- .ymax_from_area(df, "overall", pop)
      } else {
        ymax <- df %>%
          dplyr::group_by(scenario_panel, year) %>%
          dplyr::summarise(total = sum(pop, na.rm = TRUE), .groups = "drop") %>%
          dplyr::summarise(ymax = max(total, na.rm = TRUE), .groups = "drop") %>%
          dplyr::pull(ymax)
      }
      
      p <- ggplot(df, aes(x = year, y = pop, fill = fuel)) +
        geom_area(alpha = 0.95, color = "grey30", linewidth = 0.2) +
        labs(
          title = paste0("Population using each fuel in ", analysis_area_label,
                         if (!is.null(title_suffix)) paste0(" - ", title_suffix) else ""),
          subtitle = sprintf("%d-%d | Faceted by area (common Y from %s)",
                             start_year, end_year, if (has_overall) "Overall" else "max across areas"),
          x = NULL, y = "People", fill = "Fuel",
          caption = scenario_comparison_text
        ) +
        scale_x_continuous(breaks = seq(start_year, end_year, by = 5)) +
        scale_y_continuous(
          labels = scales::label_number(scale_cut = scales::cut_si("")),
          expand = expansion(mult = c(0, .05))
        ) +
        coord_cartesian(ylim = c(0, ymax)) +
        facet_grid(
          rows = vars(scenario_panel),
          cols = vars(area),
          scales = "fixed"
        ) +
        scale_fill_manual(values = fuel_palette, na.value = "grey70") +
        theme_bw(base_size = 13) +
        theme(panel.grid.minor = element_blank(),
              plot.title.position = "plot",
              legend.position = "bottom")
      
      ggsave(
        out_png, p, width = 14, height = comparison_plot_height,
        dpi = 300, bg = "white"
      )
      invisible(p)
    }
    
    # --- 1E) Make plots ---
    if (all(is.na(popdb_clean$split))) {
      
      # Single plot (no split)
      plot_pop_stack(
        df = popdb_plot,
        title_suffix = NULL,
        out_png = file.path(outdir, sprintf("%s_pop_stack_faceted_%s_%s_%s.png",
                                            pop_prefix, analysis_area_slug, start_year, end_year))
      )
      
    } else {
      
      # One plot per split (e.g., Lusaka / NotLusaka)
      splits <- sort(unique(popdb_plot$split))
      
      for (sp in splits) {
        df_sp <- popdb_plot %>% dplyr::filter(split == sp)
        
        plot_pop_stack(
          df = df_sp,
          title_suffix = sp,
          out_png = file.path(outdir, sprintf("%s_pop_stack_faceted_%s_%s_%s_%s.png",
                                              pop_prefix, analysis_area_slug, sp, start_year, end_year))
        )
      }
      
      # Optional: one combined plot with split as rows and area as columns
      # (comment out if you don’t want it)
      # Compute Y max as the maximum of Overall totals
      # across Lusaka / NotLusaka (but NOT larger combinations)
      ymax_all <- popdb_plot %>%
        dplyr::filter(area == "overall") %>%          # key line
        dplyr::group_by(scenario_panel, split, year) %>%
        dplyr::summarise(total = sum(pop, na.rm = TRUE), .groups = "drop") %>%
        dplyr::group_by(scenario_panel, split) %>%
        dplyr::summarise(ymax_split = max(total, na.rm = TRUE), .groups = "drop") %>%
        dplyr::summarise(ymax = max(ymax_split, na.rm = TRUE)) %>%
        dplyr::pull(ymax)
      
      p_combined <- ggplot(popdb_plot, aes(x = year, y = pop, fill = fuel)) +
        geom_area(alpha = 0.95, color = "grey30", linewidth = 0.2) +
        labs(
          title = sprintf("Population using each fuel in %s - by subcountry split", analysis_area_label),
          subtitle = sprintf("%d-%d | Rows = split | Columns = area (common Y across all splits)",
                             start_year, end_year),
          x = NULL, y = "People", fill = "Fuel",
          caption = scenario_comparison_text
        ) +
        scale_x_continuous(breaks = seq(start_year, end_year, by = 5)) +
        scale_y_continuous(
          labels = scales::label_number(scale_cut = scales::cut_si("")),
          expand = expansion(mult = c(0, .05))
        ) +
        coord_cartesian(ylim = c(0, ymax_all)) +
        facet_grid(
          rows = vars(scenario_panel, split),
          cols = vars(area),
          scales = "fixed"
        ) +
        scale_fill_manual(values = fuel_palette, na.value = "grey70") +
        theme_bw(base_size = 12) +
        theme(panel.grid.minor = element_blank(),
              plot.title.position = "plot",
              legend.position = "bottom")
      
      ggsave(
        file.path(outdir, sprintf("%s_pop_stack_splitgrid_%s_%s_%s.png",
                                  pop_prefix, analysis_area_slug, start_year, end_year)),
        p_combined, width = 14, height = comparison_grid_height,
        dpi = 300, bg = "white"
      )
    }
    
    
    # ─────────────────────────────────────────────────────────────────────────────
    # 2) WFDB (keep area; Fuelwood + Charcoal only; Charcoal ÷ efchratio)
    #    - If subcountry == 1, treat `country` as split (Lusaka/NotLusaka)
    # ─────────────────────────────────────────────────────────────────────────────
    
    col_sym <- rlang::sym(demand_col)
    
    build_wfdb_base <- function(source_df, scenario_label) {
      source_df %>%
        dplyr::filter(
          iso3 %in% selected_iso3,
          year >= start_year, year <= end_year,
          fuel %in% c("fuelwood", "charcoal", "imp_fuelwood", "imp_charcoal")
        ) %>%
        dplyr::mutate(
          # Collapse improved fuels into the two MoFuSS demand categories.
          fuel = dplyr::case_when(
            fuel %in% c("fuelwood", "imp_fuelwood") ~ "fuelwood",
            fuel %in% c("charcoal", "imp_charcoal") ~ "charcoal",
            TRUE ~ fuel
          ),
          area = order_area(area),
          split = if (subcountry == 1) {
            if (length(selected_iso3) == 1L) as.character(country) else paste(iso3, country, sep = ": ")
          } else {
            NA_character_
          },
          scenario_panel = scenario_label
        )
    }

    wfdb_base <- build_wfdb_base(wfdb, scenario_ver)

    wfdb_plot_base <- wfdb_base
    if (include_bau_comparison) {
      wfdb_plot_base <- dplyr::bind_rows(
        build_wfdb_base(wfdb_bau, bau_comparison_ver),
        wfdb_plot_base
      )
    }

    wfdb_plot_base <- wfdb_plot_base %>%
      mutate(
        scenario_panel = factor(scenario_panel, levels = scenario_levels)
      ) %>%
      arrange(scenario_panel, split, year, area, fuel)
    
    wfdb_check <- wfdb %>%
      dplyr::mutate(
        fuel_clean = tolower(trimws(as.character(fuel)))
      ) %>%
      dplyr::filter(
        iso3 %in% selected_iso3,
        year >= start_year, year <= end_year,
        fuel_clean %in% c("fuelwood", "imp_fuelwood", "charcoal", "imp_charcoal")
      ) %>%
      dplyr::group_by(fuel_clean) %>%
      dplyr::summarise(total = sum(!!col_sym, na.rm = TRUE), .groups = "drop")
    
    print(wfdb_check)
    
    wfdb_check2 <- wfdb_base %>%
      dplyr::group_by(fuel) %>%
      dplyr::summarise(total = sum(!!col_sym, na.rm = TRUE), .groups = "drop")
    
    print(wfdb_check2)
    
    # Summarise with or without split
    wfdb_twofuels <- if (subcountry == 1) {
      
      wfdb_base %>%
        dplyr::group_by(split, year, area, fuel) %>%
        dplyr::summarise(value_woodeq_t = sum(!!col_sym, na.rm = TRUE), .groups = "drop") %>%
        dplyr::mutate(
          value_t = dplyr::if_else(fuel == "charcoal", value_woodeq_t / efchratio, value_woodeq_t),
          units   = "tonnes"
        ) %>%
        dplyr::arrange(split, year, area, fuel)
      
    } else {
      
      wfdb_base %>%
        dplyr::group_by(year, area, fuel) %>%
        dplyr::summarise(value_woodeq_t = sum(!!col_sym, na.rm = TRUE), .groups = "drop") %>%
        dplyr::mutate(
          value_t = dplyr::if_else(fuel == "charcoal", value_woodeq_t / efchratio, value_woodeq_t),
          units   = "tonnes"
        ) %>%
        dplyr::arrange(year, area, fuel)
    }

    # Separate plotting summary: include BaU1 only for local ICS runs. Keeping
    # this separate ensures the long/wide CSV exports above and below retain
    # their original selected-scenario schema and row counts.
    wfdb_twofuels_plot <- if (subcountry == 1) {
      wfdb_plot_base %>%
        dplyr::group_by(scenario_panel, split, year, area, fuel) %>%
        dplyr::summarise(
          value_woodeq_t = sum(!!col_sym, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        dplyr::mutate(
          value_t = dplyr::if_else(
            fuel == "charcoal",
            value_woodeq_t / efchratio,
            value_woodeq_t
          ),
          units = "tonnes"
        ) %>%
        dplyr::arrange(scenario_panel, split, year, area, fuel)
    } else {
      wfdb_plot_base %>%
        dplyr::group_by(scenario_panel, year, area, fuel) %>%
        dplyr::summarise(
          value_woodeq_t = sum(!!col_sym, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        dplyr::mutate(
          value_t = dplyr::if_else(
            fuel == "charcoal",
            value_woodeq_t / efchratio,
            value_woodeq_t
          ),
          units = "tonnes"
        ) %>%
        dplyr::arrange(scenario_panel, year, area, fuel)
    }
    
    # Long table
    write_csv(
      wfdb_twofuels %>%
        dplyr::select(dplyr::any_of("split"), year, area, fuel, value_t, units),
      file.path(outdir, sprintf("wfdb_fw_char_long_%s_%s_%s_%s_byarea%s.csv",
                                analysis_area_slug, demand_col, start_year, end_year,
                                ifelse(subcountry == 1, "_bysplit", "")))
    )
    
    # Wide table
    wfdb_twofuels_wide <- wfdb_twofuels %>%
      dplyr::mutate(colname = paste0(fuel, " [t]")) %>%
      {
        if (subcountry == 1) {
          dplyr::select(., split, year, area, colname, value_t) %>%
            tidyr::pivot_wider(names_from = colname, values_from = value_t) %>%
            dplyr::arrange(split, year, area)
        } else {
          dplyr::select(., year, area, colname, value_t) %>%
            tidyr::pivot_wider(names_from = colname, values_from = value_t) %>%
            dplyr::arrange(year, area)
        }
      }
    
    write_csv(
      wfdb_twofuels_wide,
      file.path(outdir, sprintf("wfdb_fw_char_wide_%s_%s_%s_%s_byarea%s.csv",
                                analysis_area_slug, demand_col, start_year, end_year,
                                ifelse(subcountry == 1, "_bysplit", "")))
    )
    
    # ── Y limits: SAME logic you liked ──
    # If split exists: common Y = max of split-wise Overall maxima (tight, not inflated)
    # Else: common Y = Overall max (as before)
    
    if (subcountry == 1) {
      
      ymax_all_wfdb <- wfdb_twofuels_plot %>%
        dplyr::filter(area == "overall") %>%
        dplyr::group_by(scenario_panel, split, year) %>%
        dplyr::summarise(total = sum(value_t, na.rm = TRUE), .groups = "drop") %>%
        dplyr::group_by(scenario_panel, split) %>%
        dplyr::summarise(ymax_split = max(total, na.rm = TRUE), .groups = "drop") %>%
        dplyr::summarise(ymax = max(ymax_split, na.rm = TRUE), .groups = "drop") %>%
        dplyr::pull(ymax)
      
      # One plot per split (3-panel by area)
      # One plot per split (3-panel by area) — each uses its OWN ymax from Overall
      for (sp in sort(unique(wfdb_twofuels_plot$split))) {
        
        df_sp <- wfdb_twofuels_plot %>% dplyr::filter(split == sp)
        
        ymax_sp <- df_sp %>%
          dplyr::filter(area == "overall") %>%
          dplyr::group_by(scenario_panel, year) %>%
          dplyr::summarise(total = sum(value_t, na.rm = TRUE), .groups = "drop") %>%
          dplyr::summarise(ymax = max(total, na.rm = TRUE), .groups = "drop") %>%
          dplyr::pull(ymax)
        
        p_sp <- ggplot(df_sp, aes(x = year, y = value_t, fill = fuel)) +
          geom_area(alpha = 0.95, color = "grey30", linewidth = 0.2) +
          labs(
            title = sprintf("Fuelwood & Charcoal demand in %s - %s (tonnes, charcoal / %s)",
                            analysis_area_label, sp, efchratio),
            subtitle = sprintf("%d-%d | Faceted by area (Y from max stacked in Overall) | source col: %s",
                               start_year, end_year, demand_col),
            x = NULL, y = "Tonnes", fill = "Fuel",
            caption = scenario_comparison_text
          ) +
          scale_x_continuous(breaks = seq(start_year, end_year, by = 5)) +
          scale_y_continuous(
            labels = scales::label_number(scale_cut = scales::cut_si("")),
            expand = expansion(mult = c(0, .05))
          ) +
          coord_cartesian(ylim = c(0, ymax_sp)) +  # <- KEY CHANGE (per-split ymax)
          facet_grid(
            rows = vars(scenario_panel),
            cols = vars(area),
            scales = "fixed"
          ) +
          scale_fill_manual(values = c("fuelwood" = "#8B4513", "charcoal" = "#2B2B2B")) +
          theme_bw(base_size = 13) +
          theme(panel.grid.minor = element_blank(),
                plot.title.position = "plot",
                legend.position = "bottom")
        
        ggsave(
          file.path(outdir, sprintf("wfdb_fw_char_stack_faceted_%s_%s_%s_%s_%s.png",
                                    analysis_area_slug, sp, demand_col, start_year, end_year)),
          p_sp, width = 14, height = comparison_plot_height,
          dpi = 300, bg = "white"
        )
      }
      
      
      # Optional: combined split grid (rows = split, cols = area), same Y
      p_grid <- ggplot(wfdb_twofuels_plot, aes(x = year, y = value_t, fill = fuel)) +
        geom_area(alpha = 0.95, color = "grey30", linewidth = 0.2) +
        labs(
          title = sprintf("Fuelwood & Charcoal demand in %s - by subcountry split (tonnes, charcoal / %s)",
                          analysis_area_label, efchratio),
          subtitle = sprintf("%d-%d | Rows = split | Columns = area (common Y based on max(Overall) across splits) | source col: %s",
                             start_year, end_year, demand_col),
          x = NULL, y = "Tonnes", fill = "Fuel",
          caption = scenario_comparison_text
        ) +
        scale_x_continuous(breaks = seq(start_year, end_year, by = 5)) +
        scale_y_continuous(
          labels = scales::label_number(scale_cut = scales::cut_si("")),
          expand = expansion(mult = c(0, .05))
        ) +
        coord_cartesian(ylim = c(0, ymax_all_wfdb)) +
        facet_grid(
          rows = vars(scenario_panel, split),
          cols = vars(area),
          scales = "fixed"
        ) +
        scale_fill_manual(values = c("fuelwood" = "#8B4513", "charcoal" = "#2B2B2B")) +
        theme_bw(base_size = 12) +
        theme(panel.grid.minor = element_blank(),
              plot.title.position = "plot",
              legend.position = "bottom")
      
      ggsave(
        file.path(outdir, sprintf("wfdb_fw_char_stack_splitgrid_%s_%s_%s_%s.png",
                                  analysis_area_slug, demand_col, start_year, end_year)),
        p_grid, width = 14, height = comparison_grid_height,
        dpi = 300, bg = "white"
      )
      
    } else {
      
      # Original behavior (no split)
      ymax_overall_wfdb <- wfdb_twofuels_plot %>%
        dplyr::filter(area == "overall") %>%
        dplyr::group_by(scenario_panel, year) %>%
        dplyr::summarise(total = sum(value_t, na.rm = TRUE), .groups = "drop") %>%
        dplyr::summarise(ymax = max(total, na.rm = TRUE), .groups = "drop") %>%
        dplyr::pull(ymax)
      
      p_wfdb <- ggplot(wfdb_twofuels_plot, aes(x = year, y = value_t, fill = fuel)) +
        geom_area(alpha = 0.95, color = "grey30", linewidth = 0.2) +
        labs(
          title = sprintf("Fuelwood & Charcoal demand in %s (tonnes, charcoal / %s)", analysis_area_label, efchratio),
          subtitle = sprintf("%d-%d | Faceted by area (Y from max stacked in Overall) | source col: %s",
                             start_year, end_year, demand_col),
          x = NULL, y = "Tonnes", fill = "Fuel",
          caption = scenario_comparison_text
        ) +
        scale_x_continuous(breaks = seq(start_year, end_year, by = 5)) +
        scale_y_continuous(
          labels = scales::label_number(scale_cut = scales::cut_si("")),
          expand = expansion(mult = c(0, .05))
        ) +
        coord_cartesian(ylim = c(0, ymax_overall_wfdb)) +
        facet_grid(
          rows = vars(scenario_panel),
          cols = vars(area),
          scales = "fixed"
        ) +
        scale_fill_manual(values = c("fuelwood" = "#8B4513", "charcoal" = "#2B2B2B")) +
        theme_bw(base_size = 13) +
        theme(panel.grid.minor = element_blank(),
              plot.title.position = "plot",
              legend.position = "bottom")
      
      ggsave(
        file.path(outdir, sprintf("wfdb_fw_char_stack_faceted_%s_%s_%s_%s.png",
                                  analysis_area_slug, demand_col, start_year, end_year)),
        p_wfdb, width = 14, height = comparison_plot_height,
        dpi = 300, bg = "white"
      )
    }

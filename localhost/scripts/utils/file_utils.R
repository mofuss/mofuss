# nolint start

library(dplyr)
library(readr)
library(stringr)

write_log <- function(...) {
  message <- paste0(Sys.time(), " - ", ..., "\n")
  cat(message, file = log_file, append = TRUE)
}


get_column_names <- function(col_selector,admin_level) {
    # Define base column names
    cols <- list(
        x_col = c(
            adm0 = "ADM_0",
            adm1 = "ADM_1",
            adm2 = "ADM_2"
        ),
        harvest = list(
            mean = "Harv_2020_2030_mean",
            "1mc" = "Harv_2020_2030_1MC"
        ),
        nrb = list(
            mean = "NRB_2020_2030_mean",
            "1mc" = "NRB_2020_2030_1MC"
        ),
        fnrb = list(
            mean = "fNRB_2020_2030_mean",
            "1mc" = "fNRB_2020_2030_1MC"
        )
    )
    
    # Return the column names for the selected type
    list(
        harvest_col = cols$harvest[[col_selector]],
        nrb_col = cols$nrb[[col_selector]],
        fnrb_col = cols$fnrb[[col_selector]],
        x_cols = cols$x_col[[admin_level]]
    )
}

process_files <- function(directory_path, admin_filename_pattern, nrb_col, harvest_col, fnrb_col, x_col) {
    write_log(paste("Processing files in", directory_path, "with pattern", admin_filename_pattern))
    
    files <- list.files(directory_path, pattern = admin_filename_pattern, recursive = TRUE, full.names = TRUE)
    if (length(files) == 0) {
        write_log(paste("No files found matching pattern", admin_filename_pattern,"with admin level", x_col))
        return(NULL)
    }
    
    write_log(paste("Found", length(files), "files"))
    
    all_data <- list()
    for (file in files) {
        df <- read_csv(file, show_col_types = FALSE)
        
        df$filename <- basename(file)
        df$filepath <- file
        df$scenario <- str_extract(file, "(bau|(minus|plus)\\d+)")
        df$demand_value <- case_when(
            df$scenario == "bau" ~ 0,
            str_detect(df$scenario, "minus") ~ -as.numeric(str_extract(df$scenario, "\\d+")),
            str_detect(df$scenario, "plus") ~ as.numeric(str_extract(df$scenario, "\\d+")),
            TRUE ~ NA_real_
        )

        if (any(grepl("^NAME_", names(df)))) {
            df <- df %>%
                rename_with(~ gsub("^NAME_", "ADM_", .), .cols = starts_with("NAME_"))
        }

        df <- df %>%
            select(
                all_of(c(
                    x_col,
                    nrb_col,
                    harvest_col,
                    fnrb_col,
                    "scenario",
                    "demand_value",
                    "filename",
                    "filepath"
                ))
            )

        all_data[[length(all_data) + 1]] <- df
    }

    if (length(all_data) == 0) {
        stop("No files were successfully read")
    }

    combined_df <- bind_rows(all_data)
    
    # Order scenarios properly
    minus_scenarios <- combined_df$scenario[str_detect(combined_df$scenario, "minus")]
    minus_values <- as.numeric(str_extract(minus_scenarios, "\\d+"))
    minus_scenarios <- paste0("minus", sort(minus_values, decreasing = TRUE))
    
    plus_scenarios <- combined_df$scenario[str_detect(combined_df$scenario, "plus")]
    plus_values <- as.numeric(str_extract(plus_scenarios, "\\d+"))
    plus_scenarios <- paste0("plus", sort(plus_values))
    
    scenario_levels <- c(minus_scenarios, "bau", plus_scenarios)
    combined_df$scenario <- factor(combined_df$scenario, levels = unique(scenario_levels))
    
    if (!"bau" %in% combined_df$scenario) {
        stop("No BAU scenario found in the data. BAU data is required for marginal analysis.")
    }
    
    # Check for missing values in key columns
    missing_vals <- combined_df %>%
        summarise(
            missing_nrb = sum(is.na(.data[[nrb_col]])),
            missing_harvest = sum(is.na(.data[[harvest_col]])),
            missing_fnrb = sum(is.na(.data[[fnrb_col]])),
            missing_x = sum(is.na(.data[[x_col]])),
            missing_scenario = sum(is.na(scenario)),
            missing_demand = sum(is.na(demand_value))
        )
    
    if (any(missing_vals > 0)) {
        warning("Missing values found in key columns: ", 
               paste(names(missing_vals)[missing_vals > 0], collapse = ", "))
    }

    return(combined_df)
}



# nolint end

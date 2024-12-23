# nolint start

library(dplyr)
library(readr)
library(stringr)

process_files <- function(directory_path, pattern, y_col, x_col) {
    files <- list.files(
        path = directory_path,
        pattern = pattern,
        full.names = TRUE,
        recursive = TRUE
    )

    if (length(files) == 0) {
        stop("No files found matching the pattern")
    }

    combined_df <- tryCatch(
        {
            all_data <- list()
            for (file in files) {
                
                df <- try({
                    temp_df <- read_csv(file, show_col_types = FALSE)
                    
                    temp_df$filename <- basename(file)
                    temp_df$filepath <- file
                    temp_df$scenario <- str_extract(file, "(bau|(minus|plus)\\d+)")
                    temp_df$demand_value <- case_when(
                        temp_df$scenario == "bau" ~ 0,
                        str_detect(temp_df$scenario, "minus") ~ -as.numeric(str_extract(temp_df$scenario, "\\d+")),
                        str_detect(temp_df$scenario, "plus") ~ as.numeric(str_extract(temp_df$scenario, "\\d+")),
                        TRUE ~ NA_real_
                    )

                    if (any(grepl("^NAME_", names(temp_df)))) {
                        temp_df <- temp_df %>%
                            rename_with(~ gsub("^NAME_", "ADM_", .), .cols = starts_with("NAME_"))
                    }

                    adm_cols <- grep("^ADM_", names(temp_df), value = TRUE)
                    highest_adm <- adm_cols[length(adm_cols)]  # Get the highest ADM level

                    temp_df <- temp_df %>%
                        select(
                            all_of(c(
                                x_col,
                                y_col,
                                highest_adm,
                                "scenario",
                                "demand_value",
                                "filename",
                                "filepath"
                            ))
                        )

                    temp_df
                })

                if (!inherits(df, "try-error")) {
                    all_data[[length(all_data) + 1]] <- df
                } else {
                    warning(paste("Failed to read file:", file))
                }
            }

            if (length(all_data) == 0) {
                stop("No files were successfully read")
            }

            combined_df <- bind_rows(all_data)
            
            if (!"bau" %in% combined_df$scenario) {
                stop("No BAU scenario found in the data. BAU data is required for marginal analysis.")
            }
            
            # Check for missing values in key columns
            missing_vals <- combined_df %>%
                summarise(
                    missing_y = sum(is.na(.data[[y_col]])),
                    missing_x = sum(is.na(.data[[x_col]])),
                    missing_scenario = sum(is.na(scenario)),
                    missing_demand = sum(is.na(demand_value))
                )
            
            if (any(missing_vals > 0)) {
                warning("Missing values found in key columns: ", 
                       paste(names(missing_vals)[missing_vals > 0], collapse = ", "))
            }

            combined_df
        },
        error = function(e) {
            stop(paste("Error reading files:", e$message))
        }
    )

    return(combined_df)
} 



# nolint end

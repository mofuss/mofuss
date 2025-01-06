# nolint start

library(dplyr)
library(readr)
library(stringr)

write_log <- function(...) {
  message <- paste0(Sys.time(), " - ", ..., "\n")
  cat(message, file = log_file, append = TRUE)
}

error_handler <- function(e, context = "") {
  error_message <- paste0(
    Sys.time(), 
    " - ERROR in ", context, ": ", 
    conditionMessage(e), "\n",
    "Stack trace:\n",
    paste(capture.output(print(sys.calls())), collapse = "\n")
  )
  write_log(error_message)
}

warning_handler <- function(w, context = "") {
  warning_message <- paste0(
    Sys.time(),
    " - WARNING in ", context, ": ",
    conditionMessage(w)
  )
  write_log(warning_message)
}


get_column_names <- function(filename_pattern, col_selector) {
    if (col_selector == "mean") {
        y_col <- case_when(
            str_detect(filename_pattern, "frcompl") ~ "Harv_2020_2030_mean",
            str_detect(filename_pattern, "fr") ~ "NRB_2020_2030_mean",
            TRUE ~ "fNRB"
        )
        nrb_col <- "NRB_2020_2030_mean"
        harvest_col <- "Harv_2020_2030_mean"
    } else if (col_selector == "1mc") {
        y_col <- case_when(
            str_detect(filename_pattern, "frcompl") ~ "Harv_2020_2030_1MC",
            str_detect(filename_pattern, "fr") ~ "NRB_2020_2030_1MC",
            TRUE ~ "fNRB1mc"
        )
        nrb_col <- "NRB_2020_2030_1MC"
        harvest_col <- "Harv_2020_2030_1MC"
    }

    x_col <- case_when(
        str_detect(filename_pattern, "adm0") ~ "ADM_0",
        str_detect(filename_pattern, "adm1") ~ "ADM_1",
        str_detect(filename_pattern, "adm2") ~ "ADM_2",
        TRUE ~ NA_character_
    )

    if (is.na(x_col)) {
            stop(paste("Unknown filename pattern:", filename_pattern))
    }

    return(list(
        y_col = y_col,
        nrb_col = nrb_col,
        harvest_col = harvest_col,
        x_col = x_col,
        demand_col = "demand_value"
    ))
}

process_files <- function(directory_path, filename_pattern, y_col, x_col) {
    write_log(paste("Processing files in", directory_path, "with pattern", filename_pattern))
    
    tryCatch({
        files <- list.files(directory_path, pattern = filename_pattern, recursive = TRUE, full.names = TRUE)
        
        if (length(files) == 0) {
            write_log(paste("No files found matching pattern", filename_pattern))
            return(NULL)
        }
        
        write_log(paste("Found", length(files), "files"))
        
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
    }, error = function(e) {
        error_handler(e, "process_files")
        return(NULL)
    }, warning = function(w) {
        warning_handler(w, "process_files")
    })
} 



# nolint end

# nolint start: object_name_linter
# External dependencies you need to install: pandoc, pdflatex
# You may need to set the PATH to the texbin directory:
Sys.setenv(PATH=paste("/Library/TeX/texbin", Sys.getenv("PATH"), sep=":"))

if (!require("pacman")) install.packages("pacman")
library(pacman)
pacman::p_load(dplyr, ggplot2, readr, purrr, viridis, knitr, stringr, styler, rmarkdown, kableExtra, tidyr)


source("localhost/scripts/utils/file_utils.R")
source("localhost/scripts/utils/plot_utils.R")
source("localhost/scripts/utils/data_utils.R")

setwd("~/mofuss/")

# Configuration
col_selector = "mean" # "1mc" to look at the first monte carlo, "mean" to look at the mean of all monte carlos.
admin_levels <- c("adm0", "adm1") # Modify this to control which levels to process: c("adm0"), c("adm1"), c("adm2"), or any combination

base_patterns <- c("", "_fr", "_frcompl")
filename_patterns <- unlist(lapply(admin_levels, function(level) {
    paste0(level, base_patterns, ".csv")
}))

if (!dir.exists("assets")) {
    dir.create("assets")
}

base_path <- "/Users/ricardopiedrahita/Dropbox/mofuss_data"
countries <- list.dirs(base_path, full.names = FALSE, recursive = FALSE)
countries <- countries[!countries %in% c("", "assets")]

for (country in countries) {
    directory_path <- file.path(base_path, country)
    
    output_file <- paste0("assets/marginal_analysis_results_", country, "_", col_selector, "_", Sys.Date(), ".md")
    file_conn <- file(output_file, "w")
    
    tryCatch({
        write(paste0("# Marginal Analysis Results for ", toupper(country), "\n\n"), file_conn)
        write(paste0("Results are for ", col_selector, " of all monte carlos.\n\n"), file_conn)

        write("# Introduction\n\n", file_conn)
        write(paste("This report presents the marginal analysis results for", toupper(country), 
                   "across different administrative levels and scenarios.\n\n"), file_conn)
        
        combined_data_adm0 <- NULL
        combined_data_adm1 <- NULL
        combined_data_adm2 <- NULL
        
        for (pattern in filename_patterns) {
            demand_col <- "demand_value"
            if (col_selector == "mean") {
                y_col <- case_when(
                    str_detect(pattern, "frcompl") ~ "Harv_2020_2030_mean",
                    str_detect(pattern, "fr") ~ "NRB_2020_2030_mean",
                    TRUE ~ "fNRB"
                )
                nrb_col <- "NRB_2020_2030_mean"
                harvest_col <- "Harv_2020_2030_mean"
            } else if (col_selector == "1mc") {
                y_col <- case_when(
                    str_detect(pattern, "frcompl") ~ "Harv_2020_2030_1MC",
                    str_detect(pattern, "fr") ~ "NRB_2020_2030_1MC",
                    TRUE ~ "fNRB1mc"
                )
                nrb_col <- "NRB_2020_2030_1MC"
                harvest_col <- "Harv_2020_2030_1MC"
            }

            x_col <- case_when(
                str_detect(pattern, "adm0") ~ "ADM_0",
                str_detect(pattern, "adm1") ~ "ADM_1",
                str_detect(pattern, "adm2") ~ "ADM_2",
                TRUE ~ NA_character_
            )

            if (is.na(x_col)) {
                stop(paste("Unknown pattern:", pattern))
            }

            combined_data <- process_files(directory_path, pattern, y_col, x_col)
            
            combined_data <- combined_data %>%
                filter(!is.na(.data[[y_col]]), !is.na(demand_value))
    
            if (str_detect(pattern, "adm0")) {
                if (is.null(combined_data_adm0)) {
                    combined_data_adm0 <- combined_data
                } else {
                    combined_data_adm0 <- full_join(
                        combined_data_adm0,
                        combined_data,
                        by = c(x_col, "scenario", demand_col)
                    )
                }
            } else if (str_detect(pattern, "adm1")) {
                if (is.null(combined_data_adm1)) {
                    combined_data_adm1 <- combined_data
                } else {
                    combined_data_adm1 <- full_join(
                        combined_data_adm1,
                        combined_data,
                        by = c(x_col, "scenario", demand_col)
                    )
                }
            } else if (str_detect(pattern, "adm2")) {
                if (is.null(combined_data_adm2)) {
                    combined_data_adm2 <- combined_data
                } else {
                    combined_data_adm2 <- full_join(
                        combined_data_adm2,
                        combined_data,
                        by = c(x_col, "scenario", demand_col)
                    )
                }
            }

            plotz <- create_plots(combined_data, x_col, y_col, pattern)

            write(paste("\n## Results for", pattern, "\n"), file_conn, append = TRUE)
            write(paste("\n![", pattern, "](", basename(plotz$boxplot), ")\n"), file_conn, append = TRUE)
            write(paste("\n![", pattern, "](", basename(plotz$scatter), ")\n"), file_conn, append = TRUE)

            summary_stats <- combined_data %>%
                group_by(demand_value, scenario) %>%
                summarise(
                    mean = mean(.data[[y_col]], na.rm = TRUE),
                    sd = sd(.data[[y_col]], na.rm = TRUE),
                    median = median(.data[[y_col]], na.rm = TRUE),
                    q1 = quantile(.data[[y_col]], 0.25, na.rm = TRUE),
                    q3 = quantile(.data[[y_col]], 0.75, na.rm = TRUE),
                    n = n(),
                    .groups = "drop"
                )

            write(paste("\nThere are", length(unique(combined_data$filepath)), "unique files from different paths\n"),
                file_conn,
                append = TRUE
            )

            write("\n### Summary Statistics\n", file_conn, append = TRUE)
            write(knitr::kable(summary_stats, format = "markdown"),
                file_conn,
                append = TRUE
            )
        }

        data_list <- list(
            adm0 = if("adm0" %in% admin_levels) combined_data_adm0 else NULL,
            adm1 = if("adm1" %in% admin_levels) combined_data_adm1 else NULL,
            adm2 = if("adm2" %in% admin_levels) combined_data_adm2 else NULL
        )
        
        cat("\nData list contents:\n")
        for(level in names(data_list)) {
            cat(level, ": ", !is.null(data_list[[level]]), "\n")
        }
        
        data_list <- data_list[names(data_list) %in% admin_levels]
        
        for (adm_level in names(data_list)) {
            cat("\nProcessing", adm_level, "...\n")
            data <- data_list[[adm_level]]
            
            if (!is.null(data)) {
                cat("Data dimensions:", dim(data)[1], "rows,", dim(data)[2], "columns\n")
                cat("Columns present:", paste(names(data), collapse=", "), "\n")
                cat("nrb_col:", nrb_col, "\n")
                cat("harvest_col:", harvest_col, "\n")
            }
            
            if (!is.null(data) && all(c(nrb_col, harvest_col) %in% names(data))) {
                cat("Starting marginal analysis...\n")
                marginal_plot_name <- analyze_nrb_vs_harvest(
                    data = data,
                    nrb_col = nrb_col,
                    harvest_col = harvest_col,
                    demand_col = demand_col,
                    pattern = adm_level,
                    x_col = paste0("ADM_", substr(adm_level, 4, 4))
                )
                cat("Marginal plot created:", marginal_plot_name, "\n")
                
                write(paste("\n## Marginal Analysis Results for", adm_level, "\n"),
                    file_conn,
                    append = TRUE
                )
                write(paste("\n### Marginal Changes\n"), file_conn, append = TRUE)
                write(paste("\n![Marginal Analysis](", basename(marginal_plot_name), ")\n"),
                    file_conn,
                    append = TRUE
                )
            }
        }
    render(output_file, output_format = "pdf_document")

    }, error = function(e) {
        cat("\nError:", e$message, "\n")
    }, finally = {
        if (!is.null(file_conn) && isOpen(file_conn)) {
            close(file_conn)
        }
    })
}


# nolint end

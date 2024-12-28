# nolint start: object_name_linter
# External dependencies you need to install: pandoc, pdflatex
# You may need to set the PATH to the texbin directory:
Sys.setenv(PATH=paste("/Library/TeX/texbin", Sys.getenv("PATH"), sep=":"))

if (!require("pacman")) install.packages("pacman")
library(pacman)
pacman::p_load(dplyr, ggplot2, readr, purrr, viridis, knitr, stringr, styler, rmarkdown, kableExtra, tidyr,zoo)


source("localhost/scripts/utils/file_utils.R")
source("localhost/scripts/utils/plot_utils.R")
source("localhost/scripts/utils/data_utils.R")

setwd("~/mofuss/")

# Configuration
col_selector = "1mc" # "1mc" to look at the first monte carlo, "mean" to look at the mean of all monte carlos.
admin_levels <- c("adm0") # Modify this to control which levels to process: c("adm0"), c("adm1"), c("adm2"), or any combination
add_random_variation <- TRUE

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

all_country_results <- list()

output_file <- paste0("assets/marginal_analysis_results_all_countries_", col_selector, "_", Sys.Date(), ".md")
file_conn <- file(output_file, "w")

adm_pattern_to_name <- c("adm0" = "Country", "adm1" = "Region", "adm2" = "District")


tryCatch({
    for (country in countries) {
        cat("\n=== Processing country:", country, "===\n")
        directory_path <- file.path(base_path, country)
        
        if (country != countries[1]) {
            writeLines("\n\\newpage\n", file_conn)
        }
        
        writeLines(paste0("# Marginal Analysis Results for ", toupper(country), "\n\n"), file_conn)
        writeLines(paste0("Results are for ", col_selector, " of all monte carlos.\n\n"), file_conn)
        
        combined_data_adm0 <- NULL
        combined_data_adm1 <- NULL
        combined_data_adm2 <- NULL
        
        for (filename_pattern in filename_patterns) {
            demand_col <- "demand_value"
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

            cat("\nProcessing filename pattern:", filename_pattern, "for country:", country, "\n")
            data <- process_files(directory_path, filename_pattern, y_col, x_col)
            # If country is Mozambique, add random variation nrb and harvest
            if (country == "mozambique" && add_random_variation == TRUE && str_detect(filename_pattern, "frcompl")) {
                if (str_detect(col_selector, "1mc")) {
                data <- data %>%
                    mutate(Harv_2020_2030_1MC = Harv_2020_2030_1MC * (1.1 + rnorm(n(), mean = 0, sd = 0.3)))
                } else if (str_detect(col_selector, "mean")) {
                    data <- data %>%
                        mutate(Harv_2020_2030_mean = Harv_2020_2030_mean * (1.1 + rnorm(n(), mean = 0, sd = 0.3)))
                }
            } else if (country == "mozambique" && add_random_variation == TRUE && str_detect(filename_pattern, "fr")) {
                if (str_detect(col_selector, "1mc")) {
                    data <- data %>%
                        mutate(NRB_2020_2030_1MC = NRB_2020_2030_1MC * (1.1 + rnorm(n(), mean = 0, sd = 0.3)))
                } else if (str_detect(col_selector, "mean")) {
                    data <- data %>%
                        mutate(NRB_2020_2030_mean = NRB_2020_2030_mean * (1.1 + rnorm(n(), mean = 0, sd = 0.3)))
                }
            }
            
            if (!is.null(data)) {
                cat("Data loaded successfully. Dimensions:", dim(data)[1], "x", dim(data)[2], "\n")
                cat("Columns:", paste(names(data), collapse=", "), "\n")
            } else {
                cat("No data loaded for this filename pattern\n")
            }
            
            combined_data <- data %>%
                filter(!is.na(.data[[y_col]]), !is.na(demand_value))
    
            if (str_detect(filename_pattern, "adm0")) {
                if (is.null(combined_data_adm0)) {
                    combined_data_adm0 <- combined_data
                } else {
                    combined_data_adm0 <- full_join(
                        combined_data_adm0,
                        combined_data,
                        by = c(x_col, "scenario", demand_col)
                    )
                }
            } else if (str_detect(filename_pattern, "adm1")) {
                if (is.null(combined_data_adm1)) {
                    combined_data_adm1 <- combined_data
                } else {
                    combined_data_adm1 <- full_join(
                        combined_data_adm1,
                        combined_data,
                        by = c(x_col, "scenario", demand_col)
                    )
                }
            } else if (str_detect(filename_pattern, "adm2")) {
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

            plotz <- create_plots(combined_data, x_col, y_col, filename_pattern, country)

            write(paste("\n![", filename_pattern, "](", basename(plotz$boxplot), ")\n"), file_conn, append = TRUE)
            write(paste("\n![", filename_pattern, "](", basename(plotz$scatter), ")\n"), file_conn, append = TRUE)

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
        
        # Process each admin level and collect results
        for (adm_level in names(data_list)) {
            if (!is.null(data_list[[adm_level]])) {
                data <- data_list[[adm_level]]
                cat("\nChecking data for", country, "-", adm_level, ":\n")
                cat("Data dimensions:", dim(data)[1], "x", dim(data)[2], "\n")
                cat("Required columns present:", 
                    nrb_col, ":", nrb_col %in% names(data), 
                    ", ", harvest_col, ":", harvest_col %in% names(data), "\n")
                
                if (all(c(nrb_col, harvest_col) %in% names(data))) {
                    cat("\nAnalyzing NRB vs Harvest for", country, "-", adm_level, "\n")
                    tryCatch({
                        # First create the marginal analysis plots
                        marginal_plot_name <- analyze_nrb_vs_harvest(
                            data = data,
                            nrb_col = nrb_col,
                            harvest_col = harvest_col,
                            demand_col = demand_col,
                            pattern = adm_level,
                            x_col = paste0("ADM_", substr(adm_level, 4, 4)),
                            country = country
                        )
                        cat("Marginal plot created:", marginal_plot_name, "\n")
                        
                        # Write the marginal analysis results to the report
                        writeLines(paste("\n## Marginal Analysis Results for", country, "-", adm_level, "\n"),
                            file_conn)
                        writeLines(paste("\n### Marginal Changes\n"), file_conn)
                        writeLines(paste("\n![Marginal Analysis](", basename(marginal_plot_name), ")\n"),
                            file_conn)
                        
                        # Then collect results for cross-country comparison
                        results <- collect_marginal_results(data, country, adm_level, nrb_col, harvest_col)
                        all_country_results[[paste(country, adm_level, sep = "_")]] <- results
                        cat("Results collected successfully\n")
                    }, error = function(e) {
                        cat("Error in analysis:", e$message, "\n")
                    })
                }
            }
        }
        
        # Debug point 4: After collecting all results for a country
        cat("\nResults collected for", country, ":\n")
        cat("Keys:", paste(grep(country, names(all_country_results), value=TRUE), collapse=", "), "\n")
    }
    
    # After all countries are processed
    cat("\n=== Starting cross-country analysis ===\n")
    cat("Number of results collected:", length(all_country_results), "\n")
    cat("Available result keys:", paste(names(all_country_results), collapse=", "), "\n")
    
    # Make sure the file connection is still open, it seems to close sometimes.
    if (!isOpen(file_conn)) {
        cat("File connection was closed, reopening...\n")
        file_conn <- file(output_file, "a")
    }
    
    writeLines("\n\\newpage\n", file_conn)
    writeLines("\n# Cross-Country Comparison\n\n", file_conn)
    
    if (length(all_country_results) > 0) {
        # Debug the data structure
        cat("\nExamining all_country_results structure:\n")
        for (key in names(all_country_results)) {
            result <- all_country_results[[key]]
            cat("\nKey:", key, "\n")
            cat("Full data dimensions:", dim(result$full_data)[1], "x", dim(result$full_data)[2], "\n")
            cat("Summary dimensions:", dim(result$summary)[1], "x", dim(result$summary)[2], "\n")
            cat("Full data columns:", paste(names(result$full_data), collapse=", "), "\n")
        }
        
        tryCatch({
            combined_data <- bind_rows(lapply(all_country_results, function(x) {
                if (is.null(x$full_data)) {
                    cat("Warning: NULL full_data found\n")
                    return(NULL)
                }
                x$full_data
            }))
            
            cat("\nCombined data summary:\n")
            cat("Dimensions:", dim(combined_data)[1], "x", dim(combined_data)[2], "\n")
            cat("Countries:", paste(unique(combined_data$country), collapse=", "), "\n")
            cat("Admin levels:", paste(unique(combined_data$adm_level), collapse=", "), "\n")
            cat("Demand values:", paste(sort(unique(combined_data$demand_value)), collapse=", "), "\n")
            cat("Marginal ratio summary:\n")
            print(summary(combined_data$marginal_ratio))
            writeLines("\n## Combined Data Summary\n", file_conn)
            writeLines(knitr::kable(combined_data %>% select(country, adm_level, demand_value, marginal_ratio, nrb_col, harvest_col) %>% distinct(), format = "markdown"), file_conn)
            
            sensitivity_results <- analyze_demand_sensitivity(combined_data)
            
            cat("\nSensitivity results structure:\n")
            print(str(sensitivity_results))
            
            sensitivity_plots <- create_demand_sensitivity_plots(combined_data, sensitivity_results, "assets")
            
            sensitivity_tables <- create_demand_sensitivity_tables(sensitivity_results)
            
            cat("\nSensitivity tables structure:\n")
            cat("Rankings dimensions:", dim(sensitivity_tables$sensitivity_ranking)[1], "x", 
                dim(sensitivity_tables$sensitivity_ranking)[2], "\n")
            cat("Transitions dimensions:", dim(sensitivity_tables$critical_transitions)[1], "x",
                dim(sensitivity_tables$critical_transitions)[2], "\n")
            
            writeLines("\n## Cross-Country Demand Sensitivity\n", file_conn)
            writeLines("\n### Sensitivity Slopes\n", file_conn)
            writeLines(paste0("![Sensitivity Slopes](", 
                            sensitivity_plots$sensitivity_slopes, ")\n\n"), file_conn)
            
            writeLines("\n### Step Changes in Sensitivity\n", file_conn)
            writeLines(paste0("![Step Sensitivity](", 
                            sensitivity_plots$step_sensitivity, ")\n\n"), file_conn)
            
            writeLines("\n### Response Curves\n", file_conn)
            writeLines(paste0("![Response Curves](", 
                            sensitivity_plots$response_curves, ")\n\n"), file_conn)
            
            writeLines("\n## Country-Level Sensitivity Rankings\n", file_conn)
            writeLines("The following table shows how sensitive each country is to demand changes:\n\n", file_conn)
            writeLines(knitr::kable(sensitivity_tables$sensitivity_ranking, format = "markdown"), file_conn)
            
            writeLines("\n## Most Sensitive Demand Transitions\n", file_conn)
            writeLines("These are the demand level transitions that showed the largest changes in marginal ratios:\n\n", file_conn)
            writeLines(knitr::kable(sensitivity_tables$critical_transitions, format = "markdown"), file_conn)
            
            flush(file_conn)
            
            cat("\nVerifying markdown file contents...\n")
            lines <- readLines(output_file)
            cat("Total lines in markdown file:", length(lines), "\n")
            cat("Last few lines:\n")
            cat(tail(lines, 10), sep="\n")
            
        }, error = function(e) {
            cat("\nError in cross-country analysis:", e$message, "\n")
            print(str(e))
        })
    }
    
    render(output_file, output_format = "pdf_document")
    
}, error = function(e) {
    cat("\nError:", e$message, "\n")
}, finally = {
    if (!is.null(file_conn) && isOpen(file_conn)) {
        close(file_conn)
    }
})

# nolint end

# nolint start: object_name_linter
# External dependencies you need to install: pandoc, pdflatex
# You may need to set the PATH to the texbin directory:
Sys.setenv(PATH=paste("/Library/TeX/texbin", Sys.getenv("PATH"), sep=":"))

if (!require("pacman")) install.packages("pacman")
library(pacman)
pacman::p_load(dplyr, ggplot2, readr, purrr, viridis, knitr, stringr, styler, rmarkdown, kableExtra, tidyr,zoo,zeallot)


source("localhost/scripts/utils/file_utils.R")
source("localhost/scripts/utils/plot_utils.R")
source("localhost/scripts/utils/data_utils.R")

# Configuration
setwd("~/mofuss/")
data_path <- "~/Dropbox/mofuss_data"
output_dir <- "assets"
col_selector = "1mc" # "1mc" to look at the first monte carlo, "mean" to look at the mean of all monte carlos.
admin_levels <- c("adm0") # Modify this to control which levels to process: c("adm0"), c("adm1"), c("adm2"), or any combination
add_random_variation <- TRUE
base_patterns <- c("", "_fr", "_frcompl")

filename_patterns <- unlist(lapply(admin_levels, function(level) {
    paste0(level, base_patterns, ".csv")
}))

countries <- list.dirs(data_path, full.names = FALSE, recursive = FALSE)
countries <- countries[!countries %in% c("", output_dir)]

all_country_results <- list()
output_file <- paste0(output_dir, "/marginal_analysis_results_all_countries_", col_selector, "_", Sys.Date(), ".md")
file_conn <- file(output_file, "w")
adm_pattern_to_name <- c("adm0" = "Country", "adm1" = "Region", "adm2" = "District")

if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
}


log_file <- file.path(output_dir, paste0("log_", Sys.Date(), ".txt"))


for (country in countries) {
    write_log(paste("=== Processing country:", country, "==="))
    directory_path <- file.path(data_path, country)
    
    if (country != countries[1]) {
        writeLines("\n\\newpage\n", file_conn)
    }
    
    writeLines(paste0("# Marginal Analysis Results for ", toupper(country), "\n\n"), file_conn)
    writeLines(paste0("Results are for ", col_selector, " of all monte carlos.\n\n"), file_conn)

    combined_data_adm0 <- NULL
    combined_data_adm1 <- NULL
    combined_data_adm2 <- NULL

    for (filename_pattern in filename_patterns) {
        write_log(paste("Processing filename pattern:", filename_pattern, "for country:", country))
        c(y_col, nrb_col, harvest_col, x_col, demand_col) %<-% get_column_names(filename_pattern, col_selector)

        tryCatch({
            data <- process_files(directory_path, filename_pattern, y_col, x_col)
        }, error = function(e) {
            error_handler(e, paste("processing files for", country, filename_pattern))
        }, warning = function(w) {
            warning_handler(w, paste("processing files for", country, filename_pattern))
        })

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
        
        data <- data %>%
        filter(!is.na(.data[[y_col]]), !is.na(demand_value))

        plotz <- create_plots(data, x_col, y_col, filename_pattern, country, output_dir = output_dir)

        write(paste("\n![", filename_pattern, "](", basename(plotz$boxplot), ")\n"), file_conn, append = TRUE)
        write(paste("\n![", filename_pattern, "](", basename(plotz$scatter), ")\n"), file_conn, append = TRUE)

        summary_stats <- data %>%
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

        write(paste("\nThere are", length(unique(data$filepath)), "unique files from different paths\n"),
            file_conn,
            append = TRUE
        )

        write("\n### Summary Statistics\n", file_conn, append = TRUE)
        write(knitr::kable(summary_stats, format = "markdown"),
            file_conn,
            append = TRUE
        )

        admin_data <- update_admin_data(filename_pattern, data, 
                                      combined_data_adm0, combined_data_adm1, 
                                      combined_data_adm2, x_col, demand_col)
        combined_data_adm0 <- admin_data$adm0
        combined_data_adm1 <- admin_data$adm1
        combined_data_adm2 <- admin_data$adm2
    }

    data_list <- list(
        adm0 = if("adm0" %in% admin_levels) combined_data_adm0 else NULL,
        adm1 = if("adm1" %in% admin_levels) combined_data_adm1 else NULL,
        adm2 = if("adm2" %in% admin_levels) combined_data_adm2 else NULL
    )
    data_list <- Filter(Negate(is.null), data_list)

    # Process each admin level and collect results
    for (adm_level in names(data_list)) {
        if (!is.null(data_list[[adm_level]])) {
            data <- data_list[[adm_level]]            
            if (all(c(nrb_col, harvest_col) %in% names(data))) {
                write_log("\nAnalyzing NRB vs Harvest for", country, "-", adm_level)
                tryCatch({
                    plot_names <- analyze_nrb_vs_harvest(
                        data = data,
                        nrb_col = nrb_col,
                        harvest_col = harvest_col,
                        demand_col = demand_col,
                        pattern = adm_level,
                        x_col = paste0("ADM_", substr(adm_level, 4, 4)),
                        country = country,
                        output_dir = output_dir
                    )
                    
                    writeLines(paste("\n## Marginal Analysis Results for", country, "-", adm_level, "\n"),
                        file_conn)
                    
                    print(plot_names)
                    for (plot_name in plot_names) {
                        if (file.exists(plot_name)) {
                            write(paste("\n![", plot_name, "](", basename(plot_name), ")\n"), file_conn, append = TRUE)
                        } else {
                            write_log("Plot not found:", plot_name)
                        }
                    }
                    
                    results <- collect_marginal_results(data, country, adm_level, nrb_col, harvest_col)
                    all_country_results[[paste(country, adm_level, sep = "_")]] <- results
                }, error = function(e) {
                    error_handler(e, paste("analyzing NRB vs harvest for", country, "-", adm_level))
                })
            }
        }
    }
    
    # Debug point 4: After collecting all results for a country
    write_log("\nResults collected for", country, ":")
    write_log("Keys:", paste(grep(country, names(all_country_results), value=TRUE), collapse=", "))
}

# After all countries are processed
write_log("\n=== Starting cross-country analysis ===")
write_log(paste("Number of results collected:", length(all_country_results)))

# # Make sure the file connection is still open, it seems to close sometimes.
if (!isOpen(file_conn)) {
    write_log("File connection was closed, reopening...")
    file_conn <- file(output_file, "a")
}

writeLines("\n\\newpage\n", file_conn)
writeLines("\n# Cross-Country Comparison\n\n", file_conn)

if (length(all_country_results) > 0) {
    
    combined_data <- bind_rows(lapply(all_country_results, function(x) {
        if (is.null(x) || is.null(x$full_data)) {
            write_log("Warning: NULL entry found in results")
            return(NULL)
        }
        required_cols <- c("country", "adm_level", "demand_value", "marginal_ratio")
        missing_cols <- setdiff(required_cols, names(x$full_data))
        if (length(missing_cols) > 0) {
            write_log(paste("Warning: Missing columns:", paste(missing_cols, collapse=", ")))
            return(NULL)
        }
        return(x$full_data)
    }))
    

    write_log("\nCombined data summary:")
    write_log(paste("Countries:", paste(unique(combined_data$country), collapse=", ")))
    write_log(paste("Admin levels:", paste(unique(combined_data$adm_level), collapse=", ")))
    write_log(paste("Demand values:", paste(sort(unique(combined_data$demand_value)), collapse=", ")))
    write_log("Marginal ratio summary:")
    print(summary(combined_data$marginal_ratio))
    writeLines("\n## Combined Data Summary\n", file_conn)
    writeLines(knitr::kable(combined_data %>% select(country, adm_level, demand_value, marginal_ratio, nrb_col, harvest_col) %>% distinct(), format = "markdown"), file_conn)
    
    sensitivity_results <- analyze_demand_sensitivity(combined_data)
    
    sensitivity_plots <- create_demand_sensitivity_plots(combined_data, sensitivity_results, output_dir = output_dir)
    
    sensitivity_tables <- create_demand_sensitivity_tables(sensitivity_results)
    
    writeLines("\n## Cross-Country Demand Sensitivity\n", file_conn)
    writeLines("\n### Sensitivity Slopes\n", file_conn)
    writeLines(paste0("![Sensitivity Slopes](", 
                    basename(sensitivity_plots$sensitivity_slopes), ")\n\n"), file_conn)
    
    writeLines("\n### Step Changes in Sensitivity\n", file_conn)
    writeLines(paste0("![Step Sensitivity](", 
                    basename(sensitivity_plots$step_sensitivity), ")\n\n"), file_conn)
    
    writeLines("\n### Response Curves\n", file_conn)
    writeLines(paste0("![Response Curves](", 
                    basename(sensitivity_plots$response_curves), ")\n\n"), file_conn)
    
    writeLines("\n## Country-Level Sensitivity Rankings\n", file_conn)
    writeLines("The following table shows how sensitive each country is to demand changes:\n\n", file_conn)
    writeLines(knitr::kable(sensitivity_tables$sensitivity_ranking, format = "markdown"), file_conn)
    
    writeLines("\n## Most Sensitive Demand Transitions\n", file_conn)
    writeLines("These are the demand level transitions that showed the largest changes in marginal ratios:\n\n", file_conn)
    writeLines(knitr::kable(sensitivity_tables$critical_transitions, format = "markdown"), file_conn)
    
    flush(file_conn)
}

render(output_file, output_format = "pdf_document")

write_log("=== Analysis complete ===")

# nolint end

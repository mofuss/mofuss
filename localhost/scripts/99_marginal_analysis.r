# nolint start: object_name_linter
# External dependencies you need to install: pandoc, pdflatex
# You may need to set the PATH to the texbin directory:
Sys.setenv(PATH = paste("/Library/TeX/texbin", Sys.getenv("PATH"), sep = ":"))

if (!require("pacman")) install.packages("pacman")
library(pacman)
pacman::p_load(dplyr, ggplot2, readr, purrr, viridis, knitr, stringr, styler, rmarkdown, kableExtra, tidyr, zoo, zeallot, patchwork)

source("localhost/scripts/utils/file_utils.R")
source("localhost/scripts/utils/plot_utils.R")
source("localhost/scripts/utils/data_utils.R")
source("localhost/scripts/utils/tests.R")

# Configuration
setwd("~/mofuss/")
data_path <- "~/Dropbox/mofuss_data"
output_dir <- "assets"
col_selector <- "1mc" # "1mc" to look at the first monte carlo, "mean" to look at the mean of all monte carlos.
admin_levels <- c("adm0") # Modify this to control which levels to process: c("adm0"), c("adm1"), c("adm2"), or any combination
add_random_variation <- TRUE
filename_pattern <- "frcompl.csv"


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

    for (admin_level in admin_levels) {
        c(harvest_col, nrb_col, fnrb_col, x_col) %<-% get_column_names(col_selector, admin_level)
        admin_filename_pattern <- paste0(admin_level, ".*", filename_pattern)

        data <- process_files(directory_path, admin_filename_pattern, nrb_col, harvest_col, fnrb_col, x_col)

        data <- data %>%
            filter(!is.na(demand_value))

        plotz <- create_plots(data, x_col, c(nrb_col, harvest_col),
            admin_level, country,
            output_dir = output_dir
        )
        write(paste("\n![", filename_pattern, "](", basename(plotz), ")\n"),
            file_conn,
            append = TRUE
        )

        # Get combined marginal and summary statistics
        marginal_data_list <- collect_marginal_data(data = data, country = country, admin_level = admin_level, fnrb_col = fnrb_col, nrb_col = nrb_col, harvest_col = harvest_col)
        all_country_results[[paste(country, admin_level, sep = "_")]] <- marginal_data_list

        write(paste("\n### Combined Summary and Marginal Analysis for", country, "-", admin_level, "\n"), file_conn, append = TRUE)
        write(
            knitr::kable(
                marginal_data_list$summary %>%
                    select(-admin_level, -scenario, -mean_marginal_nrb, -mean_marginal_harvest) %>%
                    rename_with(~ str_replace_all(., "_", " ")),
                format = "markdown",
                digits = 2
            ),
            file_conn,
            append = TRUE
        )

        write_log("\nAnalyzing NRB vs Harvest for", country, "-", admin_level)
        marginal_plot_filename <- analyze_nrb_vs_harvest(
            marginal_data = marginal_data_list$full_data,
            nrb_col = nrb_col,
            harvest_col = harvest_col,
            demand_col = demand_col,
            pattern = admin_level,
            x_col = x_col,
            country = country,
            output_dir = output_dir
        )

        write(paste("\n![", marginal_plot_filename, "](", basename(marginal_plot_filename), ")\n"), file_conn, append = TRUE)
    }
}

# Make sure the file connection is still open, it seems to close sometimes.
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
        return(x$full_data)
    })) %>% distinct()


    formatted_data <- combined_data %>%
        select(country, admin_level, demand_value, marginal_fnrb, nrb_col, harvest_col, fnrb_col) %>%
        distinct() %>%
        mutate(
            across(where(is.character), ~ str_replace_all(., "_", " ")),
            across(where(is.character), str_to_title)
        )

    formatted_colnames <- names(formatted_data) %>%
        str_replace_all("_", " ") %>%
        str_to_title()

    writeLines("\n## Combined Data Summary\n", file_conn)
    writeLines(knitr::kable(formatted_data,
        format = "markdown",
        digits = 2,
        col.names = formatted_colnames,
        align = rep(c("l", "r"), length.out = ncol(formatted_data)) # left align text, right align numbers
    ), file_conn)

    sensitivity_results <- analyze_demand_sensitivity(combined_data)

    sensitivity_plots <- create_demand_sensitivity_plots(combined_data, sensitivity_results, output_dir = output_dir)

    sensitivity_tables <- create_demand_sensitivity_tables(sensitivity_results)

    writeLines("\n## Cross-Country Demand Sensitivity\n", file_conn)
    writeLines("\n### Sensitivity Slopes\n", file_conn)
    writeLines(paste0(
        "![Sensitivity Slopes](",
        basename(sensitivity_plots$sensitivity_slopes), ")\n\n"
    ), file_conn)

    writeLines("\n### Step Changes in Sensitivity\n", file_conn)
    writeLines(paste0(
        "![Step Sensitivity](",
        basename(sensitivity_plots$step_sensitivity), ")\n\n"
    ), file_conn)

    writeLines("\n### Response Curves\n", file_conn)
    writeLines(paste0(
        "![Response Curves](",
        basename(sensitivity_plots$response_curves), ")\n\n"
    ), file_conn)

    writeLines("\n## Country-Level Sensitivity Rankings\n", file_conn)
    writeLines("The following table shows how sensitive each country is to demand changes:\n\n", file_conn)
    writeLines(knitr::kable(sensitivity_tables$sensitivity_ranking, format = "markdown", digits = 2), file_conn)

    writeLines("\n## Most Sensitive Demand Transitions\n", file_conn)
    writeLines("These are the demand level transitions that showed the largest changes in marginal ratios:\n\n", file_conn)
    writeLines(knitr::kable(sensitivity_tables$critical_transitions, format = "markdown", digits = 2), file_conn)

    # Add new cross-country plots
    writeLines("\n\n# Cross-Country FNRB Distributions\n\n", file_conn)
    cross_country_plot_list <- create_cross_country_plots(all_country_results, output_dir = output_dir)

    writeLines(paste0("## Calculated FNRB Distribution by Country and Scenario\n\n"), file_conn)
    writeLines(paste0(
        "![Calculated FNRB Distribution](",
        basename(cross_country_plot_list$calculated_fnrb_plot_file), ")\n\n"
    ), file_conn)

    writeLines(paste0("## Marginal FNRB Distribution by Country and Scenario\n\n"), file_conn)
    writeLines(paste0(
        "![Marginal FNRB Distribution](",
        basename(cross_country_plot_list$marginal_fnrb_plot_file), ")\n\n"
    ), file_conn)

    # Optionally, if you also want to include the other two plots from the function:
    # writeLines(paste0("## Marginal FNRB Distribution (Filled by Country)\n\n"), file_conn)
    # writeLines(paste0("![Marginal FNRB Distribution Filled by Country](",
    #                 basename(cross_country_plot_list$original_comparison_plot_file), ")\n\n"), file_conn)
    #
    # writeLines(paste0("## Marginal FNRB Trends vs Demand\n\n"), file_conn)
    # writeLines(paste0("![Marginal FNRB Trends](",
    #                 basename(cross_country_plot_list$trends_plot_file), ")\n\n"), file_conn)

    flush(file_conn)
}

render(output_file, output_format = "pdf_document")

test_fun_fnrb_cals(combined_data, "Tanzania")
test_fun_fnrb_cals(combined_data, "Malawi")
test_fun_fnrb_cals(combined_data, "Kenya")
# nolint end

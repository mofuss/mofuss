# nolint start: object_name_linter
# style_file("localhost/scripts/99_marginal_analysis.r")
library(pacman)
pacman::p_load(dplyr, ggplot2, readr, purrr, viridis, knitr, stringr, styler, rmarkdown)
setwd("~/mofuss/")
country <- "tanzania"
directory_path <- paste0("~/mofuss_data/", country)
filename_patterns <- c("adm0.csv", "adm1.csv", "adm2.csv", "adm0_fr.csv", "adm1_fr.csv", "adm2_fr.csv", "adm0_frcompl.csv", "adm1_frcompl.csv", "adm2_frcompl.csv")
if (!dir.exists("assets")) {
    dir.create("assets")
}
output_file <- paste0("assets/marginal_analysis_results_", country, "_", Sys.Date(), ".md")
file_conn <- file(output_file, "w")

process_files <- function(directory_path, pattern, y_col) {
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
                cat("Attempting to read:", file, "\n")
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

                    # Check and rename columns if they exist
                    if (any(grepl("^NAME_", names(temp_df)))) {
                        temp_df <- temp_df %>%
                            rename_with(~ gsub("^NAME_", "ADM_", .), .cols = starts_with("NAME_"))
                    }

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

            bind_rows(all_data)
        },
        error = function(e) {
            stop(paste("Error reading files:", e$message))
        }
    )

    return(combined_df)
}

create_plots <- function(data, x_col, y_col, pattern) {
    lm_formula <- as.formula(paste(y_col, "~ demand_value"))
    lm_fit <- lm(lm_formula, data = data)
    lm_eq <- paste0(
        "y = ", round(coef(lm_fit)[1], 4), " + ",
        round(coef(lm_fit)[2], 4), "x\n",
        "R² = ", round(summary(lm_fit)$r.squared, 4)
    )

    p <- ggplot(data, aes(x = demand_value, y = .data[[y_col]])) +
        geom_point(alpha = 0.1, position = position_jitter(width = 0.2)) +
        geom_boxplot(aes(group = demand_value, fill = scenario), alpha = 0.7) +
        geom_smooth(method = "lm", color = "red", se = TRUE) +
        scale_fill_viridis_d() +
        scale_x_continuous(breaks = sort(unique(data$demand_value))) +
        xlim(min(data$demand_value) - 5, max(data$demand_value) + 5) +
        ylim(min(data[[y_col]]) - 0.05, max(data[[y_col]]) + 0.05) +
        theme(
            legend.position = "right",
            legend.title = element_text(size = 10),
            legend.text = element_text(size = 8),
            plot.title = element_text(hjust = 0.5)
        ) +
        annotate("text",
            x = min(data$demand_value) - 4,
            y = max(data[[y_col]]) + 0.03,
            label = lm_eq,
            hjust = 0, vjust = 1
        ) +
        labs(
            title = paste(y_col, "Distribution by Demand Scenario -", pattern),
            x = "Demand Change (%)",
            y = y_col,
            fill = "Scenario"
        )

    plot_filename_boxplot <- paste0("assets/boxplot_", gsub("\\.csv$", "", pattern), ".png")
    ggsave(plot_filename_boxplot, p, width = 10, height = 6)
    p_scatter <- ggplot(data, aes(x = .data[[x_col]], y = .data[[y_col]], color = scenario)) +
        geom_point(alpha = 0.7, position = position_jitter(width = 0.05)) +
        scale_color_viridis_d() +
        labs(
            title = paste(y_col, "Distribution by Demand Scenario -", pattern),
            x = x_col,
            y = y_col,
            color = "Scenario"
        )

    plot_filename_scatter <- paste0("assets/scatter_", gsub("\\.csv$", "", pattern), ".png")
    ggsave(plot_filename_scatter, p_scatter, width = 10, height = 6)

    return(list(boxplot = plot_filename_boxplot, scatter = plot_filename_scatter))
}

# Function to analyze change in NRB over change in harvest with changing demand
analyze_nrb_vs_harvest <- function(data, nrb_col, harvest_col, demand_col, pattern, x_col) {
    lm_formula <- as.formula(paste(nrb_col, "~", harvest_col, "+", demand_col))
    lm_fit <- lm(lm_formula, data = data)
    lm_eq <- paste0(
        "y = ", round(coef(lm_fit)[1], 4), " + ",
        round(coef(lm_fit)[2], 4), "x1 + ",
        round(coef(lm_fit)[3], 4), "x2\n",
        "R² = ", round(summary(lm_fit)$r.squared, 4)
    )

    p <- ggplot(data, aes_string(x = harvest_col, y = nrb_col, color = demand_col)) +
        geom_point(alpha = 0.5) +
        geom_smooth(method = "lm", se = TRUE, color = "red") +
        scale_color_viridis_c() +
        labs(
            title = paste("Change in", nrb_col, "over Change in", harvest_col, "with Demand -", pattern),
            x = paste("Change in", harvest_col),
            y = paste("Change in", nrb_col),
            color = "Demand"
        ) +
        annotate("text",
            x = min(data[[harvest_col]]),
            y = max(data[[nrb_col]]),
            label = lm_eq,
            hjust = 0, vjust = 1
        )

    plot_filename <- paste0("assets/nrb_vs_harvest_", x_col, gsub("\\.csv$", "", pattern), ".png")
    ggsave(plot_filename, p, width = 10, height = 6)

    return(plot_filename)
}

combined_data_adm0 <- NULL
combined_data_adm1 <- NULL
combined_data_adm2 <- NULL

for (pattern in filename_patterns) {
    y_col <- case_when(
        str_detect(pattern, "frcompl") ~ "Harv_2020_2030_mean",
        str_detect(pattern, "fr") ~ "NRB_2020_2030_mean",
        TRUE ~ "fNRB"
    )

    combined_data <- process_files(directory_path, pattern, y_col)
    x_col <- case_when(
        str_detect(pattern, "adm0") ~ "ADM_0",
        str_detect(pattern, "adm1") ~ "ADM_1",
        str_detect(pattern, "adm2") ~ "ADM_2",
        TRUE ~ NA_character_
    )

    if (is.na(x_col)) {
        stop(paste("Unknown pattern:", pattern))
    }

    combined_data <- combined_data %>%
        filter(!is.na(.data[[y_col]]), !is.na(demand_value))

    if (is.null(combined_data_all) && str_detect(pattern, "adm0")) {
        combined_data_adm0 <- combined_data
    } else if (is.null(combined_data_all) && str_detect(pattern, "adm1")) {
        combined_data_adm1 <- combined_data
    } else if (is.null(combined_data_all) && str_detect(pattern, "adm2")) {
        combined_data_adm2 <- combined_data
    } else if (str_detect(pattern, "adm0")) {
        combined_data_adm0 <- left_join(combined_data_adm0, combined_data, by = x_col)
    } else if (str_detect(pattern, "adm1")) {
        combined_data_adm1 <- left_join(combined_data_adm1, combined_data, by = x_col)
    } else if (str_detect(pattern, "adm2")) {
        combined_data_adm2 <- left_join(combined_data_adm2, combined_data, by = x_col)
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


# Determine columns based on pattern
nrb_col <- "NRB_2020_2030_mean"
harvest_col <- "Harv_2020_2030_mean"

# List of dataframes
data_list <- list(combined_data_adm0, combined_data_adm1, combined_data_adm2)

for (i in seq_along(data_list)) {
    data <- data_list[[i]]

    plot_filename <- analyze_nrb_vs_harvest(data, nrb_col, harvest_col, demand_col, pattern, x_col[i])

    # write(paste("\n## NRB vs Harvest Results for", pattern, " - Dataframe", i, "\n"), file_conn, append = TRUE)
    # write(paste("\n![", pattern, "](", basename(plot_filename), ")\n"), file_conn, append = TRUE)
}


close(file_conn)

# nolint end

render(output_file, output_format = "pdf_document")

options(error = function() {
    message("An error occurred.")
    print(last_trace())
})

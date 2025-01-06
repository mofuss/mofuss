# nolint start: object_name_linter
library(ggplot2)
library(viridis)
source("localhost/scripts/utils/file_utils.R")

create_plots <- function(data, x_col, y_col, filename_pattern, country, output_dir) {
    write_log(paste("Creating plots for", country, "with pattern", filename_pattern))
    
    tryCatch({
        lm_formula <- as.formula(paste(y_col, "~ demand_value"))
        lm_fit <- lm(lm_formula, data = data)
        lm_eq <- paste0(
            "y = ", round(coef(lm_fit)[1], 4), " + ",
            round(coef(lm_fit)[2], 4), "x\n",
            "R² = ", round(summary(lm_fit)$r.squared, 4)
        )

        groups_with_multiple_points <- data %>%
            group_by(demand_value, scenario) %>%
            summarise(n = n(), .groups = 'drop') %>%
            filter(n > 1)

        p <- ggplot(data, aes(x = demand_value, y = .data[[y_col]])) +
            # Only draw boxplots for groups with multiple points
            {if(nrow(groups_with_multiple_points) > 0)
                geom_boxplot(data = data %>% 
                                semi_join(groups_with_multiple_points, 
                                        by = c("demand_value", "scenario")),
                            aes(group = demand_value, fill = scenario), 
                            alpha = 0.7, 
                            na.rm = TRUE)
            } +
            geom_point(aes(color = scenario),
                       alpha = 0.6,
                       size = 2,
                       position = position_jitter(width = 0.1),
                       na.rm = TRUE) +
            geom_smooth(method = "lm", color = "red", se = TRUE, na.rm = TRUE, formula = y ~ x) +
            scale_fill_viridis_d(drop = FALSE) +
            scale_x_continuous(breaks = sort(unique(data$demand_value))) +
            coord_cartesian(
                xlim = c(min(data$demand_value) - 5, max(data$demand_value) + 5),
                ylim = c(min(data[[y_col]]) - 0.05, max(data[[y_col]]) + 0.05)
            ) +
            theme(
                legend.position = "right",
                legend.title = element_text(size = 10),
                legend.text = element_text(size = 8),
                plot.title = element_text(hjust = 0.5),
                axis.text.x = element_text(angle = 45, hjust = 1)
            ) +
            annotate("text",
                x = min(data$demand_value) - 4,
                y = max(data[[y_col]]) + 0.03,
                label = lm_eq,
                hjust = 0, vjust = 1
            ) +
            labs(
                title = paste(y_col, "Distribution by Demand Scenario -", filename_pattern),
                x = "Demand Change (%)",
                y = y_col,
                fill = "Scenario"
            )

        plot_filename_boxplot <- file.path(output_dir, paste0("boxplot_", country, "_", gsub("\\.csv$", "", filename_pattern), ".png"))
        ggsave(plot_filename_boxplot, p, width = 10, height = 6)

        p_scatter <- ggplot(data, aes(x = .data[[x_col]], y = .data[[y_col]], color = scenario)) +
            geom_point(alpha = 0.7, position = position_jitter(width = 0.05)) +
            scale_color_viridis_d(drop = FALSE) +
            theme(
                axis.text.x = element_text(angle = 45, hjust = 1)
            ) +
            labs(
                title = paste(y_col, "Distribution by Demand Scenario -", filename_pattern),
                x = x_col,
                y = y_col,
                color = "Scenario"
            )

        plot_filename_scatter <- file.path(output_dir, paste0("scatter_", country, "_", gsub("\\.csv$", "", filename_pattern), ".png"))
        ggsave(plot_filename_scatter, p_scatter, width = 10, height = 6)

        write_log(paste("Successfully created plots for", country))
        return(list(boxplot = plot_filename_boxplot, scatter = plot_filename_scatter))
        
    }, error = function(e) {
        error_handler(e, paste("create_plots for", country, filename_pattern))
        return(NULL)
    }, warning = function(w) {
        warning_handler(w, paste("create_plots for", country, filename_pattern))
    })
}

analyze_nrb_vs_harvest <- function(data, nrb_col, harvest_col, demand_col, pattern, x_col, country, output_dir) {
    write_log("\n=== Starting analyze_nrb_vs_harvest ===")
    write_log(paste("Pattern:", pattern))
    write_log(paste("x_col:", x_col))
    write_log(paste("nrb_col:", nrb_col))
    write_log(paste("harvest_col:", harvest_col))
    
    validate_data_for_marginals(data, nrb_col, x_col)
    validate_data_for_marginals(data, harvest_col, x_col)
    
    bau_values <- data %>%
        filter(scenario == "bau") %>%
        select(all_of(c(x_col, nrb_col, harvest_col))) %>%
        rename(
            nrb_bau = all_of(nrb_col),
            harvest_bau = all_of(harvest_col)
        )
    
    write_log("\nBAU values summary:")
    write_log(capture.output(summary(bau_values)))
    
    if(nrow(bau_values) == 0) {
        stop("No BAU values found in the data")
    }
    
    marginal_data <- data %>%
        left_join(bau_values, by = x_col) %>%
        mutate(
            marginal_nrb = .data[[nrb_col]] - nrb_bau,
            marginal_harvest = .data[[harvest_col]] - harvest_bau,
            marginal_ratio = case_when(
                scenario == "bau" ~ 0,
                abs(marginal_harvest) < 1e-10 ~ NA_real_,  # Avoid division by very small numbers
                TRUE ~ 100 * marginal_nrb / marginal_harvest
            )
        ) %>%
        filter(scenario != "bau")  # Remove BAU since marginal change will be 0
    
    write_log(paste("\nMarginal data dimensions:", dim(marginal_data)[1], "rows,", 
                    dim(marginal_data)[2], "columns"))
    write_log("Marginal ratio summary:")
    write_log(capture.output(summary(marginal_data$marginal_ratio)))
    write_log(paste("Number of NA ratios:", sum(is.na(marginal_data$marginal_ratio))))
    
    if(nrow(marginal_data) == 0) {
        stop("No valid data points after calculating marginal ratios")
    }
    
    # Plot for marginal_nrb
    p_nrb <- ggplot(marginal_data, aes(x = demand_value, y = marginal_nrb)) +
        geom_point(aes(color = scenario), alpha = 0.5, na.rm = TRUE) +
        geom_smooth(method = "lm", color = "blue", se = TRUE, na.rm = TRUE, formula = y ~ x) +
        scale_color_viridis_d(drop = FALSE) +
        theme(
            legend.position = "right",
            axis.text.x = element_text(angle = 45, hjust = 1)
        ) +
        labs(
            title = paste("Marginal NRB vs Demand Change -", pattern),
            x = "Demand Change (%)",
            y = "Marginal NRB",
            color = "Scenario"
        )
    
    plot_filename_nrb <- file.path(output_dir, paste0("marginal_nrb_vs_demand_", country, "_", x_col, "_", pattern, ".png"))
    ggsave(plot_filename_nrb, p_nrb, width = 10, height = 6)
    
    # Plot for marginal_harvest
    p_harvest <- ggplot(marginal_data, aes(x = demand_value, y = marginal_harvest)) +
        geom_point(aes(color = scenario), alpha = 0.6, na.rm = TRUE) +
        geom_smooth(method = "lm", color = "green", se = TRUE, na.rm = TRUE, formula = y ~ x) +
        scale_color_viridis_d(drop = FALSE) +
        theme(
            legend.position = "right",
            axis.text.x = element_text(angle = 45, hjust = 1)
        ) +
        labs(
            title = paste("Marginal Harvest vs Demand Change -", pattern),
            x = "Demand Change (%)",
            y = "Marginal Harvest",
            color = "Scenario"
        )
    
    plot_filename_harvest <- file.path(output_dir, paste0("marginal_harvest_vs_demand_", country, "_", x_col, "_", pattern, ".png"))
    ggsave(plot_filename_harvest, p_harvest, width = 10, height = 6)
    
    # Plot for marginal_ratio
    p <- ggplot(marginal_data, aes(x = demand_value, y = marginal_ratio)) +
        geom_point(aes(color = scenario), alpha = 0.6, na.rm = TRUE) +
        geom_boxplot(aes(group = demand_value, fill = scenario), alpha = 0.3, na.rm = TRUE) +
        geom_smooth(method = "lm", color = "red", se = TRUE, na.rm = TRUE, formula = y ~ x) +
        scale_color_viridis_d(drop = FALSE) +
        scale_fill_viridis_d(drop = FALSE) +
        coord_cartesian(ylim = quantile(marginal_data$marginal_ratio, c(0.01, 0.99), na.rm = TRUE)) +
        theme(
            legend.position = "right",
            axis.text.x = element_text(angle = 45, hjust = 1)
        ) +
        labs(
            title = paste("Marginal NRB/Marginal Harvest Ratio vs Demand Change -", pattern),
            subtitle = "Ratio = 100 * (NRB - BAU_NRB)/(Harvest - BAU_Harvest)",
            x = "Demand Change (%)",
            y = "Marginal NRB/Harvest Ratio (%)",
            color = "Scenario",
            fill = "Scenario"
        )

    plot_filename <- file.path(output_dir, paste0("marginal_ratio_vs_demand_", country, "_", x_col, "_", pattern, ".png"))
    write_log(paste("\nSaving plot to:", plot_filename))
    
    tryCatch({
        ggsave(plot_filename, p, width = 10, height = 6)
        write_log("Plot saved successfully")
    }, error = function(e) {
        write_log(paste("Error saving plot:", e$message))
    })

    write_log("=== Finished analyze_nrb_vs_harvest ===\n")
    return(list(
        marginal_ratio_plot = plot_filename,
        marginal_nrb_plot = plot_filename_nrb,
        marginal_harvest_plot = plot_filename_harvest
    ))
} 


create_demand_sensitivity_plots <- function(combined_data, sensitivity_results, output_dir) {
    write_log("Creating demand sensitivity plots")


    countries <- paste(sort(unique(combined_data$country)), collapse="_")
    
    plots <- list()
    
    write_log("\nCreating slope visualization plot...")
    p1 <- ggplot(sensitivity_results$slopes, 
                aes(x = demand_value, y = point_slope, color = country)) +
        geom_point(alpha = 0.6) +
        geom_line(aes(group = country)) +
        facet_wrap(~adm_level, scales = "free_y") +
        theme(
            legend.position = "right",
            axis.text.x = element_text(angle = 45, hjust = 1)
        ) +
        labs(
            title = "Demand Sensitivity Analysis",
            subtitle = "How marginal ratios change with demand",
            x = "Demand Change (%)",
            y = "Point Sensitivity (Δ Ratio / Δ Demand)",
            color = "Country"
        )
    plots$p1 <- p1
    slopes_filename <- paste0("demand_sensitivity_slopes_", countries, ".png")
    ggsave(file.path(output_dir, slopes_filename), p1, width = 12, height = 8)
    write_log(paste("demand_sensitivity_slopes_", countries, ".png created and saved successfully"))
    
    write_log("\nCreating step sensitivity heatmap...")
    p2 <- ggplot(sensitivity_results$step_changes %>% filter(!is.na(step_sensitivity)),
                aes(x = demand_value, y = country, fill = step_sensitivity)) +
        geom_tile() +
        facet_wrap(~adm_level) +
        scale_fill_viridis_c() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(
            title = "Demand Step Sensitivity Analysis",
            subtitle = "Change in Marginal Ratio per Unit Demand Change",
            x = "Demand Level",
            y = "Country",
            fill = "Step Sensitivity"
        )
    plots$p2 <- p2
    steps_filename <- paste0("demand_step_sensitivity_", countries, ".png")
    ggsave(file.path(output_dir, steps_filename), p2, width = 12, height = 8)
    write_log("demand_step_sensitivity_", countries, ".png created and saved successfully")
    
    write_log("\nCreating marginal ratio trends plot...")
    p3 <- ggplot(combined_data, aes(x = demand_value, y = marginal_ratio, color = country)) +
        geom_point(alpha = 0.6) +
        geom_smooth(method = "lm", formula = y ~ x, se = TRUE) +
        facet_wrap(~adm_level, scales = "free_y") +
        coord_cartesian(
            ylim = quantile(combined_data$marginal_ratio, c(0.01, 0.99), na.rm = TRUE)
        ) +
        theme(legend.position = "right") +
        labs(
            title = "Marginal Ratio Response to Demand Changes",
            x = "Demand Change from BAU (%)",
            y = "Marginal NRB/Harvest Ratio (%)",
            color = "Country"
        )
    plots$p3 <- p3
    curves_filename <- paste0("demand_response_curves_", countries, ".png")
    ggsave(file.path(output_dir, curves_filename), p3, width = 12, height = 8)
    write_log("demand_response_curves_", countries, ".png created and saved successfully")
    
    write_log("Successfully created sensitivity plots")
    return(list(
        sensitivity_slopes = slopes_filename,
        step_sensitivity = steps_filename,
        response_curves = curves_filename
    ))
}



create_cross_country_plots <- function(all_results, output_dir) {
    combined_data <- bind_rows(lapply(all_results, function(x) x$full_data))
    
    p1 <- ggplot(combined_data, aes(x = country, y = marginal_ratio)) +
        geom_boxplot(aes(fill = country), alpha = 0.7) +
        facet_grid(adm_level ~ demand_value) +
        theme(
            axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "right"
        ) +
        labs(
            title = "Marginal NRB/Harvest Ratio Comparison Across Countries",
            x = "Country",
            y = "Marginal Ratio (%)",
            fill = "Country"
        )
    
    ggsave(file.path(output_dir, "cross_country_comparison.png"), p1, width = 12, height = 8)

    p2 <- ggplot(combined_data, aes(x = demand_value, y = marginal_ratio, color = country)) +
        geom_point(alpha = 0.3) +
        geom_smooth(method = "lm", se = TRUE) +
        facet_wrap(~adm_level) +
        theme(legend.position = "right") +
        labs(
            title = "Marginal Ratio vs Demand Change by Country",
            x = "Demand Change (%)",
            y = "Marginal Ratio (%)",
            color = "Country"
        )
    
    ggsave(file.path(output_dir, "cross_country_trends.png"), p2, width = 12, height = 8)
    
    return(list(comparison = p1, trends = p2))
}


# nolint end
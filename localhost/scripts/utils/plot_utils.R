# nolint start: object_name_linter
library(ggplot2)
library(patchwork)
library(viridis)
source("localhost/scripts/utils/file_utils.R")

create_plots <- function(data, x_col, cols, filename_pattern, country, output_dir) {
    
    create_subplot <- function(data, y_col) {
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
            scale_color_viridis_d(drop = FALSE) +
            scale_x_continuous(breaks = sort(unique(data$demand_value))) +
            coord_cartesian(
                xlim = c(min(data$demand_value) - 5, max(data$demand_value) + 5),
                ylim = c(min(data[[y_col]], na.rm = TRUE) - 0.05, 
                        max(data[[y_col]], na.rm = TRUE) + 0.05)
            ) +
            theme(
                legend.position = "none",  # We'll use a shared legend
                plot.title = element_text(hjust = 0.5),
                axis.text.x = element_text(angle = 45, hjust = 1)
            ) +
            annotate("text",
                x = min(data$demand_value) - 4,
                y = max(data[[y_col]], na.rm = TRUE) + 0.03,
                label = lm_eq,
                hjust = 0, vjust = 1
            ) +
            labs(
                title = y_col,
                x = NULL,  # We'll add a shared x-axis label
                y = y_col
            )
        return(p)
    }

    # Create subplots for each column
    plots <- lapply(cols, function(col) {
        create_subplot(data %>% filter(!is.na(.data[[col]]), !is.na(demand_value)), col)
    })

    # Combine plots using patchwork
    combined_plot <- wrap_plots(plots, ncol = 1) +
        plot_layout(guides = "collect") +  # Collect legends into one
        plot_annotation(
            title = paste(country, "Distribution by Demand Scenario -", filename_pattern),
            theme = theme(
                plot.title = element_text(hjust = 0.5),
                legend.position = "bottom",
                legend.box = "horizontal"
            )
        ) &
        theme(plot.margin = margin(10, 10, 10, 10)) &
        xlab("Demand Change (%)")

    # Save the combined plot
    plot_filename <- file.path(output_dir, paste0("combined_plots_", country, "_", 
                                                 gsub("\\.csv$", "", filename_pattern), ".png"))
    ggsave(plot_filename, combined_plot, width = 12, height = 15)

    return(plot_filename)
}

analyze_nrb_vs_harvest <- function(marginal_data, nrb_col, harvest_col, demand_col, pattern, x_col, country, output_dir) {
    


    p_nrb <- ggplot(marginal_data, aes(x = demand_value, y = marginal_nrb)) +
        geom_point(alpha = 0.5, na.rm = TRUE) +
        geom_smooth(method = "loess", color = "blue", se = TRUE, na.rm = TRUE, formula = y ~ x) +
        scale_color_viridis_d(drop = FALSE) +
        theme(
            legend.position = "none",
            axis.text.x = element_text(angle = 45, hjust = 1)
        ) +
        labs(
            title = "Marginal NRB vs Demand",
            x = NULL,
            y = "Marginal NRB"
        )

    p_harvest <- ggplot(marginal_data, aes(x = demand_value, y = marginal_harvest)) +
        geom_point(alpha = 0.6, na.rm = TRUE) +
        geom_smooth(method = "loess", color = "green", se = TRUE, na.rm = TRUE, formula = y ~ x) +
        scale_color_viridis_d(drop = FALSE) +
        theme(
            legend.position = "none",
            axis.text.x = element_text(angle = 45, hjust = 1)
        ) +
        labs(
            title = "Marginal Harvest vs Demand",
            x = NULL,  # Remove x label as it will be shared
            y = "Marginal Harvest"
        )

    # Marginal fnrb plot (marginal nrb / marginal harvest) and instantaneous marginal and fnrb vs. demand 
    p_mfnrb <- ggplot() +
        geom_point(data = marginal_data %>% filter(!scenario == "bau"), 
                  aes(x = demand_value, y = marginal_fnrb, shape = "BAU-relative marginal"), 
                  alpha = 0.6, na.rm = TRUE) +
        geom_smooth(data = marginal_data %>% filter(!scenario == "bau"),
                   aes(x = demand_value, y = marginal_fnrb),
                   method = "loess", color = "red", se = TRUE, na.rm = TRUE, formula = y ~ x) +
        geom_point(data = marginal_data, 
                  aes(x = demand_value, y = calculated_fnrb, color = scenario, shape = "fNRB"),
                  alpha = 0.6, na.rm = TRUE, shape = 2) +
        geom_smooth(data = marginal_data,
                   aes(x = demand_value, y = calculated_fnrb),
                   method = "loess", color = "purple", se = TRUE, na.rm = TRUE, formula = y ~ x) +
        geom_point(data = marginal_data, 
                  aes(x = demand_value, y = local_marginal_fnrb, color = scenario, shape = "Instantaneous marginal"),
                  alpha = 0.6, na.rm = TRUE) +
        geom_smooth(data = marginal_data,
                   aes(x = demand_value, y = local_marginal_fnrb, linetype = "Instantaneous marginal"),
                   method = "loess", color = "blue", se = TRUE, na.rm = TRUE, formula = y ~ x) +
        scale_color_viridis_d(drop = FALSE) +
        scale_fill_viridis_d(drop = FALSE) +
        theme(
            legend.position = "bottom",
            legend.box = "horizontal", 
            legend.box.just = "center",
            axis.text.x = element_text(angle = 45, hjust = 1)
        ) +
        guides(color = guide_legend(nrow = 1),
               fill = guide_legend(nrow = 1)) +
        labs(
            title = "fNRB and marginal fNRB (marginal nrb / marginal harvest) vs Demand",
            x = "Demand Change (%)", 
            y = "fNRB (%)"
            # color = "Scenario",
            # fill = "Scenario"
        )


    # Marginal fnrb plot (marginal nrb / marginal harvest) and instantaneous marginal and fnrb vs. Woodfuel Harvest
    p_mfnrb_harvest <- ggplot() +
        # Original marginal ratio points and line
        geom_point(data = marginal_data %>% filter(!scenario == "bau"), 
                  aes(x = .data[[harvest_col]], y = marginal_fnrb, shape = "BAU-relative marginal"), 
                  alpha = 0.6, na.rm = TRUE) +
        geom_smooth(data = marginal_data %>% filter(!scenario == "bau"),
                   aes(x = .data[[harvest_col]], y = marginal_fnrb, linetype = "BAU-relative marginal"),
                   method = "loess", color = "red", se = TRUE, na.rm = TRUE, formula = y ~ x) +
        # FNRB points and line
        geom_point(data = marginal_data, 
                  aes(x = .data[[harvest_col]], y = calculated_fnrb, shape = "fNRB"),
                  alpha = 0.6, na.rm = TRUE) +
        geom_smooth(data = marginal_data,
                   aes(x = .data[[harvest_col]], y = calculated_fnrb, linetype = "fNRB"),
                   method = "loess", color = "purple", se = TRUE, na.rm = TRUE, formula = y ~ x) +
        # New instantaneous marginal FNRB points and line
        geom_point(data = marginal_data %>% filter(!scenario == "bau"), 
                  aes(x = .data[[harvest_col]], y = local_marginal_fnrb, color = scenario, shape = "Instantaneous marginal"),
                  alpha = 0.6, na.rm = TRUE) +
        geom_smooth(data = marginal_data %>% filter(!scenario == "bau"),
                   aes(x = .data[[harvest_col]], y = local_marginal_fnrb, linetype = "Instantaneous marginal"),
                   method = "loess", color = "blue", se = TRUE, na.rm = TRUE, formula = y ~ x) +
        scale_color_viridis_d(drop = FALSE) +
        scale_fill_viridis_d(drop = FALSE) +
        scale_shape_manual(name = "Metric", 
                          values = c("BAU-relative marginal" = 2,
                                   "fNRB" = 16,
                                   "Instantaneous marginal" = 3)) +
        scale_linetype_manual(name = "Trend lines",
                            values = c("BAU-relative marginal" = "solid",
                                     "fNRB" = "solid",
                                     "Instantaneous marginal" = "solid"),
                            labels = c("BAU-relative marginal (red)",
                                     "fNRB (purple)",
                                     "Instantaneous marginal (blue)")) +
        theme(
            legend.position = "bottom",
            legend.box = "vertical", 
            legend.box.just = "left",
            axis.text.x = element_text(angle = 45, hjust = 1)
        ) +

        labs(
            title = "fNRB and Marginal fNRB vs Woodfuel Harvest",
            x = "Woodfuel Harvest (mtons)", 
            y = "fNRB (%)"
        )

    combined_plot <-  p_nrb / p_harvest / p_mfnrb / p_mfnrb_harvest +
        plot_layout(heights = c(1, 1, 1, 1.2)) + 
        plot_annotation(
            title = paste("Marginal Analysis -", country, "-", x_col),
            subtitle = "mfNRB = 100 * (NRB - BAU_NRB)/(Harvest - BAU_Harvest)",
            theme = theme(plot.title = element_text(hjust = 0.5))
        )

    plot_filename <- file.path(output_dir, paste0("marginal_analysis_", country, "_", admin_level, ".png"))
    ggsave(plot_filename, combined_plot, width = 10, height = 15)  # Increased height to accommodate new plot
    return(plot_filename)
} 


create_demand_sensitivity_plots <- function(combined_data, sensitivity_results, output_dir) {

    countries <- paste(sort(unique(combined_data$country)), collapse="_")
    
    plots <- list()
    
    p1 <- ggplot(sensitivity_results$slopes, 
                aes(x = demand_value, y = point_slope, color = country)) +
        geom_point(alpha = 0.6) +
        geom_line(aes(group = country)) +
        facet_wrap(~admin_level, scales = "free_y") +
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
    
    p2 <- ggplot(sensitivity_results$step_changes %>% filter(!is.na(step_sensitivity)),
                aes(x = demand_value, y = country, fill = step_sensitivity)) +
        geom_tile() +
        facet_wrap(~admin_level) +
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
    
    p3 <- ggplot(combined_data, aes(x = demand_value, y = marginal_fnrb, color = country)) +
        geom_point(alpha = 0.6) +
        geom_smooth(method = "lm", formula = y ~ x, se = TRUE) +
        facet_wrap(~admin_level, scales = "free_y") +
        coord_cartesian(
            ylim = quantile(combined_data$marginal_fnrb, c(0.01, 0.99), na.rm = TRUE)
        ) +
        theme(legend.position = "right") +
        labs(
            title = "Marginal fNRB vs. Demand, for all countries",
            subtitle = "Ratio = 100 * (NRB - BAU_NRB)/(Harvest - BAU_Harvest)",
            x = "Demand Change from BAU (%)",
            y = "mfNRB (%)",
            color = "Country"
        )
    plots$p3 <- p3
    curves_filename <- paste0("demand_response_curves_", countries, ".png")
    ggsave(file.path(output_dir, curves_filename), p3, width = 12, height = 8)
    write_log("demand_response_curves_", countries, ".png created and saved successfully")
    
    return(list(
        sensitivity_slopes = slopes_filename,
        step_sensitivity = steps_filename,
        response_curves = curves_filename
    ))
}



create_cross_country_plots <- function(all_results, output_dir) {
    combined_data <- bind_rows(lapply(all_results, function(x) x$full_data))
    
    p1 <- ggplot(combined_data, aes(x = country, y = marginal_fnrb)) +
        geom_boxplot(aes(fill = country), alpha = 0.7) +
        facet_grid(admin_level ~ demand_value) +
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

    p2 <- ggplot(combined_data, aes(x = demand_value, y = marginal_fnrb, color = country)) +
        geom_point(alpha = 0.3) +
        geom_smooth(method = "lm", formula = y ~ x, se = TRUE) +
        facet_wrap(~admin_level) +
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
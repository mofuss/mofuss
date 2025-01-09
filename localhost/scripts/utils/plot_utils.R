# nolint start: object_name_linter
library(ggplot2)
library(patchwork)
library(viridis)
source("localhost/scripts/utils/file_utils.R")

create_plots <- function(data, x_col, y_col, filename_pattern, country, output_dir) {
    
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
            title = paste(country," ",y_col, "Distribution by Demand Scenario -", filename_pattern),
            x = "Demand Change (%)",
            y = y_col,
            fill = "Scenario"
        )

    plot_filename_boxplot <- file.path(output_dir, paste0("boxplot_",y_col, "_", country, "_", gsub("\\.csv$", "", filename_pattern), ".png"))
    ggsave(plot_filename_boxplot, p, width = 10, height = 6)

    p_scatter <- ggplot(data, aes(x = .data[[x_col]], y = .data[[y_col]], color = scenario)) +
        geom_point(alpha = 0.7, position = position_jitter(width = 0.05)) +
        scale_color_viridis_d(drop = FALSE) +
        theme(
            axis.text.x = element_text(angle = 45, hjust = 1)
        ) +
        labs(
            title = paste(country, " ", y_col, "Distribution by Demand Scenario -", filename_pattern),
            x = x_col,
            y = y_col,
            color = "Scenario"
        )

    plot_filename_scatter <- file.path(output_dir, paste0("scatter_",y_col, "_", country, "_", gsub("\\.csv$", "", filename_pattern), ".png"))
    ggsave(plot_filename_scatter, p_scatter, width = 10, height = 6)

    return(list(boxplot = plot_filename_boxplot, scatter = plot_filename_scatter))
}

analyze_nrb_vs_harvest <- function(marginal_data, nrb_col, harvest_col, demand_col, pattern, x_col, country, output_dir) {
    
    p_nrb <- ggplot(marginal_data, aes(x = demand_value, y = marginal_nrb)) +
        geom_point(aes(color = scenario), alpha = 0.5, na.rm = TRUE) +
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
        geom_point(aes(color = scenario), alpha = 0.6, na.rm = TRUE) +
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

    p_ratio <- marginal_data %>%
        filter(!scenario == "bau") %>%
        ggplot(aes(x = demand_value, y = marginal_ratio)) +
        geom_point(aes(color = scenario), alpha = 0.6, na.rm = TRUE) +
        geom_smooth(method = "loess", color = "red", se = TRUE, na.rm = TRUE) +
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
            title = "Marginal NRB/Harvest Ratio vs Demand",
            x = "Demand Change (%)",
            y = "Marginal Ratio (%)",
            color = "Scenario",
            fill = "Scenario"
        )

    combined_plot <- p_nrb / p_harvest / p_ratio +
        plot_layout(heights = c(1, 1, 1.2)) + 
        plot_annotation(
            title = paste("Marginal Analysis -", country, "-", x_col),
            subtitle = "Ratio = 100 * (NRB - BAU_NRB)/(Harvest - BAU_Harvest)",
            theme = theme(plot.title = element_text(hjust = 0.5))
        )

    plot_filename <- file.path(output_dir, paste0("marginal_analysis_", country, "_", admin_level, ".png"))

    ggsave(plot_filename, combined_plot, width = 10, height = 12) 
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
    
    p3 <- ggplot(combined_data, aes(x = demand_value, y = marginal_ratio, color = country)) +
        geom_point(alpha = 0.6) +
        geom_smooth(method = "lm", formula = y ~ x, se = TRUE) +
        facet_wrap(~admin_level, scales = "free_y") +
        coord_cartesian(
            ylim = quantile(combined_data$marginal_ratio, c(0.01, 0.99), na.rm = TRUE)
        ) +
        theme(legend.position = "right") +
        labs(
            title = "Marginal Ratio Response to Demand Changes",
            subtitle = "Ratio = 100 * (NRB - BAU_NRB)/(Harvest - BAU_Harvest)",
            x = "Demand Change from BAU (%)",
            y = "Marginal NRB/Marginal Harvest * 100 (%)",
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
    
    p1 <- ggplot(combined_data, aes(x = country, y = marginal_ratio)) +
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

    p2 <- ggplot(combined_data, aes(x = demand_value, y = marginal_ratio, color = country)) +
        geom_point(alpha = 0.3) +
        geom_smooth(method = "lm", se = TRUE) +
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
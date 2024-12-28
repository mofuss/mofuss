# nolint start: object_name_linter
library(ggplot2)
library(viridis)

create_plots <- function(data, x_col, y_col, pattern, country) {
    lm_formula <- as.formula(paste(y_col, "~ demand_value"))
    lm_fit <- lm(lm_formula, data = data)
    lm_eq <- paste0(
        "y = ", round(coef(lm_fit)[1], 4), " + ",
        round(coef(lm_fit)[2], 4), "x\n",
        "R² = ", round(summary(lm_fit)$r.squared, 4)
    )

    # Create ordered scenario levels with proper numeric sorting
    all_scenarios <- unique(data$scenario)
    
    minus_scenarios <- all_scenarios[str_detect(all_scenarios, "minus")]
    minus_values <- as.numeric(str_extract(minus_scenarios, "\\d+"))
    minus_scenarios <- paste0("minus", sort(minus_values, decreasing = TRUE))
    
    plus_scenarios <- all_scenarios[str_detect(all_scenarios, "plus")]
    plus_values <- as.numeric(str_extract(plus_scenarios, "\\d+"))
    plus_scenarios <- paste0("plus", sort(plus_values))
    
    scenario_levels <- c(minus_scenarios, "bau", plus_scenarios)

    p <- ggplot(data, aes(x = demand_value, y = .data[[y_col]])) +
        geom_point(alpha = 0.1, position = position_jitter(width = 0.2), na.rm = TRUE) +
        geom_boxplot(aes(group = demand_value, fill = scenario), alpha = 0.7, na.rm = TRUE) +
        geom_smooth(method = "lm", color = "red", se = TRUE, na.rm = TRUE, formula = y ~ x) +
        scale_fill_viridis_d(limits = scenario_levels, drop = FALSE) +
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
            title = paste(y_col, "Distribution by Demand Scenario -", pattern),
            x = "Demand Change (%)",
            y = y_col,
            fill = "Scenario"
        )

    plot_filename_boxplot <- file.path("assets", paste0("boxplot_", country, "_", gsub("\\.csv$", "", pattern), ".png"))
    ggsave(plot_filename_boxplot, p, width = 10, height = 6)

    p_scatter <- ggplot(data, aes(x = .data[[x_col]], y = .data[[y_col]], color = scenario)) +
        geom_point(alpha = 0.7, position = position_jitter(width = 0.05)) +
        scale_color_viridis_d(limits = scenario_levels, drop = FALSE) +
        theme(
            axis.text.x = element_text(angle = 45, hjust = 1)
        ) +
        labs(
            title = paste(y_col, "Distribution by Demand Scenario -", pattern),
            x = x_col,
            y = y_col,
            color = "Scenario"
        )

    plot_filename_scatter <- file.path("assets", paste0("scatter_", country, "_", gsub("\\.csv$", "", pattern), ".png"))
    ggsave(plot_filename_scatter, p_scatter, width = 10, height = 6)

    return(list(boxplot = plot_filename_boxplot, scatter = plot_filename_scatter))
}

analyze_nrb_vs_harvest <- function(data, nrb_col, harvest_col, demand_col, pattern, x_col, country) {
    cat("\n=== Starting analyze_nrb_vs_harvest ===\n")
    cat("Pattern:", pattern, "\n")
    cat("x_col:", x_col, "\n")
    cat("nrb_col:", nrb_col, "\n")
    cat("harvest_col:", harvest_col, "\n")
    
    validate_data_for_marginals(data, nrb_col, x_col)
    validate_data_for_marginals(data, harvest_col, x_col)
    
    bau_values <- data %>%
        filter(scenario == "bau") %>%
        select(all_of(c(x_col, nrb_col, harvest_col))) %>%
        rename(
            nrb_bau = all_of(nrb_col),
            harvest_bau = all_of(harvest_col)
        )
    
    cat("\nBAU values summary:\n")
    print(summary(bau_values))
    
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
    
    cat("\nMarginal data dimensions:", dim(marginal_data)[1], "rows,", dim(marginal_data)[2], "columns\n")
    cat("Marginal ratio summary:\n")
    print(summary(marginal_data$marginal_ratio))
    cat("Number of NA ratios:", sum(is.na(marginal_data$marginal_ratio)), "\n")
    
    if(nrow(marginal_data) == 0) {
        stop("No valid data points after calculating marginal ratios")
    }
    
    p <- ggplot(marginal_data, aes(x = demand_value, y = marginal_ratio)) +
        geom_point(aes(color = scenario), alpha = 0.5, na.rm = TRUE) +
        geom_boxplot(aes(group = demand_value, fill = scenario), alpha = 0.3, na.rm = TRUE) +
        geom_smooth(method = "lm", color = "red", se = TRUE, na.rm = TRUE, formula = y ~ x) +
        scale_color_viridis_d(drop = FALSE) +
        scale_fill_viridis_d(drop = FALSE) +
        coord_cartesian(ylim = quantile(marginal_data$marginal_ratio, c(0.1, 0.9), na.rm = TRUE)) +
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

    plot_filename <- paste0("assets/marginal_ratio_vs_demand_", country, "_", x_col, "_", pattern, ".png")
    cat("\nSaving plot to:", plot_filename, "\n")
    
    tryCatch({
        ggsave(plot_filename, p, width = 10, height = 6)
        cat("Plot saved successfully\n")
    }, error = function(e) {
        cat("Error saving plot:", e$message, "\n")
    })

    cat("=== Finished analyze_nrb_vs_harvest ===\n\n")
    return(plot_filename)
} 


create_demand_sensitivity_plots <- function(marginal_data, sensitivity_analysis, output_dir) {
    cat("\n=== Starting create_demand_sensitivity_plots ===\n")
    
    slopes_filename <- NULL
    steps_filename <- NULL
    curves_filename <- NULL
    
    countries <- paste(sort(unique(marginal_data$country)), collapse="_")
    
    plots <- list()
    
    tryCatch({
        cat("\nCreating slope visualization plot...\n")
        p1 <- ggplot(sensitivity_analysis$slopes, 
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
        cat("Plot 1 created and saved successfully\n")
    }, error = function(e) {
        cat("Error creating/saving plot 1:", e$message, "\n")
    })
    
    tryCatch({
        cat("\nCreating step sensitivity heatmap...\n")
        p2 <- ggplot(sensitivity_analysis$step_changes %>% filter(!is.na(step_sensitivity)),
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
        cat("Plot 2 created and saved successfully\n")
    }, error = function(e) {
        cat("Error creating/saving plot 2:", e$message, "\n")
    })
    
    tryCatch({
        cat("\nCreating marginal ratio trends plot...\n")
        p3 <- ggplot(marginal_data, aes(x = demand_value, y = marginal_ratio, color = country)) +
            geom_point(alpha = 0.3) +
            geom_smooth(method = "lm", formula = y ~ x, se = TRUE) +
            facet_wrap(~adm_level, scales = "free_y") +
            coord_cartesian(
                ylim = quantile(marginal_data$marginal_ratio, c(0.05, 0.95), na.rm = TRUE)
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
        cat("Plot 3 created and saved successfully\n")
    }, error = function(e) {
        cat("Error creating/saving plot 3:", e$message, "\n")
    })
    
    cat("\n=== Finished create_demand_sensitivity_plots ===\n")
    
    return(list(
        sensitivity_slopes = if(!is.null(slopes_filename)) slopes_filename else NA,
        step_sensitivity = if(!is.null(steps_filename)) steps_filename else NA,
        response_curves = if(!is.null(curves_filename)) curves_filename else NA
    ))
}


# nolint end
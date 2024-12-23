# nolint start: object_name_linter
library(ggplot2)
library(viridis)

create_plots <- function(data, x_col, y_col, pattern) {
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

    plot_filename_boxplot <- paste0("assets/boxplot_", gsub("\\.csv$", "", pattern), ".png")
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

    plot_filename_scatter <- paste0("assets/scatter_", gsub("\\.csv$", "", pattern), ".png")
    ggsave(plot_filename_scatter, p_scatter, width = 10, height = 6)

    return(list(boxplot = plot_filename_boxplot, scatter = plot_filename_scatter))
}

analyze_nrb_vs_harvest <- function(data, nrb_col, harvest_col, demand_col, pattern, x_col) {
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

    plot_filename <- paste0("assets/marginal_ratio_vs_demand_", x_col, "_", pattern, ".png")
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

# nolint end
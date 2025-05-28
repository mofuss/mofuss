# nolint start: object_name_linter
library(ggplot2)
library(patchwork)
library(viridis)
library(ggrepel)
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
            summarise(n = n(), .groups = "drop") %>%
            filter(n > 1)

        p <- ggplot(data, aes(x = demand_value, y = .data[[y_col]])) +
            # Only draw boxplots for groups with multiple points
            {
                if (nrow(groups_with_multiple_points) > 0) {
                    geom_boxplot(
                        data = data %>%
                            semi_join(groups_with_multiple_points,
                                by = c("demand_value", "scenario")
                            ),
                        aes(group = demand_value, fill = scenario),
                        alpha = 0.7,
                        na.rm = TRUE
                    )
                }
            } +
            geom_point(aes(color = scenario),
                alpha = 0.6,
                size = 2,
                position = position_jitter(width = 0.1),
                na.rm = TRUE
            ) +
            geom_smooth(method = "lm", color = "red", se = TRUE, na.rm = TRUE, formula = y ~ x) +
            scale_fill_viridis_d(drop = FALSE) +
            scale_color_viridis_d(drop = FALSE) +
            scale_x_continuous(breaks = sort(unique(data$demand_value))) +
            coord_cartesian(
                xlim = c(min(data$demand_value) - 5, max(data$demand_value) + 5),
                ylim = c(
                    min(data[[y_col]], na.rm = TRUE) - 0.05,
                    max(data[[y_col]], na.rm = TRUE) + 0.05
                )
            ) +
            theme(
                legend.position = "none", # We'll use a shared legend
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
                x = x_col,
                y = y_col
            )
        return(p)
    }

    # Create subplots for each column
    plots <- lapply(cols, function(col) {
        create_subplot(data %>% filter(!is.na(.data[[col]]), !is.na(demand_value)), col)
    })

    # nrb vs. harvest plot, with a 1:1 line with the lower y lim being 0.
    p_nrb_harvest <- ggplot(data, aes(x = .data[[harvest_col]], y = .data[[nrb_col]])) +
        geom_point(alpha = 0.6) +
        geom_smooth(method = "lm", color = "red", se = TRUE, na.rm = TRUE, formula = y ~ x) +
        geom_abline(slope = 1, intercept = 0, color = "blue") +
        labs(title = "NRB vs Harvest", x = "Harvest (Mtons)", y = "NRB (Mtons)") +
        coord_cartesian(ylim = c(0, max(data[[nrb_col]], na.rm = TRUE)), xlim = c(0, max(data[[harvest_col]], na.rm = TRUE)))

    # Add p_nrb_harvest to the plots list
    plots$nrb_harvest <- p_nrb_harvest

    # Combine plots using patchwork
    combined_plot <- wrap_plots(plots, ncol = 1) +
        plot_layout(guides = "collect") + # Collect legends into one
        plot_annotation(
            title = paste(country, "Distribution by Demand Scenario -", filename_pattern),
            theme = theme(
                plot.title = element_text(hjust = 0.5),
                legend.position = "bottom",
                legend.box = "horizontal"
            )
        ) &
        theme(plot.margin = margin(10, 10, 10, 10))

    # Save the combined plot
    plot_filename <- file.path(output_dir, paste0(
        "combined_plots_", country, "_",
        gsub("\\.csv$", "", filename_pattern), ".png"
    ))
    ggsave(plot_filename, combined_plot, width = 12, height = 15)

    return(plot_filename)
}

analyze_nrb_vs_harvest <- function(marginal_data, nrb_col, harvest_col, demand_col, pattern, x_col, country, output_dir) {
    # Ensure demand_value is numeric where needed
    if (!"demand_value_numeric" %in% names(marginal_data)) {
        marginal_data <- marginal_data %>%
            mutate(demand_value_numeric = as.numeric(as.character(demand_value)))
    }

    p_nrb <- ggplot(marginal_data, aes(x = demand_value_numeric, y = marginal_nrb)) +
        geom_point(alpha = 0.5, na.rm = TRUE) +
        geom_smooth(method = "loess", color = "blue", se = TRUE, na.rm = TRUE, formula = y ~ x) +
        scale_color_viridis_d(drop = FALSE) +
        scale_x_continuous(breaks = sort(unique(marginal_data$demand_value_numeric))) +
        theme(
            legend.position = "none",
            axis.text.x = element_text(angle = 45, hjust = 1)
        ) +
        labs(
            title = "Marginal NRB vs Demand",
            x = NULL,
            y = "Marginal NRB"
        )

    p_harvest <- ggplot(marginal_data, aes(x = demand_value_numeric, y = marginal_harvest)) +
        geom_point(alpha = 0.6, na.rm = TRUE) +
        geom_smooth(method = "loess", color = "green", se = TRUE, na.rm = TRUE, formula = y ~ x) +
        scale_color_viridis_d(drop = FALSE) +
        scale_x_continuous(breaks = sort(unique(marginal_data$demand_value_numeric))) +
        theme(
            legend.position = "none",
            axis.text.x = element_text(angle = 45, hjust = 1)
        ) +
        labs(
            title = "Marginal Harvest vs Demand",
            x = NULL,
            y = "Marginal Harvest"
        )

    # Marginal fnrb plot (marginal nrb / marginal harvest) and instantaneous marginal and fnrb vs. demand
    p_mfnrb <- ggplot() +
        geom_point(
            data = marginal_data,
            aes(x = demand_value_numeric, y = marginal_fnrb, shape = "Instantaneous marginal"),
            alpha = 0.6, na.rm = TRUE
        ) +
        geom_smooth(
            data = marginal_data,
            aes(x = demand_value_numeric, y = marginal_fnrb),
            method = "loess", color = "red", se = TRUE, na.rm = TRUE, formula = y ~ x
        ) +
        geom_point(
            data = marginal_data,
            aes(x = demand_value_numeric, y = calculated_fnrb, color = scenario, shape = "fNRB"),
            alpha = 0.6, na.rm = TRUE, shape = 2
        ) +
        geom_smooth(
            data = marginal_data,
            aes(x = demand_value_numeric, y = calculated_fnrb),
            method = "loess", color = "purple", se = TRUE, na.rm = TRUE, formula = y ~ x
        ) +
        scale_color_viridis_d(drop = FALSE) +
        scale_fill_viridis_d(drop = FALSE) +
        scale_x_continuous(breaks = sort(unique(marginal_data$demand_value_numeric))) +
        theme(
            legend.position = "bottom",
            legend.box = "horizontal",
            legend.box.just = "center",
            axis.text.x = element_text(angle = 45, hjust = 1)
        ) +
        guides(
            color = guide_legend(nrow = 1),
            fill = guide_legend(nrow = 1)
        ) +
        labs(
            title = "fNRB and marginal fNRB (marginal nrb / marginal harvest) vs Demand",
            x = "Demand Change (%)",
            y = "fNRB (%)"
        )


    # Marginal fnrb plot (marginal nrb / marginal harvest) and instantaneous marginal and fnrb vs. Woodfuel Harvest
    p_mfnrb_harvest <- ggplot() +
        # Original marginal ratio points and line
        geom_point(
            data = marginal_data,
            aes(x = .data[[harvest_col]], y = marginal_fnrb, shape = "Instantaneous marginal"),
            alpha = 0.6, na.rm = TRUE
        ) +
        geom_smooth(
            data = marginal_data,
            aes(x = .data[[harvest_col]], y = marginal_fnrb, linetype = "Instantaneous marginal"),
            method = "loess", color = "red", se = TRUE, na.rm = TRUE, formula = y ~ x
        ) +
        # FNRB points and line
        geom_point(
            data = marginal_data,
            aes(x = .data[[harvest_col]], y = calculated_fnrb, shape = "fNRB"),
            alpha = 0.6, na.rm = TRUE
        ) +
        geom_smooth(
            data = marginal_data,
            aes(x = .data[[harvest_col]], y = calculated_fnrb, linetype = "fNRB"),
            method = "loess", color = "purple", se = TRUE, na.rm = TRUE, formula = y ~ x
        ) +
        # Scales
        scale_color_viridis_d(drop = FALSE) +
        scale_fill_viridis_d(drop = FALSE) +
        scale_shape_manual(
            name = "Metric",
            values = c(
                "Instantaneous marginal" = 2,
                "fNRB" = 16
            )
        ) +
        scale_linetype_manual(
            name = "Trend lines",
            values = c(
                "Instantaneous marginal" = "solid",
                "fNRB" = "solid"
            ),
            labels = c(
                "fNRB (purple)",
                "Instantaneous marginal (red)"
            )
        ) +
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

    combined_plot <- p_nrb / p_harvest / p_mfnrb / p_mfnrb_harvest +
        plot_layout(heights = c(1, 1, 1, 1.2)) +
        plot_annotation(
            title = paste("Marginal Analysis -", country, "-", x_col),
            subtitle = "mfNRB = 100 * (NRB - NRB_lagged)/(Harvest - Harvest_lagged)",
            theme = theme(plot.title = element_text(hjust = 0.5))
        )

    plot_filename <- file.path(output_dir, paste0("marginal_analysis_", country, "_", admin_level, ".png"))
    ggsave(plot_filename, combined_plot, width = 10, height = 15)
    return(plot_filename)
}


create_demand_sensitivity_plots <- function(combined_data, sensitivity_results, output_dir) {
    # Use a generic label instead of a long list of countries for filenames
    countries_label <- "all_countries"
    if (length(unique(combined_data$country)) == 1) {
        countries_label <- unique(combined_data$country)[1] # Use single country name if only one
    }

    plots <- list()

    # Plot 1: Point Sensitivity vs Demand
    p1 <- ggplot(
        sensitivity_results$slopes,
        aes(x = demand_value_numeric, y = point_slope, color = country, group = country)
    ) +
        geom_point(alpha = 0.6) +
        geom_line() + # Group already set in aes
        facet_wrap(~admin_level, scales = "free_y") +
        scale_x_continuous(breaks = sort(unique(sensitivity_results$slopes$demand_value_numeric))) +
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
    # Update filename construction
    slopes_filename <- paste0("demand_sensitivity_slopes_", countries_label, ".png")
    ggsave(file.path(output_dir, slopes_filename), p1, width = 12, height = 8)
    write_log(paste(slopes_filename, "created and saved successfully")) # Use the modified filename

    # Plot 2: Step Sensitivity Heatmap
    p2 <- ggplot(
        sensitivity_results$step_changes %>% filter(!is.na(step_sensitivity)),
        aes(x = demand_value_numeric, y = country, fill = step_sensitivity)
    ) +
        geom_tile() +
        facet_wrap(~admin_level) +
        scale_fill_viridis_c() +
        scale_x_continuous(breaks = sort(unique(sensitivity_results$step_changes$demand_value_numeric))) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(
            title = "Demand Step Sensitivity Analysis",
            subtitle = "Change in Marginal Ratio per Unit Demand Change",
            x = "Demand Level",
            y = "Country",
            fill = "Step Sensitivity"
        )
    plots$p2 <- p2
    # Update filename construction
    steps_filename <- paste0("demand_step_sensitivity_", countries_label, ".png")
    ggsave(file.path(output_dir, steps_filename), p2, width = 12, height = 8)
    write_log(paste(steps_filename, "created and saved successfully")) # Use the modified filename

    # Ensure combined_data has demand_value_numeric
    if (!"demand_value_numeric" %in% names(combined_data)) {
        combined_data <- combined_data %>%
            mutate(demand_value_numeric = as.numeric(as.character(demand_value)))
    }

    # Plot 3: Marginal FNRB Response Curves
    p3 <- ggplot(combined_data, aes(x = demand_value_numeric, y = marginal_fnrb, color = country)) +
        geom_point(alpha = 0.6) +
        geom_smooth(method = "lm", formula = y ~ x, se = TRUE) +
        facet_wrap(~admin_level, scales = "free_y") +
        scale_x_continuous(breaks = sort(unique(combined_data$demand_value_numeric))) +
        coord_cartesian(
            ylim = c(0, 100)
        ) +
        # Add country name labels at the end of each regression line
        geom_label_repel(
            data = combined_data %>%
                group_by(country, admin_level) %>%
                filter(demand_value_numeric == max(demand_value_numeric)) %>%
                summarize(
                    demand_value_numeric = max(demand_value_numeric),
                    # Get predicted value from regression model with error handling
                    marginal_fnrb = tryCatch(
                        {
                            # Only fit model if we have enough data points
                            df <- cur_data()
                            if (nrow(df) >= 2 && sum(!is.na(df$marginal_fnrb)) >= 2) {
                                model <- lm(marginal_fnrb ~ demand_value_numeric, data = df)
                                predict(model, newdata = data.frame(demand_value_numeric = max(demand_value_numeric)))
                            } else {
                                # Use the mean if available, otherwise NA
                                if (sum(!is.na(df$marginal_fnrb)) > 0) {
                                    mean(df$marginal_fnrb, na.rm = TRUE)
                                } else {
                                    NA_real_
                                }
                            }
                        },
                        error = function(e) NA_real_
                    ),
                    .groups = "drop"
                ) %>%
                filter(!is.na(marginal_fnrb)), # Filter out any rows where prediction failed
            aes(label = country, color = country),
            size = 4,
            fontface = "bold",
            box.padding = 0.5,
            point.padding = 0.5,
            segment.color = "grey50",
            force = 5,
            seed = 42
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
    # Update filename construction
    curves_filename <- paste0("demand_response_curves_", countries_label, ".png")
    ggsave(file.path(output_dir, curves_filename), p3, width = 12, height = 8)
    write_log(paste(curves_filename, "created and saved successfully")) # Use the modified filename

    return(list(
        sensitivity_slopes = slopes_filename,
        step_sensitivity = steps_filename,
        response_curves = curves_filename
    ))
}



create_cross_country_plots <- function(all_results, output_dir) {
    combined_data <- bind_rows(lapply(all_results, function(x) {
        if (is.null(x) || is.null(x$full_data)) {
            return(NULL)
        }
        expected_cols <- c(
            "country", "admin_level", "demand_value", "scenario",
            "calculated_fnrb", "marginal_fnrb"
        )
        df <- x$full_data
        for (col_name in expected_cols) {
            if (!col_name %in% names(df)) {
                df[[col_name]] <- NA
            }
        }
        return(df[, intersect(names(df), expected_cols), drop = FALSE])
    }))

    combined_data <- combined_data %>%
        filter(!is.na(country) & !is.na(admin_level) & !is.na(demand_value) & !is.na(scenario)) %>%
        mutate(
            demand_value = factor(demand_value),
            admin_level = factor(admin_level),
            scenario = factor(scenario)
        ) # Ensure scenario is factor for alpha aesthetic

    # Plot 1: Calculated FNRB on a single set of axes
    p_calculated_fnrb <- ggplot(
        combined_data,
        aes(
            x = demand_value, y = calculated_fnrb, color = country,
            shape = admin_level, alpha = scenario
        )
    ) +
        geom_point(position = position_jitter(width = 0.25, height = 0, seed = 123), size = 2.5) +
        # Removed facet_wrap(~scenario)
        scale_color_viridis_d() +
        scale_shape_manual(name = "Admin Level", values = 1:length(unique(combined_data$admin_level))) +
        scale_alpha_discrete(name = "Scenario", range = c(0.4, 1.0)) + # Scenarios distinguished by alpha
        coord_cartesian(ylim = c(0, 100)) +
        theme_minimal(base_size = 12) +
        theme(
            axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "bottom",
            plot.title = element_text(hjust = 0.5, size = 16),
            plot.subtitle = element_text(hjust = 0.5, size = 10),
            legend.box = "vertical" # Arrange legends if they become too wide
        ) +
        guides(
            shape = guide_legend(override.aes = list(alpha = 1)), # Ensure shapes in legend are fully opaque
            alpha = guide_legend()
        ) +
        labs(
            title = "Calculated FNRB",
            subtitle = "All Scenarios Overlaid (distinguished by alpha). Points by Country (color) & Admin Level (shape).",
            x = "Demand Value",
            y = "Calculated FNRB (%)",
            color = "Country"
        )

    calculated_fnrb_filename <- file.path(output_dir, "cross_country_calculated_fnrb.png")
    ggsave(calculated_fnrb_filename, p_calculated_fnrb, width = 14, height = 8)
    write_log(paste(basename(calculated_fnrb_filename), "created and saved successfully"))

    # Plot 2: Marginal FNRB on a single set of axes, with connecting lines and scenario-specific linetypes
    p_marginal_fnrb <- ggplot() +
        # First add lines (not affected by jittering)
        geom_line(
            data = combined_data,
            aes(
                x = as.numeric(demand_value), y = marginal_fnrb,
                group = interaction(country, admin_level, scenario),
                color = country, linetype = scenario, alpha = scenario
            ),
            linewidth = 1.2
        ) +
        # Then add points with jittering
        geom_point(
            data = combined_data,
            aes(
                x = demand_value, y = marginal_fnrb,
                color = country, fill = country,
                shape = admin_level, alpha = scenario
            ),
            position = position_jitter(width = 0.25, height = 0, seed = 123),
            size = 3.5, stroke = 1.2
        ) +
        scale_color_viridis_d() +
        scale_fill_viridis_d() +
        # Use filled shapes (21-25) for better visibility with fill color
        scale_shape_manual(
            name = "Admin Level",
            values = c(21, 22, 23, 24, 25)[1:length(unique(combined_data$admin_level))]
        ) +
        scale_alpha_discrete(name = "Scenario", range = c(0.4, 0.9)) +
        scale_linetype_discrete(name = "Scenario") +
        coord_cartesian(ylim = c(0, 100)) +
        theme_minimal(base_size = 14) + # Increased base font size
        theme(
            axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
            axis.text.y = element_text(size = 12),
            axis.title = element_text(size = 14),
            legend.position = "bottom",
            plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
            plot.subtitle = element_text(hjust = 0.5, size = 12),
            legend.box = "vertical",
            legend.key.width = unit(2, "cm"), # Wider legend keys
            legend.text = element_text(size = 12),
            legend.title = element_text(size = 13)
        ) +
        guides(
            shape = guide_legend(override.aes = list(alpha = 1, size = 4)),
            alpha = guide_legend(override.aes = list(size = 4)),
            linetype = guide_legend(override.aes = list(linewidth = 1.5))
        ) +
        labs(
            title = "Marginal FNRB",
            subtitle = "Points and connecting lines by Country, Admin Level & Scenario",
            x = "Demand Value",
            y = "Marginal FNRB (%)",
            color = "Country",
            fill = "Country"
        )

    marginal_fnrb_filename <- file.path(output_dir, "cross_country_marginal_fnrb.png")
    ggsave(marginal_fnrb_filename, p_marginal_fnrb, width = 14, height = 11, dpi = 300)
    write_log(paste(basename(marginal_fnrb_filename), "created and saved successfully"))

    # Plot 3: Marginal FNRB vs Demand Change by Country (Response Curves) - user liked this structure
    p2_trends <- ggplot(
        # Make sure demand_value is properly converted to numeric
        combined_data %>%
            mutate(demand_value_numeric = as.numeric(as.character(demand_value))),
        aes(x = demand_value_numeric, y = marginal_fnrb, color = country)
    ) +
        geom_point(alpha = 0.3, na.rm = TRUE, size = 1.5) +
        geom_smooth(method = "lm", formula = y ~ x, se = TRUE, na.rm = TRUE, linewidth = 0.5, alpha = 0.5) +
        facet_wrap(~admin_level, scales = "free_y") +
        # Use regular scale instead of continuous for more flexibility
        scale_x_continuous(name = "Demand Value (%)") +
        coord_cartesian(ylim = c(0, 100)) +
        scale_color_viridis_d() +
        # Add country name labels at the end of each regression line
        geom_label_repel(
            data = combined_data %>%
                group_by(country, admin_level) %>%
                filter(demand_value_numeric == max(demand_value_numeric)) %>%
                summarize(
                    demand_value_numeric = max(demand_value_numeric),
                    # Get predicted value from regression model with error handling
                    marginal_fnrb = tryCatch(
                        {
                            # Only fit model if we have enough data points
                            df <- cur_data()
                            if (nrow(df) >= 2 && sum(!is.na(df$marginal_fnrb)) >= 2) {
                                model <- lm(marginal_fnrb ~ demand_value_numeric, data = df)
                                predict(model, newdata = data.frame(demand_value_numeric = max(demand_value_numeric)))
                            } else {
                                # Use the mean if available, otherwise NA
                                if (sum(!is.na(df$marginal_fnrb)) > 0) {
                                    mean(df$marginal_fnrb, na.rm = TRUE)
                                } else {
                                    NA_real_
                                }
                            }
                        },
                        error = function(e) NA_real_
                    ),
                    .groups = "drop"
                ) %>%
                filter(!is.na(marginal_fnrb)), # Filter out any rows where prediction failed
            aes(label = country, color = country),
            size = 4,
            fontface = "bold",
            box.padding = 0.5,
            point.padding = 0.5,
            segment.color = "grey50",
            force = 5,
            seed = 42
        ) +
        theme_minimal(base_size = 12) +
        theme(
            legend.position = "right",
            plot.title = element_text(hjust = 0.5, size = 16),
            plot.subtitle = element_text(hjust = 0.5, size = 10),
            strip.text = element_text(size = 11, face = "bold"),
            axis.text.x = element_text(angle = 45, hjust = 1)
        ) +
        labs(
            title = "Marginal FNRB vs Demand Change by Country",
            subtitle = "Lines are lm fits. Y-axis bounded 0-100.",
            y = "Marginal FNRB (%)",
            color = "Country"
        )

    trends_filename <- file.path(output_dir, "cross_country_marginal_fnrb_trends.png")
    ggsave(trends_filename, p2_trends, width = 12, height = 8)
    write_log(paste(basename(trends_filename), "created and saved successfully"))

    return(list(
        calculated_fnrb_plot_file = calculated_fnrb_filename,
        marginal_fnrb_plot_file = marginal_fnrb_filename,
        trends_plot_file = trends_filename,
        p_calculated_fnrb = p_calculated_fnrb,
        p_marginal_fnrb = p_marginal_fnrb,
        p2_trends = p2_trends
    ))
}


# nolint end

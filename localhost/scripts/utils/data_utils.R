# nolint start: object_name_linter
library(dplyr)
library(tidyr)
library(zoo)
source("localhost/scripts/utils/file_utils.R")

update_admin_data <- function(admin_filename_pattern, combined_data, combined_data_adm0, 
                            combined_data_adm1, combined_data_adm2, x_col, demand_col) {
    if (str_detect(admin_filename_pattern, "adm0")) {
        if (is.null(combined_data_adm0)) {
            combined_data_adm0 <- combined_data
        } else {
            combined_data_adm0 <- full_join(
                combined_data_adm0,
                combined_data,
                by = c(x_col, "scenario", demand_col)
            )
        }
        return(list(adm0 = combined_data_adm0, 
                   adm1 = combined_data_adm1, 
                   adm2 = combined_data_adm2))
    } else if (str_detect(admin_filename_pattern, "adm1")) {
        if (is.null(combined_data_adm1)) {
            combined_data_adm1 <- combined_data
        } else {
            combined_data_adm1 <- full_join(
                combined_data_adm1,
                combined_data,
                by = c(x_col, "scenario", demand_col)
            )
        }
        return(list(adm0 = combined_data_adm0, 
                   adm1 = combined_data_adm1, 
                   adm2 = combined_data_adm2))
    } else if (str_detect(admin_filename_pattern, "adm2")) {
        if (is.null(combined_data_adm2)) {
            combined_data_adm2 <- combined_data
        } else {
            combined_data_adm2 <- full_join(
                combined_data_adm2,
                combined_data,
                by = c(x_col, "scenario", demand_col)
            )
        }
        return(list(adm0 = combined_data_adm0, 
                   adm1 = combined_data_adm1, 
                   adm2 = combined_data_adm2))
    }
}

validate_data_for_marginals <- function(data, value_col, x_col) {
    bau_count <- sum(data$scenario == "bau", na.rm = TRUE)
    if (bau_count == 0) {
        stop("No BAU scenario found in the data")
    }
    
    # Check for unique combinations
    combinations <- data %>%
        group_by(.data[[x_col]], scenario) %>%
        summarise(count = n(), .groups = "drop")
    
    duplicates <- combinations %>%
        filter(count > 1)
    
    if (nrow(duplicates) > 0) {
        warning("Found duplicate combinations of ", x_col, " and scenario:\n",
                paste(capture.output(print(duplicates)), collapse = "\n"))
    }
    
    return(TRUE)
} 


collect_marginal_data <- function(data, country, admin_level, fnrb_col, nrb_col, harvest_col) {
    
    bau_values <- data %>%
        filter(scenario == "bau") %>%
        select(all_of(c(fnrb_col, nrb_col, harvest_col))) %>%
        rename(
            nrb_bau = all_of(nrb_col),
            harvest_bau = all_of(harvest_col)
        ) %>%
        select(nrb_bau, harvest_bau)
    
    marginal_data <- data %>%
        crossing(bau_values) %>%
        mutate(
            marginal_nrb = .data[[nrb_col]] - nrb_bau,
            marginal_harvest = .data[[harvest_col]] - harvest_bau,
            marginal_fnrb = case_when(
                scenario == "bau" ~ NA_real_,
                abs(marginal_harvest) < 1e-10 ~ NA_real_,
                TRUE ~ 100 * marginal_nrb / marginal_harvest
            ),
            country = country,
            admin_level = admin_level,
            calculated_fnrb = 100 * .data[[nrb_col]] / .data[[harvest_col]],
            fnrb = 100* .data[[fnrb_col]]
        ) %>%
        select(-ends_with(".x"), -ends_with(".y")) %>%
        distinct() %>%
        # Sort by demand value instead of scenario
        arrange(country, admin_level, demand_value) %>%
        group_by(country, admin_level) %>%
        mutate(
            local_nrb = .data[[nrb_col]] - lag(.data[[nrb_col]]),
            local_harvest = .data[[harvest_col]] - lag(.data[[harvest_col]]),
            local_marginal_fnrb = case_when(
                abs(local_harvest) < 1e-10 ~ NA_real_,
                TRUE ~ 100 * local_nrb / local_harvest
            ),
        ) %>%
        ungroup()

    # Combined summary statistics
    marginal_summary <- marginal_data %>%
        group_by(country, admin_level, demand_value, scenario) %>%
        summarise(
            # Original marginal statistics
            mean_marginal_nrb = mean(marginal_nrb, na.rm = TRUE),
            mean_marginal_harvest = mean(marginal_harvest, na.rm = TRUE),
            mean_marginal_fnrb = mean(marginal_fnrb, na.rm = TRUE),
            # median_marginal_fnrb = median(marginal_fnrb, na.rm = TRUE),
            # sd_marginal_fnrb = sd(marginal_fnrb, na.rm = TRUE),
            
            # Additional summary statistics
            mean_nrb = mean(.data[[nrb_col]], na.rm = TRUE),
            # sd_nrb = sd(.data[[nrb_col]], na.rm = TRUE),
            # median_nrb = median(.data[[nrb_col]], na.rm = TRUE),
            mean_harvest = mean(.data[[harvest_col]], na.rm = TRUE),
            # sd_harvest = sd(.data[[harvest_col]], na.rm = TRUE),
            # median_harvest = median(.data[[harvest_col]], na.rm = TRUE),
            mean_fnrb = mean(fnrb, na.rm = TRUE),
            # sd_fnrb = sd(.data[[fnrb_col]], na.rm = TRUE),
            # median_fnrb = median(.data[[fnrb_col]], na.rm = TRUE),
            .groups = "drop"
        )

    return(list(
        full_data = marginal_data,
        summary = marginal_summary
    ))
}

create_summary_tables <- function(all_results) {
    combined_summary <- bind_rows(lapply(all_results, function(x) x$summary))
    
    country_summary <- combined_summary %>%
        group_by(country, admin_level) %>%
        summarise(
            mean_ratio = mean(mean_ratio, na.rm = TRUE),
            median_ratio = median(median_ratio, na.rm = TRUE),
            sd_ratio = mean(sd_ratio, na.rm = TRUE),
            total_observations = sum(n_observations),
            .groups = "drop"
        )
    
    demand_sensitivity <- combined_summary %>%
        group_by(country, admin_level, demand_value) %>%
        summarise(
            mean_ratio = mean(mean_ratio, na.rm = TRUE),
            median_ratio = median(median_ratio, na.rm = TRUE),
            sd_ratio = mean(sd_ratio, na.rm = TRUE),
            n_observations = sum(n_observations),
            .groups = "drop"
        )
    
    return(list(
        country_summary = country_summary,
        demand_sensitivity = demand_sensitivity,
        full_summary = combined_summary
    ))
}

analyze_demand_sensitivity <- function(combined_data) {
    write_log("Starting demand sensitivity analysis")
    
    cat("\n=== Starting analyze_demand_sensitivity ===\n")
    cat("Input data dimensions:", dim(combined_data)[1], "rows,", dim(combined_data)[2], "columns\n")
    
    # Calculate point-wise slopes for each country and admin level
    slope_analysis <- combined_data %>%
        # Sort by demand value to ensure proper differencing
        arrange(country, admin_level, demand_value) %>%
        group_by(country, admin_level) %>%
        mutate(
            # Calculate point-wise slopes using adjacent points
            point_slope = (lead(marginal_fnrb) - marginal_fnrb) / 
                            (lead(demand_value) - demand_value),
            # Calculate local R-squared using rolling window
            local_r_squared = rollapply(
                marginal_fnrb, 
                width = 3, 
                FUN = function(x) {
                    if (length(unique(x)) < 2) return(NA)
                    summary(lm(x ~ seq_along(x)))$r.squared
                },
                align = "center",
                fill = NA
            ),
            sensitivity_category = case_when(
                abs(point_slope) < 0.1 ~ "Low_10perc",
                abs(point_slope) < 0.5 ~ "Medium_50perc",
                TRUE ~ "High_100perc"
            )
        ) %>%
        ungroup()
    
    cat("\nPoint-wise slope analysis results:\n")
    print(summary(slope_analysis$point_slope))
    
    step_changes <- combined_data %>%
        group_by(country, admin_level, demand_value) %>%
        summarise(
            mean_ratio = mean(marginal_fnrb, na.rm = TRUE),
            .groups = "drop"
        ) %>%
        arrange(country, admin_level, demand_value) %>%
        group_by(country, admin_level) %>%
        mutate(
            change_from_previous = mean_ratio - lag(mean_ratio),
            demand_change = demand_value - lag(demand_value),
            step_sensitivity = change_from_previous / demand_change
        ) %>%
        ungroup()
    
    cat("\nStep changes summary:\n")
    print(summary(step_changes))

    cat("\n=== Finished analyze_demand_sensitivity ===\n")
    return(list(
        slopes = slope_analysis,
        step_changes = step_changes
    ))
}

create_demand_sensitivity_tables <- function(sensitivity_analysis) {
    sensitivity_ranking <- sensitivity_analysis$slopes %>%
        group_by(country, admin_level) %>%
        summarise(
            mean_sensitivity = mean(abs(point_slope), na.rm = TRUE),
            max_sensitivity = max(abs(point_slope), na.rm = TRUE),
            min_sensitivity = min(abs(point_slope), na.rm = TRUE),
            .groups = "drop"
        ) %>%
        arrange(desc(mean_sensitivity)) %>%
        select(country, admin_level, mean_sensitivity, max_sensitivity, min_sensitivity)

    # Find critical transitions (points where sensitivity changes dramatically)
    critical_transitions <- sensitivity_analysis$slopes %>%
        arrange(country, admin_level, demand_value) %>%
        group_by(country, admin_level) %>%
        mutate(
            sensitivity_change = abs(point_slope - lag(point_slope)),
            demand_transition = paste(lag(demand_value), "→", demand_value)
        ) %>%
        filter(!is.na(sensitivity_change)) %>%
        slice_max(order_by = sensitivity_change, n = 3) %>%
        select(country, admin_level, demand_transition, sensitivity_change) %>%
        arrange(desc(sensitivity_change))

    return(list(
        sensitivity_ranking = sensitivity_ranking,
        critical_transitions = critical_transitions
    ))
}

# nolint end

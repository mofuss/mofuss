# nolint start: object_name_linter
library(dplyr)
library(tidyr)
library(zoo)
source("localhost/scripts/utils/file_utils.R")
update_admin_data <- function(filename_pattern, combined_data, combined_data_adm0, 
                            combined_data_adm1, combined_data_adm2, x_col, demand_col) {
    if (str_detect(filename_pattern, "adm0")) {
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
    } else if (str_detect(filename_pattern, "adm1")) {
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
    } else if (str_detect(filename_pattern, "adm2")) {
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

collect_marginal_results <- function(data, country, adm_level, nrb_col, harvest_col) {
    write_log(paste("Collecting marginal results for", country, "admin level:", adm_level))
    
    tryCatch({
        cat("\n=== Starting collect_marginal_results ===\n")
        cat("Country:", country, "\n")
        cat("Admin level:", adm_level, "\n")
        cat("NRB column:", nrb_col, "\n")
        cat("Harvest column:", harvest_col, "\n")
        cat("Data dimensions:", dim(data)[1], "rows,", dim(data)[2], "columns\n")
        cat("Columns available:", paste(names(data), collapse=", "), "\n")
        
        # Convert adm_level to actual column name
        adm_col <- paste0("ADM_", substr(adm_level, 4, 4))
        cat("Using admin column:", adm_col, "\n")
        
        if (!adm_col %in% names(data)) {
            stop("Admin column ", adm_col, " not found in data")
        }
        
        bau_values <- data %>%
            filter(scenario == "bau") %>%
            select(all_of(c(adm_col, nrb_col, harvest_col))) %>%
            rename(
                nrb_bau = all_of(nrb_col),
                harvest_bau = all_of(harvest_col)
            )
        
        cat("\nBAU values found:", nrow(bau_values), "\n")
        if (nrow(bau_values) == 0) {
            stop("No BAU values found")
        }
        
        marginal_data <- data %>%
            left_join(bau_values, by = adm_col) %>%
            mutate(
                marginal_nrb = .data[[nrb_col]] - nrb_bau,
                marginal_harvest = .data[[harvest_col]] - harvest_bau,
                marginal_ratio = case_when(
                    scenario == "bau" ~ 0,
                    abs(marginal_harvest) < 1e-10 ~ NA_real_,
                    TRUE ~ 100 * marginal_nrb / marginal_harvest
                ),
                country = country,
                adm_level = adm_level
            ) %>%
            filter(scenario != "bau") %>%
            select(-ends_with(".x"), -ends_with(".y")) %>%
            distinct()

        cat("\nMarginal data dimensions:", dim(marginal_data)[1], "rows,", dim(marginal_data)[2], "columns\n")
        cat("Sample of marginal ratios:\n")
        print(summary(marginal_data$marginal_ratio))

        summary_stats <- marginal_data %>%
            group_by(country, adm_level, demand_value, scenario) %>%
            summarise(
                mean_ratio = mean(marginal_ratio, na.rm = TRUE),
                median_ratio = median(marginal_ratio, na.rm = TRUE),
                sd_ratio = sd(marginal_ratio, na.rm = TRUE),
                q1_ratio = quantile(marginal_ratio, 0.25, na.rm = TRUE),
                q3_ratio = quantile(marginal_ratio, 0.75, na.rm = TRUE),
                n_observations = n(),
                .groups = "drop"
            )

        cat("\n=== Finished collect_marginal_results ===\n")
        return(list(
            full_data = marginal_data,
            summary = summary_stats
        ))
        
    }, error = function(e) {
        error_handler(e, paste("collect_marginal_results for", country, adm_level))
        return(NULL)
    }, warning = function(w) {
        warning_handler(w, paste("collect_marginal_results for", country, adm_level))
    })
}
create_summary_tables <- function(all_results) {
    combined_summary <- bind_rows(lapply(all_results, function(x) x$summary))
    
    country_summary <- combined_summary %>%
        group_by(country, adm_level) %>%
        summarise(
            mean_ratio = mean(mean_ratio, na.rm = TRUE),
            median_ratio = median(median_ratio, na.rm = TRUE),
            sd_ratio = mean(sd_ratio, na.rm = TRUE),
            total_observations = sum(n_observations),
            .groups = "drop"
        )
    
    demand_sensitivity <- combined_summary %>%
        group_by(country, adm_level, demand_value) %>%
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
    
    tryCatch({
        cat("\n=== Starting analyze_demand_sensitivity ===\n")
        cat("Input data dimensions:", dim(combined_data)[1], "rows,", dim(combined_data)[2], "columns\n")
        
        # Calculate point-wise slopes for each country and admin level
        slope_analysis <- combined_data %>%
            # Sort by demand value to ensure proper differencing
            arrange(country, adm_level, demand_value) %>%
            group_by(country, adm_level) %>%
            mutate(
                # Calculate point-wise slopes using adjacent points
                point_slope = (lead(marginal_ratio) - marginal_ratio) / 
                             (lead(demand_value) - demand_value),
                # Calculate local R-squared using rolling window
                local_r_squared = rollapply(
                    marginal_ratio, 
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
        
    }, error = function(e) {
        cat("Error in slope analysis:", e$message, "\n")
        print(str(e))
        return(NULL)
    })
    
    tryCatch({
        step_changes <- combined_data %>%
            group_by(country, adm_level, demand_value) %>%
            summarise(
                mean_ratio = mean(marginal_ratio, na.rm = TRUE),
                .groups = "drop"
            ) %>%
            arrange(country, adm_level, demand_value) %>%
            group_by(country, adm_level) %>%
            mutate(
                change_from_previous = mean_ratio - lag(mean_ratio),
                demand_change = demand_value - lag(demand_value),
                step_sensitivity = change_from_previous / demand_change
            ) %>%
            ungroup()
        
        cat("\nStep changes summary:\n")
        print(summary(step_changes))
        
    }, error = function(e) {
        cat("Error in step changes analysis:", e$message, "\n")
        print(str(e))
        return(NULL)
    })
    
    cat("\n=== Finished analyze_demand_sensitivity ===\n")
    return(list(
        slopes = slope_analysis,
        step_changes = step_changes
    ))
}

create_demand_sensitivity_tables <- function(sensitivity_analysis) {
    sensitivity_ranking <- sensitivity_analysis$slopes %>%
        group_by(country, adm_level) %>%
        summarise(
            mean_sensitivity = mean(abs(point_slope), na.rm = TRUE),
            max_sensitivity = max(abs(point_slope), na.rm = TRUE),
            min_sensitivity = min(abs(point_slope), na.rm = TRUE),
            .groups = "drop"
        ) %>%
        arrange(desc(mean_sensitivity)) %>%
        select(country, adm_level, mean_sensitivity, max_sensitivity, min_sensitivity)

    # Find critical transitions (points where sensitivity changes dramatically)
    critical_transitions <- sensitivity_analysis$slopes %>%
        arrange(country, adm_level, demand_value) %>%
        group_by(country, adm_level) %>%
        mutate(
            sensitivity_change = abs(point_slope - lag(point_slope)),
            demand_transition = paste(lag(demand_value), "→", demand_value)
        ) %>%
        filter(!is.na(sensitivity_change)) %>%
        slice_max(order_by = sensitivity_change, n = 3) %>%
        select(country, adm_level, demand_transition, sensitivity_change) %>%
        arrange(desc(sensitivity_change))

    return(list(
        sensitivity_ranking = sensitivity_ranking,
        critical_transitions = critical_transitions
    ))
}

# nolint end

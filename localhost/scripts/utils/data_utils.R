# nolint start: object_name_linter
library(dplyr)
library(tidyr)

calculate_marginals <- function(data, value_col, x_col) {
    # First, get the BAU values for each administrative unit
    bau_values <- data %>%
        filter(scenario == "bau") %>%
        select(all_of(c(x_col, value_col))) %>%
        rename(bau_value = all_of(value_col))

    # Calculate marginal changes relative to BAU
    marginal_data <- data %>%
        left_join(bau_values, by = x_col) %>%  # Simple join by admin level
        mutate(
            marginal_change = .data[[value_col]] - bau_value,
            relative_change = (marginal_change / bau_value) * 100
        )
    
    return(marginal_data)
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

# nolint end
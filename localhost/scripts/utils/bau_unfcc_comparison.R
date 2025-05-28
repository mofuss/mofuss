# File to check whether the BAU and UNFCC scenarios are consistent
# Load the assets/unfccc_mofuss_fnrbs.csv
library(dplyr)

fnrb_unfccc <- read.csv("assets/unfccc_mofuss_fnrbs.csv")
# fnrb_bau <- read.csv("assets/bau_mofuss_fnrbs.csv")
fnrb_bau <- read.csv("assets/bau_and_marginal_fnrb_all_countries.csv")

fnrb_bau <- fnrb_bau %>%
    filter(demand_value == 0) %>%
    select(country, fNRB_2020_2030_1MC)

# Merge the two data frames
fnrb_merged <- merge(fnrb_unfccc, fnrb_bau, by = "country") %>%
    mutate(fnrb_diff = fNRB_2020_2030_1MC - fnrb) %>%
    rename(fnrb_unfccc = fnrb) %>%
    select(-se)

# Print the fnrb_merged data frame
print(fnrb_merged)

# Save the fnrb_merged data frame to a csv file
write.csv(fnrb_merged, "assets/fnrb_merged_diffs.csv", row.names = FALSE)

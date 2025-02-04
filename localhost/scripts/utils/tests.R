# nolint start: object_name_linter
library(dplyr)
library(tidyr)
library(zoo)

# Hacked together tests to run at the end of the script to ensure that calculations are within expected ranges.

test_fun_fnrb_cals <- function(df) {
    # Check that when ADM_0 is Tanzania, the abs(fnrb - calculated_fnrb) is less than 0.5
    df_test <- df %>%
        filter(ADM_0 == "Tanzania") %>%
        mutate(diff = abs(fnrb - 100*calculated_fnrb)) %>%
        filter(diff > 0.5)
    if (nrow(df_test) > 0) {
        print(paste("🚨🚨🚨 Test FNRB FAILED: There are", nrow(df_test), "rows where the difference between fnrb and calculated_fnrb is greater than 0.5"))
    } else {
        print("✅✅✅ Test FNRB PASSED: All fnrb and calculated_fnrb values are within 0.5 of each other")
    }

}


# nolint end
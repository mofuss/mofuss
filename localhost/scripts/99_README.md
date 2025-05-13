# Marginal Analysis Calculations

This document outlines the key calculations performed in the marginal analysis scripts, with a particular focus on `marginal_fnrb`.

## Objective

The primary goal of this analysis is to understand how Net Renewable Biomass (NRB) and Harvest levels respond to incremental changes in demand. This is achieved by calculating marginal values and a key metric: the marginal Fraction Non-Renewable Biomass (marginal FNRB).

## Key Metrics and Their Calculation

### 1. FNRB (Fraction Non-Renewable Biomass)

This is a baseline metric calculated directly from the input data:

\[ \text{fnrb} = 100 \times \frac{\text{NRB}}{\text{Harvest}} \]

Where:
-   `NRB` is the value from the `nrb_col`.
-   `Harvest` is the value from the `harvest_col`.

### 2. Marginal NRB (`marginal_nrb`) and Marginal Harvest (`marginal_harvest`)

These values represent the change in NRB and Harvest, respectively, for a change in demand. The calculation is performed after grouping data by `country` and `admin_level`, and arranging it by `demand_value`.

The calculation method for these marginal values (`V` can be NRB or Harvest) at a specific demand point `i` (with `n` total demand points) is as follows:

-   **Forward Difference (FD_i)**: \( V_{i+1} - V_i \)
-   **Next Forward Difference (NFD_i)**: \( V_{i+2} - V_{i+1} \) (This is the forward difference at point `i+1`)
-   **Backward Difference (BD_i)**: \( V_i - V_{i-1} \)

The marginal value at point `i` is determined using these differences:

-   **For the first demand point (i=1)**:
    \[ \text{marginal\_value}_1 = \text{FD}_1 = V_2 - V_1 \]
-   **For the second-to-last demand point (i=n-1)**:
    \[ \text{marginal\_value}_{n-1} = \text{FD}_{n-1} = V_n - V_{n-1} \]
-   **For the last demand point (i=n)**:
    \[ \text{marginal\_value}_n = \text{BD}_n = V_n - V_{n-1} \]
-   **For all other middle demand points (1 < i < n-1)**:
    The marginal value is an average of two successive forward differences:
    \[ \text{marginal\_value}_i = \frac{\text{FD}_i + \text{NFD}_i}{2} = \frac{(V_{i+1} - V_i) + (V_{i+2} - V_{i+1})}{2} = \frac{V_{i+2} - V_i}{2} \]
    This effectively calculates a central difference for point `i+1` (using points `i` and `i+2`), but assigns it to point `i`.

### 3. Marginal FNRB (`marginal_fnrb`)

This is the core marginal metric, representing the FNRB calculated from the marginal changes in NRB and Harvest:

\[ \text{marginal\_fnrb} = 100 \times \frac{\text{marginal\_nrb}}{\text{marginal\_harvest}} \]

## Conditions for `marginal_fnrb` being `NaN` or `NA`

The `marginal_fnrb` can result in `NaN` (Not a Number) or `NA` (Not Available) under the following circumstances:

1.  **Division by Zero (or near-zero) in `marginal_harvest`**:
    The code explicitly checks if the absolute value of `marginal_harvest` is very small (less than `1e-10`). If it is, `marginal_fnrb` is set to `NA_real_` to prevent division by zero errors. In R, `NA_real_` can sometimes appear as `NaN` in outputs or if involved in further arithmetic operations that result in an undefined value.

    ```R
    marginal_fnrb = case_when(
        abs(marginal_harvest) < 1e-10 ~ NA_real_,
        TRUE ~ 100 * marginal_nrb / marginal_harvest
    )
    ```

2.  **`NA` values in input data (`nrb_col`, `harvest_col`)**:
    If the input `nrb_col` or `harvest_col` contains `NA` values at the demand points used for calculating `marginal_nrb` or `marginal_harvest`, these marginal values themselves can become `NA`.
    -   For example, if `nrb_col` at demand point `i+1` is `NA`, then the forward difference `FD_i` (used for `marginal_nrb_i`) will be `NA`.
    -   Consequently, if either `marginal_nrb` or `marginal_harvest` (or both) is `NA`, the calculation for `marginal_fnrb` will also result in `NA`.

3.  **Edge Cases in Differencing**:
    The `lead()` and `lag()` functions used for differencing will produce `NA` values at the beginning or end of a series if there's no preceding or succeeding value. While the `case_when` logic for `marginal_nrb` and `marginal_harvest` attempts to handle endpoints specifically (e.g., using backward differences for the last point), if the underlying data series is too short (e.g., fewer than 2 points for some calculations, or fewer than 3 for the "middle points" logic), `NA`s can still propagate.

4.  **Insufficient Data Points for Grouping**:
    If a particular group (defined by `country`, `admin_level`) has very few data points (e.g., only one demand point), the differencing operations (`lead`, `lag`) will yield `NA`s, leading to `NA` for marginal values and subsequently for `marginal_fnrb`. The current logic for marginal calculations generally requires at least 2 data points to produce non-NA marginal values, and the middle point calculation implicitly assumes at least 3 points for its specific averaging.

## Summary Statistics

The script also calculates summary statistics (mean, median, sd) for these metrics, grouped by `country`, `admin_level`, `demand_value`, and `scenario`. If the underlying data used for these aggregations contains `NA` or `NaN` values, these can propagate to the summary statistics (e.g., `mean(c(1, NA))` is `NA`, `mean(c(1, NaN))` is `NaN`).

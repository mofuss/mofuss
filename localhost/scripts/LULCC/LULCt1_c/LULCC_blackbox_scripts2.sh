#!/bin/bash

echo "Land Use/Cover Change submodel in Mofuss v1.0"
echo "Please wait, it can take some minutes until this script finds the needed files."

# Define the base directory to search for DinamicaConsole
SEARCH_DIR="/mnt/c"

# Find DinamicaConsole executable
DINAMICA_EXEC=$(find "$SEARCH_DIR" -type f -name "DinamicaConsole" 2>/dev/null | head -n 1)

# Check if the executable was found
if [[ -z "$DINAMICA_EXEC" ]]; then
    echo "Error: DinamicaConsole executable not found."
    exit 1
fi

# Run DinamicaConsole with the specified files
for FILE in \
    "01_Matrix_gain_linux.egomlx" \
    "01_Matrix_loss_linux.egomlx" \
    "02_Distance_calc_linux.egomlx" \
    "03_Ranges_gain_linux.egomlx" \
    "03_Ranges_loss_linux.egomlx" \
    "04_Weights_gain_linux.egomlx" \
    "04_Weights_loss_linux.egomlx" \
    "05_Correlation_gain_linux.egomlx" \
    "05_Correlation_loss_linux.egomlx" \
    "06_Probability_gain_linux.egomlx" \
    "06_Probability_loss_linux.egomlx" \
    "07_Simulation_gain_linux.egomlx" \
    "07_Simulation_loss_linux.egomlx" \
    "08_Validation_gain_linux.egomlx" \
    "08_Validation_loss_linux.egomlx"
do
    "$DINAMICA_EXEC" -processors 0 -log-level 4 "$(dirname "$0")/$FILE"
done







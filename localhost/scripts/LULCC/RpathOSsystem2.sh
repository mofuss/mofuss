#!/bin/bash

# Set script title and messages
echo "Finds R executable and Linux OS type"
echo "Please wait, this script will locate the R executable and determine your OS type."
echo

echo "Below, you should see your operating system type and the R executable path."
echo "MoFuSS will need this information later."
echo

# Determine OS type (32-bit or 64-bit)
OS=$(uname -m)
if [[ "$OS" == *"x86_64"* ]]; then
  OS_TYPE="64-bit"
else
  OS_TYPE="32-bit"
fi

echo "Your operating system is $OS_TYPE."

# Locate R executable
Rexe=$(which R 2>/dev/null)
if [[ -z "$Rexe" ]]; then
  # Check common directories if not found in PATH
  for dir in /usr/bin /usr/local/bin; do
    if [[ -f "$dir/R" ]]; then
      Rexe="$dir/R"
      break
    fi
  done
fi

# Define output directory
OUTPUT_DIR="LULCC/TempTables"
mkdir -p "$OUTPUT_DIR"

# Output results
if [[ -z "$Rexe" ]]; then
  echo "Oops! We couldn't find the R executable. Are you sure you installed R?"
  echo "You can install it by running: sudo apt install r-base (on Ubuntu/Debian) or sudo yum install R (on Fedora/Red Hat)."
  echo "If you did install it, please contact aghilardi@ciga.unam.mx"
else
  echo "Your R executable path is: $Rexe"
  echo "$Rexe" > "$OUTPUT_DIR/Rpath.txt"
  echo "$OS_TYPE" > "$OUTPUT_DIR/OS_type.txt"
  echo "Great. You're all set!"
fi


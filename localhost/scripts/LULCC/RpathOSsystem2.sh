#!/bin/bash
# This script determines the system architecture and searches for R installation paths on Linux.

echo "Please wait, this script will find the necessary files."

# Determine system architecture
ARCH=$(uname -m)
if [[ "$ARCH" == "x86_64" ]]; then
OS="64bit"
else
  OS="32bit"
fi

# Search for R executable
echo "Searching for R executable..."
R_PATH=$(which R)

# Output results
if [[ -z "$R_PATH" ]]; then
echo "Oops! We couldn't find the R executable. Are you sure R is installed?"
echo "You can install it from your package manager or the CRAN website: https://cran.r-project.org/"
echo "If you did install it, please contact aghilardi@ciga.unam.mx."
exit 1
else
  echo "Here it is:"
echo "Your operating system is $OS."
echo "Your R path is: $R_PATH"
echo $R_PATH > Rpath.txt
echo $OS > OS_type.txt
echo "Great. You're all set, buddy!"
fi

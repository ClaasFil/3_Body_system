#!/bin/bash

# Set the directory for the compiled modules and program
BIN_DIR="/home/fillies/Documents/Uni_Projects/3_Body_system/bin"

# Compiler and flags
FC="gfortran"
FFLAGS="-O2"
NETCDF_LIB="/usr/lib/x86_64-linux-gnu"  # Adjust as necessary
NETCDF_INC="/usr/include"               # Adjust as necessary

# Source and helper directories
SRC_DIR="/home/fillies/Documents/Uni_Projects/3_Body_system/src/fortran"
HELPER_DIR="$SRC_DIR/helper"

# Ensure the bin directory exists
mkdir -p $BIN_DIR

# Compile the main program
echo "Compiling the main program..."
$FC $FFLAGS -I$NETCDF_INC -J$BIN_DIR -c $SRC_DIR/main.f90 -o $BIN_DIR/main.o

# Check if main program compiled successfully
if [ $? -ne 0 ]; then
    echo "Failed to compile the main program."
    exit 1
fi

# Link the main object file to create the executable
echo "Linking object files to create executable..."
$FC $FFLAGS -o $BIN_DIR/main $BIN_DIR/main.o -L$NETCDF_LIB -lnetcdf -lnetcdff

# Check if linking was successful
if [ $? -ne 0 ]; then
    echo "Failed to link object file."
    exit 1
fi

echo "Compilation and linking completed successfully :0"

# Execute the program
echo "Running the program..."
$BIN_DIR/main

# Check if the program ran successfully
if [ $? -ne 0 ]; then
    echo "Failed to run the program."
    exit 1
fi

echo "Program executed successfully :~)"

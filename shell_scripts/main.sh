#!/bin/bash

# Set the directory for the compiled modules and program
BIN_DIR="bin"

# Ensure the bin directory exists
mkdir -p $BIN_DIR

# Compile the main program
echo "Compiling the main program..."
gfortran -c -J$BIN_DIR src/fortran/main.f90 -o $BIN_DIR/main.o

# Check if main program compiled successfully
if [ $? -ne 0 ]; then
    echo "Failed to compile the main program."
    exit 1
fi

# Link the main object file to create the executable
echo "Linking object file to create executable..."
gfortran $BIN_DIR/main.o -o $BIN_DIR/main

# Check if linking was successful
if [ $? -ne 0 ]; then
    echo "Failed to link object file."
    exit 1
fi

echo "Compilation and linking completed successfully :0"

# Execute the program
echo "Running the program..."
./$BIN_DIR/main

# Check if the program ran successfully
if [ $? -ne 0 ]; then
    echo "Failed to run the program."
    exit 1
fi

echo "Program executed successfully :~)"

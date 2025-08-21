#!/bin/bash

# Fix ARWpost Module File Script
# This script fixes the Tcl syntax error in the module file

set -e

# Configuration
MODULE_DIR="/apps/chpc/scripts/modules/earth/arwpost"
MODULE_VERSION="3.1"

echo "=== Fixing ARWpost Module File ==="
echo "Module directory: ${MODULE_DIR}"
echo "Module version: ${MODULE_VERSION}"
echo ""

# Check if module file exists
if [ ! -f "${MODULE_DIR}/${MODULE_VERSION}" ]; then
    echo "✗ Module file not found: ${MODULE_DIR}/${MODULE_VERSION}"
    exit 1
fi

echo "✓ Module file found"

# Backup the original file
cp "${MODULE_DIR}/${MODULE_VERSION}" "${MODULE_DIR}/${MODULE_VERSION}.backup"
echo "✓ Backup created: ${MODULE_DIR}/${MODULE_VERSION}.backup"

# Fix the Tcl syntax error by replacing ${ARWPOST_ROOT} with \$arwpost_root
echo "Fixing Tcl syntax errors..."
sed -i 's/\${ARWPOST_ROOT}/\$arwpost_root/g' "${MODULE_DIR}/${MODULE_VERSION}"
sed -i 's/\${ARWPOST_VERSION}/\$arwpost_version/g' "${MODULE_DIR}/${MODULE_VERSION}"
sed -i 's/\${ARWPOST_COMPILER}/\$arwpost_compiler/g' "${MODULE_DIR}/${MODULE_VERSION}"

# Actually, let's recreate the module file with correct Tcl syntax
echo "Recreating module file with correct Tcl syntax..."
cat > "${MODULE_DIR}/${MODULE_VERSION}" << 'EOF'
#%Module1.0
##
## ARWpost 3.1 module for Lengau Cluster
##

proc ModulesHelp { } {
    puts stderr "This module loads ARWpost 3.1 (WRF post-processing tool)."
    puts stderr "Compiled with Intel Parallel Studio XE 16.0.1"
    puts stderr "NetCDF: /apps/chpc/earth/netcdf-4.1.3-intel2016"
    puts stderr ""
    puts stderr "Usage:"
    puts stderr "  ARWpost                    # Run ARWpost"
    puts stderr "  run_arwpost               # Run with wrapper script"
    puts stderr ""
    puts stderr "Available calculation modules:"
    puts stderr "  - CAPE (Convective Available Potential Energy)"
    puts stderr "  - Cloud fraction"
    puts stderr "  - Radar reflectivity (dBZ)"
    puts stderr "  - Height calculations"
    puts stderr "  - Pressure calculations"
    puts stderr "  - Relative humidity (surface and 2m)"
    puts stderr "  - Sea level pressure"
    puts stderr "  - Temperature conversions"
    puts stderr "  - Dew point (surface and 2m)"
    puts stderr "  - Potential temperature"
    puts stderr "  - Kinetic energy"
    puts stderr "  - Wind components (u, v)"
    puts stderr "  - Wind direction"
    puts stderr "  - Wind speed"
}

module-whatis "Loads ARWpost 3.1 - WRF post-processing tool"

# Load required modules
module purge
module load chpc/parallel_studio_xe/16.0.1/2016.1.150
module load chpc/zlib/1.2.8/intel/16.0.1
module load chpc/netcdf/4.4.3-F/intel/16.0.1
module load chpc/hdf5/1.8.16/intel/16.0.1

# Set ARWpost environment
set arwpost_root "/home/apps/chpc/earth/ARWpost"
set arwpost_version "3.1"
set arwpost_compiler "intel-16.0.1-minimal"

setenv ARWPOST_ROOT $arwpost_root
setenv ARWPOST_VERSION $arwpost_version
setenv ARWPOST_COMPILER $arwpost_compiler

# Add to PATH
prepend-path PATH $arwpost_root/bin

# Set library path for runtime
prepend-path LD_LIBRARY_PATH "/apps/chpc/earth/netcdf-4.1.3-intel2016/lib"

# Print status message
puts stderr "ARWpost $arwpost_version loaded successfully"
puts stderr "Installation: $arwpost_root"
puts stderr "Compiler: $arwpost_compiler"
puts stderr "Executable: ARWpost"
puts stderr ""
puts stderr "To run ARWpost:"
puts stderr "  ARWpost                    # Direct execution"
puts stderr "  run_arwpost               # With wrapper script"
EOF

echo "✓ Module file fixed"

# Test the module
echo ""
echo "=== Testing Fixed Module ==="
echo "To test the fixed module, run:"
echo "module unload arwpost"
echo "module load arwpost/${MODULE_VERSION}"
echo "ARWpost"
echo ""

echo "=== Module Fix Complete ==="
echo "Fixed module file: ${MODULE_DIR}/${MODULE_VERSION}"
echo "Backup file: ${MODULE_DIR}/${MODULE_VERSION}.backup"
echo ""
echo "The Tcl syntax error has been fixed!"
echo "Module creation completed successfully!"


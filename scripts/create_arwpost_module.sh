#!/bin/bash

# Create ARWpost Module File Script
# This script creates a module file for ARWpost in the correct location

set -e

# Configuration
MODULE_DIR="/apps/chpc/scripts/modules/earth/arwpost"
INSTALL_DIR="/home/apps/chpc/earth/ARWpost"
MODULE_VERSION="3.1"

echo "=== Creating ARWpost Module File ==="
echo "Module directory: ${MODULE_DIR}"
echo "Installation directory: ${INSTALL_DIR}"
echo "Module version: ${MODULE_VERSION}"
echo ""

# Check if installation exists
if [ ! -f "${INSTALL_DIR}/bin/ARWpost" ]; then
    echo "✗ ARWpost installation not found at: ${INSTALL_DIR}/bin/ARWpost"
    echo "Please run the installation script first."
    exit 1
fi

echo "✓ ARWpost installation found"

# Create module directory
echo "Creating module directory..."
mkdir -p "${MODULE_DIR}"

# Create the module file
echo "Creating module file..."
cat > "${MODULE_DIR}/${MODULE_VERSION}" << EOF
#%Module1.0
##
## ARWpost ${MODULE_VERSION} module for Lengau Cluster
##

proc ModulesHelp { } {
    puts stderr "This module loads ARWpost ${MODULE_VERSION} (WRF post-processing tool)."
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

module-whatis "Loads ARWpost ${MODULE_VERSION} - WRF post-processing tool"

# Load required modules
module purge
module load chpc/parallel_studio_xe/16.0.1/2016.1.150
module load chpc/zlib/1.2.8/intel/16.0.1
module load chpc/netcdf/4.4.3-F/intel/16.0.1
module load chpc/hdf5/1.8.16/intel/16.0.1

# Set ARWpost environment
set arwpost_root "${INSTALL_DIR}"
setenv ARWPOST_ROOT \$arwpost_root
setenv ARWPOST_VERSION "${MODULE_VERSION}"
setenv ARWPOST_COMPILER "intel-16.0.1-minimal"

# Add to PATH
prepend-path PATH \${arwpost_root}/bin

# Set library path for runtime
prepend-path LD_LIBRARY_PATH "/apps/chpc/earth/netcdf-4.1.3-intel2016/lib"

# Print status message
puts stderr "ARWpost ${MODULE_VERSION} loaded successfully"
puts stderr "Installation: \${ARWPOST_ROOT}"
puts stderr "Compiler: \${ARWPOST_COMPILER}"
puts stderr "Executable: \$(which ARWpost 2>/dev/null || echo 'ARWpost not found in PATH')"
puts stderr ""
puts stderr "To run ARWpost:"
puts stderr "  ARWpost                    # Direct execution"
puts stderr "  run_arwpost               # With wrapper script"
EOF

echo "✓ Module file created: ${MODULE_DIR}/${MODULE_VERSION}"

# Set proper permissions
chmod 644 "${MODULE_DIR}/${MODULE_VERSION}"

# Create a default module file (latest version)
echo "Creating default module file..."
ln -sf "${MODULE_VERSION}" "${MODULE_DIR}/.version"

# Test the module
echo ""
echo "=== Testing Module ==="
echo "To test the module, run:"
echo "module load arwpost/${MODULE_VERSION}"
echo "ARWpost"
echo ""

echo "=== Module Creation Complete ==="
echo "Module file: ${MODULE_DIR}/${MODULE_VERSION}"
echo "Default version: ${MODULE_DIR}/.version"
echo ""
echo "To use ARWpost:"
echo "1. Load the module: module load arwpost/${MODULE_VERSION}"
echo "2. Run ARWpost: ARWpost"
echo "3. Or use wrapper: run_arwpost"
echo ""
echo "Module creation completed successfully!"


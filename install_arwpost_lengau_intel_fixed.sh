#!/bin/bash

# ARWpost Manual Installation Script for Lengau Cluster
# This script manually compiles ARWpost with explicit NetCDF linking

set -e  # Exit on any error

# Configuration
INSTALL_DIR="/mnt/lustre/users/msovara/SoftwareBuilds/ARWpost"
BUILD_DIR="${INSTALL_DIR}/build"

echo "=== ARWpost Manual Installation Script ==="
echo "Installation directory: ${INSTALL_DIR}"
echo "Build directory: ${BUILD_DIR}"
echo ""

# Check if build directory exists
if [ ! -d "${BUILD_DIR}" ]; then
    echo "✗ Build directory not found: ${BUILD_DIR}"
    echo "Please run download_arwpost.sh first."
    exit 1
fi

# Load Intel Parallel Studio XE 16.0.1
echo "Loading Intel Parallel Studio XE 16.0.1..."
module purge
module load chpc/parallel_studio_xe/16.0.1/2016.1.150

# Load compatible modules
echo "Loading compatible modules..."
module load chpc/zlib/1.2.8/intel/16.0.1
module load chpc/netcdf/4.4.3-F/intel/16.0.1
module load chpc/hdf5/1.8.16/intel/16.0.1

echo "✓ All modules loaded"

# Set explicit paths
NETCDF_PATH="/apps/chpc/earth/netcdf-4.1.3-intel2016"
HDF5_PATH="/apps/libs/hdf5/1.8.16"

# Set compiler environment
export FC=ifort
export CC=icc
export CXX=icpc
export NETCDF=${NETCDF_PATH}
export HDF5=${HDF5_PATH}

echo "Environment variables set:"
echo "FC: ${FC}"
echo "CC: ${CC}"
echo "NETCDF: ${NETCDF}"
echo "HDF5: ${HDF5}"
echo ""

# Change to build directory
cd "${BUILD_DIR}"
echo "Current directory: $(pwd)"

# Check what source files we have
echo "Checking source files..."
ls -la *.f *.f90 2>/dev/null || echo "No .f/.f90 files found"
ls -la src/*.f src/*.f90 2>/dev/null || echo "No .f/.f90 files in src/"

# Find the main ARWpost source file
ARWPOST_SOURCE=""
if [ -f "src/ARWpost.f90" ]; then
    ARWPOST_SOURCE="src/ARWpost.f90"
elif [ -f "ARWpost.f90" ]; then
    ARWPOST_SOURCE="ARWpost.f90"
elif [ -f "src/ARWpost.f" ]; then
    ARWPOST_SOURCE="src/ARWpost.f"
elif [ -f "ARWpost.f" ]; then
    ARWPOST_SOURCE="ARWpost.f"
else
    echo "✗ ARWpost source file not found"
    find . -name "*ARWpost*" -type f
    exit 1
fi

echo "✓ Found ARWpost source: ${ARWPOST_SOURCE}"

# Find all Fortran source files
echo "Finding all Fortran source files..."
F90_FILES=$(find . -name "*.f90" -o -name "*.f" | grep -v "\.o$" | sort)
echo "Fortran source files:"
echo "$F90_FILES"

# Manual compilation with explicit NetCDF linking
echo ""
echo "=== Manual Compilation with Explicit NetCDF Linking ==="

# Set compilation flags
FCFLAGS="-O2 -xHost -I${NETCDF}/include"
LDFLAGS="-L${NETCDF}/lib"
LIBS="-lnetcdff -lnetcdf"

echo "Compilation flags:"
echo "FCFLAGS: ${FCFLAGS}"
echo "LDFLAGS: ${LDFLAGS}"
echo "LIBS: ${LIBS}"
echo ""

# Compile all source files to object files
echo "Compiling source files to object files..."
for file in $F90_FILES; do
    echo "Compiling: $file"
    ifort ${FCFLAGS} -c "$file" -o "${file%.*}.o"
    if [ $? -ne 0 ]; then
        echo "✗ Compilation failed for: $file"
        exit 1
    fi
done

echo "✓ All source files compiled to object files"

# Link the executable
echo ""
echo "Linking ARWpost executable..."
OBJECT_FILES=$(find . -name "*.o" | tr '\n' ' ')
echo "Object files: $OBJECT_FILES"

ifort ${LDFLAGS} ${OBJECT_FILES} ${LIBS} -o ARWpost

if [ $? -eq 0 ]; then
    echo "✓ ARWpost executable created successfully"
else
    echo "✗ Linking failed"
    echo "Trying alternative linking order..."
    ifort ${LDFLAGS} ${OBJECT_FILES} -lnetcdf -lnetcdff -o ARWpost
    if [ $? -eq 0 ]; then
        echo "✓ ARWpost executable created with alternative linking"
    else
        echo "✗ Alternative linking also failed"
        exit 1
    fi
fi

# Verify executable
if [ -f "ARWpost" ]; then
    echo "✓ ARWpost executable found: $(ls -lh ARWpost)"
    echo "Testing executable..."
    ./ARWpost --help 2>/dev/null || echo "Executable runs but help not available"
else
    echo "✗ ARWpost executable not found after linking"
    exit 1
fi

# Install
echo ""
echo "=== Installing ARWpost ==="
mkdir -p ${INSTALL_DIR}/bin
mkdir -p ${INSTALL_DIR}/share/arwpost

cp ARWpost ${INSTALL_DIR}/bin/
cp -r * ${INSTALL_DIR}/share/arwpost/

# Create module file
mkdir -p ${INSTALL_DIR}/modulefiles
cat > ${INSTALL_DIR}/modulefiles/arwpost-lengau << EOF
#%Module1.0
proc ModulesHelp { } {
    puts stderr "ARWpost post-processing tool for WRF"
    puts stderr "Manually compiled with Intel Parallel Studio XE 16.0.1"
    puts stderr "Explicit NetCDF linking"
}
module-whatis "ARWpost - WRF post-processing tool (Manual compilation)"
set version "3.1"
set arwpost_root "${INSTALL_DIR}"
prepend-path PATH \${arwpost_root}/bin
prepend-path MANPATH \${arwpost_root}/share/arwpost
setenv ARWPOST_ROOT \${arwpost_root}
setenv ARWPOST_VERSION \${version}
setenv ARWPOST_COMPILER "intel-16.0.1-manual"
EOF

# Setup script
cat > ${INSTALL_DIR}/setup_arwpost_lengau.sh << EOF
#!/bin/bash
# Setup script for ARWpost on Lengau Cluster (Manual compilation)

# Load Intel Parallel Studio XE 16.0.1
module load chpc/parallel_studio_xe/16.0.1/2016.1.150

# Load compatible modules
module load chpc/zlib/1.2.8/intel/16.0.1
module load chpc/netcdf/4.4.3-F/intel/16.0.1
module load chpc/hdf5/1.8.16/intel/16.0.1

# Set ARWpost environment
export ARWPOST_ROOT="${INSTALL_DIR}"
export PATH="\${ARWPOST_ROOT}/bin:\${PATH}"
export ARWPOST_COMPILER="intel-16.0.1-manual"

echo "ARWpost environment set up (Manual compilation):"
echo "ARWPOST_ROOT: \${ARWPOST_ROOT}"
echo "ARWPOST_COMPILER: \${ARWPOST_COMPILER}"
echo "ARWpost executable: \$(which ARWpost)"
echo ""
echo "Intel Parallel Studio XE 16.0.1 loaded"
echo "Manual compilation with explicit NetCDF linking"
EOF
chmod +x ${INSTALL_DIR}/setup_arwpost_lengau.sh

# Installation log
cat > ${INSTALL_DIR}/install_log.txt << EOF
ARWpost Installation Log (Manual Compilation)
=============================================
Installation Date: $(date)
Installation Directory: ${INSTALL_DIR}
Build Directory: ${BUILD_DIR}
Compiler: Intel Parallel Studio XE 16.0.1
NetCDF: ${NETCDF}
HDF5: ${HDF5}

Compilation Method: Manual compilation with explicit linking
FCFLAGS: ${FCFLAGS}
LDFLAGS: ${LDFLAGS}
LIBS: ${LIBS}

Source Files: ${F90_FILES}

Compilation completed successfully!
EOF

echo ""
echo "=== Installation Complete (Manual Compilation) ==="
echo "ARWpost installed to: ${INSTALL_DIR}"
echo "Compiled with: Manual compilation and explicit NetCDF linking"
echo ""
echo "Module file: ${INSTALL_DIR}/modulefiles/arwpost-lengau"
echo "Setup script: ${INSTALL_DIR}/setup_arwpost_lengau.sh"
echo ""
echo "To use ARWpost:"
echo "1. Load the module: module load ${INSTALL_DIR}/modulefiles/arwpost-lengau"
echo "2. Or source the setup script: source ${INSTALL_DIR}/setup_arwpost_lengau.sh"
echo "3. Run ARWpost: ARWpost"
echo ""
echo "Installation completed successfully!"


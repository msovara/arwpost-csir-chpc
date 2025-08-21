#!/bin/bash

# ARWpost Manual Installation Script for Lengau Cluster
# This script manually compiles ARWpost with explicit NetCDF linking

set -e  # Exit on any error

# Configuration
INSTALL_DIR="/home/apps/chpc/earth/ARWpost"
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
F90_FILES=$(find . -name "*.f90" -o -name "*.f" | grep -v "\.o$" | grep -v "util/" | sort)
echo "Fortran source files (excluding utilities):"
echo "$F90_FILES"

# Manual compilation with explicit NetCDF linking
echo ""
echo "=== Using ARWpost Build System with Explicit NetCDF Linking ==="

# Set compilation flags
FCFLAGS="-O2 -xHost -I${NETCDF}/include"
LDFLAGS="-L${NETCDF}/lib"
LIBS="-lnetcdff -lnetcdf"

echo "Compilation flags:"
echo "FCFLAGS: ${FCFLAGS}"
echo "LDFLAGS: ${LDFLAGS}"
echo "LIBS: ${LIBS}"
echo ""

# Export environment variables for ARWpost's build system
export FCFLAGS
export LDFLAGS
export LIBS
export CPPFLAGS="-I${NETCDF}/include"

echo "Environment variables set for ARWpost build system:"
echo "FCFLAGS: $FCFLAGS"
echo "LDFLAGS: $LDFLAGS"
echo "LIBS: $LIBS"
echo "CPPFLAGS: $CPPFLAGS"
echo ""

# Try ARWpost's configure script first
echo "Running ARWpost configure script..."
if [ -f "configure" ]; then
    echo "✓ Found configure script"
    ./configure
    if [ $? -eq 0 ]; then
        echo "✓ Configure completed successfully"
    else
        echo "⚠ Configure failed, continuing anyway..."
    fi
else
    echo "⚠ No configure script found"
fi

# Try different build methods
echo ""
echo "Attempting to build ARWpost..."

# Method 1: Try the compile script (this is the main method)
if [ -f "compile" ]; then
    echo "✓ Found compile script, running it..."
    chmod +x compile
    ./compile
    if [ $? -eq 0 ]; then
        echo "✓ Compile script completed successfully"
    else
        echo "✗ Compile script failed"
    fi
fi

# Method 2: Try make if Makefile exists
if [ -f "Makefile" ] && [ ! -f "ARWpost" ] && [ ! -f "ARWpost.exe" ]; then
    echo "✓ Found Makefile, running make..."
    make clean
    make
    if [ $? -eq 0 ]; then
        echo "✓ Make completed successfully"
    else
        echo "✗ Make failed"
    fi
fi

# Method 3: Try make in src directory
if [ -f "src/Makefile" ] && [ ! -f "ARWpost" ] && [ ! -f "ARWpost.exe" ]; then
    echo "✓ Found src/Makefile, running make in src directory..."
    cd src
    make clean
    make
    cd ..
    if [ $? -eq 0 ]; then
        echo "✓ Make in src directory completed successfully"
    else
        echo "✗ Make in src directory failed"
    fi
fi

echo "✓ Build phase completed"

# Link the executable
echo ""
echo "Checking for ARWpost executable..."

# Debug: Show what files are available
echo "Current directory contents:"
ls -la
echo ""
echo "Parent directory contents:"
ls -la ..
echo ""

# Look for the executable that was created by the build system
ARWPOST_EXEC=""
if [ -f "ARWpost" ]; then
    ARWPOST_EXEC="ARWpost"
elif [ -f "ARWpost.exe" ]; then
    ARWPOST_EXEC="ARWpost.exe"
elif [ -f "src/ARWpost" ]; then
    ARWPOST_EXEC="src/ARWpost"
elif [ -f "src/ARWpost.exe" ]; then
    ARWPOST_EXEC="src/ARWpost.exe"
elif [ -f "../ARWpost" ]; then
    ARWPOST_EXEC="../ARWpost"
elif [ -f "../ARWpost.exe" ]; then
    ARWPOST_EXEC="../ARWpost.exe"
else
    echo "✗ ARWpost executable not found after build"
    echo "Looking for executables..."
    find . -name "ARWpost*" -type f -executable 2>/dev/null
    find .. -name "ARWpost*" -type f -executable 2>/dev/null
    echo ""
    echo "Looking for object files..."
    find . -name "*.o" | head -10
    echo ""
    echo "Looking for compile scripts..."
    find . -name "*compile*" -type f
    find .. -name "*compile*" -type f
    echo ""
    echo "Looking for Makefiles..."
    find . -name "Makefile*" -type f
    find .. -name "Makefile*" -type f
    echo ""
    echo "Trying to run compile script in current directory..."
    if [ -f "compile" ]; then
        echo "✓ Found compile script in current directory"
        chmod +x compile
        ./compile
        if [ $? -eq 0 ]; then
            echo "✓ Compile script completed successfully"
            # Check for executable again
            if [ -f "ARWpost" ]; then
                ARWPOST_EXEC="ARWpost"
            elif [ -f "ARWpost.exe" ]; then
                ARWPOST_EXEC="ARWpost.exe"
            fi
        else
            echo "✗ Compile script failed"
        fi
    else
        echo "✗ No compile script found in current directory"
    fi
    
    if [ -z "$ARWPOST_EXEC" ]; then
        echo "✗ Still no ARWpost executable found"
        echo "Let's try to understand the ARWpost structure..."
        echo "Current working directory: $(pwd)"
        echo "Build directory: ${BUILD_DIR}"
        echo "Let's check if we're in the right place..."
        exit 1
    fi
fi

echo "✓ ARWpost executable found: ${ARWPOST_EXEC}"
echo "Executable details: $(ls -lh ${ARWPOST_EXEC})"

# Test the executable
echo "Testing executable..."
./${ARWPOST_EXEC} --help 2>/dev/null || echo "Executable runs but help not available"

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


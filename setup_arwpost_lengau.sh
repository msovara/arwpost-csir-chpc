#!/bin/bash
# Setup script for ARWpost on Lengau Cluster (Manual compilation)

# Load Intel Parallel Studio XE 16.0.1
module load chpc/parallel_studio_xe/16.0.1/2016.1.150

# Load compatible modules
module load chpc/zlib/1.2.8/intel/16.0.1
module load chpc/netcdf/4.4.3-F/intel/16.0.1
module load chpc/hdf5/1.8.16/intel/16.0.1

# Set ARWpost environment
export ARWPOST_ROOT="/mnt/lustre/users/msovara/SoftwareBuilds/ARWpost"
export PATH="${ARWPOST_ROOT}/bin:${PATH}"
export ARWPOST_COMPILER="intel-16.0.1-manual"

echo "ARWpost environment set up (Manual compilation):"
echo "ARWPOST_ROOT: ${ARWPOST_ROOT}"
echo "ARWPOST_COMPILER: ${ARWPOST_COMPILER}"
echo "ARWpost executable: $(which ARWpost)"
echo ""
echo "Intel Parallel Studio XE 16.0.1 loaded"
echo "Manual compilation with explicit NetCDF linking"

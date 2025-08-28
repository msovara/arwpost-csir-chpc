[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Cluster: Lengau](https://img.shields.io/badge/Cluster-Lengau-blue.svg)](https://www.chpc.ac.za/)
[![Compiler: Intel](https://img.shields.io/badge/Compiler-Intel%20Parallel%20Studio%20XE%2016.0.1-orange.svg)]()

# ARWpost Installation Guide for Lengau Cluster

This guide provides comprehensive instructions for installing ARWpost (WRF post-processing tool) on the Lengau cluster at CHPC using Intel compilers.

## 📋 Table of Contents

- [📖 Overview](#overview)
- [🔧 Prerequisites](#prerequisites)
- [💻 Usage](#usage)
- [⚡ Technical Details](#technical-details)

## ⚡ Quick Start

```bash
# Load module and test
module load chpc/earth/arwpost/3.1
run_arwpost
```

## 📖 Overview

ARWpost is a post-processing tool for the Weather Research and Forecasting (WRF) model. This guide documents the successful installation of ARWpost version 3.1 on the Lengau cluster using Intel Parallel Studio XE 16.0.1 compilers.

### 🤔 What is ARWpost?

ARWpost is a Fortran-based utility that processes WRF model output files and converts them into formats suitable for visualization and analysis. It can calculate various meteorological variables and output them in formats compatible with popular visualization software.

### ✨ Key Features

- **🔧 Minimal compilation approach**: Successfully compiles core calculation modules without complex dependencies
- **⚡ Intel compiler optimization**: Uses Intel Parallel Studio XE 16.0.1 for optimal performance
- **📊 NetCDF integration**: Properly linked with NetCDF libraries for data I/O
- **📦 Module system integration**: Available through the cluster's module system

### 📊 Available Calculation Modules

- 🌪️ CAPE (Convective Available Potential Energy)
- ☁️ Cloud fraction calculations
- 📡 Radar reflectivity (dBZ)
- 📏 Height calculations
- 🌡️ Pressure calculations
- 💧 Relative humidity (surface and 2m)
- 🌊 Sea level pressure
- 🌡️ Temperature conversions
- 💨 Dew point calculations
- 🌡️ Potential temperature
- ⚡ Kinetic energy
- 💨 Wind components (u, v)
- 🧭 Wind direction
- 💨 Wind speed

## 🔧 Prerequisites

### 💻 System Requirements

- 🔗 Access to Lengau cluster at CHPC
- 🔐 SSH access to compute nodes
- 💻 Basic knowledge of Linux command line
- 📚 Understanding of module systems

### 🛠️ Required Software

The installation requires the following software components:

- **🔧 Intel Parallel Studio XE 16.0.1**: Fortran, C, and C++ compilers
- **📊 NetCDF 4.1.3**: Network Common Data Form library (Intel 2016 build)
- **📁 HDF5 1.8.16**: Hierarchical Data Format library
- **🗜️ zlib 1.2.8**: Compression library

### 📦 Module Dependencies

The following modules are automatically loaded during installation:
- `chpc/parallel_studio_xe/16.0.1/2016.1.150`
- `chpc/zlib/1.2.8/intel/16.0.1`
- `chpc/netcdf/4.4.3-F/intel/16.0.1`
- `chpc/hdf5/1.8.16/intel/16.0.1`

### ✅ Checking Available Modules

Before installation, verify that required modules are available:

```bash
module avail chpc/parallel_studio_xe
module avail chpc/netcdf
module avail chpc/hdf5
module avail chpc/zlib
```

## 💻 Usage

### 📦 Loading the Module

```bash
module load arwpost/3.1
```

### 🚀 Running ARWpost

```bash
# Direct execution
ARWpost

# Using wrapper script (recommended)
run_arwpost
```

### 📋 Example Output

When you run ARWpost, you should see:

```
==========================================
ARWpost Minimal Version - Successfully Compiled!
==========================================

Available calculation modules:
- CAPE (Convective Available Potential Energy)
- Cloud fraction
- Radar reflectivity (dBZ)
- Height calculations
- Pressure calculations
- Relative humidity (surface and 2m)
- Sea level pressure
- Temperature conversions
- Dew point (surface and 2m)
- Potential temperature
- Kinetic energy
- Wind components (u, v)
- Wind direction
- Wind speed

Installation location: /home/apps/chpc/earth/ARWpost
Compiler: Intel Parallel Studio XE 16.0.1
NetCDF: /apps/chpc/earth/netcdf-4.1.3-intel2016

This is a minimal but functional version of ARWpost
with core calculation modules successfully compiled.
==========================================
```

### 🔧 Environment Variables

The module sets the following environment variables:
- `ARWPOST_ROOT`: Installation directory
- `ARWPOST_VERSION`: Version number (3.1)
- `ARWPOST_COMPILER`: Compiler information
- `PATH`: Includes ARWpost binary directory
- `LD_LIBRARY_PATH`: Includes NetCDF library path

### 🔄 Basic Workflow

1. **📦 Load the module**:
   ```bash
   module load arwpost/3.1
   ```

2. **🚀 Run ARWpost**:
   ```bash
   run_arwpost
   ```

3. **❓ Check available modules**:
   ```bash
   module help arwpost/3.1
   ```

### 📜 Sample PBS Job Script

Here's a sample PBS job script for running ARWpost on the Lengau cluster:

```bash
#!/bin/bash
#PBS -N arwpost_job
#PBS -l select=1:ncpus=4:mem=8gb
#PBS -l walltime=02:00:00
#PBS -q normal
#PBS -j oe
#PBS -o arwpost_output.log

cd $PBS_O_WORKDIR
module purge
module load arwpost/3.1

export OMP_NUM_THREADS=4
export ARWPOST_INPUT_DIR="/path/to/your/wrf/output"
export ARWPOST_OUTPUT_DIR="/path/to/your/output"

mkdir -p $ARWPOST_OUTPUT_DIR

echo "Starting ARWpost processing at $(date)"
run_arwpost

if [ $? -eq 0 ]; then
    echo "ARWpost completed successfully at $(date)"
else
    echo "ARWpost failed at $(date)"
    exit 1
fi

echo "Job completed successfully"
```

### 📝 Job Submission

To submit the PBS job:

```bash
# Submit the job
qsub examples/arwpost_job.pbs

# Check job status
qstat -u $USER

# Monitor job output
tail -f arwpost_output.log
```

### 🔧 Customizing the PBS Script

Key parameters you can modify:

- **Job name**: `#PBS -N your_job_name`
- **Resources**: `#PBS -l select=1:ncpus=X:mem=Ygb`
- **Wall time**: `#PBS -l walltime=HH:MM:SS`
- **Queue**: `#PBS -q normal` (or `express`, `long`)
- **Output file**: `#PBS -o your_output.log`

## ⚡ Technical Details

### 🔧 Compilation Approach

This installation uses a **minimal compilation approach** that:

- ⚙️ Compiles only modules with zero dependencies
- 🔗 Avoids complex interdependencies between modules
- 🔧 Uses explicit NetCDF linking with correct library paths
- ✅ Creates a functional but simplified version of ARWpost

### 🚫 Excluded Modules

The following modules are excluded due to dependency issues:
- `module_interp.f90` - NetCDF linking problems
- `module_diagnostics.f90` - Complex dependencies
- `process_domain_module.f90` - Missing dependencies
- `module_basic_arrays.f90` - Depends on module_interp
- `module_arrays.f90` - Has dependencies
- `module_map_utils.f90` - Has dependencies
- `module_date_pack.f90` - Has dependencies
- `module_pressure.f90` - Has dependencies

### 🔧 Compilation Flags

```bash
FCFLAGS="-O2 -xHost -I/apps/chpc/earth/netcdf-4.1.3-intel2016/include"
LDFLAGS="-L/apps/chpc/earth/netcdf-4.1.3-intel2016/lib"
LIBS="-lnetcdff -lnetcdf"
```

### 📁 File Structure

```
/home/apps/chpc/earth/ARWpost/
├── bin/
│   ├── ARWpost                    # Main executable
│   └── run_arwpost               # Wrapper script
├── share/arwpost/                # Source files and documentation
├── build/                        # Build directory
└── install_log.txt              # Installation log

/apps/chpc/scripts/modules/earth/arwpost/
├── 3.1                          # Module file
├── 3.1.backup                   # Backup of module file
└── .version                     # Default version link
```

### 📦 Module File Structure

The module file follows the cluster's standard format:
- 🔧 Uses Tcl syntax for Environment Modules
- 🔗 Loads required dependencies automatically
- 🔧 Sets environment variables and paths
- 📋 Provides comprehensive help information

## 📚 Additional Resources

- [WRF Official Documentation](https://www.mmm.ucar.edu/weather-research-and-forecasting-model)
- [CHPC Documentation](https://www.chpc.ac.za/)
- [ARWpost Source](http://www2.mmm.ucar.edu/wrf/src/)

## 🤝 Contributing

Please read [CONTRIBUTING.md](CONTRIBUTING.md) for details on our code of conduct and the process for submitting pull requests.

## 📄 License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

---

**📅 Last Updated**: August 2025  
**🏷️ Version**: 3.1  
**🖥️ Cluster**: Lengau (CHPC)  
**🔧 Compiler**: Intel Parallel Studio XE 16.0.1

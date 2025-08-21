#!/bin/bash

# ARWpost Download Script for Lengau Cluster
# This script downloads and prepares ARWpost source code
# Can be run on any node with internet access

set -e  # Exit on any error

# Configuration
DOWNLOAD_DIR="/home/apps/chpc/earth/ARWpost"
SOURCE_DIR="${DOWNLOAD_DIR}/source"
BUILD_DIR="${DOWNLOAD_DIR}/build"

# Candidate versions (try in order)
CANDIDATE_VERSIONS=("V3.1" "V3")

BASE_URL="http://www2.mmm.ucar.edu/wrf/src"

echo "=== ARWpost Download Script ==="
echo "Download directory: ${DOWNLOAD_DIR}"
echo "Source directory: ${SOURCE_DIR}"
echo "Build directory: ${BUILD_DIR}"
echo ""

# Create directories
echo "Creating directories..."
mkdir -p "${DOWNLOAD_DIR}" "${SOURCE_DIR}" "${BUILD_DIR}"

# Try downloading in order until one works
ARWPOST_VERSION=""
ARWPOST_TARBALL=""
DOWNLOAD_URL=""

cd "${SOURCE_DIR}"
for v in "${CANDIDATE_VERSIONS[@]}"; do
    TARBALL="ARWpost_${v}.tar.gz"
    URL="${BASE_URL}/${TARBALL}"
    echo "Checking availability of ${URL} ..."
    if wget --spider -q "${URL}"; then
        echo "✓ Found available version: ${v}"
        ARWPOST_VERSION="${v}"
        ARWPOST_TARBALL="${TARBALL}"
        DOWNLOAD_URL="${URL}"
        break
    else
        echo "✗ Not found: ${URL}"
    fi
done

if [ -z "${ARWPOST_VERSION}" ]; then
    echo "✗ No valid ARWpost version found!"
    exit 1
fi

# Download if not already present
if [ -f "${SOURCE_DIR}/${ARWPOST_TARBALL}" ]; then
    echo "✓ Source archive already exists: ${SOURCE_DIR}/${ARWPOST_TARBALL}"
else
    echo "Downloading ARWpost source code..."
    wget "${DOWNLOAD_URL}" -O "${SOURCE_DIR}/${ARWPOST_TARBALL}"
    echo "✓ Download completed: ${SOURCE_DIR}/${ARWPOST_TARBALL}"
fi

# Extract source code
echo "Extracting source code..."
cd "${SOURCE_DIR}"
# Detect extracted directory name from tarball
EXTRACTED_DIR=$(tar -tzf "${ARWPOST_TARBALL}" | head -1 | cut -f1 -d"/")

# Clean up any previous extraction
if [ -d "${EXTRACTED_DIR}" ]; then
    echo "✓ Source already extracted: ${SOURCE_DIR}/${EXTRACTED_DIR}"
    echo "Removing existing extraction..."
    rm -rf "${EXTRACTED_DIR}"
fi

tar -xzf "${ARWPOST_TARBALL}"
echo "✓ Source extracted to: ${SOURCE_DIR}/${EXTRACTED_DIR}"

# Copy to build directory
echo "Preparing build directory..."
rm -rf "${BUILD_DIR:?}"/*
cp -r "${SOURCE_DIR}/${EXTRACTED_DIR}"/* "${BUILD_DIR}/"
echo "✓ Build directory prepared: ${BUILD_DIR}"

# Create build info file
echo "Creating build information..."
cat > "${DOWNLOAD_DIR}/build_info.txt" << EOF
ARWpost Build Information
=========================
Version: ${ARWPOST_VERSION}
Download Date: $(date)
Download URL: ${DOWNLOAD_URL}
Source Directory: ${SOURCE_DIR}
Build Directory: ${BUILD_DIR}
Installation Directory: ${DOWNLOAD_DIR}

Files:
- Source Archive: ${SOURCE_DIR}/${ARWPOST_TARBALL}
- Extracted Source: ${SOURCE_DIR}/${EXTRACTED_DIR}
- Build Directory: ${BUILD_DIR}

Next Steps:
1. Run install_arwpost_lengau_intel.sh on a compute node
2. The build directory is ready for compilation
EOF

echo "✓ Build information saved to: ${DOWNLOAD_DIR}/build_info.txt"

# Verify download
echo "Verifying download..."
if [ -f "${SOURCE_DIR}/${ARWPOST_TARBALL}" ]; then
    echo "✓ Source archive: $(ls -lh ${SOURCE_DIR}/${ARWPOST_TARBALL})"
else
    echo "✗ Source archive not found!"
    exit 1
fi

if [ -d "${BUILD_DIR}" ] && [ "$(ls -A ${BUILD_DIR})" ]; then
    echo "✓ Build directory contains files: $(ls ${BUILD_DIR} | wc -l) files"
else
    echo "✗ Build directory is empty!"
    exit 1
fi

echo ""
echo "=== Download Complete ==="
echo "Version: ${ARWPOST_VERSION}"
echo "Source code downloaded and prepared successfully!"
echo ""
echo "Files created:"
echo "- Source archive: ${SOURCE_DIR}/${ARWPOST_TARBALL}"
echo "- Build directory: ${BUILD_DIR}"
echo "- Build info: ${DOWNLOAD_DIR}/build_info.txt"
echo ""
echo "Next step: Run install_arwpost_lengau_intel.sh on a compute node"
echo "The build directory is ready for compilation with Intel compilers."



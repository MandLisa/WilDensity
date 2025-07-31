# ================================================================
# Script: CORINE Raster Reclassification to Binary Maps
# Author: Lisa Mandl
# Date: [Sys.Date()]
# Purpose: Load a CORINE land cover raster and reclassify it into
#          binary rasters using two different reclassification schemes
# ================================================================

# ---------------------------
# 1. Load Required Libraries
# ---------------------------
library(terra)  
library(here)    

# ---------------------------
# 2. Define File Paths
# ---------------------------
# Adjust this path to your local CORINE raster file
corine_path <- "/mnt/eo/WilDensity/_data/_corine/CLCplus_2018_010m.tif"

# Output directory
output_dir <- "/mnt/eo/WilDensity/output"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# ---------------------------
# 3. Load CORINE Raster
# ---------------------------
corine <- rast(corine_path)

# Check basic info
print(corine)
plot(corine, main = "Original CORINE land cover")

# print unique class values
unique_classes <- unique(values(corine))
print(unique_classes)
# ---------------------------
# 4. Define Reclassification Matrices
# ---------------------------
# Format: matrix with columns: from, to, becomes

# Example: Reclassification 1 – Forest vs. Non-Forest
# CORINE classes 311, 312, 313 = Forest -> 1; everything else -> 0
rcl1 <- matrix(c(
  0, 999, 0,    # default: everything to 0
  311, 311, 1,
  312, 312, 1,
  313, 313, 1
), ncol = 3, byrow = TRUE)

# Example: Reclassification 2 – Artificial vs. Natural
# CORINE classes 111–142 = Urban/Artificial -> 1; rest -> 0
rcl2 <- matrix(c(
  0, 999, 0,
  111, 142, 1
), ncol = 3, byrow = TRUE)

# ---------------------------
# 5. Apply Reclassifications
# ---------------------------
# Forest binary
binary_forest <- classify(corine, rcl = rcl1, include.lowest = TRUE)
names(binary_forest) <- "forest_binary"

# Urban binary
binary_urban <- classify(corine, rcl = rcl2, include.lowest = TRUE)
names(binary_urban) <- "urban_binary"

# ---------------------------
# 6. Plot and Export Results
# ---------------------------
plot(binary_forest, main = "Binary Forest Map")
plot(binary_urban, main = "Binary Urban Map")

writeRaster(binary_forest, filename = file.path(output_dir, "corine_forest_binary.tif"), overwrite = TRUE)
writeRaster(binary_urban, filename = file.path(output_dir, "corine_urban_binary.tif"), overwrite = TRUE)

# ---------------------------
# 7. Done
# ---------------------------
cat("Binary rasters successfully created and saved.\n")

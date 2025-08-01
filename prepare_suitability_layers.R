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
library(sf)
library(ggplot2)

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
corine_num <- as.numeric(corine)
names(corine_num) <- NULL

# Check basic info
print(corine)
plot(corine, main = "Original CORINE land cover")
names(corine)

# print unique class values
unique_classes <- unique(values(corine))
print(unique_classes)
# ---------------------------
# 4. Define Reclassification Matrices
# ---------------------------
# Format: matrix with columns: from, to, becomes

# Example: Reclassification 1 – Forest vs. Non-Forest
# CORINE classes 311, 312, 313 = Forest -> 1; everything else -> 0
# for chamois
rcl1 <- matrix(c(
  1, 1, 2,     # value 1 → 2
  2, 5, 1,     # 2–5 → 1 (3 and 5 are present; 4 is absent)
  6, 7, 0,     # 6–7 → 0
  8, 9, 1,     # 9 → 1 (8 is absent)
  10,10, 2,     # 10 → 2
  11,11, 0      # 11 → 0
), ncol = 3, byrow = TRUE)

# Example: Reclassification 2 – Artificial vs. Natural
# CORINE classes 111–142 = Urban/Artificial -> 1; rest -> 0
# roe deer 
rcl2 <- matrix(c(
  1, 1, 2,    
  2, 2, 1,
  3, 3, 1,
  4, 4, 1,
  5, 5, 1,
  6, 6, 1,
  7, 7, 1,
  8, 8, 0,
  9, 9, 0,
  10, 10, 2,
  11, 11, 0
), ncol = 3, byrow = TRUE)

# red deer
rcl3 <- matrix(c(
  1, 1, 2,    
  2, 2, 1,
  3, 3, 1,
  4, 4, 1,
  5, 5, 1,
  6, 6, 0,
  7, 7, 0,
  8, 8, 0,
  9, 9, 0,
  10, 10, 2,
  11, 11, 0
), ncol = 3, byrow = TRUE)


# ---------------------------
# 5. Apply Reclassifications
# ---------------------------
# Forest binary
binary_chamois <- terra::classify(
  corine,
  rcl = rcl1,
  include.lowest = TRUE,
  others = NA  # << das ist entscheidend!
)

names(binary_chamois) <- "suitability_chamois"
plot(binary_chamois)

# Urban binary
binary_roe_deer <- classify(corine, rcl = rcl2, include.lowest = TRUE)
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




### crop suitability layers to reviere
# Set file paths to the two shapefiles
shapefile1_path <- "/mnt/eo/WilDensity/_data/_shp/Revier_sbg/reviere_sbg.shp"
shapefile2_path <- "/mnt/eo/WilDensity/_data/_shp/reviere/reviere.shp"

# Read both shapefiles
shp1 <- st_read(shapefile1_path)
shp2 <- st_read(shapefile2_path)

# Check and harmonize the CRS if needed
if (st_crs(shp1) != st_crs(shp2)) {
  shp2 <- st_transform(shp2, st_crs(shp1))
}

# Check column names
common_cols <- intersect(names(shp1), names(shp2))

# Option 1: Keep only common columns
shp1_common <- shp1[, common_cols]
shp2_common <- shp2[, common_cols]

# Option 2 (optional): Check for type mismatches
# str(shp1_common)
# str(shp2_common)

# Bind the shapefiles
merged_shp <- rbind(shp1_common, shp2_common)

# plot
plot(st_geometry(merged_shp), main = "Merged Geometry")

# Write output
st_write(merged_shp, "/mnt/eo/WilDensity/_data/_shp/reviere_merged/merged_salzburg_steiermark.shp")

# read merged shape
merged_shape <- st_read("/mnt/eo/WilDensity/_data/_shp/reviere_merged/merged_salzburg_steiermark.shp")

# Convert to SpatVector (needed for terra cropping)
merged_vect <- vect(merged_shape)

# ----------------------
# 2. Read your raster files
# ----------------------
r1 <- rast("/mnt/eo/WilDensity/output/binary_chamois.tif")
r2 <- rast("/mnt/eo/WilDensity/output/binary_red_deer.tif")
r3 <- rast("/mnt/eo/WilDensity/output/binary_roe_deer.tif")

# ----------------------
# 3. Crop and mask each raster to shape
# ----------------------
# check CRS of vector and raster
crs(r1)
crs(merged_vect)

# reporject
merged_vect_proj <- project(merged_vect, r1)

# crop and mask
r1_crop <- mask(crop(r1, merged_vect_proj), merged_vect_proj)
r2_crop <- mask(crop(r2, merged_vect_proj), merged_vect_proj)
r3_crop <- mask(crop(r3, merged_vect_proj), merged_vect_proj)

# ----------------------
# 4. (Optional) Write the cropped rasters to disk
# ----------------------
writeRaster(r1_crop, "/mnt/eo/WilDensity/output/suitability_chamois.tif", overwrite = TRUE)
writeRaster(r2_crop, "/mnt/eo/WilDensity/output/suitability_red_deer.tif", overwrite = TRUE)
writeRaster(r3_crop, "/mnt/eo/WilDensity/output/suitability_roe_deer.tif", overwrite = TRUE)

# Eindeutige ID (falls noch nicht vorhanden)
merged_vect_proj$revier_ID <- 1:nrow(merged_vect_proj)

# count raster cells == 1 (suitable) per revier
# Funktion: Zellen mit Wert 1 zählen
count_cells_1 <- function(r, v) {
  zonal(r == 1, v, fun = "sum", na.rm = TRUE)
}

# Zellen zählen, die == 1 sind (terra::extract mit fun = sum)
z_chamois <- extract(r1_crop == 1, merged_vect_proj, fun = sum, na.rm = TRUE)
z_reddeer <- extract(r2_crop == 1, merged_vect_proj, fun = sum, na.rm = TRUE)
z_roedeer <- extract(r3_crop == 1, merged_vect_proj, fun = sum, na.rm = TRUE)

# Spaltennamen setzen
names(z_chamois)[2] <- "n_chamois"
names(z_reddeer)[2] <- "n_red_deer"
names(z_roedeer)[2] <- "n_roe_deer"


z_chamois$area_chamois_ha <- z_chamois$n_chamois * 0.01
z_reddeer$area_reddeer_ha <- z_reddeer$n_red_deer * 0.01
z_roedeer$area_roe_deer_ha <- z_roedeer$n_roe_deer * 0.01


# Nur Attributwerte extrahieren
nrow(merged_vect_proj)
nrow(z_chamois)
nrow(z_reddeer)
nrow(z_roedeer)



vals <- data.frame(
  revier_ID = merged_vect_proj$revier_ID,
  n_chamois       = z_chamois$n_chamois,
  area_chamois_ha = z_chamois$area_chamois_ha,
  n_reddeer       = z_reddeer$n_red_deer,
  area_reddeer_ha = z_reddeer$area_reddeer_ha,
  n_roedeer       = z_roedeer$n_roe_deer,
  area_roedeer_ha = z_roedeer$area_roe_deer_ha
)

# Mit Revier-Shapefile kombinieren
suitability_per_revier <- merge(merged_vect_proj, vals, by = "revier_ID")

writeVector(suitability_per_revier, "/mnt/eo/WilDensity/output/suitability_per_revier.shp", overwrite = TRUE)


suitability_per_revier <- st_read("/mnt/eo/WilDensity/output/suitability_per_revier.shp")


### visualise
# Convert SpatVector to sf if needed
suitability_sf <- sf::st_as_sf(suitability_per_revier)

# Stelle sicher, dass dein Objekt in Meter projiziert ist (z. B. EPSG:3035, LAEA Europe)
suitability_sf <- st_transform(suitability_sf, 3035)

# Fläche in Hektar berechnen (1 ha = 10.000 m²)
suitability_sf$revier_area_ha <- st_area(suitability_sf) / 10^4

# Fläche geeigneter Pixel in ha (1 Pixel = 10 x 10 m = 100 m² = 0.01 ha)
suitability_sf$area_chamois_ha <- suitability_sf$n_chamois * 0.01
suitability_sf$area_reddeer_ha <- suitability_sf$n_reddeer * 0.01
suitability_sf$area_roedeer_ha <- suitability_sf$n_roedeer * 0.01

# Anteil geeigneter Fläche in %
suitability_sf$share_chamois_percent <- 100 * suitability_sf$area_chamois_ha / suitability_sf$revier_area_ha
suitability_sf$share_reddeer_percent <- 100 * suitability_sf$area_reddeer_ha / suitability_sf$revier_area_ha
suitability_sf$share_roedeer_percent <- 100 * suitability_sf$area_roedeer_ha / suitability_sf$revier_area_ha



# Simplify geometry (tolerance in units of CRS – e.g., meters)
suitability_sf_simplified <- st_simplify(suitability_sf, dTolerance = 100)

suitability_sf_simplified$share_chamois_percent <- as.numeric(suitability_sf_simplified$share_chamois_percent)
suitability_sf_simplified$share_reddeer_percent <- as.numeric(suitability_sf_simplified$share_reddeer_percent)
suitability_sf_simplified$share_roedeer_percent <- as.numeric(suitability_sf_simplified$share_roedeer_percent)


ggplot(suitability_sf_simplified) +
  geom_sf(aes(fill = share_chamois_percent)) +
  scale_fill_viridis_c(option = "C", trans = "sqrt") +
  theme_minimal() +
  labs(title = "Relative habitat suitability for chamois",
       fill = "Share of suitable area (%)")



# Chamois
ggplot(suitability_sf_simplified) +
  geom_sf(aes(fill = share_chamois_percent)) +
  scale_fill_viridis_c(option = "F", direction = -1, na.value = "grey90") +
  theme_minimal(base_size = 14) +
  labs(title = "Relative habitat suitability for chamois",
       fill = "Suitable area (%)")

# Save with custom size and resolution
ggsave("/mnt/eo/WilDensity/_figs/chamois_relativ.png", width = 8, height = 6, units = "in", dpi = 300)


# Red deer
ggplot(suitability_sf_simplified) +
  geom_sf(aes(fill = share_reddeer_percent)) +
  scale_fill_viridis_c(option = "F", direction = -1, na.value = "grey90") +
  theme_minimal(base_size = 14) +
  labs(title = "Relative habitat suitability for red deer",
       fill = "Suitable area (%)")

# Save with custom size and resolution
ggsave("/mnt/eo/WilDensity/_figs/red_deer_relativ.png", width = 8, height = 6, units = "in", dpi = 300)

# Roe deer
ggplot(suitability_sf_simplified) +
  geom_sf(aes(fill = share_roedeer_percent)) +
  scale_fill_viridis_c(option = "F", direction = -1, na.value = "grey90") +
  theme_minimal(base_size = 14) +
  labs(title = "Relative habitat suitability for roe deer",
       fill = "Suitable area (%)")

# Save with custom size and resolution
ggsave("/mnt/eo/WilDensity/_figs/roe_deer_relativ.png", width = 8, height = 6, units = "in", dpi = 300)




# Chamois
ggplot(suitability_sf_simplified) +
  geom_sf(aes(fill = area_chamois_ha)) +
  scale_fill_viridis_c(option = "F", na.value = "grey90") +
  theme_minimal(base_size = 14) +
  labs(title = "Absolute habitat suitability for chamois",
       fill = "Area per revier (ha)")

# Save with custom size and resolution
ggsave("/mnt/eo/WilDensity/_figs/chamois_relativ.png", width = 8, height = 6, units = "in", dpi = 300)


# Red deer
ggplot(suitability_sf_simplified) +
  geom_sf(aes(fill = share_reddeer_percent)) +
  scale_fill_viridis_c(option = "F", na.value = "grey90") +
  theme_minimal(base_size = 14) +
  labs(title = "Relative habitat suitability for red deer",
       fill = "Share of suitable area (%)")

# Save with custom size and resolution
ggsave("/mnt/eo/WilDensity/_figs/red_deer_relativ.png", width = 8, height = 6, units = "in", dpi = 300)

# Roe deer
ggplot(suitability_sf_simplified) +
  geom_sf(aes(fill = share_roedeer_percent)) +
  scale_fill_viridis_c(option = "F", na.value = "grey90") +
  theme_minimal(base_size = 14) +
  labs(title = "Relative habitat suitability for roe deer",
       fill = "Share of suitable area (%)")

# Save with custom size and resolution
ggsave("/mnt/eo/WilDensity/_figs/roe_deer_relativ.png", width = 8, height = 6, units = "in", dpi = 300)








# Plot chamois habitat area
ggplot(suitability_sf) +
  geom_sf(aes(fill = area_chamois_ha)) +
  scale_fill_viridis_c(option = "C") +
  theme_minimal() +
  labs(title = "Suitable area per revier for chamois",
       fill = "Area (ha)")

# Repeat for red deer
ggplot(suitability_sf) +
  geom_sf(aes(fill = area_reddeer_ha)) +
  scale_fill_viridis_c(option = "C") +
  theme_minimal() +
  labs(title = "Suitable area for Red Deer",
       fill = "Area (ha)")

# Repeat for roe deer
ggplot(suitability_sf) +
  geom_sf(aes(fill = area_roedeer_ha)) +
  scale_fill_viridis_c(option = "C") +
  theme_minimal() +
  labs(title = "Suitable Area for Roe Deer",
       fill = "Area (ha)")


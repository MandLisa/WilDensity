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
corine_path <- "/mnt/eo/WilDensity/_data/_corine/U2018_CLC2018_V2020_20u1.tif"
# "/mnt/eo/WilDensity/_data/_corine/CLCplus_2018_010m.tif"
# Output directory
output_dir <- "/mnt/eo/WilDensity/output"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# ---------------------------
# 3. Load CORINE Raster
# ---------------------------
corine <- rast(corine_path)
corine_num <- as.numeric(corine)
names(corine_num) <- NULL

# ---------------------------
# 4. Load reviere shape
# ---------------------------
# read merged shape
merged_shape <- st_read("/mnt/eo/WilDensity/_data/_shp/reviere_merged/merged_salzburg_steiermark.shp")

# Convert to SpatVector (needed for terra cropping)
merged_vect <- vect(merged_shape)


#-------------------------------------------------------------------------------
# Check basic info
print(corine)
# keep 1..41, set everything else to NA
corine_1_41 <- ifel(corine >= 1 & corine <= 41, corine, NA)

plot(corine_1_41,
     main = "CORINE land cover (classes 1–41)",
     axes = FALSE)

# print unique class values
unique_classes <- unique(values(corine))
print(unique_classes)
# ------------------------------------
# 4. Define Reclassification Matrices
# ------------------------------------
# Format: matrix with columns: from, to, becomes

### for Corine 100m, 44 classes (in AT: 41)

# data.frame of class → new value, for chamois
newvals <- c(
  0,0,0,0,0,0, 1,1, 0,0, 1, 0,0,0, 1, 0,0, 1,
  0,0,0,0, 1,1,1,1,1,1,1,1,1,1,1,
  0,0,0,0,0,0,0,0,0,0,0
)

# build range table with half-open bins around integers (exact mapping)
rcl <- cbind(from = (1:44) - 0.5,
             to   = (1:44) + 0.5,
             becomes = newvals)

corine_bin_chamois <- terra::classify(corine, rcl = rcl, include.lowest = TRUE, right = FALSE, others = NA)

# rename
names(corine_bin_chamois) <- "suitability_chamois"
plot(corine_bin_chamois, main = "CORINE reclassified (binary 0/1)")

# export binary raster for chamois
writeRaster(corine_bin_chamois, filename = file.path(output_dir, "chamois_binary.tif"), overwrite = TRUE)

### same for roe deer
# mapping for classes 1..44 → 0/1 (exactly as you specified)
newvals <- c(
  0,0,0,0,0,0,0,0,0,      # 1–9
  1,1,1,                  # 10–12
  0,0,                    # 13–14
  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  # 15–30
  0,                      # 31
  1,1,                    # 32–33
  0,0,                    # 34–35
  1,                      # 36
  0,0,0,0,0,0,0,0         # 37–44
)
stopifnot(length(newvals) == 44)

# build range table with half-open bins around integers (exact mapping)
rcl <- cbind(from = (1:44) - 0.5,
             to   = (1:44) + 0.5,
             becomes = newvals)

corine_bin_roedeer <- terra::classify(corine, rcl = rcl, include.lowest = TRUE, right = FALSE, others = NA)

# rename
names(corine_bin_roedeer) <- "suitability_roedeer"
plot(corine_bin_roedeer, main = "CORINE reclassified (binary 0/1)")

# export binary raster for chamois
writeRaster(corine_bin_roedeer, filename = file.path(output_dir, "roedeer_binary.tif"), overwrite = TRUE)


### and now for red deer
# mapping: class 1..44 → 0/1
newvals <- c(
  0,0,0,0,0,0,0,0,0,0,  # 1–10
  1,1,                  # 11–12
  0,0,                  # 13–14
  1,1,1,1,              # 15–18
  0,0,                  # 19–20
  1,1,1,1,1,1,1,1,1,1,  # 21–30
  0,                    # 31
  1,1,                  # 32–33
  0,0,                  # 34–35
  1,                    # 36
  0,0,0,0,0,0,0,0       # 37–44
)
stopifnot(length(newvals) == 44)

# build range table with half-open bins around integers (exact mapping)
rcl <- cbind(from = (1:44) - 0.5,
             to   = (1:44) + 0.5,
             becomes = newvals)

corine_bin_reddeer <- terra::classify(corine, rcl = rcl, include.lowest = TRUE, right = FALSE, others = NA)

# rename
names(corine_bin_reddeer) <- "suitability_reddeer"
plot(corine_bin_reddeer, main = "CORINE reclassified (binary 0/1)")

# export binary raster for chamois
writeRaster(corine_bin_reddeer, filename = file.path(output_dir, "reddeer_binary.tif"), overwrite = TRUE)


#-------------------------------------------------------------------------------
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

#-------------------------------------------------------------------------------
# ----------------------
# 2. Read your raster files
# ----------------------
chamois <- rast("/mnt/eo/WilDensity/output/chamois_binary.tif")
reddeer <- rast("/mnt/eo/WilDensity/output/reddeer_binary.tif")
roedeer <- rast("/mnt/eo/WilDensity/output/roedeer_binary.tif")

# ----------------------
# 3. Crop and mask each raster to shape
# ----------------------
# check CRS of vector and raster
crs(chamois)
crs(merged_vect)

# reporject
merged_vect_proj <- project(merged_vect, chamois)

# crop and mask
chamois_crop <- mask(crop(chamois, merged_vect_proj), merged_vect_proj)
reddeer_crop <- mask(crop(reddeer, merged_vect_proj), merged_vect_proj)
roedeer_crop <- mask(crop(roedeer, merged_vect_proj), merged_vect_proj)

# ----------------------
# 4. (Optional) Write the cropped rasters to disk
# ----------------------
writeRaster(chamois_crop, "/mnt/eo/WilDensity/output/suitability_chamois_0311.tif", overwrite = TRUE)
writeRaster(reddeer_crop, "/mnt/eo/WilDensity/output/suitability_red_deer_0311.tif", overwrite = TRUE)
writeRaster(roedeer_crop, "/mnt/eo/WilDensity/output/suitability_roe_deer_0311.tif", overwrite = TRUE)

# Eindeutige ID (falls noch nicht vorhanden)
merged_vect_proj$revier_ID <- 1:nrow(merged_vect_proj)

# count raster cells == 1 (suitable) per revier
# Funktion: Zellen mit Wert 1 zählen
count_cells_1 <- function(r, v) {
  zonal(r == 1, v, fun = "sum", na.rm = TRUE)
}

# Zellen zählen, die == 1 sind (terra::extract mit fun = sum)
z_chamois  <- extract(chamois_crop  == 1, merged_vect_proj, fun = sum, na.rm = TRUE)
z_reddeer  <- extract(reddeer_crop  == 1, merged_vect_proj, fun = sum, na.rm = TRUE)
z_roedeer  <- extract(roedeer_crop  == 1, merged_vect_proj, fun = sum, na.rm = TRUE)

z_chamois$area_chamois_ha <- z_chamois$n_chamois        # 100x100 m → 1 ha per pixel
z_reddeer$area_reddeer_ha <- z_reddeer$n_red_deer
z_roedeer$area_roe_deer_ha<- z_roedeer$n_roe_deer

# Spaltennamen setzen
names(z_chamois)[2] <- "n_chamois"
names(z_reddeer)[2] <- "n_red_deer"
names(z_roedeer)[2] <- "n_roe_deer"

# not needed for 100 x 100m pixel, is already ha
z_chamois$area_chamois_ha <- z_chamois$n_chamois 
z_reddeer$area_reddeer_ha <- z_reddeer$n_red_deer 
z_roedeer$area_roe_deer_ha <- z_roedeer$n_roe_deer 

# in case of 10 x 10m pixel:
# x 0.01

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
suitability_per_revier_100 <- merge(merged_vect_proj, vals, by = "revier_ID")

writeVector(suitability_per_revier_100, "/mnt/eo/WilDensity/output/suitability_per_revier_0311.shp", overwrite = TRUE)


suitability_per_revier <- st_read("/mnt/eo/WilDensity/output/suitability_per_revier_0311.shp")


### visualise
# Convert SpatVector to sf if needed
suitability_sf_100 <- sf::st_as_sf(suitability_per_revier_100)

# Stelle sicher, dass dein Objekt in Meter projiziert ist (z. B. EPSG:3035, LAEA Europe)
suitability_sf_100 <- st_transform(suitability_sf_100, 3035)

# Fläche in Hektar berechnen (1 ha = 10.000 m²)
# area in ha as plain numeric
suitability_sf_100$revier_area_ha <-
  as.numeric(st_area(suitability_sf_100)) / 1e4

# Fläche geeigneter Pixel in ha (1 Pixel = 10 x 10 m = 100 m² = 0.01 ha)
suitability_sf_100$share_chamois_percent <-
  100 * suitability_sf_100$area_chamois_ha / suitability_sf_100$revier_area_ha

suitability_sf_100$share_roedeer_percent <-
  100 * suitability_sf_100$area_roedeer_ha / suitability_sf_100$revier_area_ha

suitability_sf_100$share_reddeer_percent <-
  100 * suitability_sf_100$area_reddeer_ha / suitability_sf_100$revier_area_ha


# Fläche geeigneter Pixel in ha (1 Pixel = 10 x 10 m = 100 m² = 0.01 ha)
suitability_sf_100$area_chamois_ha <- suitability_sf_100$n_chamois 
suitability_sf_100$area_reddeer_ha <- suitability_sf_100$n_reddeer 
suitability_sf_100$area_roedeer_ha <- suitability_sf_100$n_roedeer 

# für 10 x 10m pixel: suitability_sf$n_roedeer * 0.01

# Anteil geeigneter Fläche in %
suitability_sf_100$share_chamois_percent <- 100 * suitability_sf_100$area_chamois_ha / suitability_sf_100$revier_area_ha
suitability_sf_100$share_reddeer_percent <- 100 * suitability_sf_100$area_reddeer_ha / suitability_sf_100$revier_area_ha
suitability_sf_100$share_roedeer_percent <- 100 * suitability_sf_100$area_roedeer_ha / suitability_sf_100$revier_area_ha



# Simplify geometry (tolerance in units of CRS – e.g., meters)
suitability_sf_100_simplified <- st_simplify(suitability_sf_100, dTolerance = 100)

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
ggsave("/mnt/eo/WilDensity/_figs/red_deer_relativ_0311.png", width = 8, height = 6, units = "in", dpi = 300)

# Roe deer
ggplot(suitability_sf_simplified) +
  geom_sf(aes(fill = share_roedeer_percent)) +
  scale_fill_viridis_c(option = "F", direction = -1, na.value = "grey90") +
  theme_minimal(base_size = 14) +
  labs(title = "Relative habitat suitability for roe deer",
       fill = "Suitable area (%)")

# Save with custom size and resolution
ggsave("/mnt/eo/WilDensity/_figs/roe_deer_relativ_0311.png", width = 8, height = 6, units = "in", dpi = 300)




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


### histograms
# If share_reddeer_percent is already in percent (0–100):
p_reddeer <- ggplot(suitability_sf_simplified,
                    aes(x = share_reddeer_percent)) +
  geom_histogram(bins = 30, color = "black", fill = "grey80") +
  labs(
    title = "Distribution of red deer habitat share",
    x     = "Share of suitable area for red deer (%)",
    y     = "Frequency"
  ) +
  xlim(0,100) +
  ylim(0, 600) +
  theme_minimal()

# draw it to the screen (optional)
print(p_reddeer)

### same for roe deer
p_roedeer <- ggplot(suitability_sf_simplified,
                    aes(x = share_roedeer_percent)) +
  geom_histogram(bins = 30, color = "black", fill = "grey80") +
  labs(
    title = "Distribution of roe deer habitat share",
    x     = "Share of suitable area for roe deer (%)",
    y     = "Frequency"
  ) +
  xlim(0,100) +
  ylim(0, 600) +
  theme_minimal()

# draw it to the screen (optional)
print(p_roedeer)

### same for chamois
p_chamois <- ggplot(suitability_sf_simplified,
                    aes(x = share_chamois_percent)) +
  geom_histogram(bins = 30, color = "black", fill = "grey80") +
  labs(
    title = "Distribution of chamois habitat share",
    x     = "Share of suitable area for chamois (%)",
    y     = "Frequency"
  ) +
  xlim(0,100) +
  ylim(0, 600) +
  theme_minimal()

# draw it to the screen (optional)
print(p_chamois)


# Save with custom size and resolution
ggsave("/mnt/eo/WilDensity/_figs/histo_reddeer_new.png", p_reddeer, width = 7, height = 6, units = "in", dpi = 300)
ggsave("/mnt/eo/WilDensity/_figs/histo_roedeer_new.png", p_roedeer, width = 7, height = 6, units = "in", dpi = 300)
ggsave("/mnt/eo/WilDensity/_figs/histo_chamois_new.png", p_chamois, width = 7, height = 6, units = "in", dpi = 300)


# If share_reddeer_percent is already in percent (0–100):
hist(
  suitability_sf_simplified$share_roedeer_percent,
  breaks = 30,
  main   = "Distribution of roe deer habitat share",
  xlab   = "Share of suitable area for ror deer (%)",
  ylab   = "Frequency"
)


# If share_reddeer_percent is already in percent (0–100):
hist(
  suitability_sf_simplified$share_chamois_percent,
  breaks = 30,
  main   = "Distribution of chamois habitat share",
  xlab   = "Share of suitable area for chamois (%)",
  ylab   = "Frequency"
)




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


# =====================================================================
# PRIOR 1: FOREST TYPES
# =====================================================================

library(terra)

# ---------------------------------------------------------------------
# 1. Paths and settings
# ---------------------------------------------------------------------

input_shp  <- "/mnt/eo/WilDensity/_data/_shp/corine/CORINE_2018.shp"
output_tif <- "/mnt/eo/WilDensity/_data/_corine/CORINE_forest_types.tif"

target_res <- 100


# ---------------------------------------------------------------------
# 2. Load CORINE vector data
# ---------------------------------------------------------------------

corine <- vect(input_shp)


# ---------------------------------------------------------------------
# 3. Select forest classes
# ---------------------------------------------------------------------

forest_codes <- c(311, 312, 313)

corine_forest <- corine[
  corine$Code_18 %in% forest_codes,
]


# ---------------------------------------------------------------------
# 4. Create 100 m raster template
# ---------------------------------------------------------------------

r_100m <- rast(
  ext(corine_forest),
  resolution = target_res,
  crs = crs(corine_forest)
)


# ---------------------------------------------------------------------
# 5. Determine dominant forest type per 100 m pixel
# ---------------------------------------------------------------------

class_area <- lapply(
  forest_codes,
  function(code) {
    
    x <- corine_forest[corine_forest$Code_18 == code, ]
    
    rasterize(
      x,
      r_100m,
      cover = TRUE,
      background = 0
    )
  }
)

class_area <- rast(class_area)


# Assign CODE_18 of class with largest area
forest_type_100m <- app(
  class_area,
  fun = function(x) {
    
    if (all(x == 0 | is.na(x))) {
      return(NA)
    }
    
    forest_codes[which.max(x)]
  }
)

names(forest_type_100m) <- "CODE_18"


# ---------------------------------------------------------------------
# 6. Save
# ---------------------------------------------------------------------

writeRaster(
  forest_type_100m,
  output_tif,
  datatype = "INT2U",
  overwrite = TRUE
)



# =====================================================================
# PRIOR 2: FOREST COVER
# =====================================================================

library(terra)

# ---------------------------------------------------------------------
# 1. Paths and settings
# ---------------------------------------------------------------------

input_shp  <- "/mnt/eo/WilDensity/_data/_shp/corine/CORINE_2018.shp"
output_tif <- "/mnt/eo/WilDensity/_data/_corine/CORINE_forest_cover.tif"

target_res <- 100


# ---------------------------------------------------------------------
# 2. Load CORINE vector data
# ---------------------------------------------------------------------

corine <- vect(input_shp)


# ---------------------------------------------------------------------
# 3. Select all forest classes
# ---------------------------------------------------------------------

forest_codes <- c(311, 312, 313)

corine_forest <- corine[
  corine$Code_18 %in% forest_codes,
]


# ---------------------------------------------------------------------
# 4. Create 100 m raster template
# ---------------------------------------------------------------------

r_100m <- rast(
  ext(corine_forest),
  resolution = target_res,
  crs = crs(corine_forest)
)


# ---------------------------------------------------------------------
# 5. Calculate forest cover per 100 m pixel
# ---------------------------------------------------------------------

forest_cover_100m <- rasterize(
  corine_forest,
  r_100m,
  cover = TRUE,
  background = 0
)

names(forest_cover_100m) <- "forest_cover"


# ---------------------------------------------------------------------
# 6. Save
# ---------------------------------------------------------------------

writeRaster(
  forest_cover_100m,
  output_tif,
  datatype = "FLT4S",
  overwrite = TRUE
)



# =====================================================================
# PRIOR 3: SLOPE
# =====================================================================

library(terra)

# ---------------------------------------------------------------------
# 1. Paths and settings
# ---------------------------------------------------------------------

input_dem  <- "/mnt/dss_europe/misc/dem_glo30/glo30_dem.vrt"
forest_tif <- "/mnt/eo/WilDensity/_data/_priors/CORINE_forest_types.tif"
output_tif <- "/mnt/eo/WilDensity/_data/_priors/slope_100m.tif"


# ---------------------------------------------------------------------
# 2. Load data
# ---------------------------------------------------------------------

dem <- rast(input_dem)
forest_type <- rast(forest_tif)


# ---------------------------------------------------------------------
# 3. Get bounding box of forest-type raster
# ---------------------------------------------------------------------

bbox <- as.polygons(ext(forest_type))
crs(bbox) <- crs(forest_type)

# Transform bounding box into DEM CRS
bbox_dem <- project(
  bbox,
  crs(dem)
)


# ---------------------------------------------------------------------
# 4. Crop DEM to bounding box
# ---------------------------------------------------------------------

dem_crop <- crop(
  dem,
  bbox_dem
)


# ---------------------------------------------------------------------
# 5. Create EMPTY 100 m template
# ---------------------------------------------------------------------

template_100m <- rast(
  ext = ext(forest_type),
  resolution = 100,
  crs = crs(forest_type)
)


# ---------------------------------------------------------------------
# 6. Reproject/resample DEM to 100 m
# ---------------------------------------------------------------------

dem_100m <- project(
  dem_crop,
  template_100m,
  method = "bilinear"
)


# ---------------------------------------------------------------------
# 7. Calculate slope
# ---------------------------------------------------------------------

slope_100m <- terrain(
  dem_100m,
  v = "slope",
  unit = "degrees",
  neighbors = 8
)

names(slope_100m) <- "slope"


# ---------------------------------------------------------------------
# 8. Save
# ---------------------------------------------------------------------

writeRaster(
  slope_100m,
  output_tif,
  datatype = "FLT4S",
  overwrite = TRUE
)
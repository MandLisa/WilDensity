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
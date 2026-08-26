library(terra)

# =====================================================================
# SETTINGS
# =====================================================================

corine_shp <- "/mnt/eo/WilDensity/_data/_shp/corine/CORINE_2018.shp"
study_shp  <- "/mnt/eo/WilDensity/_data/_shp/corine/CORINE_2018.shp"
dem_vrt    <- "/mnt/dss_europe/misc/dem_glo30/glo30_dem.vrt"

out_dir <- "/mnt/eo/WilDensity/_data/_priors"

target_res   <- 500
forest_codes <- c(311, 312, 313)

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)


# =====================================================================
# INPUT DATA
# =====================================================================

corine <- vect(corine_shp)
study_area <- vect(study_shp)

corine$Code_18 <- as.integer(corine$Code_18)

if (!same.crs(study_area, corine)) {
  study_area <- project(study_area, crs(corine))
}

corine_forest <- corine[
  corine$Code_18 %in% forest_codes,
]


# =====================================================================
# 500 M TEMPLATE
# =====================================================================

r_500m <- rast(
  ext(study_area),
  resolution = target_res,
  crs = crs(study_area)
)


# =====================================================================
# PRIOR 1: FOREST TYPES
# =====================================================================

class_cover <- lapply(
  forest_codes,
  function(code) {
    rasterize(
      corine_forest[corine_forest$Code_18 == code, ],
      r_500m,
      cover = TRUE,
      background = 0
    )
  }
)

class_cover <- rast(class_cover)

forest_type_500m <- app(
  class_cover,
  function(x) {
    if (all(is.na(x) | x == 0)) {
      return(NA)
    }
    forest_codes[which.max(x)]
  }
)

forest_type_500m <- mask(
  forest_type_500m,
  study_area,
  touches = TRUE
)

names(forest_type_500m) <- "forest_type"

writeRaster(
  forest_type_500m,
  file.path(out_dir, "CORINE_forest_types_500m.tif"),
  datatype = "INT2U",
  overwrite = TRUE
)


# =====================================================================
# PRIOR 2: FOREST COVER
# =====================================================================

forest_cover_500m <- rasterize(
  corine_forest,
  r_500m,
  cover = TRUE,
  background = 0
)

forest_cover_500m <- mask(
  forest_cover_500m,
  study_area,
  touches = TRUE
)

names(forest_cover_500m) <- "forest_cover"

writeRaster(
  forest_cover_500m,
  file.path(out_dir, "CORINE_forest_cover_500m.tif"),
  datatype = "FLT4S",
  overwrite = TRUE
)


# =====================================================================
# TERRAIN DATA
# =====================================================================

dem <- rast(dem_vrt)

bbox <- as.polygons(ext(r_500m))
crs(bbox) <- crs(r_500m)

bbox_dem <- project(
  bbox,
  crs(dem)
)

dem_crop <- crop(
  dem,
  bbox_dem,
  snap = "out"
)

dem_500m <- project(
  dem_crop,
  r_500m,
  method = "bilinear"
)


# =====================================================================
# PRIOR 3: ELEVATION
# =====================================================================

elevation_500m <- mask(
  dem_500m,
  study_area,
  touches = TRUE
)

names(elevation_500m) <- "elevation"

writeRaster(
  elevation_500m,
  file.path(out_dir, "elevation_500m.tif"),
  datatype = "FLT4S",
  overwrite = TRUE
)


# =====================================================================
# PRIOR 4: SLOPE
# =====================================================================

slope_500m <- terrain(
  dem_500m,
  v = "slope",
  unit = "degrees",
  neighbors = 8
)

slope_500m <- mask(
  slope_500m,
  study_area,
  touches = TRUE
)

names(slope_500m) <- "slope"

writeRaster(
  slope_500m,
  file.path(out_dir, "slope_500m.tif"),
  datatype = "FLT4S",
  overwrite = TRUE
)


# =====================================================================
# PRIOR 5: RUGGEDNESS
# =====================================================================

ruggedness_500m <- terrain(
  dem_500m,
  v = "TRI",
  neighbors = 8
)

ruggedness_500m <- mask(
  ruggedness_500m,
  study_area,
  touches = TRUE
)

names(ruggedness_500m) <- "ruggedness"

writeRaster(
  ruggedness_500m,
  file.path(out_dir, "ruggedness_500m.tif"),
  datatype = "FLT4S",
  overwrite = TRUE
)


# =====================================================================
# CHECK
# =====================================================================

priors <- c(
  forest_type_500m,
  forest_cover_500m,
  elevation_500m,
  slope_500m,
  ruggedness_500m
)

res(priors)
ext(priors)
crs(priors)
library(terra)

# =====================================================================
# SETTINGS
# =====================================================================

corine_shp <- "/mnt/eo/WilDensity/_data/_shp/corine/CORINE_2018.shp"
study_shp  <- "/mnt/eo/WilDensity/_data/_shp/corine/CORINE_2018.shp"
dem_vrt    <- "/mnt/dss_europe/misc/dem_glo30/glo30_dem.vrt"

out_corine <- "/mnt/eo/WilDensity/_data/_corine"
out_priors <- "/mnt/eo/WilDensity/_data/_priors"

dir.create(out_corine, recursive = TRUE, showWarnings = FALSE)
dir.create(out_priors, recursive = TRUE, showWarnings = FALSE)

target_res  <- 100
forest_codes <- c(311, 312, 313)


# =====================================================================
# CORINE
# =====================================================================

corine <- vect(corine_shp)
corine$Code_18 <- as.integer(corine$Code_18)

corine_forest <- corine[
  corine$Code_18 %in% forest_codes,
]

r_100m <- rast(
  ext(corine_forest),
  resolution = target_res,
  crs = crs(corine_forest)
)


# =====================================================================
# PRIOR 1: FOREST TYPES
# =====================================================================

class_cover <- lapply(
  forest_codes,
  function(code) {
    rasterize(
      corine_forest[corine_forest$Code_18 == code, ],
      r_100m,
      cover = TRUE,
      background = 0
    )
  }
)

class_cover <- rast(class_cover)

forest_type_100m <- app(
  class_cover,
  function(x) {
    if (all(is.na(x) | x == 0)) {
      return(NA)
    }
    
    forest_codes[which.max(x)]
  }
)

names(forest_type_100m) <- "forest_type"

writeRaster(
  forest_type_100m,
  file.path(out_corine, "CORINE_forest_types.tif"),
  datatype = "INT2U",
  overwrite = TRUE
)


# =====================================================================
# PRIOR 2: FOREST COVER
# =====================================================================

forest_cover_100m <- rasterize(
  corine_forest,
  r_100m,
  cover = TRUE,
  background = 0
)

names(forest_cover_100m) <- "forest_cover"

writeRaster(
  forest_cover_100m,
  file.path(out_corine, "CORINE_forest_cover.tif"),
  datatype = "FLT4S",
  overwrite = TRUE
)


# =====================================================================
# TERRAIN DATA
# =====================================================================

dem <- rast(dem_vrt)

bbox <- as.polygons(ext(r_100m))
crs(bbox) <- crs(r_100m)

bbox_dem <- project(
  bbox,
  crs(dem)
)

dem_crop <- crop(
  dem,
  bbox_dem,
  snap = "out"
)

dem_100m <- project(
  dem_crop,
  r_100m,
  method = "bilinear"
)


# =====================================================================
# PRIOR 3: SLOPE
# =====================================================================

slope_100m <- terrain(
  dem_100m,
  v = "slope",
  unit = "degrees",
  neighbors = 8
)

names(slope_100m) <- "slope"


# =====================================================================
# PRIOR 4: RUGGEDNESS
# =====================================================================

ruggedness_100m <- terrain(
  dem_100m,
  v = "TRI",
  neighbors = 8
)

names(ruggedness_100m) <- "ruggedness"


# =====================================================================
# STUDY AREA
# =====================================================================

study_area <- vect(study_shp)

if (!same.crs(study_area, r_100m)) {
  study_area <- project(
    study_area,
    crs(r_100m)
  )
}

elevation_study <- crop(
  dem_100m,
  study_area,
  mask = TRUE,
  touches = TRUE
)

slope_study <- crop(
  slope_100m,
  study_area,
  mask = TRUE,
  touches = TRUE
)

ruggedness_study <- crop(
  ruggedness_100m,
  study_area,
  mask = TRUE,
  touches = TRUE
)

names(elevation_study)  <- "elevation"
names(slope_study)      <- "slope"
names(ruggedness_study) <- "ruggedness"


# =====================================================================
# SAVE TERRAIN PRIORS
# =====================================================================

writeRaster(
  elevation_study,
  file.path(out_priors, "elevation_100m.tif"),
  datatype = "FLT4S",
  overwrite = TRUE
)

writeRaster(
  slope_study,
  file.path(out_priors, "slope_100m.tif"),
  datatype = "FLT4S",
  overwrite = TRUE
)

writeRaster(
  ruggedness_study,
  file.path(out_priors, "ruggedness_100m.tif"),
  datatype = "FLT4S",
  overwrite = TRUE
)
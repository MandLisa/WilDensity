
library(terra)

# =====================================================
# 1. SETTINGS
# =====================================================

output_dir <- "/mnt/eo/WilDensity/output/edge_density"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

terraOptions(
  memfrac = 0.6,
  progress = 1
)


# =====================================================
# 2. LOAD INPUT RASTERS
# =====================================================

# CORINE forest types
fty <- rast(
  "/mnt/eo/WilDensity/_data/_priors/CORINE_forest_types.tif"
)

# Habitat suitability rasters
suitability <- list(
  roe_deer = rast(
    "/mnt/eo/WilDensity/output/suitability_roe_deer_0311.tif"
  ),
  red_deer = rast(
    "/mnt/eo/WilDensity/output/suitability_red_deer_0311.tif"
  ),
  chamois = rast(
    "/mnt/eo/WilDensity/output/suitability_chamois_0311.tif"
  )
)

# Reference prior
template <- rast(
  "/mnt/eo/WilDensity/_data/_priors/slope_500m.tif"
)

print(fty)
print(template)
print(freq(fty))

stopifnot(!is.lonlat(fty))
stopifnot(same.crs(fty, template))
stopifnot(all(abs(res(template) - 500) < 0.2))

for (s in suitability) {
  stopifnot(same.crs(s, template))
}


# =====================================================
# 3. PREPARE BINARY FOREST RASTER
# =====================================================

# CORINE:
# 311 = Broadleaved forest
# 312 = Coniferous forest
# 313 = Mixed forest
#
# Assumption: NA inside the valid study area
# represents non-forest.
#
# This requires a complete CORINE dataset.

fty <- crop(fty, template, snap = "out")

# Valid study area
study_area <- ifel(!is.na(template), 1, NA)

study_area_fty <- resample(
  study_area,
  fty,
  method = "near"
)

forest <- ifel(
  !is.na(study_area_fty),
  ifel(fty %in% c(311, 312, 313), 1, 0),
  NA
)

names(forest) <- "forest"

print(freq(forest))

plot(forest, main = "Forest / non-forest")


# =====================================================
# 4. DETECT FOREST EDGES
# =====================================================

# A forest edge exists when adjacent cells
# differ between forest (1) and non-forest (0).
#
# Only horizontal and vertical boundaries count.

get_neighbour <- function(x, row_offset, col_offset) {
  
  w <- matrix(NA_real_, 3, 3)
  w[2 + row_offset, 2 + col_offset] <- 1
  
  focal(
    x,
    w = w,
    fun = "sum",
    na.rm = FALSE,
    fillvalue = NA
  )
}

right <- get_neighbour(forest, 0, 1)
left <- get_neighbour(forest, 0, -1)
top <- get_neighbour(forest, -1, 0)
bottom <- get_neighbour(forest, 1, 0)

detect_edge <- function(x, neighbour) {
  
  ifel(
    !is.na(x) & !is.na(neighbour),
    ifel(x != neighbour, 1, 0),
    0
  )
}

e_right <- detect_edge(forest, right)
e_left <- detect_edge(forest, left)
e_top <- detect_edge(forest, top)
e_bottom <- detect_edge(forest, bottom)


# =====================================================
# 5. CALCULATE EDGE LENGTH
# =====================================================

pixel_width <- res(forest)[1]
pixel_height <- res(forest)[2]

# Assign half of each shared boundary
# to each neighbouring raster cell.

edge_length <- (
  (e_right + e_left) * pixel_height +
    (e_top + e_bottom) * pixel_width
) / 2

edge_length <- ifel(
  !is.na(forest),
  edge_length,
  NA
)

names(edge_length) <- "edge_length_m"


# =====================================================
# 6. AGGREGATE TO REFERENCE GRID
# =====================================================

# If already aligned, no resampling needed.

if (compareGeom(
  edge_length,
  template,
  stopOnError = FALSE
)) {
  
  edge_length_500 <- edge_length
  
} else {
  
  edge_length_500 <- resample(
    edge_length,
    template,
    method = "sum"
  )
}

# Mask to valid reference area
edge_length_500 <- ifel(
  !is.na(template),
  edge_length_500,
  NA
)


# =====================================================
# 7. CALCULATE EDGE DENSITY (m/ha)
# =====================================================

area_ha <- prod(res(template)) / 10000

edge_density <- edge_length_500 / area_ha

names(edge_density) <- "edge_density_m_ha"

writeRaster(
  edge_density,
  file.path(output_dir, "ED_500m_all.tif"),
  overwrite = TRUE
)


# =====================================================
# 8. APPLY SPECIES SUITABILITY MASKS
# =====================================================

ed_results <- list()

for (species in names(suitability)) {
  
  message("Processing: ", species)
  
  suit <- suitability[[species]]
  
  if (!compareGeom(
    suit,
    template,
    stopOnError = FALSE
  )) {
    
    suit <- resample(
      suit,
      template,
      method = "near"
    )
  }
  
  # Assumption: 1 = suitable
  ed <- ifel(
    !is.na(suit) & suit == 1,
    edge_density,
    NA
  )
  
  names(ed) <- paste0("ED_", species)
  
  ed_results[[species]] <- ed
  
  writeRaster(
    ed,
    file.path(
      output_dir,
      paste0("ED_500m_", species, ".tif")
    ),
    overwrite = TRUE
  )
}


# =====================================================
# 9. SUMMARY STATISTICS
# =====================================================

summary_ed <- do.call(
  rbind,
  lapply(names(ed_results), function(species) {
    
    ed <- ed_results[[species]]
    
    data.frame(
      species = species,
      mean_ED = terra::global(
        ed, fun = "mean", na.rm = TRUE
      )[1, 1],
      median_ED = terra::global(
        ed, fun = median, na.rm = TRUE
      )[1, 1],
      sd_ED = terra::global(
        ed, fun = "sd", na.rm = TRUE
      )[1, 1]
    )
  })
)

print(summary_ed)

write.csv(
  summary_ed,
  file.path(output_dir, "ED_summary.csv"),
  row.names = FALSE
)


# =====================================================
# 10. VISUALIZATION
# =====================================================

par(mfrow = c(2, 2))

plot(edge_density, main = "Forest edge density")
plot(ed_results$roe_deer, main = "Roe deer")
plot(ed_results$red_deer, main = "Red deer")
plot(ed_results$chamois, main = "Chamois")

par(mfrow = c(1, 1))


# =====================================================
# 11. VALIDATE OUTPUT
# =====================================================

print(res(edge_density))

print(
  compareGeom(
    edge_density,
    template,
    stopOnError = FALSE
  )
)

print(
  global(
    edge_density,
    "range",
    na.rm = TRUE
  )
)

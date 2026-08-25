# =====================================================================
# CONTINUOUS HARVEST DENSITY RASTERS
# Hunting district x species x year -> continuous 100 m raster
#
# Workflow:
#   1. Read final annual hunting-district harvest data
#   2. Create a common 100 m grid in EPSG:3035
#   3. Use binary species-specific suitability masks (1 = suitable)
#   4. Distribute district harvest uniformly across suitable pixels
#   5. Fallback for districts with positive harvest but zero suitable pixels:
#        search suitable pixels within 500 -> 1000 -> 2000 m
#   6. Spatial smoothing with an OCTAGONAL moving window
#   7. Temporal smoothing (1 / 3 / 5 years)
#   8. Rescale each species x year raster to preserve annual total harvest
#
# Output units:
#   harvest density = n / km2 / year
# =====================================================================


# =====================================================================
# 0. PACKAGES
# =====================================================================

library(sf)
library(terra)
library(dplyr)

terraOptions(progress = 1)


# =====================================================================
# 1. SETTINGS
# =====================================================================

# Final, interpolated hunting-district time series
input_gpkg <-
  "/mnt/eo/WilDensity/output/final_integer_allocation/interpolated_missing_reviere/final_revier_timeseries_complete_interpolated.gpkg"

input_layer <- "revier_timeseries"


# Binary species-specific suitability rasters
# 1 = suitable
# 0 = unsuitable
suitability_files <- c(
  roe_deer =
    "/mnt/eo/WilDensity/output/suitability_roe_deer_0311.tif",
  red_deer =
    "/mnt/eo/WilDensity/output/suitability_red_deer_0311.tif",
  chamois =
    "/mnt/eo/WilDensity/output/suitability_chamois_0311.tif"
)


# Output
output_dir <-
  "/mnt/eo/WilDensity/output/continuous_harvest_density"

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)


# Raster settings
target_crs <- "EPSG:3035"
target_resolution_m <- 100
cell_area_km2 <- (target_resolution_m^2) / 1e6


# Spatial moving-window radius
# Start with 2000 m; later test e.g. 500, 1000, 2000, 5000 m.
spatial_radius_m <- 2000


# Temporal moving window
# 1 = target year only
# 3 = t-1, t, t+1
# 5 = t-2, ..., t+2
temporal_window_years <- 3


# If FALSE, edge years use the available neighbouring years.
# If TRUE, edge years without a complete window are skipped.
require_full_temporal_window <- FALSE


# Fallback search radii for districts with positive harvest
# but no suitable 100 m pixel.
fallback_radii_m <- c(500, 1000, 2000)


# Restrict fallback pixels to the same province.
fallback_same_province <- TRUE


# =====================================================================
# 2. CHECK SETTINGS
# =====================================================================

if (!temporal_window_years %in% c(1, 3, 5)) {
  stop("temporal_window_years must be 1, 3 or 5.")
}

if (target_resolution_m <= 0) {
  stop("target_resolution_m must be > 0.")
}

if (spatial_radius_m <= 0) {
  stop("spatial_radius_m must be > 0.")
}

if (any(fallback_radii_m <= 0)) {
  stop("All fallback_radii_m values must be > 0.")
}

fallback_radii_m <- sort(unique(fallback_radii_m))


# =====================================================================
# 3. READ HUNT DATA
# =====================================================================

cat("Reading hunt data...\n")

hunt_sf <- st_read(
  input_gpkg,
  layer = input_layer,
  quiet = TRUE
) %>%
  mutate(
    poly_id = as.integer(poly_id),
    species = as.character(species),
    year = as.integer(year),
    n = as.numeric(n),
    prov = as.character(prov)
  ) %>%
  filter(
    species %in% names(suitability_files),
    year != 1992
  )

if (nrow(hunt_sf) == 0) {
  stop("No hunting data found after filtering.")
}

if (any(is.na(hunt_sf$poly_id))) {
  stop("Missing poly_id values found in hunting data.")
}


# =====================================================================
# 4. UNIQUE HUNTING-DISTRICT GEOMETRIES
# =====================================================================

cat("Preparing hunting-district geometries...\n")

reviere <- hunt_sf %>%
  arrange(poly_id) %>%
  filter(!duplicated(poly_id)) %>%
  select(poly_id, prov) %>%
  st_make_valid() %>%
  st_transform(target_crs)

if (anyDuplicated(reviere$poly_id) > 0) {
  stop("poly_id is not unique in the geometry table.")
}


# =====================================================================
# 5. PROVINCE LOOKUP
# =====================================================================

province_lookup <- reviere %>%
  st_drop_geometry() %>%
  distinct(prov) %>%
  arrange(prov) %>%
  mutate(prov_code = row_number())

reviere <- reviere %>%
  left_join(
    province_lookup,
    by = "prov"
  )


# =====================================================================
# 6. ATTRIBUTE TABLE + CHECKS
# =====================================================================

hunt <- hunt_sf %>%
  st_drop_geometry() %>%
  select(
    poly_id,
    prov,
    species,
    year,
    n
  )


duplicates <- hunt %>%
  count(
    poly_id,
    species,
    year,
    name = "n_rows"
  ) %>%
  filter(n_rows > 1)

if (nrow(duplicates) > 0) {
  print(duplicates)
  stop("Duplicate poly_id x species x year combinations found.")
}


# =====================================================================
# 7. CREATE 100 M TEMPLATE
# =====================================================================

cat("Creating ", target_resolution_m, " m raster template...\n", sep = "")

rev_v <- vect(reviere)
e <- ext(rev_v)

xmin_template <-
  floor(xmin(e) / target_resolution_m) * target_resolution_m

xmax_template <-
  ceiling(xmax(e) / target_resolution_m) * target_resolution_m

ymin_template <-
  floor(ymin(e) / target_resolution_m) * target_resolution_m

ymax_template <-
  ceiling(ymax(e) / target_resolution_m) * target_resolution_m


template <- rast(
  xmin = xmin_template,
  xmax = xmax_template,
  ymin = ymin_template,
  ymax = ymax_template,
  resolution = target_resolution_m,
  crs = target_crs
)


# =====================================================================
# 8. RASTERIZE HUNTING DISTRICTS + PROVINCES
# =====================================================================

cat("Rasterizing hunting districts...\n")

revier_id_raster <- rasterize(
  rev_v,
  template,
  field = "poly_id"
)
names(revier_id_raster) <- "poly_id"

province_raster <- rasterize(
  rev_v,
  template,
  field = "prov_code"
)
names(province_raster) <- "prov_code"


# =====================================================================
# 9. OCTAGONAL MOVING-WINDOW KERNEL
# =====================================================================

make_octagon_kernel <- function(radius_m, resolution_m) {
  
  r_cells <- ceiling(radius_m / resolution_m)
  coords <- -r_cells:r_cells
  
  xx <- outer(
    rep(1, length(coords)),
    coords
  )
  
  yy <- outer(
    coords,
    rep(1, length(coords))
  )
  
  # Regular-octagon approximation on the raster grid.
  # The square corners are clipped by the diagonal constraint.
  inside <-
    abs(xx) <= r_cells &
    abs(yy) <= r_cells &
    (abs(xx) + abs(yy)) <= sqrt(2) * r_cells
  
  weights <- matrix(
    0,
    nrow = length(coords),
    ncol = length(coords)
  )
  
  weights[inside] <- 1
  
  weights
}


window_weights <- make_octagon_kernel(
  radius_m = spatial_radius_m,
  resolution_m = target_resolution_m
)

cat("Octagonal moving window:\n")
cat("  radius: ", spatial_radius_m, " m\n", sep = "")
cat(
  "  matrix: ",
  nrow(window_weights),
  " x ",
  ncol(window_weights),
  " cells\n",
  sep = ""
)
cat("  included cells: ", sum(window_weights), "\n", sep = "")


# =====================================================================
# 10. SPATIAL SMOOTHING FUNCTION
# =====================================================================

spatial_smooth <- function(
    density_raster,
    output_habitat_mask,
    weights
) {
  
  # Count only pixels that actually contain a source value.
  source_mask <- ifel(
    !is.na(density_raster),
    1,
    NA
  )
  
  numerator <- focal(
    density_raster,
    w = weights,
    fun = "sum",
    na.rm = TRUE
  )
  
  denominator <- focal(
    source_mask,
    w = weights,
    fun = "sum",
    na.rm = TRUE
  )
  
  out <- numerator / denominator
  
  out <- ifel(
    denominator > 0,
    out,
    NA
  )
  
  # Final values only on suitable habitat.
  out <- mask(
    out,
    output_habitat_mask
  )
  
  out
}


# =====================================================================
# 11. FALLBACK FUNCTION
# =====================================================================
#
# Used only when a hunting district has:
#   - positive harvest
#   - zero suitable pixels
#
# Search suitable pixels progressively within the specified radii.
# The first radius with suitable cells is used.
# =====================================================================

find_fallback_cells <- function(
    poly_id_target,
    prov_target,
    habitat_mask,
    province_raster,
    rev_v,
    province_lookup,
    search_radii,
    same_province = TRUE
) {
  
  target_index <- which(
    rev_v$poly_id == poly_id_target
  )
  
  if (length(target_index) != 1) {
    stop(
      "Could not uniquely identify poly_id ",
      poly_id_target,
      ". Found ",
      length(target_index),
      " polygons."
    )
  }
  
  target_polygon <- rev_v[target_index, ]
  
  province_match <- province_lookup %>%
    filter(prov == prov_target)
  
  if (nrow(province_match) != 1) {
    stop(
      "Could not uniquely identify province code for ",
      prov_target,
      "."
    )
  }
  
  target_prov_code <- province_match$prov_code[1]
  
  
  for (radius_now in search_radii) {
    
    buffer_polygon <- terra::buffer(
      target_polygon,
      width = radius_now
    )
    
    extracted <- terra::extract(
      c(habitat_mask, province_raster),
      buffer_polygon,
      cells = TRUE
    )
    
    extracted <- as.data.frame(extracted)
    
    if (nrow(extracted) == 0) {
      next
    }
    
    # terra::extract() can include an ID column; we only use named fields.
    if (!all(c("suitable", "prov_code", "cell") %in% names(extracted))) {
      stop(
        "Fallback extraction did not return the expected columns ",
        "'suitable', 'prov_code' and 'cell'."
      )
    }
    
    extracted <- extracted %>%
      filter(
        !is.na(suitable),
        suitable == 1,
        !is.na(cell)
      )
    
    if (same_province) {
      extracted <- extracted %>%
        filter(
          !is.na(prov_code),
          prov_code == target_prov_code
        )
    }
    
    target_cells <- unique(extracted$cell)
    target_cells <- target_cells[!is.na(target_cells)]
    
    if (length(target_cells) > 0) {
      return(
        list(
          radius_m = radius_now,
          cells = target_cells
        )
      )
    }
  }
  
  NULL
}


# =====================================================================
# 12. PROCESS EACH SPECIES
# =====================================================================

for (sp in names(suitability_files)) {
  
  cat("\n")
  cat("============================================================\n")
  cat("SPECIES: ", sp, "\n", sep = "")
  cat("============================================================\n")
  
  
  # ===================================================================
  # 12.1 OUTPUT DIRECTORIES
  # ===================================================================
  
  species_dir <- file.path(
    output_dir,
    sp
  )
  
  spatial_tmp_dir <- file.path(
    species_dir,
    paste0(
      "_spatial_tmp_",
      target_resolution_m,
      "m_octagon_",
      spatial_radius_m,
      "m"
    )
  )
  
  final_dir <- file.path(
    species_dir,
    paste0(
      "resolution_",
      target_resolution_m,
      "m_octagon_",
      spatial_radius_m,
      "m_temporal_",
      temporal_window_years,
      "yr"
    )
  )
  
  dir.create(
    species_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  dir.create(
    spatial_tmp_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  dir.create(
    final_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  
  # ===================================================================
  # 12.2 READ + ALIGN SUITABILITY
  # ===================================================================
  
  suitability_file <- unname(
    suitability_files[sp]
  )
  
  if (
    length(suitability_file) != 1 ||
    is.na(suitability_file) ||
    !file.exists(suitability_file)
  ) {
    stop(
      "Suitability file not found for species ",
      sp,
      ": ",
      suitability_file
    )
  }
  
  cat("Reading suitability raster: ", suitability_file, "\n", sep = "")
  
  suitability_raw <- rast(
    suitability_file
  )
  
  suitability <- project(
    suitability_raw,
    template,
    method = "near"
  )
  
  
  # ===================================================================
  # 12.3 HABITAT MASK
  # ===================================================================
  
  # 1  = suitable habitat
  # NA = unsuitable habitat
  habitat_mask <- ifel(
    suitability == 1 &
      !is.na(revier_id_raster),
    1,
    NA
  )
  
  names(habitat_mask) <- "suitable"
  
  
  # ===================================================================
  # 12.4 SUITABLE PIXELS / AREA PER DISTRICT
  # ===================================================================
  
  suitable_counts <- zonal(
    habitat_mask,
    revier_id_raster,
    fun = "sum",
    na.rm = TRUE
  )
  
  suitable_counts <- as.data.frame(
    suitable_counts
  )
  
  if (ncol(suitable_counts) < 2) {
    stop(
      "Could not calculate suitable pixel counts for species ",
      sp,
      "."
    )
  }
  
  names(suitable_counts)[1:2] <- c(
    "poly_id",
    "n_suitable_pixels"
  )
  
  suitable_counts <- suitable_counts %>%
    mutate(
      poly_id = as.integer(poly_id),
      n_suitable_pixels = as.numeric(n_suitable_pixels),
      n_suitable_pixels = ifelse(
        is.finite(n_suitable_pixels),
        n_suitable_pixels,
        0
      ),
      suitable_area_km2 =
        n_suitable_pixels * cell_area_km2
    )
  
  
  # ===================================================================
  # 12.5 JOIN HARVEST DATA
  # ===================================================================
  
  sp_hunt <- hunt %>%
    filter(species == sp) %>%
    left_join(
      suitable_counts,
      by = "poly_id"
    ) %>%
    mutate(
      n_suitable_pixels = ifelse(
        is.na(n_suitable_pixels),
        0,
        n_suitable_pixels
      ),
      suitable_area_km2 = ifelse(
        is.na(suitable_area_km2),
        0,
        suitable_area_km2
      )
    )
  
  
  # ===================================================================
  # 12.6 FIND DISTRICTS THAT NEED FALLBACK
  # ===================================================================
  
  fallback_districts <- sp_hunt %>%
    group_by(
      poly_id,
      prov
    ) %>%
    summarise(
      suitable_area_km2 = first(suitable_area_km2),
      max_harvest = ifelse(
        all(is.na(n)),
        NA_real_,
        max(n, na.rm = TRUE)
      ),
      .groups = "drop"
    ) %>%
    filter(
      suitable_area_km2 <= 0,
      !is.na(max_harvest),
      max_harvest > 0
    )
  
  cat(
    "Fallback districts: ",
    nrow(fallback_districts),
    "\n",
    sep = ""
  )
  
  
  # ===================================================================
  # 12.7 FIND FALLBACK PIXELS
  # ===================================================================
  
  # Named-list access is handled without double-bracket indexing.
  fallback_cells <- list()
  fallback_diagnostics <- list()
  fallback_diagnostics_df <- data.frame()
  
  
  if (nrow(fallback_districts) > 0) {
    
    for (i in seq_len(nrow(fallback_districts))) {
      
      pid <- fallback_districts$poly_id[i]
      pprov <- fallback_districts$prov[i]
      
      cat(
        "Finding fallback habitat for poly_id ",
        pid,
        "...\n",
        sep = ""
      )
      
      result <- find_fallback_cells(
        poly_id_target = pid,
        prov_target = pprov,
        habitat_mask = habitat_mask,
        province_raster = province_raster,
        rev_v = rev_v,
        province_lookup = province_lookup,
        search_radii = fallback_radii_m,
        same_province = fallback_same_province
      )
      
      if (is.null(result)) {
        stop(
          "No suitable fallback pixels found within ",
          max(fallback_radii_m),
          " m for poly_id ",
          pid,
          " (",
          sp,
          ")."
        )
      }
      
      result_cells <- result$cells
      result_radius <- result$radius_m
      
      fallback_cells[as.character(pid)] <- list(
        result_cells
      )
      
      fallback_diagnostics <- append(
        fallback_diagnostics,
        list(
          data.frame(
            species = sp,
            poly_id = pid,
            prov = pprov,
            fallback_radius_m = result_radius,
            n_target_pixels = length(result_cells),
            target_area_km2 =
              length(result_cells) * cell_area_km2
          )
        )
      )
      
      cat(
        "  -> radius: ",
        result_radius,
        " m | suitable pixels: ",
        length(result_cells),
        "\n",
        sep = ""
      )
    }
  }
  
  
  # ===================================================================
  # 12.8 SAVE FALLBACK DIAGNOSTICS
  # ===================================================================
  
  if (length(fallback_diagnostics) > 0) {
    
    fallback_diagnostics_df <- bind_rows(
      fallback_diagnostics
    )
    
    write.csv(
      fallback_diagnostics_df,
      file.path(
        species_dir,
        "fallback_districts.csv"
      ),
      row.names = FALSE
    )
  }
  
  
  # ===================================================================
  # 12.9 AVAILABLE YEARS
  # ===================================================================
  
  years <- sp_hunt %>%
    filter(!is.na(year)) %>%
    pull(year) %>%
    unique() %>%
    sort()
  
  if (length(years) == 0) {
    stop(
      "No years available for species ",
      sp,
      "."
    )
  }
  
  cat(
    "Years: ",
    min(years),
    "-",
    max(years),
    "\n",
    sep = ""
  )
  
  
  # ===================================================================
  # 13. CREATE SPATIALLY SMOOTHED RASTER FOR EACH YEAR
  # ===================================================================
  
  fallback_year_log <- list()
  
  
  for (yr in years) {
    
    cat(
      "Spatial smoothing: ",
      sp,
      " ",
      yr,
      "\n",
      sep = ""
    )
    
    
    # -----------------------------------------------------------------
    # 13.1 ANNUAL DATA
    # -----------------------------------------------------------------
    
    yr_data <- sp_hunt %>%
      filter(year == yr)
    
    if (nrow(yr_data) == 0) {
      next
    }
    
    active_ids <- unique(
      yr_data$poly_id[!is.na(yr_data$poly_id)]
    )
    
    if (length(active_ids) == 0) {
      next
    }
    
    
    # -----------------------------------------------------------------
    # 13.2 ACTIVE DISTRICTS + SUITABLE HABITAT IN THIS YEAR
    # -----------------------------------------------------------------
    
    active_raster <- subst(
      revier_id_raster,
      from = active_ids,
      to = rep(1, length(active_ids)),
      others = NA
    )
    
    year_habitat_mask <- ifel(
      habitat_mask == 1 &
        !is.na(active_raster),
      1,
      NA
    )
    
    names(year_habitat_mask) <- "suitable"
    
    revier_suitable_year <- mask(
      revier_id_raster,
      year_habitat_mask
    )
    
    
    # -----------------------------------------------------------------
    # 13.3 STANDARD DISTRICTS
    # -----------------------------------------------------------------
    
    regular_yr_data <- yr_data %>%
      filter(
        suitable_area_km2 > 0,
        !is.na(n)
      ) %>%
      mutate(
        district_density =
          n / suitable_area_km2
      )
    
    
    if (nrow(regular_yr_data) > 0) {
      
      density_raster <- subst(
        revier_suitable_year,
        from = regular_yr_data$poly_id,
        to = regular_yr_data$district_density,
        others = NA
      )
      
    } else {
      
      density_raster <- year_habitat_mask * NA_real_
    }
    
    names(density_raster) <- "harvest_density"
    
    
    # -----------------------------------------------------------------
    # 13.4 ADD FALLBACK HARVEST
    # -----------------------------------------------------------------
    
    if (length(fallback_cells) > 0) {
      
      for (pid_character in names(fallback_cells)) {
        
        pid <- as.integer(pid_character)
        
        fallback_n <- yr_data %>%
          filter(poly_id == pid) %>%
          pull(n)
        
        if (length(fallback_n) == 0) {
          next
        }
        
        fallback_n <- fallback_n[1]
        
        if (
          is.na(fallback_n) ||
          fallback_n <= 0
        ) {
          next
        }
        
        target_cells <- unlist(
          fallback_cells[pid_character],
          use.names = FALSE
        )
        
        target_cells <- unique(
          as.integer(target_cells)
        )
        
        target_cells <- target_cells[
          !is.na(target_cells)
        ]
        
        if (length(target_cells) == 0) {
          stop(
            "Fallback cell vector is empty for poly_id ",
            pid,
            "."
          )
        }
        
        # The fallback cells were already selected from suitable habitat
        # and restricted to the same province. Because the final dataset
        # is complete within each province's observation period, no extra
        # year-specific filtering of these cells is required.
        
        fallback_density_increment <-
          fallback_n /
          (length(target_cells) * cell_area_km2)
        
        old_values <- as.numeric(
          density_raster[target_cells]
        )
        
        old_values[is.na(old_values)] <- 0
        
        density_raster[target_cells] <-
          old_values + fallback_density_increment
        
        
        fallback_radius_now <- NA_real_
        
        if (nrow(fallback_diagnostics_df) > 0) {
          
          radius_position <- match(
            pid,
            fallback_diagnostics_df$poly_id
          )
          
          if (!is.na(radius_position)) {
            fallback_radius_now <-
              fallback_diagnostics_df$fallback_radius_m[
                radius_position
              ]
          }
        }
        
        
        fallback_year_log <- append(
          fallback_year_log,
          list(
            data.frame(
              species = sp,
              year = yr,
              poly_id = pid,
              harvest_n = fallback_n,
              fallback_radius_m = fallback_radius_now,
              n_target_pixels = length(target_cells),
              fallback_density_increment =
                fallback_density_increment
            )
          )
        )
        
        cat(
          "  fallback poly_id ",
          pid,
          ": n = ",
          fallback_n,
          " -> ",
          length(target_cells),
          " suitable pixels\n",
          sep = ""
        )
      }
    }
    
    
    # -----------------------------------------------------------------
    # 13.5 CHECK INITIAL ANNUAL TOTAL
    # -----------------------------------------------------------------
    
    initial_total <- global(
      density_raster * cell_area_km2,
      fun = "sum",
      na.rm = TRUE
    )[1, 1]
    
    target_total_initial <- yr_data %>%
      summarise(
        total = sum(n, na.rm = TRUE)
      ) %>%
      pull(total)
    
    cat(
      "  before smoothing: district total = ",
      round(target_total_initial, 4),
      " | raster total = ",
      round(initial_total, 4),
      "\n",
      sep = ""
    )
    
    
    # -----------------------------------------------------------------
    # 13.6 OCTAGONAL SPATIAL MOVING WINDOW
    # -----------------------------------------------------------------
    
    spatial_raster <- spatial_smooth(
      density_raster = density_raster,
      output_habitat_mask = year_habitat_mask,
      weights = window_weights
    )
    
    
    # -----------------------------------------------------------------
    # 13.7 SAVE TEMPORARY SPATIAL RASTER
    # -----------------------------------------------------------------
    
    spatial_file <- file.path(
      spatial_tmp_dir,
      paste0(
        sp,
        "_",
        yr,
        "_spatial_octagon.tif"
      )
    )
    
    writeRaster(
      spatial_raster,
      spatial_file,
      overwrite = TRUE,
      datatype = "FLT4S",
      wopt = list(
        gdal = c(
          "COMPRESS=DEFLATE",
          "PREDICTOR=3",
          "TILED=YES"
        )
      )
    )
  }
  
  
  # ===================================================================
  # 14. SAVE YEAR-SPECIFIC FALLBACK LOG
  # ===================================================================
  
  if (length(fallback_year_log) > 0) {
    
    fallback_year_log_df <- bind_rows(
      fallback_year_log
    )
    
    write.csv(
      fallback_year_log_df,
      file.path(
        species_dir,
        "fallback_assignments_by_year.csv"
      ),
      row.names = FALSE
    )
  }
  
  
  # ===================================================================
  # 15. TEMPORAL MOVING WINDOW
  # ===================================================================
  
  half_window <- floor(
    temporal_window_years / 2
  )
  
  
  for (yr in years) {
    
    cat(
      "Temporal smoothing: ",
      sp,
      " ",
      yr,
      "\n",
      sep = ""
    )
    
    
    # -----------------------------------------------------------------
    # 15.1 TEMPORAL NEIGHBOURHOOD
    # -----------------------------------------------------------------
    
    desired_years <- seq(
      yr - half_window,
      yr + half_window
    )
    
    available_window_years <- desired_years[
      desired_years %in% years
    ]
    
    
    if (
      require_full_temporal_window &&
      length(available_window_years) < temporal_window_years
    ) {
      cat("  skipped: incomplete temporal window\n")
      next
    }
    
    
    temporal_files <- file.path(
      spatial_tmp_dir,
      paste0(
        sp,
        "_",
        available_window_years,
        "_spatial_octagon.tif"
      )
    )
    
    files_exist <- file.exists(
      temporal_files
    )
    
    available_window_years <- available_window_years[
      files_exist
    ]
    
    temporal_files <- temporal_files[
      files_exist
    ]
    
    
    if (length(temporal_files) == 0) {
      warning(
        "No spatial rasters available for ",
        sp,
        " ",
        yr,
        "."
      )
      next
    }
    
    
    if (
      require_full_temporal_window &&
      length(temporal_files) < temporal_window_years
    ) {
      cat("  skipped: incomplete temporal raster window\n")
      next
    }
    
    
    cat(
      "  years used: ",
      paste(
        available_window_years,
        collapse = ", "
      ),
      "\n",
      sep = ""
    )
    
    
    # -----------------------------------------------------------------
    # 15.2 TEMPORAL MEAN
    # -----------------------------------------------------------------
    
    temporal_stack <- rast(
      temporal_files
    )
    
    temporally_smoothed <- mean(
      temporal_stack,
      na.rm = TRUE
    )
    
    
    # -----------------------------------------------------------------
    # 15.3 TARGET-YEAR HABITAT MASK
    # -----------------------------------------------------------------
    
    target_year_ids <- sp_hunt %>%
      filter(year == yr) %>%
      pull(poly_id) %>%
      unique()
    
    target_year_ids <- target_year_ids[
      !is.na(target_year_ids)
    ]
    
    if (length(target_year_ids) == 0) {
      next
    }
    
    target_active_raster <- subst(
      revier_id_raster,
      from = target_year_ids,
      to = rep(1, length(target_year_ids)),
      others = NA
    )
    
    target_year_habitat <- ifel(
      habitat_mask == 1 &
        !is.na(target_active_raster),
      1,
      NA
    )
    
    temporally_smoothed <- mask(
      temporally_smoothed,
      target_year_habitat
    )
    
    
    # =================================================================
    # 16. RESTORE ANNUAL TOTAL HARVEST
    # =================================================================
    
    target_total <- sp_hunt %>%
      filter(year == yr) %>%
      summarise(
        total = sum(n, na.rm = TRUE)
      ) %>%
      pull(total)
    
    
    if (
      is.na(target_total) ||
      target_total <= 0
    ) {
      
      final_raster <- ifel(
        !is.na(target_year_habitat),
        0,
        NA
      )
      
      correction_factor <- NA_real_
      
    } else {
      
      current_total <- global(
        temporally_smoothed * cell_area_km2,
        fun = "sum",
        na.rm = TRUE
      )[1, 1]
      
      if (
        !is.finite(current_total) ||
        current_total <= 0
      ) {
        stop(
          "Invalid raster total for ",
          sp,
          " ",
          yr,
          ". Target total = ",
          target_total,
          "; raster total = ",
          current_total,
          "."
        )
      }
      
      correction_factor <-
        target_total / current_total
      
      final_raster <-
        temporally_smoothed * correction_factor
    }
    
    names(final_raster) <- "harvest_density"
    
    
    # =================================================================
    # 17. WRITE FINAL RASTER
    # =================================================================
    
    output_file <- file.path(
      final_dir,
      paste0(
        sp,
        "_harvest_density_",
        yr,
        ".tif"
      )
    )
    
    writeRaster(
      final_raster,
      output_file,
      overwrite = TRUE,
      datatype = "FLT4S",
      wopt = list(
        gdal = c(
          "COMPRESS=DEFLATE",
          "PREDICTOR=3",
          "TILED=YES"
        )
      )
    )
    
    
    # =================================================================
    # 18. FINAL CONSERVATION CHECK
    # =================================================================
    
    final_total <- global(
      final_raster * cell_area_km2,
      fun = "sum",
      na.rm = TRUE
    )[1, 1]
    
    cat(
      "  original total = ",
      round(target_total, 4),
      " | final raster total = ",
      round(final_total, 4),
      " | correction factor = ",
      ifelse(
        is.na(correction_factor),
        "NA",
        format(
          round(correction_factor, 6),
          scientific = FALSE
        )
      ),
      "\n",
      sep = ""
    )
  }
  
  
  cat(
    "Finished species: ",
    sp,
    "\n",
    sep = ""
  )
}


# =====================================================================
# 19. FINISHED
# =====================================================================

cat("\n")
cat("============================================================\n")
cat("ALL SPECIES COMPLETE\n")
cat("============================================================\n")
cat("Resolution:             ", target_resolution_m, " m\n", sep = "")
cat("Spatial window:         octagon\n")
cat("Spatial radius:         ", spatial_radius_m, " m\n", sep = "")
cat("Temporal window:        ", temporal_window_years, " years\n", sep = "")
cat("Suitable habitat only:  yes\n")
cat(
  "Fallback radii:        ",
  paste(fallback_radii_m, collapse = ", "),
  " m\n",
  sep = ""
)
cat("Fallback same province: ", fallback_same_province, "\n", sep = "")
cat("Output:                 ", output_dir, "\n", sep = "")
cat("============================================================\n")
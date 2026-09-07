# =====================================================================
# CONTINUOUS HARVEST DENSITY RASTERS
# Hunting district x species x year -> continuous 100 m raster
#
# Workflow:
# 1. distribute annual district harvest over suitable habitat
# 2. use nearby suitable habitat as fallback where necessary
# 3. spatially smooth with an octagonal moving window
# 4. temporally smooth across neighbouring years
# 5. rescale each species-year raster to preserve the annual harvest total
#
# Output unit: n / km² / year
# Final values occur only in suitable habitat.
# =====================================================================

library(sf)
library(terra)
library(dplyr)

terraOptions(progress = 1)

# =====================================================================
# 1. SETTINGS
# =====================================================================

input_gpkg  <- "/mnt/eo/WilDensity/output/final_integer_allocation/interpolated_missing_reviere/final_revier_timeseries_complete_interpolated.gpkg"
input_layer <- "revier_timeseries"

suitability_files <- c(
  roe_deer = "/mnt/eo/WilDensity/output/suitability_roe_deer_0311.tif",
  red_deer = "/mnt/eo/WilDensity/output/suitability_red_deer_0311.tif",
  chamois   = "/mnt/eo/WilDensity/output/suitability_chamois_0311.tif"
)

output_dir <- "/mnt/eo/WilDensity/output/continuous_harvest_density_7y"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

target_crs          <- "EPSG:3035"
target_resolution_m <- 100
spatial_radius_m    <- 1880

# Allowed: 1, 3, 5, 7, 9
temporal_window_years <- 7
require_full_temporal_window <- FALSE

fallback_radii_m <- c(500, 1000, 2000)
fallback_same_province <- TRUE

cell_area_km2 <- target_resolution_m^2 / 1e6

if (!temporal_window_years %in% c(1, 3, 5, 7, 9)) {
  stop("temporal_window_years must be 1, 3, 5, 7 or 9.")
}

if (!file.exists(input_gpkg)) {
  stop("Input GPKG does not exist: ", input_gpkg)
}

missing_suitability <- suitability_files[!file.exists(suitability_files)]

if (length(missing_suitability) > 0) {
  stop("Suitability raster does not exist: ", missing_suitability[1])
}


# =====================================================================
# 2. READ AND PREPARE INPUT DATA
# =====================================================================

cat("Reading hunting data...\n")

hunt_sf <- st_read(
  input_gpkg,
  layer = input_layer,
  quiet = TRUE
) %>%
  mutate(
    poly_id = as.integer(poly_id),
    species = as.character(species),
    year    = as.integer(year),
    n       = as.numeric(n),
    prov    = as.character(prov)
  ) %>%
  filter(
    species %in% names(suitability_files),
    year != 1992
  )

if (nrow(hunt_sf) == 0) {
  stop("No hunting data found after filtering.")
}


cat("Preparing hunting-district geometries...\n")

reviere <- hunt_sf %>%
  arrange(poly_id) %>%
  filter(!duplicated(poly_id)) %>%
  select(poly_id, prov) %>%
  st_make_valid() %>%
  st_transform(target_crs)

if (anyDuplicated(reviere$poly_id) > 0) {
  stop("poly_id is not unique.")
}


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
# 3. CREATE COMMON 100 M GRID
# =====================================================================

cat("Creating 100 m raster template...\n")

rev_v <- vect(reviere)
e <- ext(rev_v)

xmin_template <-
  floor(xmin(e) / target_resolution_m) *
  target_resolution_m

xmax_template <-
  ceiling(xmax(e) / target_resolution_m) *
  target_resolution_m

ymin_template <-
  floor(ymin(e) / target_resolution_m) *
  target_resolution_m

ymax_template <-
  ceiling(ymax(e) / target_resolution_m) *
  target_resolution_m


template <- rast(
  xmin = xmin_template,
  xmax = xmax_template,
  ymin = ymin_template,
  ymax = ymax_template,
  resolution = target_resolution_m,
  crs = target_crs
)

cat(
  "Raster dimensions:",
  ncol(template),
  "x",
  nrow(template),
  "\n"
)


cat("Rasterizing hunting districts...\n")

revier_id_raster <- rasterize(
  rev_v,
  template,
  field = "poly_id"
)

province_raster <- rasterize(
  rev_v,
  template,
  field = "prov_code"
)

names(revier_id_raster) <- "poly_id"
names(province_raster) <- "prov_code"


# =====================================================================
# 4. OCTAGONAL MOVING WINDOW
# =====================================================================

radius_cells <- ceiling(
  spatial_radius_m /
    target_resolution_m
)

coords <- -radius_cells:radius_cells

xx <- matrix(
  rep(
    coords,
    each = length(coords)
  ),
  nrow = length(coords)
)

yy <- t(xx)

inside_octagon <-
  abs(xx) <= radius_cells &
  abs(yy) <= radius_cells &
  (abs(xx) + abs(yy)) <=
  sqrt(2) * radius_cells


window_weights <- matrix(
  0,
  nrow = nrow(xx),
  ncol = ncol(xx)
)

window_weights[inside_octagon] <- 1


cat(
  "\nOctagonal moving window:\n",
  "  radius: ", spatial_radius_m, " m\n",
  "  matrix: ", nrow(window_weights), " x ", ncol(window_weights), "\n",
  "  included cells: ", sum(window_weights), "\n\n",
  sep = ""
)


# =====================================================================
# 5. PROCESS EACH SPECIES
# =====================================================================

for (sp in names(suitability_files)) {
  
  cat("\n============================================================\n")
  cat("SPECIES:", sp, "\n")
  cat("============================================================\n")
  
  
  # -------------------------------------------------------------------
  # Output folders
  # -------------------------------------------------------------------
  
  species_dir <- file.path(
    output_dir,
    sp
  )
  
  spatial_tmp_dir <- file.path(
    species_dir,
    paste0(
      "_spatial_tmp_octagon_",
      spatial_radius_m,
      "m"
    )
  )
  
  final_dir <- file.path(
    species_dir,
    paste0(
      "octagon_",
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
  
  
  # -------------------------------------------------------------------
  # Suitability and habitat mask
  # -------------------------------------------------------------------
  
  cat("Reading suitability raster...\n")
  
  suitability <- project(
    rast(
      unname(
        suitability_files[sp]
      )
    ),
    template,
    method = "near"
  )
  
  
  # 1 = suitable habitat inside hunting district
  # NA = unsuitable
  
  habitat_mask <- ifel(
    suitability == 1 &
      !is.na(revier_id_raster),
    1,
    NA
  )
  
  names(habitat_mask) <- "suitable"
  
  
  # -------------------------------------------------------------------
  # Suitable area per district
  # -------------------------------------------------------------------
  
  suitable_counts <- zonal(
    habitat_mask,
    revier_id_raster,
    fun = "sum",
    na.rm = TRUE
  ) %>%
    as.data.frame()
  
  
  names(suitable_counts)[1:2] <- c(
    "poly_id",
    "n_suitable_pixels"
  )
  
  
  suitable_counts <- suitable_counts %>%
    mutate(
      poly_id =
        as.integer(poly_id),
      
      n_suitable_pixels =
        coalesce(
          as.numeric(n_suitable_pixels),
          0
        ),
      
      suitable_area_km2 =
        n_suitable_pixels *
        cell_area_km2
    )
  
  
  sp_hunt <- hunt %>%
    filter(
      species == sp
    ) %>%
    left_join(
      suitable_counts,
      by = "poly_id"
    ) %>%
    mutate(
      n_suitable_pixels =
        coalesce(
          n_suitable_pixels,
          0
        ),
      
      suitable_area_km2 =
        coalesce(
          suitable_area_km2,
          0
        )
    )
  
  
  # -------------------------------------------------------------------
  # Districts that need fallback habitat
  # -------------------------------------------------------------------
  
  fallback_districts <- sp_hunt %>%
    group_by(
      poly_id,
      prov
    ) %>%
    summarise(
      suitable_area_km2 =
        first(
          suitable_area_km2
        ),
      
      max_harvest =
        ifelse(
          all(is.na(n)),
          NA_real_,
          max(
            n,
            na.rm = TRUE
          )
        ),
      
      .groups = "drop"
    ) %>%
    filter(
      suitable_area_km2 <= 0,
      !is.na(max_harvest),
      max_harvest > 0
    )
  
  
  cat(
    "Fallback districts:",
    nrow(fallback_districts),
    "\n"
  )
  
  
  # -------------------------------------------------------------------
  # Find fallback cells once per district
  # -------------------------------------------------------------------
  
  fallback_cells <- list()
  fallback_diagnostics <- list()
  
  
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
      
      
      target_index <- which(
        rev_v$poly_id == pid
      )
      
      if (length(target_index) != 1) {
        stop(
          "Could not uniquely identify poly_id ",
          pid,
          "."
        )
      }
      
      
      target_polygon <- rev_v[
        target_index,
      ]
      
      
      target_prov_code <-
        province_lookup$prov_code[
          match(
            pprov,
            province_lookup$prov
          )
        ]
      
      
      if (is.na(target_prov_code)) {
        stop(
          "Could not uniquely identify province ",
          pprov,
          "."
        )
      }
      
      
      found <- FALSE
      
      
      for (radius_now in fallback_radii_m) {
        
        buffer_polygon <- terra::buffer(
          target_polygon,
          width = radius_now
        )
        
        
        extracted <- terra::extract(
          c(
            habitat_mask,
            province_raster
          ),
          buffer_polygon,
          cells = TRUE,
          touches = TRUE
        ) %>%
          as.data.frame() %>%
          filter(
            !is.na(suitable),
            suitable == 1,
            !is.na(cell)
          )
        
        
        if (fallback_same_province) {
          
          extracted <- extracted %>%
            filter(
              !is.na(prov_code),
              prov_code ==
                target_prov_code
            )
        }
        
        
        target_cells <- unique(
          extracted$cell
        )
        
        target_cells <- target_cells[
          !is.na(target_cells)
        ]
        
        
        if (length(target_cells) > 0) {
          
          fallback_cells[
            as.character(pid)
          ] <- list(
            target_cells
          )
          
          
          fallback_diagnostics <- append(
            fallback_diagnostics,
            list(
              data.frame(
                species = sp,
                poly_id = pid,
                prov = pprov,
                fallback_radius_m =
                  radius_now,
                n_target_pixels =
                  length(target_cells),
                target_area_km2 =
                  length(target_cells) *
                  cell_area_km2
              )
            )
          )
          
          
          cat(
            "  -> radius: ",
            radius_now,
            " m | suitable pixels: ",
            length(target_cells),
            "\n",
            sep = ""
          )
          
          
          found <- TRUE
          break
        }
      }
      
      
      if (!found) {
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
    }
  }
  
  
  fallback_diagnostics_df <- data.frame()
  
  
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
  
  
  # -------------------------------------------------------------------
  # Years
  # -------------------------------------------------------------------
  
  years <- sort(
    unique(
      sp_hunt$year
    )
  )
  
  
  if (length(years) == 0) {
    stop(
      "No years available for ",
      sp
    )
  }
  
  
  cat(
    "Years: ",
    min(years),
    "–",
    max(years),
    "\n",
    sep = ""
  )
  
  
  # ===================================================================
  # 6. SPATIAL RASTER FOR EACH YEAR
  # ===================================================================
  
  fallback_year_log <- list()
  
  
  for (yr in years) {
    
    cat(
      "\nSpatial smoothing: ",
      sp,
      " ",
      yr,
      "\n",
      sep = ""
    )
    
    
    yr_data <- sp_hunt %>%
      filter(
        year == yr
      )
    
    
    if (nrow(yr_data) == 0) {
      next
    }
    
    
    active_ids <- unique(
      yr_data$poly_id[
        !is.na(
          yr_data$poly_id
        )
      ]
    )
    
    
    if (length(active_ids) == 0) {
      next
    }
    
    
    active_raster <- subst(
      revier_id_raster,
      from = active_ids,
      to = rep(
        1,
        length(active_ids)
      ),
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
    # Regular districts
    # -----------------------------------------------------------------
    
    regular_yr_data <- yr_data %>%
      filter(
        suitable_area_km2 > 0,
        !is.na(n)
      ) %>%
      mutate(
        district_density =
          n /
          suitable_area_km2
      )
    
    
    if (nrow(regular_yr_data) > 0) {
      
      density_raster <- subst(
        revier_suitable_year,
        from =
          regular_yr_data$poly_id,
        to =
          regular_yr_data$district_density,
        others = NA
      )
      
    } else {
      
      density_raster <-
        year_habitat_mask *
        NA_real_
    }
    
    
    names(density_raster) <- "harvest_density"
    
    
    # -----------------------------------------------------------------
    # Add fallback harvest
    # -----------------------------------------------------------------
    
    if (length(fallback_cells) > 0) {
      
      for (pid_character in names(fallback_cells)) {
        
        pid <- as.integer(
          pid_character
        )
        
        
        fallback_n <- yr_data$n[
          !is.na(yr_data$poly_id) &
            yr_data$poly_id == pid
        ]
        
        
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
          fallback_cells[
            pid_character
          ],
          use.names = FALSE
        )
        
        
        target_status <- as.numeric(
          terra::extract(
            year_habitat_mask,
            target_cells,
            raw = TRUE
          )
        )
        
        
        target_cells <- target_cells[
          !is.na(target_status)
        ]
        
        
        if (length(target_cells) == 0) {
          stop(
            "No active suitable fallback cells for poly_id ",
            pid,
            " in year ",
            yr,
            "."
          )
        }
        
        
        fallback_density_increment <-
          fallback_n /
          (
            length(target_cells) *
              cell_area_km2
          )
        
        
        old_values <- as.numeric(
          terra::extract(
            density_raster,
            target_cells,
            raw = TRUE
          )
        )
        
        
        old_values[
          is.na(old_values)
        ] <- 0
        
        
        density_raster[
          target_cells
        ] <-
          old_values +
          fallback_density_increment
        
        
        fallback_radius_now <- NA_real_
        
        
        if (nrow(fallback_diagnostics_df) > 0) {
          
          pos <- match(
            pid,
            fallback_diagnostics_df$poly_id
          )
          
          if (!is.na(pos)) {
            
            fallback_radius_now <-
              fallback_diagnostics_df$
              fallback_radius_m[pos]
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
              fallback_radius_m =
                fallback_radius_now,
              n_target_pixels =
                length(target_cells),
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
    # Spatial octagonal moving-window mean
    # -----------------------------------------------------------------
    
    numerator <- focal(
      density_raster,
      w = window_weights,
      fun = "sum",
      na.rm = TRUE
    )
    
    
    denominator <- focal(
      year_habitat_mask,
      w = window_weights,
      fun = "sum",
      na.rm = TRUE
    )
    
    
    spatial_raster <-
      numerator /
      denominator
    
    
    spatial_raster <- mask(
      spatial_raster,
      year_habitat_mask
    )
    
    
    spatial_raster <- ifel(
      denominator > 0,
      spatial_raster,
      NA
    )
    
    
    names(spatial_raster) <-
      "harvest_density"
    
    
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
  
  
  # -------------------------------------------------------------------
  # Save year-specific fallback log
  # -------------------------------------------------------------------
  
  if (length(fallback_year_log) > 0) {
    
    write.csv(
      bind_rows(
        fallback_year_log
      ),
      file.path(
        species_dir,
        "fallback_assignments_by_year.csv"
      ),
      row.names = FALSE
    )
  }
  
  
  # ===================================================================
  # 7. TEMPORAL SMOOTHING + ANNUAL RESCALING
  # ===================================================================
  
  half_window <- floor(
    temporal_window_years /
      2
  )
  
  
  for (yr in years) {
    
    cat(
      "\nTemporal smoothing: ",
      sp,
      " ",
      yr,
      "\n",
      sep = ""
    )
    
    
    desired_years <- seq(
      yr - half_window,
      yr + half_window
    )
    
    
    available_window_years <-
      desired_years[
        desired_years %in%
          years
      ]
    
    
    if (
      require_full_temporal_window &&
      length(available_window_years) <
      temporal_window_years
    ) {
      
      cat(
        "  skipped: incomplete temporal window\n"
      )
      
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
    
    
    available_window_years <-
      available_window_years[
        files_exist
      ]
    
    
    temporal_files <-
      temporal_files[
        files_exist
      ]
    
    
    if (length(temporal_files) == 0) {
      
      warning(
        "No spatial rasters available for ",
        sp,
        " ",
        yr
      )
      
      next
    }
    
    
    if (
      require_full_temporal_window &&
      length(temporal_files) <
      temporal_window_years
    ) {
      
      cat(
        "  skipped: incomplete temporal raster window\n"
      )
      
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
    
    
    temporally_smoothed <- mean(
      rast(
        temporal_files
      ),
      na.rm = TRUE
    )
    
    
    # -----------------------------------------------------------------
    # Restrict final raster to suitable habitat in target year
    # -----------------------------------------------------------------
    
    target_year_ids <- unique(
      sp_hunt$poly_id[
        !is.na(sp_hunt$year) &
          sp_hunt$year == yr &
          !is.na(sp_hunt$poly_id)
      ]
    )
    
    
    if (length(target_year_ids) == 0) {
      next
    }
    
    
    target_active_raster <- subst(
      revier_id_raster,
      from = target_year_ids,
      to = rep(
        1,
        length(target_year_ids)
      ),
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
    
    
    # -----------------------------------------------------------------
    # Restore original annual harvest total
    # -----------------------------------------------------------------
    
    target_total <- sum(
      sp_hunt$n[
        sp_hunt$year == yr
      ],
      na.rm = TRUE
    )
    
    
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
        temporally_smoothed *
          cell_area_km2,
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
          current_total
        )
      }
      
      
      correction_factor <-
        target_total /
        current_total
      
      
      final_raster <-
        temporally_smoothed *
        correction_factor
    }
    
    
    names(final_raster) <-
      "harvest_density"
    
    
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
    
    
    final_total <- global(
      final_raster *
        cell_area_km2,
      fun = "sum",
      na.rm = TRUE
    )[1, 1]
    
    
    cat(
      "  original total = ",
      round(
        target_total,
        2
      ),
      " | raster total = ",
      round(
        final_total,
        2
      ),
      " | correction factor = ",
      ifelse(
        is.na(correction_factor),
        "NA",
        as.character(
          round(
            correction_factor,
            4
          )
        )
      ),
      "\n",
      sep = ""
    )
  }
  
  
  cat(
    "\nFinished: ",
    sp,
    "\n",
    sep = ""
  )
}


# =====================================================================
# 8. FINISHED
# =====================================================================

cat("\n============================================================\n")
cat("ALL SPECIES COMPLETE\n")
cat("============================================================\n")

cat(
  "Resolution:              ",
  target_resolution_m,
  " m\n",
  sep = ""
)

cat(
  "Spatial window:          octagon\n"
)

cat(
  "Spatial radius:          ",
  spatial_radius_m,
  " m\n",
  sep = ""
)

cat(
  "Temporal window:         ",
  temporal_window_years,
  " years\n",
  sep = ""
)

cat(
  "Suitable habitat only:   yes\n"
)

cat(
  "Fallback radii:          ",
  paste(
    fallback_radii_m,
    collapse = ", "
  ),
  " m\n",
  sep = ""
)

cat(
  "Fallback same province:  ",
  fallback_same_province,
  "\n",
  sep = ""
)

cat(
  "Output:                  ",
  output_dir,
  "\n",
  sep = ""
)

cat("============================================================\n")
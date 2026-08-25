# =====================================================================
# CONTINUOUS HARVEST DENSITY RASTERS
# Hunting district x species x year -> continuous 100 m raster
#
# WORKFLOW
# --------
# 1. Read final hunting-district harvest data
# 2. Create common 100 m raster grid
# 3. Use species-specific binary suitability:
#       1  = suitable
#       0  = unsuitable
# 4. Distribute district harvest across suitable habitat
# 5. Fallback for districts with harvest but zero suitable pixels:
#       search within 500 m -> 1000 m -> 2000 m
# 6. Spatial smoothing with an OCTAGONAL moving window
# 7. Temporal smoothing:
#       e.g. 3 years = t-1, t, t+1
# 8. Rescale each species x year raster so that annual total harvest
#    is exactly conserved
#
# OUTPUT UNIT
# -----------
# harvest density = n / km² / year
#
# IMPORTANT!
# ---------
# Only suitable habitat receives final raster values.
# =====================================================================


# =====================================================================
# 0. PACKAGES
# =====================================================================

library(sf)
library(terra)
library(dplyr)


terraOptions(
  progress = 1
)


# =====================================================================
# 1. SETTINGS
# =====================================================================

# ---------------------------------------------------------------------
# Final complete hunting-district dataset
# ---------------------------------------------------------------------

input_gpkg <-
  "/mnt/eo/WilDensity/output/final_integer_allocation/interpolated_missing_reviere/final_revier_timeseries_complete_interpolated.gpkg"

input_layer <-
  "revier_timeseries"


# ---------------------------------------------------------------------
# Species-specific binary suitability rasters
#
# 1 = suitable
# 0 = unsuitable
# ---------------------------------------------------------------------

suitability_files <- c(
  
  roe_deer =
    "/mnt/eo/WilDensity/output/suitability_roe_deer_0311.tif",
  
  red_deer =
    "/mnt/eo/WilDensity/output/suitability_red_deer_0311.tif",
  
  chamois =
    "/mnt/eo/WilDensity/output/suitability_chamois_0311.tif"
)


# ---------------------------------------------------------------------
# Output
# ---------------------------------------------------------------------

output_dir <-
  "/mnt/eo/WilDensity/output/continuous_harvest_density"

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)


# ---------------------------------------------------------------------
# Raster settings
# ---------------------------------------------------------------------

target_crs <-
  "EPSG:3035"

target_resolution_m <-
  100


# ---------------------------------------------------------------------
# Spatial moving window
#
# Radius from centre to horizontal / vertical edge of octagon
# ---------------------------------------------------------------------

spatial_radius_m <-
  2000


# ---------------------------------------------------------------------
# Temporal moving window
#
# 1 = only target year
# 3 = t-1, t, t+1
# 5 = t-2, ..., t+2
# ---------------------------------------------------------------------

temporal_window_years <-
  3


# FALSE:
# At beginning/end of time series, use available neighbouring years.
#
# TRUE:
# Skip years without complete temporal window.
require_full_temporal_window <-
  FALSE


# ---------------------------------------------------------------------
# Fallback
#
# Hunting district has:
#   n > 0
# but:
#   0 suitable pixels
#
# Search suitable habitat at increasing distances.
# ---------------------------------------------------------------------

fallback_radii_m <-
  c(
    500,
    1000,
    2000
  )


# Restrict fallback habitat to same province
fallback_same_province <-
  TRUE


# ---------------------------------------------------------------------
# Pixel area
# ---------------------------------------------------------------------

cell_area_km2 <-
  (
    target_resolution_m^2
  ) / 1e6


# =====================================================================
# 2. CHECK SETTINGS
# =====================================================================

if (
  !temporal_window_years %in%
  c(1, 3, 5)
) {
  
  stop(
    "temporal_window_years must be 1, 3 or 5."
  )
}


for (ff in unname(suitability_files)) {
  
  if (!file.exists(ff)) {
    
    stop(
      "Suitability raster does not exist: ",
      ff
    )
  }
}


if (!file.exists(input_gpkg)) {
  
  stop(
    "Input GPKG does not exist: ",
    input_gpkg
  )
}


# =====================================================================
# 3. READ HUNT DATA
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
    year = as.integer(year),
    n = as.numeric(n),
    prov = as.character(prov)
  ) %>%
  
  filter(
    species %in% names(suitability_files),
    year != 1992
  )


if (nrow(hunt_sf) == 0) {
  
  stop(
    "No hunting data found after filtering."
  )
}


# =====================================================================
# 4. UNIQUE HUNTING-DISTRICT GEOMETRIES
# =====================================================================

cat("Preparing hunting-district geometries...\n")


reviere <- hunt_sf %>%
  
  arrange(poly_id) %>%
  
  filter(
    !duplicated(poly_id)
  ) %>%
  
  select(
    poly_id,
    prov
  ) %>%
  
  st_make_valid() %>%
  
  st_transform(
    target_crs
  )


if (anyDuplicated(reviere$poly_id) > 0) {
  
  stop(
    "poly_id is not unique."
  )
}


# =====================================================================
# 5. PROVINCE LOOKUP
# =====================================================================

province_lookup <- reviere %>%
  
  st_drop_geometry() %>%
  
  distinct(prov) %>%
  
  arrange(prov) %>%
  
  mutate(
    prov_code = row_number()
  )


reviere <- reviere %>%
  
  left_join(
    province_lookup,
    by = "prov"
  )


# =====================================================================
# 6. HARVEST ATTRIBUTE TABLE
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


# ---------------------------------------------------------------------
# Check uniqueness
# ---------------------------------------------------------------------

duplicates <- hunt %>%
  
  count(
    poly_id,
    species,
    year,
    name = "n_rows"
  ) %>%
  
  filter(
    n_rows > 1
  )


if (nrow(duplicates) > 0) {
  
  print(duplicates)
  
  stop(
    "Duplicate poly_id x species x year combinations found."
  )
}


# =====================================================================
# 7. CREATE 100 M TEMPLATE
# =====================================================================

cat("Creating 100 m raster template...\n")


rev_v <- vect(
  reviere
)


e <- ext(
  rev_v
)


xmin_template <-
  floor(
    xmin(e) /
      target_resolution_m
  ) *
  target_resolution_m


xmax_template <-
  ceiling(
    xmax(e) /
      target_resolution_m
  ) *
  target_resolution_m


ymin_template <-
  floor(
    ymin(e) /
      target_resolution_m
  ) *
  target_resolution_m


ymax_template <-
  ceiling(
    ymax(e) /
      target_resolution_m
  ) *
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


# =====================================================================
# 8. RASTERIZE DISTRICTS + PROVINCES
# =====================================================================

cat("Rasterizing hunting districts...\n")


revier_id_raster <- rasterize(
  rev_v,
  template,
  field = "poly_id"
)

names(revier_id_raster) <-
  "poly_id"


province_raster <- rasterize(
  rev_v,
  template,
  field = "prov_code"
)

names(province_raster) <-
  "prov_code"


# =====================================================================
# 9. CREATE OCTAGONAL MOVING WINDOW
# =====================================================================

make_octagon_kernel <- function(
    radius_m,
    resolution_m
) {
  
  radius_cells <-
    ceiling(
      radius_m /
        resolution_m
    )
  
  
  coords <-
    -radius_cells:radius_cells
  
  
  n <-
    length(coords)
  
  
  xx <- matrix(
    rep(
      coords,
      each = n
    ),
    nrow = n,
    ncol = n
  )
  
  
  yy <-
    t(xx)
  
  
  # Regular octagon:
  #
  # square:
  #   |x| <= r
  #   |y| <= r
  #
  # corners cut using:
  #   |x| + |y| <= sqrt(2) * r
  
  inside <- (
    
    abs(xx) <= radius_cells &
      
      abs(yy) <= radius_cells &
      
      (
        abs(xx) +
          abs(yy)
      ) <=
      sqrt(2) *
      radius_cells
  )
  
  
  weights <- matrix(
    0,
    nrow = n,
    ncol = n
  )
  
  
  weights[inside] <-
    1
  
  
  return(weights)
}


window_weights <- make_octagon_kernel(
  radius_m = spatial_radius_m,
  resolution_m = target_resolution_m
)


cat("\nOctagonal moving window:\n")

cat(
  "  radius:",
  spatial_radius_m,
  "m\n"
)

cat(
  "  matrix:",
  nrow(window_weights),
  "x",
  ncol(window_weights),
  "\n"
)

cat(
  "  included cells:",
  sum(window_weights),
  "\n\n"
)


# =====================================================================
# 10. SPATIAL SMOOTHING FUNCTION
# =====================================================================

spatial_smooth <- function(
    density_raster,
    valid_habitat_mask,
    weights
) {
  
  # -------------------------------------------------------------------
  # Numerator:
  # sum of density values in suitable neighbourhood
  # -------------------------------------------------------------------
  
  numerator <- focal(
    density_raster,
    w = weights,
    fun = "sum",
    na.rm = TRUE
  )
  
  
  # -------------------------------------------------------------------
  # Denominator:
  # number / weight of suitable pixels
  # -------------------------------------------------------------------
  
  denominator <- focal(
    valid_habitat_mask,
    w = weights,
    fun = "sum",
    na.rm = TRUE
  )
  
  
  # -------------------------------------------------------------------
  # Moving-window mean
  # -------------------------------------------------------------------
  
  out <-
    numerator /
    denominator
  
  
  out <- mask(
    out,
    valid_habitat_mask
  )
  
  
  out <- ifel(
    denominator > 0,
    out,
    NA
  )
  
  
  names(out) <-
    "harvest_density"
  
  
  return(out)
}


# =====================================================================
# 11. FALLBACK FUNCTION
# =====================================================================
#
# For districts with:
#
#   positive harvest
#   BUT
#   no suitable habitat
#
# Search:
#
#   500 m
#   1000 m
#   2000 m
#
# The first radius containing suitable habitat is used.
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
  
  # -------------------------------------------------------------------
  # Target polygon
  # -------------------------------------------------------------------
  
  target_index <- which(
    rev_v$poly_id ==
      poly_id_target
  )
  
  
  if (length(target_index) != 1) {
    
    stop(
      "Could not uniquely identify poly_id ",
      poly_id_target,
      "."
    )
  }
  
  
  target_polygon <-
    rev_v[target_index, ]
  
  
  # -------------------------------------------------------------------
  # Province
  # -------------------------------------------------------------------
  
  province_match <- province_lookup %>%
    
    filter(
      prov ==
        prov_target
    )
  
  
  if (nrow(province_match) != 1) {
    
    stop(
      "Could not uniquely identify province ",
      prov_target,
      "."
    )
  }
  
  
  target_prov_code <-
    province_match$prov_code[1]
  
  
  # -------------------------------------------------------------------
  # Progressive search
  # -------------------------------------------------------------------
  
  for (radius_now in search_radii) {
    
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
    )
    
    
    extracted <- as.data.frame(
      extracted
    )
    
    
    if (nrow(extracted) == 0) {
      next
    }
    
    
    # -----------------------------------------------------------------
    # Suitable cells only
    # -----------------------------------------------------------------
    
    extracted <- extracted %>%
      
      filter(
        !is.na(suitable),
        suitable == 1,
        !is.na(cell)
      )
    
    
    # -----------------------------------------------------------------
    # Same province only
    # -----------------------------------------------------------------
    
    if (same_province) {
      
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
    
    
    # -----------------------------------------------------------------
    # Successful radius
    # -----------------------------------------------------------------
    
    if (length(target_cells) > 0) {
      
      return(
        list(
          radius_m = radius_now,
          cells = target_cells
        )
      )
    }
  }
  
  
  return(NULL)
}


# =====================================================================
# 12. PROCESS EACH SPECIES
# =====================================================================

for (sp in names(suitability_files)) {
  
  cat("\n")
  cat("============================================================\n")
  cat("SPECIES:", sp, "\n")
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
  
  
  # ===================================================================
  # 12.2 READ SUITABILITY
  # ===================================================================
  
  cat("Reading suitability raster...\n")
  
  
  suitability_file <- unname(
    suitability_files[sp]
  )
  
  
  suitability_raw <- rast(
    suitability_file
  )
  
  
  # Binary data -> nearest neighbour
  suitability <- project(
    suitability_raw,
    template,
    method = "near"
  )
  
  
  # ===================================================================
  # 12.3 HABITAT MASK
  # ===================================================================
  
  # 1  = suitable
  # NA = unsuitable
  
  habitat_mask <- ifel(
    suitability == 1 &
      !is.na(revier_id_raster),
    1,
    NA
  )
  
  
  names(habitat_mask) <-
    "suitable"
  
  
  # ===================================================================
  # 12.4 COUNT SUITABLE PIXELS PER DISTRICT
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
  
  
  names(suitable_counts)[1:2] <- c(
    "poly_id",
    "n_suitable_pixels"
  )
  
  
  suitable_counts <- suitable_counts %>%
    
    mutate(
      poly_id =
        as.integer(poly_id),
      
      n_suitable_pixels =
        as.numeric(n_suitable_pixels),
      
      n_suitable_pixels =
        coalesce(
          n_suitable_pixels,
          0
        ),
      
      suitable_area_km2 =
        n_suitable_pixels *
        cell_area_km2
    )
  
  
  # ===================================================================
  # 12.5 JOIN HARVEST DATA
  # ===================================================================
  
  sp_hunt <- hunt %>%
    
    filter(
      species ==
        sp
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
  
  
  # ===================================================================
  # 12.6 FIND FALLBACK DISTRICTS
  # ===================================================================
  
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
  
  
  # ===================================================================
  # 12.7 FIND FALLBACK CELLS
  # ===================================================================
  
  fallback_cells <-
    list()
  
  
  fallback_diagnostics <-
    list()
  
  
  fallback_diagnostics_df <-
    data.frame()
  
  
  if (nrow(fallback_districts) > 0) {
    
    for (i in seq_len(nrow(fallback_districts))) {
      
      pid <-
        fallback_districts$poly_id[i]
      
      
      pprov <-
        fallback_districts$prov[i]
      
      
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
      
      
      # ---------------------------------------------------------------
      # Store target cells
      #
      # No [[ ]] syntax.
      # ---------------------------------------------------------------
      
      fallback_cells[
        as.character(pid)
      ] <- list(
        result$cells
      )
      
      
      # ---------------------------------------------------------------
      # Diagnostic
      # ---------------------------------------------------------------
      
      fallback_diagnostics <- append(
        fallback_diagnostics,
        list(
          data.frame(
            species = sp,
            poly_id = pid,
            prov = pprov,
            fallback_radius_m =
              result$radius_m,
            n_target_pixels =
              length(result$cells),
            target_area_km2 =
              length(result$cells) *
              cell_area_km2
          )
        )
      )
      
      
      cat(
        "  -> radius: ",
        result$radius_m,
        " m | suitable pixels: ",
        length(result$cells),
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
  # 12.9 YEARS
  # ===================================================================
  
  years <- sp_hunt %>%
    
    pull(year) %>%
    
    unique() %>%
    
    sort()
  
  
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
  # 13. SPATIAL RASTER FOR EACH YEAR
  # ===================================================================
  
  fallback_year_log <-
    list()
  
  
  for (yr in years) {
    
    cat(
      "\nSpatial smoothing: ",
      sp,
      " ",
      yr,
      "\n",
      sep = ""
    )
    
    
    # -----------------------------------------------------------------
    # Annual district data
    # -----------------------------------------------------------------
    
    yr_data <- sp_hunt %>%
      
      filter(
        year ==
          yr
      )
    
    
    if (nrow(yr_data) == 0) {
      next
    }
    
    
    # -----------------------------------------------------------------
    # Active polygons in target year
    # -----------------------------------------------------------------
    
    active_ids <- unique(
      yr_data$poly_id[
        !is.na(yr_data$poly_id)
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
    
    
    # -----------------------------------------------------------------
    # Suitable habitat in active polygons
    # -----------------------------------------------------------------
    
    year_habitat_mask <- ifel(
      habitat_mask == 1 &
        !is.na(active_raster),
      1,
      NA
    )
    
    
    names(year_habitat_mask) <-
      "suitable"
    
    
    # -----------------------------------------------------------------
    # District ID only on suitable pixels
    # -----------------------------------------------------------------
    
    revier_suitable_year <- mask(
      revier_id_raster,
      year_habitat_mask
    )
    
    
    # =================================================================
    # 13.1 STANDARD DISTRICTS
    # =================================================================
    
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
        year_habitat_mask
      
      density_raster <-
        density_raster *
        NA_real_
    }
    
    
    names(density_raster) <-
      "harvest_density"
    
    
    # =================================================================
    # 13.2 FALLBACK HARVEST
    # =================================================================
    
    if (length(fallback_cells) > 0) {
      
      for (
        pid_character in names(fallback_cells)
      ) {
        
        pid <-
          as.integer(
            pid_character
          )
        
        
        # -------------------------------------------------------------
        # Harvest for fallback district in this year
        # -------------------------------------------------------------
        
        fallback_n <- yr_data %>%
          
          filter(
            poly_id ==
              pid
          ) %>%
          
          pull(n)
        
        
        if (length(fallback_n) == 0) {
          next
        }
        
        
        fallback_n <-
          fallback_n[1]
        
        
        if (
          is.na(fallback_n) ||
          fallback_n <= 0
        ) {
          next
        }
        
        
        # -------------------------------------------------------------
        # Retrieve stored target cells
        # -------------------------------------------------------------
        
        target_cells <- unlist(
          fallback_cells[
            pid_character
          ],
          use.names = FALSE
        )
        
        
        if (length(target_cells) == 0) {
          
          stop(
            "No fallback cells stored for poly_id ",
            pid
          )
        }
        
        
        # -------------------------------------------------------------
        # Only use cells that are active/suitable in this year
        # -------------------------------------------------------------
        
        target_status <- terra::extract(
          year_habitat_mask,
          target_cells,
          raw = TRUE
        )
        
        
        target_status <-
          as.numeric(
            target_status
          )
        
        
        target_cells <-
          target_cells[
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
        
        
        # -------------------------------------------------------------
        # Density increment
        #
        # n / fallback suitable area
        # -------------------------------------------------------------
        
        fallback_density_increment <-
          fallback_n /
          (
            length(target_cells) *
              cell_area_km2
          )
        
        
        # -------------------------------------------------------------
        # Existing values in fallback cells
        #
        # IMPORTANT:
        # terra::extract with numeric cell IDs + raw=TRUE
        # gives a numeric matrix.
        # -------------------------------------------------------------
        
        old_values <- terra::extract(
          density_raster,
          target_cells,
          raw = TRUE
        )
        
        
        old_values <-
          as.numeric(
            old_values
          )
        
        
        old_values[
          is.na(old_values)
        ] <- 0
        
        
        # -------------------------------------------------------------
        # Add fallback contribution
        # -------------------------------------------------------------
        
        density_raster[
          target_cells
        ] <-
          old_values +
          fallback_density_increment
        
        
        # -------------------------------------------------------------
        # Radius for diagnostics
        # -------------------------------------------------------------
        
        fallback_radius_now <-
          NA_real_
        
        
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
        
        
        # -------------------------------------------------------------
        # Log
        # -------------------------------------------------------------
        
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
    
    
    # =================================================================
    # 13.3 SPATIAL OCTAGONAL MOVING WINDOW
    # =================================================================
    
    spatial_raster <- spatial_smooth(
      density_raster =
        density_raster,
      valid_habitat_mask =
        year_habitat_mask,
      weights =
        window_weights
    )
    
    
    # =================================================================
    # 13.4 WRITE TEMPORARY SPATIAL RASTER
    # =================================================================
    
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
  
  half_window <-
    floor(
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
    
    
    # -----------------------------------------------------------------
    # Temporal neighbourhood
    # -----------------------------------------------------------------
    
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
    
    
    # -----------------------------------------------------------------
    # Spatial raster files
    # -----------------------------------------------------------------
    
    temporal_files <- file.path(
      spatial_tmp_dir,
      paste0(
        sp,
        "_",
        available_window_years,
        "_spatial_octagon.tif"
      )
    )
    
    
    files_exist <-
      file.exists(
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
    
    
    # -----------------------------------------------------------------
    # Temporal stack
    # -----------------------------------------------------------------
    
    temporal_stack <- rast(
      temporal_files
    )
    
    
    # -----------------------------------------------------------------
    # Temporal mean
    # -----------------------------------------------------------------
    
    temporally_smoothed <- mean(
      temporal_stack,
      na.rm = TRUE
    )
    
    
    # =================================================================
    # 15.1 TARGET-YEAR HABITAT MASK
    # =================================================================
    
    target_year_ids <- sp_hunt %>%
      
      filter(
        year ==
          yr
      ) %>%
      
      pull(poly_id) %>%
      
      unique()
    
    
    target_year_ids <-
      target_year_ids[
        !is.na(target_year_ids)
      ]
    
    
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
    
    
    # =================================================================
    # 16. RESTORE ANNUAL TOTAL HARVEST
    # =================================================================
    
    target_total <- sp_hunt %>%
      
      filter(
        year ==
          yr
      ) %>%
      
      summarise(
        total =
          sum(
            n,
            na.rm = TRUE
          )
      ) %>%
      
      pull(total)
    
    
    # -----------------------------------------------------------------
    # Zero-harvest year
    # -----------------------------------------------------------------
    
    if (
      is.na(target_total) ||
      target_total <= 0
    ) {
      
      final_raster <- ifel(
        !is.na(target_year_habitat),
        0,
        NA
      )
      
      
      correction_factor <-
        NA_real_
      
    } else {
      
      
      # ---------------------------------------------------------------
      # Current raster total
      # ---------------------------------------------------------------
      
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
      
      
      # ---------------------------------------------------------------
      # Annual conservation
      # ---------------------------------------------------------------
      
      correction_factor <-
        target_total /
        current_total
      
      
      final_raster <-
        temporally_smoothed *
        correction_factor
    }
    
    
    names(final_raster) <-
      "harvest_density"
    
    
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
    # 18. CONSERVATION CHECK
    # =================================================================
    
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
# 19. FINISHED
# =====================================================================

cat("\n")
cat("============================================================\n")
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
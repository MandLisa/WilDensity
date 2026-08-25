# =====================================================================
# CONTINUOUS HARVEST DENSITY RASTERS
# Hunting district x species x year -> continuous 100 m raster
#
# Workflow:
#   1. Create 100 m raster
#   2. Keep only species-specific suitable habitat
#   3. Assign hunting-district harvest density to suitable pixels
#   4. FALLBACK for districts with harvest but no suitable pixels:
#        search suitable pixels within:
#        500 m -> 1000 m -> 2000 m
#        and distribute harvest there
#   5. Spatial smoothing with OCTAGONAL moving window
#   6. Temporal smoothing (e.g. t-1, t, t+1)
#   7. Rescale to preserve annual total harvest
#
# Output units:
#   n / km² / year
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
# Binary species-specific suitability rasters
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
# Radius from centre to horizontal / vertical edge
#
# Examples:
#   500
#   1000
#   2000
#   5000
# ---------------------------------------------------------------------

spatial_radius_m <-
  2000


# ---------------------------------------------------------------------
# Temporal moving window
#
# 1 = t only
# 3 = t-1, t, t+1
# 5 = t-2, ..., t+2
# ---------------------------------------------------------------------

temporal_window_years <-
  3


# FALSE:
# first / last years use whatever neighbouring years are available
#
# TRUE:
# years without a complete temporal window are skipped
require_full_temporal_window <-
  FALSE


# =====================================================================
# FALLBACK SETTINGS
# =====================================================================

# For a district with:
#
#   harvest > 0
#   BUT
#   zero suitable pixels
#
# search suitable habitat progressively within these distances.

fallback_radii_m <-
  c(
    500,
    1000,
    2000
  )


# Keep fallback pixels in same province
fallback_same_province <-
  TRUE


# ---------------------------------------------------------------------
# Cell area
# ---------------------------------------------------------------------

cell_area_km2 <-
  (target_resolution_m^2) / 1e6


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
    year    = as.integer(year),
    n       = as.numeric(n),
    prov    = as.character(prov)
  ) %>%
  
  filter(
    species %in% names(suitability_files),
    year != 1992
  )


if (
  nrow(hunt_sf) == 0
) {
  
  stop(
    "No hunting data found."
  )
}


# =====================================================================
# 4. UNIQUE HUNTING-DISTRICT GEOMETRIES
# =====================================================================

cat("Preparing hunting-district geometries...\n")


reviere <- hunt_sf %>%
  
  arrange(
    poly_id
  ) %>%
  
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


if (
  anyDuplicated(
    reviere$poly_id
  ) > 0
) {
  
  stop(
    "poly_id is not unique."
  )
}


# =====================================================================
# 5. PROVINCE CODES
# =====================================================================

province_lookup <- reviere %>%
  
  st_drop_geometry() %>%
  
  distinct(
    prov
  ) %>%
  
  arrange(
    prov
  ) %>%
  
  mutate(
    prov_code =
      row_number()
  )


reviere <- reviere %>%
  
  left_join(
    province_lookup,
    by = "prov"
  )


# =====================================================================
# 6. ATTRIBUTE TABLE
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
    year
  ) %>%
  
  filter(
    n > 1
  )


if (
  nrow(duplicates) > 0
) {
  
  print(
    duplicates
  )
  
  stop(
    "Duplicate poly_id x species x year combinations."
  )
}


# =====================================================================
# 7. CREATE 100 M TEMPLATE
# =====================================================================

cat("Creating 100 m raster template...\n")


rev_v <-
  vect(
    reviere
  )


e <-
  ext(
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
  
  xmin =
    xmin_template,
  
  xmax =
    xmax_template,
  
  ymin =
    ymin_template,
  
  ymax =
    ymax_template,
  
  resolution =
    target_resolution_m,
  
  crs =
    target_crs
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


names(
  revier_id_raster
) <- "poly_id"


province_raster <- rasterize(
  rev_v,
  template,
  field = "prov_code"
)


names(
  province_raster
) <- "prov_code"


# =====================================================================
# 9. OCTAGONAL MOVING WINDOW
# =====================================================================

make_octagon_kernel <- function(
    radius_m,
    resolution_m
) {
  
  r <-
    ceiling(
      radius_m /
        resolution_m
    )
  
  
  xy <-
    -r:r
  
  
  xx <-
    matrix(
      rep(
        xy,
        each = length(xy)
      ),
      nrow = length(xy)
    )
  
  
  yy <-
    t(xx)
  
  
  # Regular octagon:
  #
  # square constraints:
  #   |x| <= r
  #   |y| <= r
  #
  # diagonal constraints:
  #   |x| + |y| <= sqrt(2) * r
  
  inside <-
    
    abs(xx) <= r &
    
    abs(yy) <= r &
    
    (
      abs(xx) +
        abs(yy)
    ) <=
    sqrt(2) * r
  
  
  weights <-
    matrix(
      0,
      nrow = length(xy),
      ncol = length(xy)
    )
  
  
  weights[
    inside
  ] <- 1
  
  
  weights
}


window_weights <- make_octagon_kernel(
  
  radius_m =
    spatial_radius_m,
  
  resolution_m =
    target_resolution_m
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
  "  included pixels:",
  sum(window_weights),
  "\n"
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
  # Sum of density values in neighbourhood
  # -------------------------------------------------------------------
  
  numerator <- focal(
    
    density_raster,
    
    w =
      weights,
    
    fun =
      "sum",
    
    na.rm =
      TRUE
  )
  
  
  # -------------------------------------------------------------------
  # Number of suitable + valid pixels represented in neighbourhood
  # -------------------------------------------------------------------
  
  denominator <- focal(
    
    valid_habitat_mask,
    
    w =
      weights,
    
    fun =
      "sum",
    
    na.rm =
      TRUE
  )
  
  
  # -------------------------------------------------------------------
  # Moving-window mean
  # -------------------------------------------------------------------
  
  out <-
    numerator /
    denominator
  
  
  # Output only on suitable habitat
  out <- mask(
    out,
    valid_habitat_mask
  )
  
  
  out[
    denominator <= 0
  ] <- NA
  
  
  out
}


# =====================================================================
# 11. FALLBACK FUNCTION
# =====================================================================
#
# For a district without suitable habitat:
#
#   1. buffer polygon by 500 m
#   2. search suitable pixels
#   3. if none -> 1000 m
#   4. if none -> 2000 m
#
# As soon as suitable pixels are found:
#   stop search and return those pixels.
#
# If fallback_same_province = TRUE:
#   only suitable pixels in the same province are allowed.
# =====================================================================

find_fallback_cells <- function(
    poly_id_target,
    prov_target,
    habitat_mask,
    province_raster,
    rev_v,
    province_lookup,
    search_radii
) {
  
  
  target_polygon <-
    rev_v[
      rev_v$poly_id ==
        poly_id_target,
    ]
  
  
  if (
    nrow(target_polygon) != 1
  ) {
    
    stop(
      "Could not uniquely identify polygon ",
      poly_id_target
    )
  }
  
  
  target_prov_code <-
    province_lookup %>%
    
    filter(
      prov ==
        prov_target
    ) %>%
    
    pull(
      prov_code
    )
  
  
  for (
    radius_now in search_radii
  ) {
    
    
    buffer_polygon <- buffer(
      target_polygon,
      width = radius_now
    )
    
    
    extracted <- extract(
      
      c(
        habitat_mask,
        province_raster
      ),
      
      buffer_polygon,
      
      cells =
        TRUE
    ) %>%
      
      as.data.frame()
    
    
    if (
      nrow(extracted) == 0
    ) {
      
      next
    }
    
    
    # ---------------------------------------------------------------
    # Suitable pixels
    # ---------------------------------------------------------------
    
    extracted <- extracted %>%
      
      filter(
        suitable == 1,
        !is.na(cell)
      )
    
    
    # ---------------------------------------------------------------
    # Optional province restriction
    # ---------------------------------------------------------------
    
    if (
      fallback_same_province
    ) {
      
      extracted <- extracted %>%
        
        filter(
          prov_code ==
            target_prov_code
        )
    }
    
    
    target_cells <- fallback_cells[[pid_character]]
    
    
    if (
      length(target_cells) > 0
    ) {
      
      return(
        list(
          radius_m =
            radius_now,
          
          cells =
            target_cells
        )
      )
    }
  }
  
  
  # No suitable cells found
  NULL
}


# =====================================================================
# 12. PROCESS EACH SPECIES
# =====================================================================

for (
  sp in names(
    suitability_files
  )
) {
  
  
  cat("\n")
  cat("============================================================\n")
  cat("SPECIES:", sp, "\n")
  cat("============================================================\n")
  
  
  # ===================================================================
  # 12.1 DIRECTORIES
  # ===================================================================
  
  species_dir <-
    file.path(
      output_dir,
      sp
    )
  
  
  spatial_tmp_dir <-
    file.path(
      species_dir,
      "_spatial_tmp"
    )
  
  
  final_dir <-
    file.path(
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
  
  
  suitability_raw <-
    rast(
      suitability_files[[sp]]
    )
  
  
  # Binary raster -> nearest-neighbour interpolation
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
  
  
  names(
    habitat_mask
  ) <- "suitable"
  
  
  # ===================================================================
  # 12.4 COUNT SUITABLE PIXELS PER DISTRICT
  # ===================================================================
  
  suitable_counts <- zonal(
    
    habitat_mask,
    
    revier_id_raster,
    
    fun =
      "sum",
    
    na.rm =
      TRUE
  ) %>%
    
    as.data.frame()
  
  
  names(
    suitable_counts
  )[1:2] <-
    c(
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
        max(
          n,
          na.rm = TRUE
        ),
      
      .groups =
        "drop"
    ) %>%
    
    filter(
      suitable_area_km2 <= 0,
      max_harvest > 0
    )
  
  
  cat(
    "Fallback districts:",
    nrow(fallback_districts),
    "\n"
  )
  
  
  # ===================================================================
  # 12.7 FIND FALLBACK PIXELS
  # ===================================================================
  
  fallback_cells <-
    list()
  
  
  fallback_diagnostics <-
    list()
  
  
  if (
    nrow(fallback_districts) > 0
  ) {
    
    
    for (
      i in seq_len(
        nrow(fallback_districts)
      )
    ) {
      
      
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
        
        poly_id_target =
          pid,
        
        prov_target =
          pprov,
        
        habitat_mask =
          habitat_mask,
        
        province_raster =
          province_raster,
        
        rev_v =
          rev_v,
        
        province_lookup =
          province_lookup,
        
        search_radii =
          fallback_radii_m
      )
      
      
      # ---------------------------------------------------------------
      # No suitable habitat found
      # ---------------------------------------------------------------
      
      if (
        is.null(result)
      ) {
        
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
      # Store cell IDs
      # ---------------------------------------------------------------
      
      fallback_cells[[as.character(pid)]] <- result$cells
      
      
      # ---------------------------------------------------------------
      # Store diagnostics
      # ---------------------------------------------------------------
      
      fallback_diagnostics[[length(fallback_diagnostics) + 1]] <- data.frame(
        
        species = sp,
        poly_id = pid,
        prov = pprov,
        fallback_radius_m = result$radius_m,
        n_target_pixels = length(result$cells),
        target_area_km2 = length(result$cells) * cell_area_km2
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
  
  
  # -------------------------------------------------------------------
  # Save fallback diagnostic table
  # -------------------------------------------------------------------
  
  if (
    length(
      fallback_diagnostics
    ) > 0
  ) {
    
    fallback_diagnostics_df <-
      bind_rows(
        fallback_diagnostics
      )
    
    
    write.csv(
      
      fallback_diagnostics_df,
      
      file.path(
        species_dir,
        "fallback_districts.csv"
      ),
      
      row.names =
        FALSE
    )
  }
  
  
  # ===================================================================
  # 12.8 AVAILABLE YEARS
  # ===================================================================
  
  years <-
    sp_hunt %>%
    
    pull(
      year
    ) %>%
    
    unique() %>%
    
    sort()
  
  
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
  
  
  for (
    yr in years
  ) {
    
    
    cat(
      "\nSpatial smoothing: ",
      sp,
      " ",
      yr,
      "\n",
      sep = ""
    )
    
    
    # ---------------------------------------------------------------
    # Data actually available in this year
    #
    # Important because Salzburg starts later than Styria.
    # ---------------------------------------------------------------
    
    yr_data <- sp_hunt %>%
      
      filter(
        year ==
          yr
      )
    
    
    active_ids <-
      unique(
        yr_data$poly_id
      )
    
    
    # ---------------------------------------------------------------
    # Raster indicating polygons with data in current year
    # ---------------------------------------------------------------
    
    active_raster <- subst(
      
      revier_id_raster,
      
      from =
        active_ids,
      
      to =
        rep(
          1,
          length(active_ids)
        ),
      
      others =
        NA
    )
    
    
    # ---------------------------------------------------------------
    # Species habitat available in target year
    # ---------------------------------------------------------------
    
    year_habitat_mask <- ifel(
      
      habitat_mask == 1 &
        !is.na(active_raster),
      
      1,
      
      NA
    )
    
    
    names(
      year_habitat_mask
    ) <- "suitable"
    
    
    # ---------------------------------------------------------------
    # Revier IDs on suitable habitat
    # ---------------------------------------------------------------
    
    revier_suitable_year <- mask(
      
      revier_id_raster,
      
      year_habitat_mask
    )
    
    
    # ---------------------------------------------------------------
    # Standard districts:
    # density = harvest / suitable habitat area
    # ---------------------------------------------------------------
    
    regular_yr_data <- yr_data %>%
      
      filter(
        suitable_area_km2 > 0
      ) %>%
      
      mutate(
        
        district_density =
          n /
          suitable_area_km2
      )
    
    
    # ---------------------------------------------------------------
    # Rasterize density using lookup
    #
    # others = NA is important:
    # polygons without values in this year must NOT retain poly_id.
    # ---------------------------------------------------------------
    
    density_raster <- subst(
      
      revier_suitable_year,
      
      from =
        regular_yr_data$poly_id,
      
      to =
        regular_yr_data$district_density,
      
      others =
        NA
    )
    
    
    names(
      density_raster
    ) <- "harvest_density"
    
    
    # =================================================================
    # 13.1 ADD FALLBACK HARVEST
    # =================================================================
    
    if (
      length(
        fallback_cells
      ) > 0
    ) {
      
      
      for (
        pid_character in names(
          fallback_cells
        )
      ) {
        
        
        pid <-
          as.integer(
            pid_character
          )
        
        
        fallback_n <- yr_data %>%
          
          filter(
            poly_id ==
              pid
          ) %>%
          
          pull(
            n
          )
        
        
        # No data for this polygon/year
        if (
          length(fallback_n) == 0
        ) {
          
          next
        }
        
        
        fallback_n <-
          fallback_n[1]
        
        
        # Nothing to distribute
        if (
          is.na(fallback_n) ||
          fallback_n <= 0
        ) {
          
          next
        }
        
        
        target_cells <-
          fallback_cells[
            [pid_character]
          ]
        
        
        # -------------------------------------------------------------
        # Density increment:
        #
        # n / total area of fallback pixels
        #
        # This ensures that:
        #
        # sum(
        #   increment *
        #   cell_area
        # )
        #
        # == fallback_n
        # -------------------------------------------------------------
        
        fallback_density_increment <-
          fallback_n /
          (
            length(target_cells) *
              cell_area_km2
          )
        
        
        # -------------------------------------------------------------
        # Add fallback contribution to existing density
        # -------------------------------------------------------------
        
        old_values <-
          density_raster[
            target_cells
          ]
        
        
        old_values <-
          as.numeric(
            old_values
          )
        
        
        old_values[
          is.na(old_values)
        ] <- 0
        
        
        density_raster[
          target_cells
        ] <-
          old_values +
          fallback_density_increment
        
        
        # -------------------------------------------------------------
        # Log
        # -------------------------------------------------------------
        
        fallback_info <-
          if (
            exists(
              "fallback_diagnostics_df"
            )
          ) {
            
            fallback_diagnostics_df %>%
              
              filter(
                poly_id ==
                  pid
              )
            
          } else {
            
            NULL
          }
        
        
        fallback_year_log[
          [length(fallback_year_log) + 1]
        ] <-
          data.frame(
            
            species =
              sp,
            
            year =
              yr,
            
            poly_id =
              pid,
            
            harvest_n =
              fallback_n,
            
            n_target_pixels =
              length(target_cells),
            
            fallback_density_increment =
              fallback_density_increment,
            
            fallback_radius_m =
              if (
                !is.null(fallback_info) &&
                nrow(fallback_info) > 0
              ) {
                
                fallback_info$fallback_radius_m[1]
                
              } else {
                
                NA_real_
              }
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
    # 13.2 SPATIAL OCTAGONAL MOVING WINDOW
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
    # 13.3 SAVE TEMPORARY SPATIAL RASTER
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
      
      overwrite =
        TRUE,
      
      datatype =
        "FLT4S",
      
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
  
  if (
    length(
      fallback_year_log
    ) > 0
  ) {
    
    write.csv(
      
      bind_rows(
        fallback_year_log
      ),
      
      file.path(
        species_dir,
        "fallback_assignments_by_year.csv"
      ),
      
      row.names =
        FALSE
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
  
  
  for (
    yr in years
  ) {
    
    
    cat(
      "\nTemporal smoothing: ",
      sp,
      " ",
      yr,
      "\n",
      sep = ""
    )
    
    
    # ---------------------------------------------------------------
    # Temporal neighbourhood
    # ---------------------------------------------------------------
    
    desired_years <- seq(
      
      yr -
        half_window,
      
      yr +
        half_window
    )
    
    
    available_window_years <-
      desired_years[
        desired_years %in%
          years
      ]
    
    
    if (
      require_full_temporal_window &&
      length(
        available_window_years
      ) <
      temporal_window_years
    ) {
      
      cat(
        "  skipped: incomplete temporal window\n"
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
    
    
    # ---------------------------------------------------------------
    # Load spatial rasters
    # ---------------------------------------------------------------
    
    temporal_files <- file.path(
      
      spatial_tmp_dir,
      
      paste0(
        sp,
        "_",
        available_window_years,
        "_spatial_octagon.tif"
      )
    )
    
    
    temporal_stack <-
      rast(
        temporal_files
      )
    
    
    # ---------------------------------------------------------------
    # Temporal mean
    # ---------------------------------------------------------------
    
    temporally_smoothed <- mean(
      
      temporal_stack,
      
      na.rm =
        TRUE
    )
    
    
    # =================================================================
    # 15.1 TARGET-YEAR HABITAT MASK
    # =================================================================
    
    target_year_ids <- sp_hunt %>%
      
      filter(
        year ==
          yr
      ) %>%
      
      pull(
        poly_id
      ) %>%
      
      unique()
    
    
    target_active_raster <- subst(
      
      revier_id_raster,
      
      from =
        target_year_ids,
      
      to =
        rep(
          1,
          length(target_year_ids)
        ),
      
      others =
        NA
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
    # 16. RESTORE TOTAL HARVEST OF TARGET YEAR
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
      
      pull(
        total
      )
    
    
    current_total <- global(
      
      temporally_smoothed *
        cell_area_km2,
      
      fun =
        "sum",
      
      na.rm =
        TRUE
    )[1, 1]
    
    
    if (
      !is.finite(current_total) ||
      current_total <= 0
    ) {
      
      stop(
        "Invalid raster total for ",
        sp,
        " ",
        yr
      )
    }
    
    
    # -----------------------------------------------------------------
    # Annual conservation
    # -----------------------------------------------------------------
    
    correction_factor <-
      target_total /
      current_total
    
    
    final_raster <-
      temporally_smoothed *
      correction_factor
    
    
    names(
      final_raster
    ) <- "harvest_density"
    
    
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
      
      overwrite =
        TRUE,
      
      datatype =
        "FLT4S",
      
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
      
      final_raster *
        cell_area_km2,
      
      fun =
        "sum",
      
      na.rm =
        TRUE
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
      round(
        correction_factor,
        4
      ),
      "\n",
      sep = ""
    )
  }
  
  
  cat("\n")
  cat(
    "Finished: ",
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
cat("Resolution:             ", target_resolution_m, " m\n")
cat("Spatial window:         octagon\n")
cat("Spatial radius:         ", spatial_radius_m, " m\n")
cat("Temporal window:        ", temporal_window_years, " years\n")
cat("Suitable habitat only:  yes\n")
cat(
  "Fallback radii:        ",
  paste(
    fallback_radii_m,
    collapse = ", "
  ),
  " m\n",
  sep = ""
)
cat(
  "Fallback same province:",
  fallback_same_province,
  "\n"
)
cat("Output:                 ", output_dir, "\n")
cat("============================================================\n")
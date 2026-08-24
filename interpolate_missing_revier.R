# =====================================================================
# STEP 6 — INTERPOLATE COMPLETELY MISSING HUNT SITES
# =====================================================================
#
# Goal:
#   Fill current hunt-site polygons that are completely missing from
#   the final time series.
#
# Method:
#   For each missing polygon x species x year:
#
#     1) identify directly adjacent hunt-site polygons (st_touches)
#     2) select the 3 closest adjacent polygons with valid harvest data
#     3) calculate harvest density of these neighbours [n / km²]
#     4) take the mean neighbour density
#     5) multiply by target polygon area
#     6) round to integer harvest n
#
# Existing harvest values are NEVER changed.
# 1992 is NOT interpolated.
#
# Output:
#   final_revier_timeseries_complete_interpolated.gpkg
# =====================================================================


# =====================================================================
# 0. PACKAGES
# =====================================================================

library(sf)
library(dplyr)
library(tidyr)


# =====================================================================
# 1. PATHS
# =====================================================================

# Final time series
input_timeseries <-
  "/mnt/eo/WilDensity/output/final_integer_allocation/final_revier_timeseries_complete.gpkg"

input_layer <-
  "revier_timeseries"


# Complete current polygon geometry
input_reviere <-
  "/mnt/eo/WilDensity/output/allocation_groups_final/reviere_with_allocation_group.gpkg"


# Output
output_dir <-
  "/mnt/eo/WilDensity/output/final_integer_allocation/interpolated_missing_reviere"

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)


output_gpkg <-
  file.path(
    output_dir,
    "final_revier_timeseries_complete_interpolated.gpkg"
  )


# =====================================================================
# 2. SETTINGS
# =====================================================================

species_use <- c(
  "roe_deer",
  "red_deer",
  "chamois"
)


# 1992 deliberately excluded
year_ranges <- list(
  sbg    = 1998:2024,
  styria = 1993:2024
)


# Equal-area CRS
area_crs <- 3035


# Number of adjacent polygons used
n_neighbors <- 3


# =====================================================================
# 3. READ DATA
# =====================================================================

final_sf <- st_read(
  input_timeseries,
  layer = input_layer,
  quiet = TRUE
) %>%
  
  mutate(
    poly_id = as.integer(poly_id),
    species = as.character(species),
    year    = as.integer(year),
    n       = as.numeric(n),
    prov    = as.character(prov)
  )


all_reviere <- st_read(
  input_reviere,
  quiet = TRUE
) %>%
  
  mutate(
    poly_id = as.integer(poly_id),
    prov    = as.character(prov)
  )


# =====================================================================
# 4. BASIC CHECKS
# =====================================================================

if (anyDuplicated(all_reviere$poly_id) > 0) {
  stop(
    "poly_id is not unique in the complete Revier geometry."
  )
}


if (any(!st_is_valid(all_reviere))) {
  
  all_reviere <-
    st_make_valid(
      all_reviere
    )
}


# Use same CRS as polygon source
final_sf <-
  st_transform(
    final_sf,
    st_crs(all_reviere)
  )


# =====================================================================
# 5. FIND COMPLETELY MISSING REVIERE
# =====================================================================

present_poly_ids <- final_sf %>%
  
  st_drop_geometry() %>%
  
  distinct(
    poly_id
  )


missing_reviere <- all_reviere %>%
  
  anti_join(
    present_poly_ids,
    by = "poly_id"
  )


cat(
  "\nCompletely missing Reviere:",
  nrow(missing_reviere),
  "\n"
)


if (nrow(missing_reviere) == 0) {
  
  stop(
    "No completely missing Reviere were found."
  )
}


cat("\nMissing polygons:\n")

print(
  missing_reviere %>%
    st_drop_geometry() %>%
    select(
      any_of(
        c(
          "poly_id",
          "hunt_site",
          "name",
          "prov",
          "region_current"
        )
      )
    ),
  n = Inf
)


# Save diagnostic
write.csv(
  
  missing_reviere %>%
    st_drop_geometry(),
  
  file.path(
    output_dir,
    "missing_reviere_before_interpolation.csv"
  ),
  
  row.names = FALSE
)


# =====================================================================
# 6. AREA OF ALL REVIERE
# =====================================================================

reviere_3035 <- all_reviere %>%
  
  st_transform(
    area_crs
  )


area_lookup <- reviere_3035 %>%
  
  mutate(
    area_km2 =
      as.numeric(
        st_area(geom)
      ) / 1e6
  ) %>%
  
  st_drop_geometry() %>%
  
  select(
    poly_id,
    area_km2
  )


if (
  any(
    is.na(area_lookup$area_km2) |
    area_lookup$area_km2 <= 0
  )
) {
  
  stop(
    "At least one polygon has an invalid area."
  )
}


# =====================================================================
# 7. PREPARE EXISTING HARVEST DENSITIES
# =====================================================================

final_tbl <- final_sf %>%
  st_drop_geometry()


source_data <- final_tbl %>%
  
  filter(
    species %in% species_use,
    year != 1992,
    !is.na(n)
  ) %>%
  
  left_join(
    area_lookup,
    by = "poly_id"
  ) %>%
  
  mutate(
    harvest_density =
      n / area_km2
  )


# =====================================================================
# 8. IDENTIFY ADJACENT REVIERE
# =====================================================================

# Topological neighbours:
# polygons must directly touch each other.

touch_list <-
  st_touches(
    reviere_3035
  )


# Use points inside polygons for ranking neighbours by distance.
# Distance is only used if a polygon has MORE than 3 adjacent neighbours.

revier_points <-
  suppressWarnings(
    st_point_on_surface(
      reviere_3035
    )
  )


missing_ids <-
  missing_reviere$poly_id


neighbor_lookup <- bind_rows(
  
  lapply(
    missing_ids,
    
    function(target_id) {
      
      target_index <-
        match(
          target_id,
          reviere_3035$poly_id
        )
      
      
      neighbor_indices <-
        touch_list[[target_index]]
      
      
      if (length(neighbor_indices) == 0) {
        
        return(
          tibble(
            target_poly_id   = integer(),
            neighbor_poly_id = integer(),
            distance_m       = numeric()
          )
        )
      }
      
      
      distances <-
        as.numeric(
          st_distance(
            revier_points[target_index, ],
            revier_points[neighbor_indices, ]
          )
        )
      
      
      tibble(
        target_poly_id =
          target_id,
        
        neighbor_poly_id =
          reviere_3035$poly_id[
            neighbor_indices
          ],
        
        distance_m =
          distances
      )
    }
  )
)


# Attach provinces and only retain neighbours from same province
province_lookup <- all_reviere %>%
  
  st_drop_geometry() %>%
  
  select(
    poly_id,
    prov
  )


neighbor_lookup <- neighbor_lookup %>%
  
  left_join(
    province_lookup %>%
      rename(
        target_poly_id = poly_id,
        target_prov = prov
      ),
    by = "target_poly_id"
  ) %>%
  
  left_join(
    province_lookup %>%
      rename(
        neighbor_poly_id = poly_id,
        neighbor_prov = prov
      ),
    by = "neighbor_poly_id"
  ) %>%
  
  filter(
    target_prov == neighbor_prov
  )


# =====================================================================
# 9. SHOW NUMBER OF ADJACENT REVIERE
# =====================================================================

neighbor_count <- neighbor_lookup %>%
  
  count(
    target_poly_id,
    name = "n_adjacent"
  )


cat("\nAdjacent polygons per missing Revier:\n")

print(
  neighbor_count,
  n = Inf
)


# Hard check:
# each missing polygon must have at least 3 adjacent polygons

bad_neighbor_count <- missing_ids[
  
  !missing_ids %in%
    neighbor_count$target_poly_id[
      neighbor_count$n_adjacent >= n_neighbors
    ]
]


if (length(bad_neighbor_count) > 0) {
  
  stop(
    "At least one missing Revier has fewer than 3 adjacent polygons. ",
    "Affected poly_id(s): ",
    paste(
      bad_neighbor_count,
      collapse = ", "
    )
  )
}


# =====================================================================
# 10. BUILD REQUIRED TARGET ROWS
# =====================================================================

missing_attributes <- missing_reviere %>%
  
  st_drop_geometry()


target_rows <- bind_rows(
  
  lapply(
    seq_len(
      nrow(missing_attributes)
    ),
    
    function(i) {
      
      this_polygon <-
        missing_attributes[
          i,
          ,
          drop = FALSE
        ]
      
      
      pv <-
        this_polygon$prov[[1]]
      
      
      if (
        !pv %in%
        names(year_ranges)
      ) {
        
        stop(
          "Unknown province for poly_id ",
          this_polygon$poly_id[[1]],
          ": ",
          pv
        )
      }
      
      
      tidyr::crossing(
        this_polygon,
        species = species_use,
        year = year_ranges[[pv]]
      )
    }
  )
)


cat(
  "\nRows to interpolate:",
  nrow(target_rows),
  "\n"
)


# =====================================================================
# 11. INTERPOLATE ONE POLYGON x SPECIES x YEAR
# =====================================================================

interpolate_one <- function(
    target_id,
    species_name,
    target_year
) {
  
  
  # ---------------------------------------------------------------
  # Candidate adjacent polygons
  # ---------------------------------------------------------------
  
  candidates <- neighbor_lookup %>%
    
    filter(
      target_poly_id == target_id
    ) %>%
    
    inner_join(
      
      source_data %>%
        
        filter(
          species == species_name,
          year == target_year
        ) %>%
        
        select(
          neighbor_poly_id = poly_id,
          neighbor_n = n,
          neighbor_density = harvest_density
        ),
      
      by = "neighbor_poly_id"
    ) %>%
    
    arrange(
      distance_m
    )
  
  
  # ---------------------------------------------------------------
  # Require exactly three valid adjacent source polygons
  # ---------------------------------------------------------------
  
  if (nrow(candidates) < n_neighbors) {
    
    return(
      tibble(
        poly_id = target_id,
        species = species_name,
        year = target_year,
        success = FALSE,
        n_neighbors_used = nrow(candidates),
        estimated_density = NA_real_,
        estimated_n = NA_integer_,
        neighbor_ids = paste(
          candidates$neighbor_poly_id,
          collapse = ";"
        )
      )
    )
  }
  
  
  # Three nearest DIRECT neighbours with valid data
  candidates <-
    candidates %>%
    
    slice_head(
      n = n_neighbors
    )
  
  
  # ---------------------------------------------------------------
  # Mean density of the three neighbouring polygons
  # ---------------------------------------------------------------
  
  estimated_density <-
    mean(
      candidates$neighbor_density,
      na.rm = TRUE
    )
  
  
  # Target polygon area
  target_area <-
    area_lookup$area_km2[
      match(
        target_id,
        area_lookup$poly_id
      )
    ]
  
  
  # Convert density back to harvest count.
  #
  # floor(x + 0.5) gives conventional rounding to nearest integer.
  
  estimated_n <-
    as.integer(
      floor(
        estimated_density *
          target_area +
          0.5
      )
    )
  
  
  tibble(
    poly_id =
      target_id,
    
    species =
      species_name,
    
    year =
      target_year,
    
    success =
      TRUE,
    
    n_neighbors_used =
      n_neighbors,
    
    estimated_density =
      estimated_density,
    
    estimated_n =
      estimated_n,
    
    neighbor_ids =
      paste(
        candidates$neighbor_poly_id,
        collapse = ";"
      )
  )
}


# =====================================================================
# 12. RUN INTERPOLATION
# =====================================================================

interpolation_results <- bind_rows(
  
  lapply(
    seq_len(
      nrow(target_rows)
    ),
    
    function(i) {
      
      interpolate_one(
        target_id =
          target_rows$poly_id[[i]],
        
        species_name =
          target_rows$species[[i]],
        
        target_year =
          target_rows$year[[i]]
      )
    }
  )
)


# =====================================================================
# 13. CHECK INTERPOLATION
# =====================================================================

failed <- interpolation_results %>%
  
  filter(
    !success
  )


write.csv(
  interpolation_results,
  file.path(
    output_dir,
    "interpolation_diagnostics.csv"
  ),
  row.names = FALSE
)


if (nrow(failed) > 0) {
  
  print(
    failed,
    n = Inf
  )
  
  stop(
    "Interpolation failed for some polygon x species x year rows ",
    "because fewer than 3 adjacent polygons had valid data. ",
    "Inspect interpolation_diagnostics.csv."
  )
}


# =====================================================================
# 14. CREATE IMPUTED ROWS
# =====================================================================

imputed_rows <- target_rows %>%
  
  left_join(
    interpolation_results %>%
      select(
        poly_id,
        species,
        year,
        estimated_density,
        estimated_n,
        neighbor_ids
      ),
    
    by = c(
      "poly_id",
      "species",
      "year"
    )
  ) %>%
  
  left_join(
    area_lookup,
    by = "poly_id"
  ) %>%
  
  mutate(
    
    n =
      estimated_n,
    
    n_source =
      "interpolated_3_adjacent_reviere",
    
    was_interpolated =
      TRUE,
    
    interpolation_method =
      "mean_density_3_adjacent_reviere",
    
    interpolation_neighbors =
      neighbor_ids
  )


# =====================================================================
# 15. PREPARE ORIGINAL ROWS
# =====================================================================

original_rows <- final_tbl %>%
  
  mutate(
    was_interpolated =
      FALSE,
    
    interpolation_method =
      NA_character_,
    
    interpolation_neighbors =
      NA_character_
  )


# =====================================================================
# 16. COMBINE ORIGINAL + INTERPOLATED
# =====================================================================

# bind_rows deliberately keeps all original columns.
# Fields not relevant to the interpolated rows, e.g. suitability_area_ha,
# remain NA for these three polygons.

combined_tbl <- bind_rows(
  original_rows,
  imputed_rows
) %>%
  
  arrange(
    prov,
    poly_id,
    year,
    species
  )


# =====================================================================
# 17. HARD CHECK — ORIGINAL VALUES UNCHANGED
# =====================================================================

original_check <- combined_tbl %>%
  
  filter(
    !was_interpolated
  ) %>%
  
  select(
    poly_id,
    species,
    year,
    n
  ) %>%
  
  inner_join(
    
    final_tbl %>%
      select(
        poly_id,
        species,
        year,
        n_original = n
      ),
    
    by = c(
      "poly_id",
      "species",
      "year"
    )
  ) %>%
  
  filter(
    n != n_original |
      xor(
        is.na(n),
        is.na(n_original)
      )
  )


if (nrow(original_check) > 0) {
  
  stop(
    "At least one original harvest value was modified."
  )
}


# =====================================================================
# 18. ATTACH GEOMETRY
# =====================================================================

geometry_lookup <- all_reviere %>%
  
  select(
    poly_id
  )


combined_sf <- geometry_lookup %>%
  
  inner_join(
    combined_tbl,
    by = "poly_id",
    relationship = "one-to-many"
  )


# =====================================================================
# 19. SAVE OUTPUT
# =====================================================================

st_write(
  combined_sf,
  output_gpkg,
  layer = "revier_timeseries",
  delete_dsn = TRUE,
  quiet = TRUE
)


write.csv(
  combined_tbl,
  file.path(
    output_dir,
    "final_revier_timeseries_complete_interpolated.csv"
  ),
  row.names = FALSE
)


# Only interpolated values
write.csv(
  
  combined_tbl %>%
    filter(
      was_interpolated
    ),
  
  file.path(
    output_dir,
    "interpolated_values_only.csv"
  ),
  
  row.names = FALSE
)


# =====================================================================
# 20. SUMMARY
# =====================================================================

cat("\n")
cat("============================================================\n")
cat("INTERPOLATION COMPLETE\n")
cat("============================================================\n")

cat(
  "Completely missing Reviere: ",
  nrow(missing_reviere),
  "\n",
  sep = ""
)

cat(
  "Interpolated species x year rows: ",
  sum(combined_tbl$was_interpolated),
  "\n",
  sep = ""
)

cat(
  "Failed interpolations: ",
  nrow(failed),
  "\n",
  sep = ""
)

cat("\nOutput:\n")
cat(output_gpkg, "\n")

cat("\nMethod:\n")
cat(
  "mean harvest density of 3 directly adjacent Reviere ",
  "-> multiplied by target Revier area -> integer n\n",
  sep = ""
)

cat("1992 was not interpolated.\n")

cat("============================================================\n")

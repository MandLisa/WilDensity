# =====================================================================
# STEP 5 — HARVEST DENSITY MAPS
# =====================================================================
#
# Creates:
#   1) yearly harvest density maps per species
#      -> harvest n / hunt-site area [km²]
#
#   2) mean harvest density maps per 5-year period per species
#      -> mean annual harvest density within each period
#
# 1992 is excluded from all analyses.
#
# Input:
#   final_integer_allocation/final_revier_timeseries_complete.gpkg
#
# Output:
#   final_integer_allocation/maps_species_density
# =====================================================================


# =====================================================================
# 0. PACKAGES
# =====================================================================

library(sf)
library(dplyr)
library(ggplot2)
library(scales)


# =====================================================================
# 1. PATHS
# =====================================================================

input_gpkg <-
  "/mnt/eo/WilDensity/output/final_integer_allocation/final_revier_timeseries_complete.gpkg"

input_layer <-
  "revier_timeseries"

output_dir <-
  "/mnt/eo/WilDensity/output/final_integer_allocation/maps_species_density"

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)


# =====================================================================
# 2. SETTINGS
# =====================================================================

species_order <- c(
  "roe_deer",
  "red_deer",
  "chamois"
)

species_labels <- c(
  roe_deer = "Roe deer",
  red_deer = "Red deer",
  chamois  = "Chamois"
)

# Equal-area CRS for area calculation
area_crs <- 3035


# =====================================================================
# 3. READ DATA
# =====================================================================

x <- st_read(
  input_gpkg,
  layer = input_layer,
  quiet = TRUE
) %>%
  
  mutate(
    species = as.character(species),
    year    = as.integer(year),
    n       = as.numeric(n),
    prov    = as.character(prov)
  ) %>%
  
  filter(
    species %in% species_order,
    year != 1992,
    
    (prov == "sbg" &
       year >= 1998 &
       year <= 2024) |
      
      (prov == "styria" &
         year >= 1993 &
         year <= 2024)
  )


if (nrow(x) == 0) {
  stop("No rows found after filtering.")
}


# =====================================================================
# 4. UNIQUE REVIER GEOMETRIES
# =====================================================================

# Geometry occurs repeatedly because the GPKG is long:
# one row per polygon x species x year.
# Therefore keep each polygon only once for the area calculation.

reviere_geom <- x %>%
  
  arrange(poly_id) %>%
  
  filter(
    !duplicated(poly_id)
  ) %>%
  
  select(
    poly_id,
    prov
  )


# =====================================================================
# 5. CALCULATE REVIER AREA [km²]
# =====================================================================

area_lookup <- reviere_geom %>%
  
  st_transform(
    area_crs
  ) %>%
  
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


# Check areas
invalid_area <- area_lookup %>%
  
  filter(
    is.na(area_km2) |
      area_km2 <= 0
  )


if (nrow(invalid_area) > 0) {
  stop(
    "At least one hunt-site polygon has missing or invalid area."
  )
}


# =====================================================================
# 6. CALCULATE ANNUAL HARVEST DENSITY
# =====================================================================

annual_data <- x %>%
  
  st_drop_geometry() %>%
  
  select(
    poly_id,
    hunt_site,
    name,
    prov,
    species,
    year,
    n
  ) %>%
  
  left_join(
    area_lookup,
    by = "poly_id"
  ) %>%
  
  mutate(
    harvest_density =
      n / area_km2
  )


# Check uniqueness
duplicate_check <- annual_data %>%
  
  count(
    poly_id,
    species,
    year,
    name = "n_rows"
  ) %>%
  
  filter(
    n_rows > 1
  )


if (nrow(duplicate_check) > 0) {
  stop(
    "Duplicate poly_id x species x year combinations found."
  )
}


# Attach geometry
annual_sf <- reviere_geom %>%
  
  select(
    poly_id
  ) %>%
  
  left_join(
    annual_data,
    by = "poly_id",
    relationship = "one-to-many"
  )


# =====================================================================
# 7. DEFINE 5-YEAR PERIODS
# =====================================================================

annual_data <- annual_data %>%
  
  mutate(
    
    period_5yr = case_when(
      
      year >= 1993 & year <= 1997 ~ "1993–1997",
      year >= 1998 & year <= 2002 ~ "1998–2002",
      year >= 2003 & year <= 2007 ~ "2003–2007",
      year >= 2008 & year <= 2012 ~ "2008–2012",
      year >= 2013 & year <= 2017 ~ "2013–2017",
      year >= 2018 & year <= 2022 ~ "2018–2022",
      
      # Incomplete final interval
      year >= 2023 & year <= 2024 ~ "2023–2024",
      
      TRUE ~ NA_character_
    )
  )


period_levels <- c(
  "1993–1997",
  "1998–2002",
  "2003–2007",
  "2008–2012",
  "2013–2017",
  "2018–2022",
  "2023–2024"
)


annual_data <- annual_data %>%
  
  mutate(
    period_5yr =
      factor(
        period_5yr,
        levels = period_levels
      )
  )


# =====================================================================
# 8. CALCULATE MEAN ANNUAL DENSITY PER PERIOD
# =====================================================================

mean_5yr <- annual_data %>%
  
  filter(
    !is.na(period_5yr)
  ) %>%
  
  group_by(
    poly_id,
    prov,
    species,
    period_5yr
  ) %>%
  
  summarise(
    
    mean_harvest_density =
      mean(
        harvest_density,
        na.rm = TRUE
      ),
    
    n_years =
      n_distinct(year),
    
    .groups = "drop"
  )


# Attach geometry
mean_5yr_sf <- reviere_geom %>%
  
  select(
    poly_id
  ) %>%
  
  left_join(
    mean_5yr,
    by = "poly_id",
    relationship = "one-to-many"
  )


# =====================================================================
# 9. PROVINCE OUTLINES + COMMON EXTENT
# =====================================================================

prov_outline <- reviere_geom %>%
  
  group_by(
    prov
  ) %>%
  
  summarise(
    geometry =
      st_union(geom),
    .groups = "drop"
  ) %>%
  
  st_make_valid()


full_bbox <- st_bbox(
  prov_outline
)


# =====================================================================
# 10. MAP THEME
# =====================================================================

theme_map <- theme_minimal(
  base_size = 12
) +
  
  theme(
    
    panel.grid =
      element_blank(),
    
    axis.title =
      element_blank(),
    
    axis.text =
      element_blank(),
    
    axis.ticks =
      element_blank(),
    
    legend.position =
      "bottom",
    
    legend.box =
      "horizontal",
    
    strip.text =
      element_text(
        size = 10,
        face = "bold"
      ),
    
    plot.title =
      element_text(
        size = 18,
        face = "bold"
      ),
    
    plot.subtitle =
      element_text(
        size = 11
      ),
    
    plot.title.position =
      "plot",
    
    plot.margin =
      margin(
        5,
        5,
        5,
        5
      )
  )


# =====================================================================
# 11. CREATE MAPS
# =====================================================================

for (sp in species_order) {
  
  
  # -------------------------------------------------------------------
  # Species
  # -------------------------------------------------------------------
  
  sp_label <-
    species_labels[[sp]]
  
  
  sp_annual <- annual_sf %>%
    
    filter(
      species == sp
    )
  
  
  sp_5yr <- mean_5yr_sf %>%
    
    filter(
      species == sp
    )
  
  
  # -------------------------------------------------------------------
  # Common scale for annual + 5-year maps of the same species
  #
  # Use 99th percentile instead of absolute maximum.
  # This prevents a few extreme hunt sites from compressing the
  # colour scale for all other polygons.
  # Values above the limit are squished to the maximum colour.
  # -------------------------------------------------------------------
  
  scale_max <- quantile(
    sp_annual$harvest_density,
    probs = 0.99,
    na.rm = TRUE
  )
  
  
  # ===================================================================
  # 11A. ANNUAL HARVEST DENSITY
  # ===================================================================
  
  p_annual <- ggplot() +
    
    geom_sf(
      data = prov_outline,
      fill = "grey94",
      linewidth = 0.12,
      color = "grey45"
    ) +
    
    geom_sf(
      data = sp_annual,
      aes(
        fill = harvest_density
      ),
      linewidth = 0.01,
      color = NA
    ) +
    
    scale_fill_distiller(
      
      palette = "YlOrRd",
      direction = 1,
      
      limits = c(
        0,
        scale_max
      ),
      
      oob =
        squish,
      
      na.value =
        "grey94",
      
      name =
        expression(
          "Harvest density" ~
            (n ~ km^{-2} ~ yr^{-1})
        )
    ) +
    
    coord_sf(
      
      xlim = c(
        full_bbox["xmin"],
        full_bbox["xmax"]
      ),
      
      ylim = c(
        full_bbox["ymin"],
        full_bbox["ymax"]
      ),
      
      expand = FALSE
    ) +
    
    facet_wrap(
      ~ year,
      ncol = 7
    ) +
    
    labs(
      
      title =
        paste0(
          sp_label,
          " harvest density by year"
        ),
      
      subtitle =
        "Annual harvest per hunt-site area; 1992 excluded"
    ) +
    
    theme_map
  
  
  # High-resolution PNG
  ggsave(
    
    filename =
      file.path(
        output_dir,
        paste0(
          "overview_",
          sp,
          "_all_years_density_landscape.png"
        )
      ),
    
    plot =
      p_annual,
    
    width =
      28,
    
    height =
      19,
    
    units =
      "in",
    
    dpi =
      600,
    
    bg =
      "white",
    
    limitsize =
      FALSE
  )
  
  
  # Vector PDF
  ggsave(
    
    filename =
      file.path(
        output_dir,
        paste0(
          "overview_",
          sp,
          "_all_years_density_landscape.pdf"
        )
      ),
    
    plot =
      p_annual,
    
    width =
      28,
    
    height =
      19,
    
    units =
      "in",
    
    bg =
      "white",
    
    limitsize =
      FALSE
  )
  
  
  # ===================================================================
  # 11B. MEAN HARVEST DENSITY PER 5-YEAR PERIOD
  # ===================================================================
  
  p_5yr <- ggplot() +
    
    geom_sf(
      data = prov_outline,
      fill = "grey94",
      linewidth = 0.16,
      color = "grey45"
    ) +
    
    geom_sf(
      data = sp_5yr,
      aes(
        fill = mean_harvest_density
      ),
      linewidth = 0.015,
      color = NA
    ) +
    
    scale_fill_distiller(
      
      palette =
        "YlOrRd",
      
      direction =
        1,
      
      limits = c(
        0,
        scale_max
      ),
      
      oob =
        squish,
      
      na.value =
        "grey94",
      
      name =
        expression(
          "Mean harvest density" ~
            (n ~ km^{-2} ~ yr^{-1})
        )
    ) +
    
    coord_sf(
      
      xlim = c(
        full_bbox["xmin"],
        full_bbox["xmax"]
      ),
      
      ylim = c(
        full_bbox["ymin"],
        full_bbox["ymax"]
      ),
      
      expand =
        FALSE
    ) +
    
    facet_wrap(
      ~ period_5yr,
      ncol = 4
    ) +
    
    labs(
      
      title =
        paste0(
          sp_label,
          " mean harvest density"
        ),
      
      subtitle =
        paste0(
          "Mean annual harvest density; 1992 excluded. ",
          "Final interval contains 2023–2024 only."
        )
    ) +
    
    theme_map
  
  
  # High-resolution PNG
  ggsave(
    
    filename =
      file.path(
        output_dir,
        paste0(
          "overview_",
          sp,
          "_5year_mean_density_landscape.png"
        )
      ),
    
    plot =
      p_5yr,
    
    width =
      22,
    
    height =
      12,
    
    units =
      "in",
    
    dpi =
      600,
    
    bg =
      "white",
    
    limitsize =
      FALSE
  )
  
  
  # Vector PDF
  ggsave(
    
    filename =
      file.path(
        output_dir,
        paste0(
          "overview_",
          sp,
          "_5year_mean_density_landscape.pdf"
        )
      ),
    
    plot =
      p_5yr,
    
    width =
      22,
    
    height =
      12,
    
    units =
      "in",
    
    bg =
      "white",
    
    limitsize =
      FALSE
  )
  
  
  cat(
    "Finished: ",
    sp_label,
    "\n",
    sep = ""
  )
}


# =====================================================================
# 12. SAVE DERIVED DATA
# =====================================================================

write.csv(
  
  annual_data %>%
    
    select(
      poly_id,
      hunt_site,
      name,
      prov,
      species,
      year,
      n,
      area_km2,
      harvest_density
    ),
  
  file.path(
    output_dir,
    "annual_harvest_density_by_revier.csv"
  ),
  
  row.names = FALSE
)


write.csv(
  
  mean_5yr %>%
    
    select(
      poly_id,
      prov,
      species,
      period_5yr,
      mean_harvest_density,
      n_years
    ),
  
  file.path(
    output_dir,
    "mean_harvest_density_by_5year_period.csv"
  ),
  
  row.names = FALSE
)


# =====================================================================
# 13. SUMMARY
# =====================================================================

cat("\n")
cat("============================================================\n")
cat("HARVEST DENSITY MAPS COMPLETE\n")
cat("============================================================\n")
cat("\nOutput directory:\n")
cat(output_dir, "\n")

cat("\nAnnual maps:\n")
cat("  n / hunt-site area [km²]\n")

cat("\nPeriod maps:\n")
cat("  mean annual n / hunt-site area [km²]\n")

cat("\n1992 excluded.\n")
cat("PNG resolution: 600 dpi\n")
cat("PDF output: vector graphics\n")
cat("============================================================\n")
# =====================================================================
# STEP 4 — YEARLY MAPS OF HARVEST BY SPECIES
# =====================================================================
#
# Creates:
#   1) one map per year for each species (PNG)
#   2) one multi-page PDF per species
#   3) one faceted overview PNG per species
#
# Input:
#   final_integer_allocation/final_revier_timeseries_complete.gpkg
#
# Output:
#   /mnt/eo/WilDensity/output/final_integer_allocation/maps_species_year
# =====================================================================


# =====================================================================
# 0. PACKAGES
# =====================================================================

library(sf)
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(forcats)
install.packages("forcats")

# =====================================================================
# 1. PATHS
# =====================================================================

input_gpkg <-
  "/mnt/eo/WilDensity/output/final_integer_allocation/final_revier_timeseries_complete.gpkg"

input_layer <-
  "revier_timeseries"

output_dir <-
  "/mnt/eo/WilDensity/output/final_integer_allocation/maps_species_year"

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

year_ranges <- list(
  sbg    = 1998:2024,
  styria = 1992:2024
)

# Fixed harvest classes.
# These are easier to compare across years than a free continuous scale.
make_harvest_class <- function(x) {
  
  cut(
    x,
    breaks = c(-Inf, 0, 1, 5, 10, 20, 50, 100, Inf),
    labels = c(
      "0",
      "1",
      "2–5",
      "6–10",
      "11–20",
      "21–50",
      "51–100",
      ">100"
    ),
    right = TRUE
  )
}


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
    n       = as.integer(n),
    prov    = as.character(prov)
  ) %>%
  
  filter(
    species %in% species_order
  )


# Keep only intended study periods
x <- x %>%
  
  filter(
    (prov == "sbg"    & year %in% year_ranges$sbg) |
      (prov == "styria" & year %in% year_ranges$styria)
  )


if (nrow(x) == 0) {
  stop("No rows found in the final GPKG.")
}


# Common map extent for all figures
full_bbox <- st_bbox(x)


# Province boundary for light background / orientation
prov_outline <- x %>%
  group_by(prov) %>%
  summarise(
    geometry = st_union(geom),
    .groups = "drop"
  ) %>%
  st_make_valid()


# =====================================================================
# 4. PREPARE DATA
# =====================================================================

x <- x %>%
  
  mutate(
    species_label =
      recode(species, !!!species_labels),
    
    harvest_class =
      make_harvest_class(n)
  )


# A complete year vector per species helps for overview plots
species_years <- x %>%
  
  st_drop_geometry() %>%
  
  group_by(species) %>%
  
  summarise(
    first_year = min(year, na.rm = TRUE),
    last_year  = max(year, na.rm = TRUE),
    .groups = "drop"
  )


# =====================================================================
# 5. MAP THEME
# =====================================================================

theme_map <- theme_minimal(base_size = 11) +
  theme(
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text  = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "right",
    plot.title.position = "plot",
    strip.text = element_text(face = "bold")
  )


# =====================================================================
# 6. FUNCTION — DRAW ONE MAP
# =====================================================================

plot_one_year <- function(dat, title_text, subtitle_text = NULL) {
  
  ggplot() +
    
    geom_sf(
      data = prov_outline,
      fill = NA,
      linewidth = 0.25,
      color = "grey40"
    ) +
    
    geom_sf(
      data = dat,
      aes(fill = harvest_class),
      linewidth = 0.05,
      color = NA
    ) +
    
    scale_fill_brewer(
      palette = "YlOrRd",
      drop = FALSE,
      na.value = "grey90",
      name = "Harvest n"
    ) +
    
    coord_sf(
      xlim = c(full_bbox["xmin"], full_bbox["xmax"]),
      ylim = c(full_bbox["ymin"], full_bbox["ymax"]),
      expand = FALSE
    ) +
    
    labs(
      title = title_text,
      subtitle = subtitle_text
    ) +
    
    theme_map
}


# =====================================================================
# 7. YEARLY PNGS + MULTI-PAGE PDF + FACETED OVERVIEW
# =====================================================================

for (sp in species_order) {
  
  sp_label <- species_labels[[sp]]
  
  sp_dir <- file.path(
    output_dir,
    sp
  )
  
  dir.create(
    sp_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  sp_dat <- x %>%
    filter(species == sp)
  
  years <- sort(unique(sp_dat$year))
  
  cat(
    "\nCreating maps for ",
    sp_label,
    " (",
    length(years),
    " years)\n",
    sep = ""
  )
  
  
  # ---------------------------------------------------------------
  # 7A. One PNG per year
  # ---------------------------------------------------------------
  
  for (yr in years) {
    
    dat_year <- sp_dat %>%
      filter(year == yr)
    
    p <- plot_one_year(
      dat = dat_year,
      title_text = paste0(sp_label, " harvest"),
      subtitle_text = paste0("Year: ", yr)
    )
    
    ggsave(
      filename = file.path(
        output_dir,
        paste0("overview_", sp, "_all_years_landscape.png")
      ),
      plot = p_overview,
      width = 24,
      height = 14,
      dpi = 300
    )
  
  
  # ---------------------------------------------------------------
  # 7B. Multi-page PDF: one year per page
  # ---------------------------------------------------------------
  
  pdf(
    file = file.path(
      output_dir,
      paste0("timeseries_maps_", sp, ".pdf")
    ),
    width = 8.5,
    height = 9.5
  )
  
  for (yr in years) {
    
    dat_year <- sp_dat %>%
      filter(year == yr)
    
    p <- plot_one_year(
      dat = dat_year,
      title_text = paste0(sp_label, " harvest"),
      subtitle_text = paste0("Year: ", yr)
    )
    
    print(p)
  }
  
  dev.off()
  
  
  # ---------------------------------------------------------------
  # 7C. Faceted overview PNG
  # ---------------------------------------------------------------
  
  p_overview <- ggplot() +
    
    geom_sf(
      data = prov_outline,
      fill = NA,
      linewidth = 0.1,
      color = "grey40"
    ) +
    
    geom_sf(
      data = sp_dat,
      aes(fill = harvest_class),
      linewidth = 0.02,
      color = NA
    ) +
    
    scale_fill_brewer(
      palette = "YlOrRd",
      drop = FALSE,
      na.value = "grey90",
      name = "Harvest n"
    ) +
    
    coord_sf(
      xlim = c(full_bbox["xmin"], full_bbox["xmax"]),
      ylim = c(full_bbox["ymin"], full_bbox["ymax"]),
      expand = FALSE
    ) +
    
    facet_wrap(
      ~ year,
      ncol = 8
    ) +
    
    labs(
      title = paste0(sp_label, " harvest by year"),
      subtitle = ""
    ) +
    
    theme_map +
    theme(
      legend.position = "bottom"
    )
  
  ggsave(
    filename = file.path(
      output_dir,
      paste0("overview_", sp, "_all_years.png")
    ),
    plot = p_overview,
    width = 16,
    height = 22,
    dpi = 300
  )
}

  
  

  # ---------------------------------------------------------------
  # 7C. Faceted overview PNG — landscape
  # ---------------------------------------------------------------
  
  p_overview <- ggplot() +
    
    geom_sf(
      data = prov_outline,
      fill = NA,
      linewidth = 0.08,
      color = "grey50"
    ) +
    
    geom_sf(
      data = sp_dat,
      aes(fill = harvest_class),
      linewidth = 0.01,
      color = NA
    ) +
    
    scale_fill_brewer(
      palette = "YlOrRd",
      drop = FALSE,
      na.value = "grey90",
      name = "Harvest n"
    ) +
    
    coord_sf(
      xlim = c(full_bbox["xmin"], full_bbox["xmax"]),
      ylim = c(full_bbox["ymin"], full_bbox["ymax"]),
      expand = FALSE
    ) +
    
    facet_wrap(
      ~ year,
      ncol = 7
    ) +
    
    labs(
      title = paste0(sp_label, " harvest by year"),
      subtitle = ""
    ) +
    
    theme_map +
    theme(
      legend.position = "bottom",
      legend.box = "horizontal",
      strip.text = element_text(size = 9, face = "bold"),
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 11),
      plot.margin = margin(5, 5, 5, 5)
    )
  
  ggsave(
    filename = file.path(
      output_dir,
      paste0("overview_", sp, "_all_years_landscape.png")
    ),
    plot = p_overview,
    width = 24,
    height = 14,
    dpi = 300
  )
  
  
  
# =====================================================================
# 8. OPTIONAL: PROVINCE-SPECIFIC FACET OVERVIEWS
# =====================================================================

for (sp in species_order) {
  
  sp_label <- species_labels[[sp]]
  
  for (pv in c("sbg", "styria")) {
    
    pv_label <- ifelse(pv == "sbg", "Salzburg", "Styria")
    
    dat_sub <- x %>%
      filter(
        species == sp,
        prov == pv
      )
    
    p_sub <- ggplot() +
      
      geom_sf(
        data = prov_outline %>% filter(prov == pv),
        fill = NA,
        linewidth = 0.15,
        color = "grey40"
      ) +
      
      geom_sf(
        data = dat_sub,
        aes(fill = harvest_class),
        linewidth = 0.02,
        color = NA
      ) +
      
      scale_fill_brewer(
        palette = "YlOrRd",
        drop = FALSE,
        na.value = "grey90",
        name = "Harvest n"
      ) +
      
      coord_sf(expand = FALSE) +
      
      facet_wrap(
        ~ year,
        ncol = 5
      ) +
      
      labs(
        title = paste0(sp_label, " harvest by year — ", pv_label),
        subtitle = ""
      ) +
      
      theme_map +
      theme(
        legend.position = "bottom"
      )
    
    ggsave(
      filename = file.path(
        output_dir,
        paste0("overview_", sp, "_", pv, "_all_years.png")
      ),
      plot = p_sub,
      width = 16,
      height = 22,
      dpi = 300
    )
  }
}


# =====================================================================
# 9. SUMMARY
# =====================================================================

cat("\n")
cat("============================================================\n")
cat("YEARLY MAP EXPORT COMPLETE\n")
cat("============================================================\n")
cat("Input:\n")
cat(input_gpkg, "\n\n")
cat("Output directory:\n")
cat(output_dir, "\n\n")
cat("Created outputs:\n")
cat("- one PNG per year for each species\n")
cat("- one multi-page PDF per species\n")
cat("- one faceted overview PNG per species\n")
cat("- one faceted overview PNG per species and province\n")
cat("============================================================\n")



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


for (sp in species_order) {
  
  sp_label <- species_labels[[sp]]
  
  sp_dat <- x %>%
    filter(species == sp)
  
  
  p_overview <- ggplot() +
    
    geom_sf(
      data = prov_outline,
      fill = NA,
      linewidth = 0.08,
      color = "grey50"
    ) +
    
    geom_sf(
      data = sp_dat,
      aes(fill = harvest_class),
      linewidth = 0.01,
      color = NA
    ) +
    
    scale_fill_brewer(
      palette = "YlOrRd",
      drop = FALSE,
      na.value = "grey90",
      name = "Harvest n"
    ) +
    
    coord_sf(
      xlim = c(full_bbox["xmin"], full_bbox["xmax"]),
      ylim = c(full_bbox["ymin"], full_bbox["ymax"]),
      expand = FALSE
    ) +
    
    facet_wrap(
      ~ year,
      ncol = 7
    ) +
    
    labs(
      title = paste0(sp_label, " harvest by year"),
      subtitle = ""
    ) +
    
    theme_map +
    
    theme(
      legend.position = "bottom",
      legend.box = "horizontal",
      
      strip.text = element_text(
        size = 9,
        face = "bold"
      ),
      
      plot.title = element_text(
        size = 16,
        face = "bold"
      ),
      
      plot.subtitle = element_text(
        size = 11
      ),
      
      plot.margin = margin(
        5, 5, 5, 5
      )
    )
  
  
  ggsave(
    filename = file.path(
      output_dir,
      paste0(
        "overview_",
        sp,
        "_all_years_landscape.png"
      )
    ),
    plot = p_overview,
    width = 24,
    height = 14,
    dpi = 300
  )
}

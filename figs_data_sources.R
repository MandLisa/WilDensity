# =====================================================================
# ALLOCATION SOURCE FIGURE — FINAL
# =====================================================================
#
# Creates one final figure showing the share of current polygon x species
# values by source, faceted by:
#   - rows    = species
#   - columns = province (Salzburg / Styria)
#
# Recommended input:
#   NON-interpolated final allocation dataset
#
# Output:
#   allocation_source_share_by_year_both_provinces.png
# =====================================================================


# =====================================================================
# 0. PACKAGES
# =====================================================================

library(sf)
library(dplyr)
library(ggplot2)
library(scales)
library(forcats)


# =====================================================================
# 1. PATHS
# =====================================================================

input_gpkg <-
  "/mnt/eo/WilDensity/output/final_integer_allocation/final_revier_timeseries_complete.gpkg"

input_layer <-
  "revier_timeseries"

output_dir <-
  "/mnt/eo/WilDensity/output/final_integer_allocation/allocation_source_figures"

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)


# =====================================================================
# 2. SETTINGS
# =====================================================================

species_order <- c(
  "chamois",
  "red_deer",
  "roe_deer"
)

species_labels <- c(
  chamois  = "chamois",
  red_deer = "red_deer",
  roe_deer = "roe_deer"
)

prov_labels <- c(
  sbg    = "Salzburg",
  styria = "Styria"
)

source_order <- c(
  "Observed",
  "Suitability allocated",
  "Allocated zero"
)

source_colors <- c(
  "Observed"              = "#2E707A",
  "Suitability allocated" = "#A78E01",
  "Allocated zero"        = "#A73B01"
)


# =====================================================================
# 3. READ DATA
# =====================================================================

x <- st_read(
  input_gpkg,
  layer = input_layer,
  quiet = TRUE
) %>%
  
  st_drop_geometry() %>%
  
  mutate(
    species  = as.character(species),
    year     = as.integer(year),
    prov     = as.character(prov),
    n_source = as.character(n_source)
  ) %>%
  
  filter(
    species %in% species_order
  )


if (nrow(x) == 0) {
  stop("No rows found in input data.")
}


# =====================================================================
# 4. MAP SOURCE CATEGORIES
# =====================================================================

# IMPORTANT:
# Adjust the n_source mapping below if your actual source labels differ.
# The code is written defensively so a few likely variants are covered.

x <- x %>%
  
  mutate(
    value_source = case_when(
      
      n_source %in% c(
        "observed",
        "Observed",
        "direct_observed",
        "mapped_observed"
      ) ~ "Observed",
      
      n_source %in% c(
        "allocated_suitability",
        "suitability_allocated",
        "allocated_by_suitability",
        "allocation",
        "allocated"
      ) ~ "Suitability allocated",
      
      n_source %in% c(
        "allocated_zero",
        "zero",
        "zero_completed",
        "filled_zero"
      ) ~ "Allocated zero",
      
      TRUE ~ NA_character_
    )
  )


unknown_sources <- x %>%
  
  filter(
    is.na(value_source)
  ) %>%
  
  distinct(n_source)


if (nrow(unknown_sources) > 0) {
  
  cat("\nUnknown n_source values found:\n")
  print(unknown_sources)
  
  stop(
    "Some n_source values could not be mapped to figure categories. ",
    "Please inspect 'unknown_sources' above and extend the mapping."
  )
}


# =====================================================================
# 5. PREPARE SUMMARY TABLE
# =====================================================================

plot_df <- x %>%
  
  mutate(
    prov_label =
      recode(prov, !!!prov_labels),
    
    species_label =
      recode(species, !!!species_labels),
    
    value_source =
      factor(
        value_source,
        levels = source_order
      ),
    
    species_label =
      factor(
        species_label,
        levels = species_labels[species_order]
      ),
    
    prov_label =
      factor(
        prov_label,
        levels = c("Salzburg", "Styria")
      )
  ) %>%
  
  group_by(
    prov_label,
    species_label,
    year,
    value_source
  ) %>%
  
  summarise(
    n_values = n(),
    .groups = "drop"
  ) %>%
  
  group_by(
    prov_label,
    species_label,
    year
  ) %>%
  
  mutate(
    share = n_values / sum(n_values)
  ) %>%
  
  ungroup()


# Optional diagnostic table
write.csv(
  plot_df,
  file.path(
    output_dir,
    "allocation_source_share_by_year_both_provinces.csv"
  ),
  row.names = FALSE
)


# =====================================================================
# 6. PLOT
# =====================================================================

p <- ggplot(
  plot_df,
  aes(
    x = year,
    y = share,
    fill = value_source
  )
) +
  
  geom_col(
    width = 0.9,
    color = NA
  ) +
  
  facet_grid(
    species_label ~ prov_label,
    scales = "free_x",
    space = "free_x"
  ) +
  
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    expand = expansion(mult = c(0, 0.02))
  ) +
  
  scale_fill_manual(
    values = source_colors,
    drop = FALSE
  ) +
  
  labs(
    title = "Origin of hunt-site values by year",
    subtitle = "",
    x = "Year",
    y = "Share of values",
    fill = "Value source"
  ) +
  
  theme_minimal(base_size = 16) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    
    strip.text = element_text(
      face = "bold",
      size = 15
    ),
    
    plot.title = element_text(
      face = "bold",
      size = 22
    ),
    
    plot.subtitle = element_text(
      size = 16
    ),
    
    axis.title = element_text(
      size = 16
    ),
    
    axis.text = element_text(
      size = 13
    ),
    
    legend.position = "bottom",
    legend.direction = "horizontal",
    
    legend.title = element_text(
      size = 16
    ),
    
    legend.text = element_text(
      size = 14
    )
  )


# =====================================================================
# 7. SAVE
# =====================================================================

ggsave(
  filename = file.path(
    output_dir,
    "allocation_source_share_by_year_both_provinces.png"
  ),
  plot = p,
  width = 18,
  height = 14,
  dpi = 600,
  bg = "white"
)


# =====================================================================
# 8. SUMMARY
# =====================================================================

cat("\n")
cat("============================================================\n")
cat("ALLOCATION SOURCE FIGURE COMPLETE\n")
cat("============================================================\n")
cat("Input:\n")
cat(input_gpkg, "\n\n")
cat("Output directory:\n")
cat(output_dir, "\n\n")
cat("Created:\n")
cat("- allocation_source_share_by_year_both_provinces.png\n")
cat("- allocation_source_share_by_year_both_provinces.csv\n")
cat("============================================================\n")



### Metrcis

# =====================================================================
# 6B. ALLOCATION METRICS
# =====================================================================


# ---------------------------------------------------------------------
# A. SHARE OF POLYGON x SPECIES x YEAR VALUES
# ---------------------------------------------------------------------

value_metrics <- x %>%
  
  group_by(
    prov,
    species
  ) %>%
  
  summarise(
    
    n_total =
      n(),
    
    n_observed =
      sum(
        value_source == "Observed"
      ),
    
    n_suitability_allocated =
      sum(
        value_source == "Suitability allocated"
      ),
    
    n_allocated_zero =
      sum(
        value_source == "Allocated zero"
      ),
    
    pct_observed =
      100 *
      n_observed /
      n_total,
    
    pct_suitability_allocated =
      100 *
      n_suitability_allocated /
      n_total,
    
    pct_allocated_zero =
      100 *
      n_allocated_zero /
      n_total,
    
    pct_total_allocated =
      100 *
      (
        n_suitability_allocated +
          n_allocated_zero
      ) /
      n_total,
    
    .groups =
      "drop"
  )


print(
  value_metrics
)


# ---------------------------------------------------------------------
# B. SAME METRICS BY PROVINCE ONLY
# ---------------------------------------------------------------------

province_metrics <- x %>%
  
  group_by(
    prov
  ) %>%
  
  summarise(
    
    n_total =
      n(),
    
    pct_observed =
      100 *
      mean(
        value_source == "Observed"
      ),
    
    pct_suitability_allocated =
      100 *
      mean(
        value_source == "Suitability allocated"
      ),
    
    pct_allocated_zero =
      100 *
      mean(
        value_source == "Allocated zero"
      ),
    
    pct_total_allocated =
      100 *
      mean(
        value_source != "Observed"
      ),
    
    .groups =
      "drop"
  )


print(
  province_metrics
)


# ---------------------------------------------------------------------
# C. OVERALL METRICS
# ---------------------------------------------------------------------

overall_metrics <- x %>%
  
  summarise(
    
    n_total =
      n(),
    
    pct_observed =
      100 *
      mean(
        value_source == "Observed"
      ),
    
    pct_suitability_allocated =
      100 *
      mean(
        value_source == "Suitability allocated"
      ),
    
    pct_allocated_zero =
      100 *
      mean(
        value_source == "Allocated zero"
      ),
    
    pct_total_allocated =
      100 *
      mean(
        value_source != "Observed"
      )
  )


print(
  overall_metrics
)


# =====================================================================
# 6C. SHARE OF TOTAL HARVEST COMING FROM EACH SOURCE
# =====================================================================

harvest_metrics <- x %>%
  
  group_by(
    prov,
    species,
    value_source
  ) %>%
  
  summarise(
    
    harvest_n =
      sum(
        n,
        na.rm = TRUE
      ),
    
    .groups =
      "drop"
  ) %>%
  
  group_by(
    prov,
    species
  ) %>%
  
  mutate(
    
    harvest_share_pct =
      100 *
      harvest_n /
      sum(harvest_n)
  ) %>%
  
  ungroup()


print(
  harvest_metrics
)


# =====================================================================
# 6D. SAVE METRICS
# =====================================================================

write.csv(
  value_metrics,
  file.path(
    output_dir,
    "allocation_metrics_by_province_species.csv"
  ),
  row.names = FALSE
)


write.csv(
  province_metrics,
  file.path(
    output_dir,
    "allocation_metrics_by_province.csv"
  ),
  row.names = FALSE
)


write.csv(
  overall_metrics,
  file.path(
    output_dir,
    "allocation_metrics_overall.csv"
  ),
  row.names = FALSE
)


write.csv(
  harvest_metrics,
  file.path(
    output_dir,
    "harvest_share_by_allocation_source.csv"
  ),
  row.names = FALSE
)


# =====================================================================
# 6E. FIGURES FOR ALLOCATION METRICS
# =====================================================================

library(tidyr)
library(forcats)


# ---------------------------------------------------------------------
# Labels / factor order
# ---------------------------------------------------------------------

prov_labels <- c(
  sbg    = "Salzburg",
  styria = "Styria"
)

species_order <- c(
  "chamois",
  "red_deer",
  "roe_deer"
)

species_labels <- c(
  chamois  = "chamois",
  red_deer = "red_deer",
  roe_deer = "roe_deer"
)

source_order <- c(
  "Observed",
  "Suitability allocated",
  "Allocated zero"
)

source_colors <- c(
  "Observed"              = "#2E707A",
  "Suitability allocated" = "#A78E01",
  "Allocated zero"        = "#A73B01"
)


# =====================================================================
# FIGURE 1 — SHARE OF VALUE SOURCES
# =====================================================================

value_metrics_long <- value_metrics %>%
  
  mutate(
    prov_label =
      recode(prov, !!!prov_labels),
    
    species_label =
      recode(species, !!!species_labels)
  ) %>%
  
  select(
    prov_label,
    species_label,
    pct_observed,
    pct_suitability_allocated,
    pct_allocated_zero
  ) %>%
  
  pivot_longer(
    cols = starts_with("pct_"),
    names_to = "value_source",
    values_to = "share_pct"
  ) %>%
  
  mutate(
    value_source = recode(
      value_source,
      pct_observed              = "Observed",
      pct_suitability_allocated = "Suitability allocated",
      pct_allocated_zero        = "Allocated zero"
    ),
    
    value_source =
      factor(
        value_source,
        levels = source_order
      ),
    
    species_label =
      factor(
        species_label,
        levels = species_labels[species_order]
      ),
    
    prov_label =
      factor(
        prov_label,
        levels = c("Salzburg", "Styria")
      )
  )


p1 <- ggplot(
  value_metrics_long,
  aes(
    x = species_label,
    y = share_pct / 100,
    fill = value_source
  )
) +
  
  geom_col(
    width = 0.75
  ) +
  
  facet_wrap(
    ~ prov_label,
    nrow = 1
  ) +
  
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    expand = expansion(mult = c(0, 0.02))
  ) +
  
  scale_fill_manual(
    values = source_colors,
    drop = FALSE
  ) +
  
  labs(
    title = "Source of polygon × species × year values",
    subtitle = "Share of values by province and species",
    x = NULL,
    y = "Share of values",
    fill = "Value source"
  ) +
  
  theme_minimal(base_size = 15) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    
    strip.text = element_text(
      face = "bold",
      size = 14
    ),
    
    axis.text.x = element_text(
      size = 13
    ),
    
    legend.position = "bottom",
    legend.direction = "horizontal"
  )


ggsave(
  file.path(
    output_dir,
    "allocation_metrics_value_source_share.png"
  ),
  p1,
  width = 13,
  height = 7,
  dpi = 600,
  bg = "white"
)


# =====================================================================
# FIGURE 2 — TOTAL ALLOCATED SHARE
# =====================================================================

p2_df <- value_metrics %>%
  
  mutate(
    prov_label =
      recode(prov, !!!prov_labels),
    
    species_label =
      recode(species, !!!species_labels),
    
    species_label =
      factor(
        species_label,
        levels = rev(species_labels[species_order])
      ),
    
    prov_label =
      factor(
        prov_label,
        levels = c("Salzburg", "Styria")
      )
  )


p2 <- ggplot(
  p2_df,
  aes(
    x = pct_total_allocated,
    y = species_label,
    color = prov_label
  )
) +
  
  geom_point(
    size = 4
  ) +
  
  geom_segment(
    aes(
      x = 0,
      xend = pct_total_allocated,
      y = species_label,
      yend = species_label
    ),
    linewidth = 1
  ) +
  
  scale_x_continuous(
    labels = function(x) paste0(round(x), "%"),
    limits = c(0, 100),
    expand = expansion(mult = c(0, 0.02))
  ) +
  
  labs(
    title = "Share of reconstructed values",
    subtitle = "Observed values excluded; reconstruction = suitability allocated + allocated zero",
    x = "Reconstructed values [%]",
    y = NULL,
    color = "Province"
  ) +
  
  theme_minimal(base_size = 15) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )


ggsave(
  file.path(
    output_dir,
    "allocation_metrics_total_allocated_share.png"
  ),
  p2,
  width = 10,
  height = 6,
  dpi = 600,
  bg = "white"
)


# =====================================================================
# FIGURE 3 — SHARE OF TOTAL HARVEST BY VALUE SOURCE
# =====================================================================

harvest_metrics_plot <- harvest_metrics %>%
  
  mutate(
    prov_label =
      recode(prov, !!!prov_labels),
    
    species_label =
      recode(species, !!!species_labels),
    
    value_source =
      factor(
        value_source,
        levels = source_order
      ),
    
    species_label =
      factor(
        species_label,
        levels = species_labels[species_order]
      ),
    
    prov_label =
      factor(
        prov_label,
        levels = c("Salzburg", "Styria")
      )
  )


p3 <- ggplot(
  harvest_metrics_plot,
  aes(
    x = species_label,
    y = harvest_share_pct / 100,
    fill = value_source
  )
) +
  
  geom_col(
    width = 0.75
  ) +
  
  facet_wrap(
    ~ prov_label,
    nrow = 1
  ) +
  
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    expand = expansion(mult = c(0, 0.02))
  ) +
  
  scale_fill_manual(
    values = source_colors,
    drop = FALSE
  ) +
  
  labs(
    title = "Share of total harvest by value source",
    subtitle = "Based on summed harvest counts n",
    x = NULL,
    y = "Share of total harvest",
    fill = "Value source"
  ) +
  
  theme_minimal(base_size = 15) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    
    strip.text = element_text(
      face = "bold",
      size = 14
    ),
    
    axis.text.x = element_text(
      size = 13
    ),
    
    legend.position = "bottom",
    legend.direction = "horizontal"
  )


ggsave(
  file.path(
    output_dir,
    "allocation_metrics_harvest_share_by_source.png"
  ),
  p3,
  width = 13,
  height = 7,
  dpi = 600,
  bg = "white"
)

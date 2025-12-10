library(data.table)
library(sf)
library(ggplot2)
library(gt)

# --- 1) Load data -------------------------------------------------------------
df_sf <- st_read(
  "/mnt/eo/WilDensity/_data/_shp/count_per_rev_cleaned_1012.gpkg",
  layer = "rev_clean"
)

# Convert to data.table (drops geometry for processing speed)
df <- as.data.table(df_sf)
df[, geom := NULL]  # Remove geometry to speed up aggregation


# --- 2) Expected years per province ------------------------------------------
expected_years_sbg  <- 1998:2024
expected_years_stmk <- 1992:2024


# --- 3) Extract existing years per hunt_site (FAST) ---------------------------
# dt syntax: .(colname = expression) and by = grouping
yrs <- df[
  , .(years = list(sort(unique(year)))), 
  by = .(hunt_site, prov)
]


# --- 4) Compute missing years -------------------------------------------------
yrs[
  prov == "sbg",    expected_years := list(expected_years_sbg)
][prov == "styria", expected_years := list(expected_years_stmk)
][is.na(expected_years), expected_years := list(integer(0))]

# Compute missing list + counts
yrs[
  , `:=`(
    missing_list = Map(setdiff, expected_years, years),
    missing_n    = lengths(missing_list)
  )
]

# Keep only sites with missing years
missing_tbl <- yrs[missing_n > 0]


# --- 5) Add species information back -----------------------------------------
species_lookup <- unique(df[, .(hunt_site, species)])

missing_tbl_species <- merge(
  missing_tbl, species_lookup,
  by = "hunt_site",
  all.x = TRUE, allow.cartesian = TRUE
)[
  , missing_years := vapply(missing_list, paste, collapse = ", ", FUN.VALUE = "")
][
  order(prov, hunt_site)
][
  , .(hunt_site, prov, species, missing_years, missing_n)
]


# --- 6) Histogram plot --------------------------------------------------------
ggplot(missing_tbl_species, aes(x = missing_n, fill = prov)) +
  geom_histogram(binwidth = 1, alpha = 0.6, color = "white") +
  facet_wrap(~ species, scales = "free_y") +
  labs(
    x = "Number of missing years",
    y = "Count of hunt sites",
    fill = "Province"
  ) +
  theme_minimal(base_size = 13) +
  theme(strip.text = element_text(face = "bold"))

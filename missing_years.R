library(data.table)
library(sf)
library(ggplot2)
library(gt)

# =====================================================================
# 1) LOAD DATA
# =====================================================================
df_sf <- st_read(
  "/mnt/eo/WilDensity/_data/_shp/count_per_rev_cleaned_1012.gpkg",
  layer = "rev_clean"
)

# convert to data.table and remove geometry for speed
df <- as.data.table(df_sf)
df[, geom := NULL]


# =====================================================================
# 2) EXPECTED YEARS PER PROVINCE
# =====================================================================
expected_years_sbg  <- 1998:2024
expected_years_stmk <- 1992:2024


# =====================================================================
# 3) EXTRACT EXISTING YEARS FOR EACH HUNT SITE
# =====================================================================
yrs <- df[
  , .(years = list(sort(unique(year)))), 
  by = .(hunt_site, prov.x)
]


# =====================================================================
# 4) ASSIGN EXPECTED YEARS
# =====================================================================
yrs[
  prov.x == "sbg",    expected_years := list(expected_years_sbg)
][prov.x == "styria", expected_years := list(expected_years_stmk)
][is.na(expected_years), expected_years := list(integer(0))]


# =====================================================================
# 5) COMPUTE MISSING YEARS (two-step for data.table)
# =====================================================================
# Step 1: list of missing years
yrs[
  , missing_list := Map(setdiff, expected_years, years)
]

# Step 2: number of missing years
yrs[
  , missing_n := lengths(missing_list)
]

# Filter only sites with missing years
missing_tbl <- yrs[missing_n > 0]


# =====================================================================
# 6) ADD SPECIES INFORMATION
# =====================================================================
species_lookup <- unique(df[, .(hunt_site, species)])

missing_tbl_species <- merge(
  missing_tbl, species_lookup,
  by = "hunt_site",
  all.x = TRUE, allow.cartesian = TRUE
)[
  , missing_years := vapply(missing_list, paste, collapse = ", ", FUN.VALUE = "")
][
  order(prov.x, hunt_site)
][
  , .(hunt_site, prov.x, species, missing_years, missing_n)
]


# =====================================================================
# 7) HISTOGRAM OF MISSING YEARS PER SPECIES
# =====================================================================
ggplot(missing_tbl_species, aes(x = missing_n, fill = prov.x)) +
  geom_histogram(binwidth = 1, alpha = 0.6, color = "white") +
  facet_wrap(~ species) +
  labs(
    x = "Number of missing years",
    y = "Count of hunt sites",
    fill = "Province"
  ) +
  theme_minimal(base_size = 13) +
  theme(strip.text = element_text(face = "bold"))


# =====================================================================
# 8) Print hunt sites with missing years
# =====================================================================
setDT(missing_tbl_species)

missing_tbl_species[
  order(`prov.x`, hunt_site),
  .(hunt_site, species, missing_years),
  by = `prov.x`
]



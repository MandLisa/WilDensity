# =====================================================================
# Time-series completeness per hunt_site × species × province
# Starting from a FULL JOIN of CSV + GPKG
# =====================================================================

library(data.table)
library(sf)
library(ggplot2)

# =====================================================================
# 1) LOAD DATA
# =====================================================================

# 1a) Density table (per hunt_site × species × year)
dens <- fread("/mnt/eo/WilDensity/_data/_csv/density_hunt_site.csv")

# 1b) GPKG with hunt_site, species, year, province etc.
rev_sf <- st_read(
  "/mnt/eo/WilDensity/_data/_shp/count_per_rev_cleaned_1012.gpkg",
  layer = "rev_clean"
)

rev_dt <- as.data.table(st_drop_geometry(rev_sf))
rev_dt[, c("prov.x", "prov.y") := NULL]

# names(rev_dt)
# [1] "hunt_site" "species" "year" "prov.x" "n" "name" "area" "prov.y"

# For completeness analysis we only need unique combinations of hunt_site/species/year
dens_years <- unique(dens[, .(hunt_site, species, year, prov, n)])
rev_years  <- unique(rev_dt[, .(hunt_site, species, year)])

# =====================================================================
# 2) FULL JOIN CSV + GPKG ON hunt_site × species × year
# =====================================================================

setDT(dens_years)
setDT(rev_years)

dens_years[, hunt_site := as.character(hunt_site)]
rev_years[, hunt_site  := as.character(hunt_site)]

df_full <- merge(
  dens_years,
  rev_years,
  by = c("hunt_site", "species", "year"),
  all = TRUE   # FULL join
)

# Now df_full contains:
# hunt_site, species, year, prov.x, prov.y (plus maybe other cols; we ignore them)

# =====================================================================
# 3) CLEAN PROVINCE COLUMN
# =====================================================================
# Remove rows without province or year, if any
#df_full <- df_full[!is.na(prov) & !is.na(year) & !is.na(species)]

# =====================================================================
# 4) EXPECTED YEARS PER PROVINCE
# =====================================================================

expected_years_sbg  <- 1998:2024
expected_years_stmk <- 1992:2024   # for Styria (coded as "styria" in your data)

# =====================================================================
# 5) EXTRACT EXISTING YEARS PER hunt_site × species × prov
# =====================================================================

yrs <- df_full[
  ,
  .(years = list(sort(unique(year)))),
  by = .(hunt_site, prov, species)
]

# =====================================================================
# 6) ASSIGN EXPECTED YEARS PER PROVINCE
# =====================================================================

yrs[
  prov == "sbg",    expected_years := list(expected_years_sbg)
][
  prov == "styria", expected_years := list(expected_years_stmk)
][
  is.na(expected_years), expected_years := list(integer(0))
]

# =====================================================================
# 7) COMPUTE MISSING YEARS
# =====================================================================

# Step 1: list of missing years
yrs[
  ,
  missing_list := Map(setdiff, expected_years, years)
]

# Step 2: number of missing years
yrs[
  ,
  missing_n := lengths(missing_list)
]

# Keep only hunt_site × species × prov combinations with at least one missing year
missing_tbl_species <- yrs[missing_n > 0]

# Human-readable string of missing years
missing_tbl_species[
  ,
  missing_years := vapply(
    missing_list,
    paste,
    collapse = ", ",
    FUN.VALUE = character(1)
  )
]

# Order nicely for inspection
missing_tbl_species <- missing_tbl_species[
  order(prov, hunt_site, species),
  .(hunt_site, prov, species, missing_years, missing_n)
]

# =====================================================================
# 8) HISTOGRAM OF MISSING YEARS (per species, coloured by province)
# =====================================================================

ggplot(missing_tbl_species, aes(x = missing_n, fill = prov)) +
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
# 9) OPTIONAL: PRINT HUNT SITES WITH MISSING YEARS
# =====================================================================

setDT(missing_tbl_species)

missing_tbl_species[
  order(prov, hunt_site, species),
  .(hunt_site, species, missing_years),
  by = prov
]

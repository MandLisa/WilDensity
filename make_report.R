library(data.table)
library(sf)

# 1) Load dataset
df_sf <- st_read(
  "/mnt/eo/WilDensity/_data/_shp/count_per_rev_cleaned_1012.gpkg",
  layer = "rev_clean"
)

df <- as.data.table(df_sf)
df[, geom := NULL]

# 2) Expected years
expected_years_sbg  <- 1998:2024
expected_years_stmk <- 1992:2024

# 3) Extract observed years per hunt site
yrs <- df[, .(years = list(sort(unique(year)))), by = .(hunt_site, prov.x)]

# 4) Assign expected years
yrs[prov.x == "sbg",    expected := list(expected_years_sbg)]
yrs[prov.x == "styria", expected := list(expected_years_stmk)]
yrs[is.na(expected), expected := list(integer(0))]

# 5) Compute missing years
yrs[, missing_list := Map(setdiff, expected, years)]
yrs[, missing_n := lengths(missing_list)]

# 6) Keep only hunt sites with missing years
missing_tbl <- yrs[missing_n > 0]

# 7) Add species info
species_lookup <- unique(df[, .(hunt_site, species)])
missing_tbl <- merge(missing_tbl, species_lookup, by = "hunt_site", all.x = TRUE)

# 8) Convert list → string
missing_tbl[, missing_years := sapply(missing_list, function(x) paste(x, collapse = ", "))]

# 9) Final clean table
missing_years_list <- missing_tbl[
  , .(hunt_site, prov = prov.x, species, missing_years, missing_n)
][order(prov, hunt_site)]

missing_years_list


fwrite(missing_years_list, "/mnt/eo/WilDensity/missing_years_per_hunt_site.csv")



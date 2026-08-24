# =====================================================================
# STEP 1 — PREPARE CLEAN INPUTS FOR HUNT-SITE ALLOCATION
# =====================================================================
#
# IMPORTANT
# ---------
# This script performs NO allocation.
#
# It:
#   1) cleans hunt-site and region IDs,
#   2) corrects known Salzburg region-code errors,
#   3) reconstructs the Styria regional rows with missing region IDs,
#   4) assigns every CURRENT hunt-site polygon to a CURRENT region
#      spatially,
#   5) checks the regional totals against the hunt-site source data,
#   6) writes diagnostics for historical/source region IDs that do not
#      exist in the current region geometry, and vice versa.
#
# Only after these diagnostics are clean should the allocation be run.
# =====================================================================


# =====================================================================
# 0. PACKAGES
# =====================================================================

library(sf)
library(dplyr)
library(tidyr)
library(data.table)
library(tibble)


# =====================================================================
# 1. PATHS
# =====================================================================

regional_csv <-
  "/mnt/eo/WilDensity/_data/_csv/ungulates_by_region.csv"

hunt_csv <-
  "/mnt/eo/WilDensity/_data/_csv/density_hunt_site_with_region.csv"

region_gpkg <-
  "/mnt/eo/WilDensity/_data/_shp/regionen_merged/merged_regions_unique.gpkg"

revier_gpkg <-
  "/mnt/eo/WilDensity/_data/_shp/count_per_rev_cleaned_1708.gpkg"

output_dir <-
  "/mnt/eo/WilDensity/output/prepared_allocation_inputs"

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)


# =====================================================================
# 2. SETTINGS
# =====================================================================

species_use <- c(
  "chamois",
  "roe_deer",
  "red_deer"
)

tolerance <- 1e-8


# =====================================================================
# 3. HELPERS
# =====================================================================

normalise_prov <- function(x) {
  
  x <- trimws(
    tolower(
      as.character(x)
    )
  )
  
  case_when(
    x %in% c("sbg", "salzburg") ~ "sbg",
    x %in% c("styria", "stmk", "steiermark") ~ "styria",
    TRUE ~ x
  )
}


normalise_hunt_site <- function(x) {
  
  x <- trimws(
    as.character(x)
  )
  
  x <- gsub(
    "\u00A0",
    "",
    x,
    fixed = TRUE
  )
  
  idx <- grepl(
    "^\\d+\\.0+$",
    x
  )
  
  x[idx] <- sub(
    "\\.0+$",
    "",
    x[idx]
  )
  
  x[
    is.na(x) |
      x %in% c("", "NA", "NULL", "0")
  ] <- NA_character_
  
  x
}


normalise_region <- function(region, prov) {
  
  x <- trimws(
    as.character(region)
  )
  
  p <- normalise_prov(
    prov
  )
  
  x[
    is.na(x) |
      x %in% c("", "NA", "NULL")
  ] <- NA_character_
  
  
  # Salzburg:
  # 12_1 -> 12.1
  idx_sbg <-
    !is.na(x) &
    p == "sbg"
  
  x[idx_sbg] <- gsub(
    "_",
    ".",
    x[idx_sbg],
    fixed = TRUE
  )
  
  
  # Styria:
  # 1501 -> 01501
  idx_styria <-
    !is.na(x) &
    p == "styria" &
    grepl(
      "^\\d{4}$",
      x
    )
  
  x[idx_styria] <- paste0(
    "0",
    x[idx_styria]
  )
  
  x
}


# =====================================================================
# 4. READ + CLEAN HUNT-SITE HARVEST
# =====================================================================

hunt_raw <- fread(
  hunt_csv,
  
  colClasses = list(
    character = c(
      "hunt_site",
      "species",
      "prov",
      "region"
    )
  )
) %>%
  as_tibble()


required_hunt_columns <- c(
  "hunt_site",
  "species",
  "year",
  "prov",
  "region",
  "n"
)

missing_hunt_columns <- setdiff(
  required_hunt_columns,
  names(hunt_raw)
)

if (length(missing_hunt_columns) > 0) {
  stop(
    "Missing hunt-site columns: ",
    paste(
      missing_hunt_columns,
      collapse = ", "
    )
  )
}


hunt_clean <- hunt_raw %>%
  
  transmute(
    source_row_id =
      row_number(),
    
    hunt_site =
      normalise_hunt_site(
        hunt_site
      ),
    
    species =
      trimws(
        as.character(species)
      ),
    
    year =
      as.integer(
        year
      ),
    
    prov =
      normalise_prov(
        prov
      ),
    
    region_original =
      normalise_region(
        region,
        prov
      ),
    
    n =
      as.integer(
        n
      )
  ) %>%
  
  filter(
    species %in%
      species_use
  )


if (any(hunt_clean$n < 0, na.rm = TRUE)) {
  stop(
    "Negative hunt-site harvest values exist."
  )
}


# ---------------------------------------------------------------------
# Final SOURCE-region assignment in hunt CSV
#
# Styria:
#   9-digit IDs -> first 5 digits
#   8-digit IDs -> existing region column
#
# One 8-digit historical ID has missing source region:
#   25021957 -> 25021
#
# Salzburg:
#   use existing source region
# ---------------------------------------------------------------------

hunt_clean <- hunt_clean %>%
  
  mutate(
    region_source = case_when(
      
      prov == "styria" &
        !is.na(hunt_site) &
        nchar(hunt_site) == 9 ~
        substr(
          hunt_site,
          1,
          5
        ),
      
      prov == "styria" &
        hunt_site == "25021957" &
        is.na(region_original) ~
        "25021",
      
      TRUE ~
        region_original
    )
  )


hunt_missing_source_region <- hunt_clean %>%
  filter(
    is.na(region_source)
  )

fwrite(
  hunt_missing_source_region,
  file.path(
    output_dir,
    "hunt_rows_without_source_region.csv"
  )
)

if (nrow(hunt_missing_source_region) > 0) {
  stop(
    "Some hunt-site rows still have no source region. ",
    "Inspect hunt_rows_without_source_region.csv."
  )
}


duplicate_hunt_rows <- hunt_clean %>%
  
  count(
    hunt_site,
    species,
    year,
    name = "n_rows"
  ) %>%
  
  filter(
    !is.na(hunt_site),
    n_rows > 1
  )

if (nrow(duplicate_hunt_rows) > 0) {
  
  fwrite(
    duplicate_hunt_rows,
    file.path(
      output_dir,
      "duplicate_hunt_site_species_year.csv"
    )
  )
  
  stop(
    "Duplicate hunt_site x species x year rows exist."
  )
}


# =====================================================================
# 5. READ + CLEAN INDEPENDENT REGIONAL TOTALS
# =====================================================================

regional_raw <- fread(
  regional_csv,
  
  colClasses = list(
    character = c(
      "region",
      "species",
      "prov"
    )
  )
) %>%
  as_tibble()


required_regional_columns <- c(
  "region",
  "species",
  "year",
  "prov",
  "n"
)

missing_regional_columns <- setdiff(
  required_regional_columns,
  names(regional_raw)
)

if (length(missing_regional_columns) > 0) {
  stop(
    "Missing regional columns: ",
    paste(
      missing_regional_columns,
      collapse = ", "
    )
  )
}


regional_clean <- regional_raw %>%
  
  transmute(
    region =
      normalise_region(
        region,
        prov
      ),
    
    species =
      trimws(
        as.character(species)
      ),
    
    year =
      as.integer(
        year
      ),
    
    prov =
      normalise_prov(
        prov
      ),
    
    n =
      as.integer(
        n
      )
  ) %>%
  
  filter(
    species %in%
      species_use
  )


if (any(regional_clean$n < 0, na.rm = TRUE)) {
  stop(
    "Negative regional totals exist."
  )
}


# =====================================================================
# 6. EXPLICIT SALZBURG REGION-CODE CORRECTIONS
#
# These are explicit corrections identified in the source table.
# They are NOT inferred during allocation.
# =====================================================================

regional_clean <- regional_clean %>%
  
  mutate(
    region = case_when(
      
      prov == "sbg" &
        region == "1.10" ~
        "10.1",
      
      prov == "sbg" &
        region == "2.9" ~
        "9.2",
      
      prov == "sbg" &
        region == "3.5" ~
        "5.3",
      
      prov == "sbg" &
        region == "4.8" ~
        "8.4",
      
      # One row:
      # roe_deer, 2021, n = 2
      # The correction to 6.4 closes the 6.3 / 6.4 balance.
      prov == "sbg" &
        region == "NA.NA" &
        species == "roe_deer" &
        year == 2021 &
        n == 2 ~
        "6.4",
      
      TRUE ~
        region
    )
  )


remaining_invalid_sbg_codes <- regional_clean %>%
  filter(
    prov == "sbg",
    is.na(region) |
      region == "NA.NA"
  )

if (nrow(remaining_invalid_sbg_codes) > 0) {
  
  fwrite(
    remaining_invalid_sbg_codes,
    file.path(
      output_dir,
      "remaining_invalid_salzburg_region_codes.csv"
    )
  )
  
  stop(
    "Unresolved Salzburg regional codes remain."
  )
}


# =====================================================================
# 7. RECONSTRUCT STYRIA REGIONAL ROWS WITH MISSING REGION
#
# The raw regional table contains 39 Styria rows with region = NA
# (3 species x 13 years, 1992-2004).
#
# We reconstruct ONLY the region labels of these totals from the hunt-site
# rows whose original region is also missing.
#
# Important:
#   - total harvest is not changed
#   - the NA regional total for each species x year must equal exactly
#     the sum of the reconstructed source regions
# =====================================================================

styria_regional_na <- regional_clean %>%
  
  filter(
    prov == "styria",
    is.na(region)
  )


styria_hunt_missing_original_region <- hunt_clean %>%
  
  filter(
    prov == "styria",
    is.na(region_original)
  )


reconstructed_styria_regions <-
  styria_hunt_missing_original_region %>%
  
  group_by(
    region = region_source,
    species,
    year,
    prov
  ) %>%
  
  summarise(
    n =
      sum(
        n,
        na.rm = TRUE
      ),
    
    .groups =
      "drop"
  )


# Verify species x year totals BEFORE replacing NA regional rows
styria_na_check <- styria_regional_na %>%
  
  group_by(
    prov,
    species,
    year
  ) %>%
  
  summarise(
    regional_na_n =
      sum(
        n,
        na.rm = TRUE
      ),
    
    .groups =
      "drop"
  ) %>%
  
  full_join(
    
    reconstructed_styria_regions %>%
      
      group_by(
        prov,
        species,
        year
      ) %>%
      
      summarise(
        reconstructed_n =
          sum(
            n,
            na.rm = TRUE
          ),
        
        .groups =
          "drop"
      ),
    
    by = c(
      "prov",
      "species",
      "year"
    )
  ) %>%
  
  mutate(
    regional_na_n =
      coalesce(
        regional_na_n,
        0
      ),
    
    reconstructed_n =
      coalesce(
        reconstructed_n,
        0
      ),
    
    difference =
      regional_na_n -
      reconstructed_n
  )


fwrite(
  styria_na_check,
  file.path(
    output_dir,
    "styria_missing_region_reconstruction_check.csv"
  )
)


bad_styria_reconstruction <- styria_na_check %>%
  filter(
    abs(difference) >
      tolerance
  )


if (nrow(bad_styria_reconstruction) > 0) {
  
  print(
    bad_styria_reconstruction,
    n = Inf
  )
  
  stop(
    "Styria NA-region totals are not exactly reproduced by ",
    "the hunt-site reconstruction."
  )
}


# Replace only the Styria region=NA rows
regional_clean <- regional_clean %>%
  
  filter(
    !(
      prov == "styria" &
        is.na(region)
    )
  ) %>%
  
  bind_rows(
    reconstructed_styria_regions
  )


# Code corrections can create duplicate region x species x year rows.
# Collapse them.
regional_clean <- regional_clean %>%
  
  group_by(
    prov,
    region,
    species,
    year
  ) %>%
  
  summarise(
    n =
      sum(
        n,
        na.rm = TRUE
      ),
    
    .groups =
      "drop"
  )


# =====================================================================
# 8. HARD CONTROL:
# REGIONAL TOTAL == HUNT-SITE TOTAL FOR EVERY PROVINCE x SPECIES x YEAR
# =====================================================================

province_species_year_check <- full_join(
  
  regional_clean %>%
    
    group_by(
      prov,
      species,
      year
    ) %>%
    
    summarise(
      regional_n =
        sum(
          n,
          na.rm = TRUE
        ),
      
      .groups =
        "drop"
    ),
  
  hunt_clean %>%
    
    group_by(
      prov,
      species,
      year
    ) %>%
    
    summarise(
      hunt_n =
        sum(
          n,
          na.rm = TRUE
        ),
      
      .groups =
        "drop"
    ),
  
  by = c(
    "prov",
    "species",
    "year"
  )
  
) %>%
  
  mutate(
    regional_n =
      coalesce(
        regional_n,
        0
      ),
    
    hunt_n =
      coalesce(
        hunt_n,
        0
      ),
    
    difference =
      regional_n -
      hunt_n
  )


fwrite(
  province_species_year_check,
  file.path(
    output_dir,
    "province_species_year_totals_check.csv"
  )
)


bad_total_checks <- province_species_year_check %>%
  filter(
    abs(difference) >
      tolerance
  )


if (nrow(bad_total_checks) > 0) {
  
  print(
    bad_total_checks,
    n = Inf
  )
  
  stop(
    "Regional and hunt-site totals differ for at least one ",
    "province x species x year."
  )
}


# =====================================================================
# 9. READ CURRENT REGION GEOMETRY
# =====================================================================

regions <- st_read(
  region_gpkg,
  quiet = TRUE
) %>%
  
  mutate(
    prov =
      normalise_prov(
        prov
      ),
    
    region_current =
      normalise_region(
        region,
        prov
      )
  )


if (any(!st_is_valid(regions), na.rm = TRUE)) {
  regions <- st_make_valid(
    regions
  )
}


duplicate_current_regions <- regions %>%
  
  st_drop_geometry() %>%
  
  count(
    prov,
    region_current,
    name = "n_features"
  ) %>%
  
  filter(
    n_features > 1
  )


if (nrow(duplicate_current_regions) > 0) {
  
  fwrite(
    duplicate_current_regions,
    file.path(
      output_dir,
      "duplicate_current_region_geometries.csv"
    )
  )
  
  stop(
    "Current region GPKG is not unique by province + region."
  )
}


# =====================================================================
# 10. READ CURRENT HUNT-SITE POLYGONS
# =====================================================================

reviere <- st_read(
  revier_gpkg,
  quiet = TRUE
) %>%
  
  mutate(
    poly_id =
      row_number(),
    
    hunt_site =
      normalise_hunt_site(
        hunt_site
      ),
    
    prov =
      normalise_prov(
        prov
      )
  )


if (any(!st_is_valid(reviere), na.rm = TRUE)) {
  reviere <- st_make_valid(
    reviere
  )
}


if (any(st_is_empty(reviere))) {
  stop(
    "Empty hunt-site geometries exist."
  )
}


# =====================================================================
# 11. ASSIGN EVERY CURRENT REVIER POLYGON TO A CURRENT REGION
#
# Current polygon region is determined spatially from merged_regions_unique.
# We deliberately do NOT overwrite it with historical/source region codes.
# =====================================================================

regions_for_join <- regions %>%
  
  st_transform(
    st_crs(
      reviere
    )
  ) %>%
  
  select(
    prov_region =
      prov,
    
    region_current
  )


# Use a point guaranteed to lie within each polygon.
revier_points <- suppressWarnings(
  st_point_on_surface(
    reviere %>%
      select(
        poly_id,
        prov
      )
  )
)


revier_region_join <- st_join(
  
  revier_points,
  
  regions_for_join,
  
  join =
    st_within,
  
  left =
    TRUE
) %>%
  
  st_drop_geometry() %>%
  
  filter(
    is.na(prov_region) |
      prov == prov_region
  ) %>%
  
  select(
    poly_id,
    region_current
  )


duplicate_spatial_matches <- revier_region_join %>%
  
  count(
    poly_id,
    name = "n_matches"
  ) %>%
  
  filter(
    n_matches > 1
  )


if (nrow(duplicate_spatial_matches) > 0) {
  
  fwrite(
    duplicate_spatial_matches,
    file.path(
      output_dir,
      "duplicate_spatial_region_matches.csv"
    )
  )
  
  stop(
    "Some current hunt-site polygons matched more than one current region."
  )
}


reviere <- reviere %>%
  
  left_join(
    revier_region_join,
    by = "poly_id"
  )


polygons_without_current_region <- reviere %>%
  
  st_drop_geometry() %>%
  
  filter(
    is.na(region_current)
  )


fwrite(
  polygons_without_current_region,
  file.path(
    output_dir,
    "polygons_without_current_region.csv"
  )
)


# =====================================================================
# 12. EXACT HUNT-SITE-ID MATCH BETWEEN SOURCE CSV AND CURRENT POLYGONS
# =====================================================================

hunt_site_lookup <- hunt_clean %>%
  
  filter(
    !is.na(hunt_site)
  ) %>%
  
  distinct(
    hunt_site,
    prov,
    region_source
  )


ambiguous_hunt_site_regions <- hunt_site_lookup %>%
  
  count(
    hunt_site,
    prov,
    name = "n_regions"
  ) %>%
  
  filter(
    n_regions > 1
  )


if (nrow(ambiguous_hunt_site_regions) > 0) {
  
  fwrite(
    ambiguous_hunt_site_regions,
    file.path(
      output_dir,
      "ambiguous_hunt_site_source_regions.csv"
    )
  )
  
  stop(
    "Some hunt_site IDs belong to multiple source regions."
  )
}


reviere <- reviere %>%
  
  left_join(
    hunt_site_lookup %>%
      rename(
        region_source_if_exact_match =
          region_source
      ),
    
    by = c(
      "hunt_site",
      "prov"
    )
  ) %>%
  
  mutate(
    has_exact_hunt_site_match =
      !is.na(
        region_source_if_exact_match
      )
  )


exact_match_region_mismatches <- reviere %>%
  
  st_drop_geometry() %>%
  
  filter(
    has_exact_hunt_site_match,
    !is.na(region_current),
    region_source_if_exact_match !=
      region_current
  )


fwrite(
  exact_match_region_mismatches,
  file.path(
    output_dir,
    "exact_match_source_vs_current_region_mismatches.csv"
  )
)


# =====================================================================
# 13. SOURCE-REGION VS CURRENT-REGION CODE DIAGNOSTIC
#
# This is the key diagnostic before allocation.
#
# "source_only"  = regional/hunt source code exists, but no current
#                  region geometry with that code
#
# "current_only" = current region geometry exists, but no regional
#                  source total uses that code
#
# These must NOT silently be treated as zero.
# =====================================================================

source_region_codes <- regional_clean %>%
  
  distinct(
    prov,
    region
  ) %>%
  
  rename(
    region_code =
      region
  ) %>%
  
  mutate(
    in_source =
      TRUE
  )


current_region_codes <- regions %>%
  
  st_drop_geometry() %>%
  
  distinct(
    prov,
    region_current
  ) %>%
  
  rename(
    region_code =
      region_current
  ) %>%
  
  mutate(
    in_current_geometry =
      TRUE
  )


region_code_diagnostic <- full_join(
  
  source_region_codes,
  
  current_region_codes,
  
  by = c(
    "prov",
    "region_code"
  )
  
) %>%
  
  mutate(
    in_source =
      coalesce(
        in_source,
        FALSE
      ),
    
    in_current_geometry =
      coalesce(
        in_current_geometry,
        FALSE
      ),
    
    status = case_when(
      
      in_source &
        in_current_geometry ~
        "both",
      
      in_source &
        !in_current_geometry ~
        "source_only",
      
      !in_source &
        in_current_geometry ~
        "current_only",
      
      TRUE ~
        "unexpected"
    )
  ) %>%
  
  arrange(
    prov,
    status,
    region_code
  )


fwrite(
  region_code_diagnostic,
  file.path(
    output_dir,
    "region_code_diagnostic.csv"
  )
)


# =====================================================================
# 14. HUNT-SITE-ID COVERAGE DIAGNOSTIC
# =====================================================================

source_hunt_sites <- hunt_clean %>%
  
  filter(
    !is.na(hunt_site)
  ) %>%
  
  distinct(
    prov,
    hunt_site
  )


current_hunt_sites <- reviere %>%
  
  st_drop_geometry() %>%
  
  filter(
    !is.na(hunt_site)
  ) %>%
  
  distinct(
    prov,
    hunt_site
  )


hunt_site_id_diagnostic <- full_join(
  
  source_hunt_sites %>%
    mutate(
      in_hunt_csv =
        TRUE
    ),
  
  current_hunt_sites %>%
    mutate(
      in_current_gpkg =
        TRUE
    ),
  
  by = c(
    "prov",
    "hunt_site"
  )
  
) %>%
  
  mutate(
    in_hunt_csv =
      coalesce(
        in_hunt_csv,
        FALSE
      ),
    
    in_current_gpkg =
      coalesce(
        in_current_gpkg,
        FALSE
      ),
    
    status = case_when(
      
      in_hunt_csv &
        in_current_gpkg ~
        "exact_match",
      
      in_hunt_csv &
        !in_current_gpkg ~
        "source_hunt_site_without_current_polygon",
      
      !in_hunt_csv &
        in_current_gpkg ~
        "current_polygon_without_source_hunt_site",
      
      TRUE ~
        "unexpected"
    )
  )


fwrite(
  hunt_site_id_diagnostic,
  file.path(
    output_dir,
    "hunt_site_id_diagnostic.csv"
  )
)


# =====================================================================
# 15. SAVE CLEAN INPUTS
# =====================================================================

fwrite(
  hunt_clean,
  file.path(
    output_dir,
    "hunt_site_harvest_clean.csv"
  )
)


fwrite(
  regional_clean,
  file.path(
    output_dir,
    "regional_harvest_clean.csv"
  )
)


st_write(
  reviere,
  file.path(
    output_dir,
    "current_reviere_with_region.gpkg"
  ),
  delete_dsn = TRUE,
  quiet = TRUE
)


# =====================================================================
# 16. SUMMARY
# =====================================================================

cat("\n")
cat("============================================================\n")
cat("STEP 1 COMPLETE — CLEAN INPUTS / NO ALLOCATION YET\n")
cat("============================================================\n")

cat(
  "Regional total harvest: ",
  sum(
    regional_clean$n,
    na.rm = TRUE
  ),
  "\n"
)

cat(
  "Hunt-site total harvest: ",
  sum(
    hunt_clean$n,
    na.rm = TRUE
  ),
  "\n"
)

cat(
  "Province x species x year total mismatches: ",
  nrow(
    bad_total_checks
  ),
  "\n"
)

cat(
  "Current hunt-site polygons: ",
  nrow(
    reviere
  ),
  "\n"
)

cat(
  "Exact hunt_site matches: ",
  sum(
    reviere$has_exact_hunt_site_match,
    na.rm = TRUE
  ),
  "\n"
)

cat(
  "Current polygons without exact source hunt_site: ",
  sum(
    !reviere$has_exact_hunt_site_match
  ),
  "\n"
)

cat(
  "Current polygons without spatial current region: ",
  nrow(
    polygons_without_current_region
  ),
  "\n"
)

cat(
  "Source/current region mismatches among exact hunt_site matches: ",
  nrow(
    exact_match_region_mismatches
  ),
  "\n"
)

cat("\nRegion-code status:\n")

print(
  region_code_diagnostic %>%
    count(
      prov,
      status
    )
)

cat("\nSource-only region codes:\n")

print(
  region_code_diagnostic %>%
    filter(
      status ==
        "source_only"
    ),
  n = Inf
)

cat("\nCurrent-only region codes:\n")

print(
  region_code_diagnostic %>%
    filter(
      status ==
        "current_only"
    ),
  n = Inf
)

cat("\nHunt-site ID status:\n")

print(
  hunt_site_id_diagnostic %>%
    count(
      prov,
      status
    )
)

cat(
  "\nOutputs written to:\n",
  output_dir,
  "\n"
)

cat("============================================================\n")



# =====================================================================
# STEP 2 — BUILD STABLE ALLOCATION GROUPS
# =====================================================================
#
# This script performs NO allocation.
#
# Input:
#   outputs from 01_prepare_clean_inputs.R
#
# Purpose:
#   - create stable spatial allocation groups
#   - keep observed hunt-site values untouched
#   - resolve historical/current region-code inconsistencies without
#     inventing one-to-one replacements
#   - verify that regional totals and hunt-site totals agree inside every
#     allocation group, species and year
#
# After this script succeeds, Step 3 can perform integer suitability-
# weighted allocation.
# =====================================================================


# =====================================================================
# 0. PACKAGES
# =====================================================================

library(sf)
library(dplyr)
library(tidyr)
library(data.table)
library(tibble)
library(stringr)


# =====================================================================
# 1. PATHS
# =====================================================================

input_dir <-
  "/mnt/eo/WilDensity/output/prepared_allocation_inputs"

hunt_csv <-
  file.path(
    input_dir,
    "hunt_site_harvest_clean.csv"
  )

regional_csv <-
  file.path(
    input_dir,
    "regional_harvest_clean.csv"
  )

reviere_gpkg <-
  file.path(
    input_dir,
    "current_reviere_with_region.gpkg"
  )

output_dir <-
  "/mnt/eo/WilDensity/output/allocation_groups_final"

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

tolerance <- 1e-8


# =====================================================================
# 2. READ STEP-1 OUTPUTS
# =====================================================================

hunt <- fread(
  hunt_csv,
  colClasses = list(
    character = c(
      "hunt_site",
      "species",
      "prov",
      "region_original",
      "region_source"
    )
  )
) %>%
  as_tibble()


regional <- fread(
  regional_csv,
  colClasses = list(
    character = c(
      "prov",
      "region",
      "species"
    )
  )
) %>%
  as_tibble()


reviere <- st_read(
  reviere_gpkg,
  quiet = TRUE
)


# =====================================================================
# 3. DEFINE FINAL STABLE GROUPS
# =====================================================================
#
# SALZBURG
# --------
# These groups combine:
#   a) the historical region-code inconsistencies already identified
#      from regional totals versus hunt-site totals, and
#   b) the exact hunt-site cases where the source region differs from
#      the present-day spatial region.
#
# The former groups involving
#   5.4 + 10.2 + 10.4 + 10.5
# and
#   10.7 + 11.1 + 12.8
# are merged here because one exact hunt-site mismatch links
# 5.4 <-> 11.1.
#
# 10.6 is included because one exact hunt-site mismatch links
# 10.6 <-> 10.4.
#
# STEIERMARK
# ----------
# Only problematic 3-digit region families are harmonised.
#
# Example:
#   12503, 12506, 12507, 12520 occur only in the current geometry,
#   while 12522 and 12529 occur only in the source data.
#
# Instead of inventing pairwise replacements, all 125xx regions are
# treated as one stable allocation unit.
#
# The same conservative rule is used for other Styria families that
# contain source-only region codes:
#   115xx, 155xx, 175xx, 250xx
#
# All other Styria regions remain singleton groups.
# =====================================================================


sbg_groups <- list(
  
  c(
    "2.3",
    "6.1"
  ),
  
  c(
    "10.1",
    "10.3"
  ),
  
  c(
    "7.1",
    "8.6"
  ),
  
  c(
    "3.3",
    "4.2"
  ),
  
  c(
    "5.2",
    "5.3",
    "9.3"
  ),
  
  c(
    "6.3",
    "6.4",
    "8.1",
    "8.2",
    "9.2"
  ),
  
  c(
    "1.2",
    "1.3",
    "2.1",
    "2.2",
    "3.1",
    "4.3",
    "5.1",
    "8.4"
  ),
  
  c(
    "5.4",
    "10.2",
    "10.4",
    "10.5",
    "10.6",
    "10.7",
    "11.1",
    "12.8"
  )
)


styria_harmonised_prefixes <- c(
  "115",
  "125",
  "155",
  "175",
  "250"
)


# =====================================================================
# 4. BUILD LOOKUP FROM UNION OF SOURCE + CURRENT REGION CODES
# =====================================================================

source_regions <- regional %>%
  
  distinct(
    prov,
    region
  )


current_regions <- reviere %>%
  
  st_drop_geometry() %>%
  
  filter(
    !is.na(region_current)
  ) %>%
  
  distinct(
    prov,
    region =
      region_current
  )


all_regions <- bind_rows(
  source_regions,
  current_regions
) %>%
  
  distinct(
    prov,
    region
  )


# ---------------------------------------------------------------------
# Salzburg lookup
# ---------------------------------------------------------------------

sbg_multi_lookup <- bind_rows(
  
  lapply(
    sbg_groups,
    function(x) {
      
      tibble(
        prov =
          "sbg",
        
        region =
          x,
        
        allocation_group =
          paste0(
            "sbg::",
            paste(
              sort(x),
              collapse = "+"
            )
          ),
        
        group_type =
          "harmonised_multi_region"
      )
    }
  )
)


# ---------------------------------------------------------------------
# Final lookup
# ---------------------------------------------------------------------

allocation_group_lookup <- all_regions %>%
  
  left_join(
    sbg_multi_lookup,
    by = c(
      "prov",
      "region"
    )
  ) %>%
  
  mutate(
    
    styria_prefix =
      if_else(
        prov == "styria" &
          !is.na(region) &
          nchar(region) >= 3,
        substr(
          region,
          1,
          3
        ),
        NA_character_
      ),
    
    allocation_group = case_when(
      
      # Salzburg region already assigned to explicit stable group
      prov == "sbg" &
        !is.na(allocation_group) ~
        allocation_group,
      
      # Other Salzburg regions remain singleton
      prov == "sbg" ~
        paste0(
          "sbg::",
          region
        ),
      
      # Problematic Styria family -> one stable broader group
      prov == "styria" &
        styria_prefix %in%
        styria_harmonised_prefixes ~
        paste0(
          "styria::",
          styria_prefix,
          "xx"
        ),
      
      # Other Styria regions remain singleton
      prov == "styria" ~
        paste0(
          "styria::",
          region
        ),
      
      TRUE ~
        paste0(
          prov,
          "::",
          region
        )
    ),
    
    group_type = case_when(
      
      prov == "sbg" &
        !is.na(group_type) ~
        group_type,
      
      prov == "styria" &
        styria_prefix %in%
        styria_harmonised_prefixes ~
        "harmonised_3digit_family",
      
      TRUE ~
        "singleton"
    )
  ) %>%
  
  select(
    prov,
    region,
    allocation_group,
    group_type
  ) %>%
  
  arrange(
    prov,
    allocation_group,
    region
  )


fwrite(
  allocation_group_lookup,
  file.path(
    output_dir,
    "allocation_group_lookup.csv"
  )
)


# =====================================================================
# 5. CHECK THAT EVERY SOURCE + CURRENT REGION RECEIVED A GROUP
# =====================================================================

if (
  any(
    is.na(
      allocation_group_lookup$allocation_group
    )
  )
) {
  stop(
    "At least one region did not receive an allocation group."
  )
}


# =====================================================================
# 6. ATTACH GROUPS TO REGIONAL TOTALS
# =====================================================================

regional_grouped <- regional %>%
  
  left_join(
    allocation_group_lookup %>%
      select(
        prov,
        region,
        allocation_group,
        group_type
      ),
    
    by = c(
      "prov",
      "region"
    )
  )


if (
  any(
    is.na(
      regional_grouped$allocation_group
    )
  )
) {
  stop(
    "At least one regional harvest row has no allocation group."
  )
}


regional_totals_group <- regional_grouped %>%
  
  group_by(
    prov,
    allocation_group,
    species,
    year
  ) %>%
  
  summarise(
    regional_total_n =
      sum(
        n,
        na.rm = TRUE
      ),
    
    .groups =
      "drop"
  )


fwrite(
  regional_totals_group,
  file.path(
    output_dir,
    "regional_totals_by_allocation_group.csv"
  )
)


# =====================================================================
# 7. ATTACH GROUPS TO HUNT-SITE SOURCE DATA
# =====================================================================

hunt_grouped <- hunt %>%
  
  left_join(
    allocation_group_lookup %>%
      select(
        prov,
        region,
        allocation_group
      ),
    
    by = c(
      "prov",
      "region_source" =
        "region"
    )
  )


if (
  any(
    is.na(
      hunt_grouped$allocation_group
    )
  )
) {
  
  missing_hunt_groups <- hunt_grouped %>%
    
    filter(
      is.na(
        allocation_group
      )
    )
  
  fwrite(
    missing_hunt_groups,
    file.path(
      output_dir,
      "ERROR_hunt_rows_without_allocation_group.csv"
    )
  )
  
  stop(
    "At least one hunt-site source row has no allocation group."
  )
}


hunt_totals_group <- hunt_grouped %>%
  
  group_by(
    prov,
    allocation_group,
    species,
    year
  ) %>%
  
  summarise(
    hunt_total_n =
      sum(
        n,
        na.rm = TRUE
      ),
    
    .groups =
      "drop"
  )


# =====================================================================
# 8. HARD CHECK:
# REGIONAL TOTAL == HUNT-SITE TOTAL WITHIN EVERY GROUP x SPECIES x YEAR
# =====================================================================

group_total_check <- full_join(
  
  regional_totals_group,
  
  hunt_totals_group,
  
  by = c(
    "prov",
    "allocation_group",
    "species",
    "year"
  )
  
) %>%
  
  mutate(
    
    regional_total_n =
      coalesce(
        regional_total_n,
        0
      ),
    
    hunt_total_n =
      coalesce(
        hunt_total_n,
        0
      ),
    
    difference =
      regional_total_n -
      hunt_total_n
  )


fwrite(
  group_total_check,
  file.path(
    output_dir,
    "group_total_consistency_check.csv"
  )
)


bad_group_totals <- group_total_check %>%
  
  filter(
    abs(difference) >
      tolerance
  )


if (
  nrow(
    bad_group_totals
  ) > 0
) {
  
  fwrite(
    bad_group_totals,
    file.path(
      output_dir,
      "ERROR_group_total_mismatches.csv"
    )
  )
  
  print(
    bad_group_totals,
    n = 100
  )
  
  stop(
    "Regional totals and hunt-site totals do not match ",
    "inside at least one allocation group x species x year."
  )
}


# =====================================================================
# 9. ATTACH GROUPS TO CURRENT REVIER POLYGONS
# =====================================================================

reviere_grouped <- reviere %>%
  
  left_join(
    allocation_group_lookup %>%
      select(
        prov,
        region =
          region,
        allocation_group_current =
          allocation_group,
        group_type_current =
          group_type
      ),
    
    by = c(
      "prov",
      "region_current" =
        "region"
    )
  )


# Polygons without hunt_site IDs remain INCLUDED when their current
# region/allocation group is known. Only geometries without a usable
# current region/allocation group are excluded.
reviere_grouped <- reviere_grouped %>%
  
  mutate(
    # A hunt_site ID is NOT required for allocation.
    # Polygons without a source hunt_site are precisely valid candidates
    # for estimation, provided they have a current region and therefore
    # a valid allocation group.
    include_in_allocation =
      !is.na(
        region_current
      ) &
      !is.na(
        allocation_group_current
      )
  )


excluded_polygons <- reviere_grouped %>%
  
  st_drop_geometry() %>%
  
  filter(
    !include_in_allocation
  )


fwrite(
  excluded_polygons,
  file.path(
    output_dir,
    "excluded_current_polygons.csv"
  )
)


st_write(
  reviere_grouped,
  file.path(
    output_dir,
    "reviere_with_allocation_group.gpkg"
  ),
  delete_dsn = TRUE,
  quiet = TRUE
)


# =====================================================================
# 10. CHECK EXACT HUNT-SITE MATCHES:
# SOURCE REGION AND CURRENT REGION MUST NOW BELONG TO SAME GROUP
# =====================================================================

exact_crosswalk_check <- reviere_grouped %>%
  
  st_drop_geometry() %>%
  
  filter(
    has_exact_hunt_site_match,
    !is.na(
      region_source_if_exact_match
    ),
    !is.na(
      region_current
    )
  ) %>%
  
  left_join(
    allocation_group_lookup %>%
      
      select(
        prov,
        region,
        allocation_group_source =
          allocation_group
      ),
    
    by = c(
      "prov",
      "region_source_if_exact_match" =
        "region"
    )
  ) %>%
  
  mutate(
    same_allocation_group =
      allocation_group_source ==
      allocation_group_current
  )


exact_crosswalk_mismatches <- exact_crosswalk_check %>%
  
  filter(
    !same_allocation_group
  )


fwrite(
  exact_crosswalk_check,
  file.path(
    output_dir,
    "exact_hunt_site_crosswalk_check.csv"
  )
)


if (
  nrow(
    exact_crosswalk_mismatches
  ) > 0
) {
  
  fwrite(
    exact_crosswalk_mismatches,
    file.path(
      output_dir,
      "ERROR_exact_hunt_site_crosswalk_mismatches.csv"
    )
  )
  
  print(
    exact_crosswalk_mismatches,
    n = Inf
  )
  
  stop(
    "Some exact hunt-site matches still cross allocation groups."
  )
}


# =====================================================================
# 11. GROUP PRESENCE:
# WHICH GROUPS HAVE SOURCE TOTALS AND/OR CURRENT POLYGONS?
# =====================================================================

source_group_presence <- regional_grouped %>%
  
  distinct(
    prov,
    allocation_group
  ) %>%
  
  mutate(
    has_source_total =
      TRUE
  )


current_group_presence <- reviere_grouped %>%
  
  st_drop_geometry() %>%
  
  filter(
    include_in_allocation
  ) %>%
  
  distinct(
    prov,
    allocation_group =
      allocation_group_current
  ) %>%
  
  mutate(
    has_current_polygon =
      TRUE
  )


group_presence <- full_join(
  
  source_group_presence,
  
  current_group_presence,
  
  by = c(
    "prov",
    "allocation_group"
  )
  
) %>%
  
  mutate(
    
    has_source_total =
      coalesce(
        has_source_total,
        FALSE
      ),
    
    has_current_polygon =
      coalesce(
        has_current_polygon,
        FALSE
      ),
    
    status = case_when(
      
      has_source_total &
        has_current_polygon ~
        "source_and_current",
      
      has_source_total &
        !has_current_polygon ~
        "source_only_group",
      
      !has_source_total &
        has_current_polygon ~
        "current_only_group",
      
      TRUE ~
        "unexpected"
    )
  ) %>%
  
  arrange(
    prov,
    status,
    allocation_group
  )


fwrite(
  group_presence,
  file.path(
    output_dir,
    "allocation_group_presence.csv"
  )
)


current_only_groups <- group_presence %>%
  
  filter(
    status ==
      "current_only_group"
  )


if (
  nrow(
    current_only_groups
  ) > 0
) {
  
  print(
    current_only_groups,
    n = Inf
  )
  
  stop(
    "At least one current polygon allocation group has no ",
    "regional source total."
  )
}


# =====================================================================
# 12. SUMMARY OF GROUP MEMBERS
# =====================================================================

group_members <- allocation_group_lookup %>%
  
  group_by(
    prov,
    allocation_group,
    group_type
  ) %>%
  
  summarise(
    
    n_regions =
      n(),
    
    regions =
      paste(
        sort(region),
        collapse = ", "
      ),
    
    .groups =
      "drop"
  )


fwrite(
  group_members,
  file.path(
    output_dir,
    "allocation_group_members.csv"
  )
)


# =====================================================================
# 13. FINAL SUMMARY
# =====================================================================

cat("\n")
cat("============================================================\n")
cat("STEP 2 COMPLETE — STABLE ALLOCATION GROUPS / NO ALLOCATION\n")
cat("============================================================\n")

cat(
  "Allocation groups total:                 ",
  n_distinct(
    allocation_group_lookup$allocation_group
  ),
  "\n"
)

cat(
  "Harmonised Salzburg groups:              ",
  n_distinct(
    allocation_group_lookup$allocation_group[
      allocation_group_lookup$prov == "sbg" &
        allocation_group_lookup$group_type ==
        "harmonised_multi_region"
    ]
  ),
  "\n"
)

cat(
  "Harmonised Styria 3-digit groups:        ",
  n_distinct(
    allocation_group_lookup$allocation_group[
      allocation_group_lookup$prov == "styria" &
        allocation_group_lookup$group_type ==
        "harmonised_3digit_family"
    ]
  ),
  "\n"
)

cat(
  "Group total mismatches:                  ",
  nrow(
    bad_group_totals
  ),
  "\n"
)

cat(
  "Exact hunt-site cross-group mismatches:  ",
  nrow(
    exact_crosswalk_mismatches
  ),
  "\n"
)

cat(
  "Current-only allocation groups:          ",
  nrow(
    current_only_groups
  ),
  "\n"
)

cat(
  "Excluded current polygons:               ",
  nrow(
    excluded_polygons
  ),
  "\n"
)

cat("\nGroup presence:\n")

print(
  group_presence %>%
    count(
      prov,
      status
    )
)

cat("\nSource-only groups retained as fallback candidates:\n")

print(
  group_presence %>%
    filter(
      status ==
        "source_only_group"
    ),
  n = Inf
)

cat("\nOutputs written to:\n")
cat(
  output_dir,
  "\n"
)

cat("============================================================\n")



# =====================================================================
# STEP 3 — INTEGER SUITABILITY-WEIGHTED HARVEST ALLOCATION
# =====================================================================
#
# Goal
# ----
# Create complete current hunt-site polygon time series:
#
#   Styria:   1992-2024
#   Salzburg: 1998-2024
#   Species:  chamois, roe_deer, red_deer
#
# Rules
# -----
# 1) Existing observed harvest values are NEVER changed.
#
# 2) A missing hunt-site x species x year value is treated as MISSING,
#    not as zero. This also applies when another species is observed in
#    the same hunt-site-year.
#
# 3) Allocation targets are therefore defined independently for every
#    polygon x species x year combination.
#
# 4) For every allocation_group x species x year:
#
#       residual =
#         independent regional/group total
#         - direct mapped polygon harvest
#
# 5) The positive residual is distributed only to target polygons,
#    weighted by species-specific suitability.
#
# 6) All allocated harvest values are INTEGERS.
#    Integerisation uses the Hamilton / largest-remainder method:
#       - floor proportional values
#       - distribute remaining animals to the largest decimal remainders
#
#    Therefore:
#       sum(integer allocations) == residual exactly
#
# 7) Positive residual that cannot be assigned to a target polygon is
#    retained explicitly as fallback and is NOT forced onto observed
#    polygons.
#
# 8) Final conservation check:
#
#       direct polygon harvest
#       + allocated polygon harvest
#       + fallback
#       == independent regional/group total
#
# =====================================================================


# =====================================================================
# 0. PACKAGES
# =====================================================================

library(sf)
library(dplyr)
library(tidyr)
library(data.table)
library(tibble)
library(purrr)
library(terra)
library(exactextractr)
library(ggplot2)
library(scales)


# =====================================================================
# 1. PATHS
# =====================================================================

prepared_dir <-
  "/mnt/eo/WilDensity/output/prepared_allocation_inputs"

group_dir <-
  "/mnt/eo/WilDensity/output/allocation_groups_final"

hunt_csv <-
  file.path(
    prepared_dir,
    "hunt_site_harvest_clean.csv"
  )

reviere_gpkg <-
  file.path(
    group_dir,
    "reviere_with_allocation_group.gpkg"
  )

regional_group_csv <-
  file.path(
    group_dir,
    "regional_totals_by_allocation_group.csv"
  )

output_dir <-
  "/mnt/eo/WilDensity/output/final_integer_allocation"

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)


# Species-specific suitability rasters
suitability_files <- c(
  
  chamois =
    "/mnt/eo/WilDensity/output/suitability_chamois_0311.tif",
  
  roe_deer =
    "/mnt/eo/WilDensity/output/suitability_roe_deer_0311.tif",
  
  red_deer =
    "/mnt/eo/WilDensity/output/suitability_red_deer_0311.tif"
)


# =====================================================================
# 2. SETTINGS
# =====================================================================

species_use <- c(
  "chamois",
  "roe_deer",
  "red_deer"
)

tolerance <- 1e-8

year_lookup <- bind_rows(
  
  tibble(
    prov = "sbg",
    year = 1998:2024
  ),
  
  tibble(
    prov = "styria",
    year = 1992:2024
  )
)


# =====================================================================
# 3. HELPERS
# =====================================================================

# ---------------------------------------------------------------------
# Integer largest-remainder allocation
# ---------------------------------------------------------------------
#
# Input:
#   residual_n          integer number of animals to distribute
#   suitability_area_ha non-negative suitability score/area per target
#
# Output:
#   integer n_allocated whose sum equals residual_n exactly
#
# Tie-breaking is deterministic:
#   1) larger fractional remainder
#   2) larger suitability
#   3) smaller poly_id
# ---------------------------------------------------------------------

largest_remainder_allocate <- function(
    residual_n,
    suitability_area_ha,
    poly_id
) {
  
  if (
    length(
      unique(
        residual_n
      )
    ) != 1
  ) {
    stop(
      "Residual is not unique within allocation group."
    )
  }
  
  R <- unique(
    residual_n
  )
  
  if (
    is.na(
      R
    )
  ) {
    stop(
      "Missing residual encountered."
    )
  }
  
  R <- as.integer(
    round(
      R
    )
  )
  
  if (
    R < 0
  ) {
    stop(
      "Negative residual passed to allocation function."
    )
  }
  
  n_targets <- length(
    suitability_area_ha
  )
  
  if (
    n_targets == 0
  ) {
    return(
      integer(0)
    )
  }
  
  if (
    R == 0
  ) {
    return(
      rep(
        0L,
        n_targets
      )
    )
  }
  
  if (
    any(
      is.na(
        suitability_area_ha
      )
    )
  ) {
    stop(
      "Missing suitability encountered in target polygons."
    )
  }
  
  if (
    any(
      suitability_area_ha < 0
    )
  ) {
    stop(
      "Negative suitability encountered."
    )
  }
  
  total_suitability <- sum(
    suitability_area_ha
  )
  
  # No suitable area:
  # no animals are spatially assigned here.
  # The residual is retained later as fallback.
  if (
    total_suitability <= 0
  ) {
    return(
      rep(
        0L,
        n_targets
      )
    )
  }
  
  weights <-
    suitability_area_ha /
    total_suitability
  
  raw_allocation <-
    R *
    weights
  
  base_allocation <- floor(
    raw_allocation
  )
  
  remainder <- raw_allocation -
    base_allocation
  
  animals_left <- as.integer(
    R -
      sum(
        base_allocation
      )
  )
  
  n_allocated <- as.integer(
    base_allocation
  )
  
  if (
    animals_left > 0
  ) {
    
    allocation_order <- order(
      -remainder,
      -suitability_area_ha,
      poly_id
    )
    
    take <- allocation_order[
      seq_len(
        animals_left
      )
    ]
    
    n_allocated[take] <-
      n_allocated[take] +
      1L
  }
  
  if (
    sum(
      n_allocated
    ) != R
  ) {
    stop(
      "Largest-remainder allocation does not conserve residual."
    )
  }
  
  n_allocated
}


# ---------------------------------------------------------------------
# Suitability extraction
# ---------------------------------------------------------------------

extract_suitability <- function(
    raster_file,
    species_name,
    polygons
) {
  
  cat(
    "\nExtracting suitability for ",
    species_name,
    " ...\n",
    sep = ""
  )
  
  r <- terra::rast(
    raster_file
  )
  
  if (
    terra::nlyr(
      r
    ) != 1
  ) {
    stop(
      "Suitability raster contains more than one layer: ",
      raster_file
    )
  }
  
  polygons_r <- st_transform(
    polygons,
    terra::crs(
      r
    )
  )
  
  # Cell area in hectares.
  # Multiplying raster value by cell area means:
  #
  # binary raster:
  #   sum = suitable area in ha
  #
  # continuous 0-1 raster:
  #   sum = suitability-weighted area in ha
  cell_area_ha <- terra::cellSize(
    r,
    unit = "ha"
  )
  
  effective_area <-
    r *
    cell_area_ha
  
  suitable_area <- exactextractr::exact_extract(
    effective_area,
    polygons_r,
    fun = "sum",
    progress = TRUE
  )
  
  tibble(
    poly_id =
      polygons_r$poly_id,
    
    species =
      species_name,
    
    suitability_area_ha =
      as.numeric(
        suitable_area
      )
  )
}


# =====================================================================
# 4. READ CLEAN INPUTS
# =====================================================================

hunt <- fread(
  hunt_csv,
  colClasses = list(
    character = c(
      "hunt_site",
      "species",
      "prov",
      "region_original",
      "region_source"
    )
  )
) %>%
  as_tibble()


reviere_all <- st_read(
  reviere_gpkg,
  quiet = TRUE
)


regional_group_all <- fread(
  regional_group_csv,
  colClasses = list(
    character = c(
      "prov",
      "allocation_group",
      "species"
    )
  )
) %>%
  as_tibble() %>%
  
  mutate(
    year =
      as.integer(
        year
      ),
    
    regional_total_n =
      as.integer(
        round(
          regional_total_n
        )
      )
  )


# Keep only the study periods used for the final polygon time series:
#   Salzburg: 1998-2024
#   Styria:   1992-2024
#
# Rows outside these periods must not enter fallback or conservation
# checks, because no final polygon-year values are created for them.

regional_group_outside_study_period <- regional_group_all %>%
  
  anti_join(
    year_lookup,
    by = c(
      "prov",
      "year"
    )
  )


fwrite(
  regional_group_outside_study_period,
  file.path(
    output_dir,
    "regional_group_outside_study_period.csv"
  )
)


regional_group <- regional_group_all %>%
  
  semi_join(
    year_lookup,
    by = c(
      "prov",
      "year"
    )
  )


# =====================================================================
# 5. KEEP VALID CURRENT POLYGONS ONLY
# =====================================================================

reviere <- reviere_all %>%
  
  filter(
    include_in_allocation
  )


excluded_polygons <- reviere_all %>%
  
  filter(
    !include_in_allocation
  )


cat(
  "\nCurrent polygons used for allocation:",
  nrow(
    reviere
  ),
  "\n"
)

cat(
  "Excluded polygons:",
  nrow(
    excluded_polygons
  ),
  "\n"
)


# Every included polygon needs a valid current allocation group.
if (
  any(
    is.na(
      reviere$allocation_group_current
    )
  )
) {
  stop(
    "Included polygon without allocation group."
  )
}


# poly_id must be unique
if (
  anyDuplicated(
    reviere$poly_id
  ) > 0
) {
  stop(
    "poly_id is not unique."
  )
}


# Current hunt_site IDs, where present, must also be unique.
duplicate_current_hunt_sites <- reviere %>%
  
  st_drop_geometry() %>%
  
  filter(
    !is.na(
      hunt_site
    )
  ) %>%
  
  count(
    prov,
    hunt_site,
    name = "n_polygons"
  ) %>%
  
  filter(
    n_polygons > 1
  )


if (
  nrow(
    duplicate_current_hunt_sites
  ) > 0
) {
  
  fwrite(
    duplicate_current_hunt_sites,
    file.path(
      output_dir,
      "ERROR_duplicate_current_hunt_sites.csv"
    )
  )
  
  stop(
    "A current hunt_site ID occurs on more than one polygon."
  )
}


# =====================================================================
# 6. MAP OBSERVED SOURCE VALUES TO CURRENT POLYGONS
# =====================================================================
#
# IMPORTANT CHANGE
# ----------------
# The hunt-site source table is sparse. A missing species row is NOT
# converted to zero here.
#
# Example:
#   roe deer observed, red deer missing, chamois missing
#
# becomes:
#   roe deer  = observed value
#   red deer  = allocation target
#   chamois   = allocation target
#
# Each polygon x species x year is treated independently.
# =====================================================================

polygon_lookup <- reviere %>%
  
  st_drop_geometry() %>%
  
  filter(
    !is.na(
      hunt_site
    )
  ) %>%
  
  select(
    poly_id,
    prov,
    hunt_site,
    allocation_group_current
  )


mapped_direct <- hunt %>%
  
  inner_join(
    polygon_lookup,
    by = c(
      "prov",
      "hunt_site"
    )
  ) %>%
  
  semi_join(
    year_lookup,
    by = c(
      "prov",
      "year"
    )
  ) %>%
  
  transmute(
    poly_id,
    prov,
    allocation_group =
      allocation_group_current,
    year =
      as.integer(
        year
      ),
    species,
    n_direct =
      as.integer(
        n
      )
  )


# There must be at most one observed value per current
# polygon x species x year.
duplicate_mapped_direct <- mapped_direct %>%
  
  count(
    poly_id,
    species,
    year,
    name = "n_rows"
  ) %>%
  
  filter(
    n_rows > 1
  )


if (
  nrow(
    duplicate_mapped_direct
  ) > 0
) {
  
  fwrite(
    duplicate_mapped_direct,
    file.path(
      output_dir,
      "ERROR_duplicate_mapped_direct_values.csv"
    )
  )
  
  stop(
    "Duplicate observed polygon x species x year values exist."
  )
}


# =====================================================================
# 7. BUILD COMPLETE CURRENT POLYGON x YEAR x SPECIES GRID
# =====================================================================

revier_attributes <- reviere %>%
  
  st_drop_geometry() %>%
  
  select(
    poly_id,
    hunt_site,
    name,
    prov,
    region_current,
    allocation_group =
      allocation_group_current,
    group_type =
      group_type_current
  )


polygon_year_grid <- revier_attributes %>%
  
  inner_join(
    year_lookup,
    by = "prov",
    relationship = "many-to-many"
  )


full_grid <- tidyr::crossing(
  polygon_year_grid,
  species =
    species_use
) %>%
  
  left_join(
    
    mapped_direct %>%
      select(
        poly_id,
        year,
        species,
        n_direct
      ),
    
    by = c(
      "poly_id",
      "year",
      "species"
    )
  ) %>%
  
  mutate(
    is_observed =
      !is.na(
        n_direct
      ),
    
    needs_allocation =
      is.na(
        n_direct
      )
  )


allocation_targets <- full_grid %>%
  
  filter(
    needs_allocation
  )


cat(
  "Expected polygon x species x year values:",
  nrow(
    full_grid
  ),
  "\n"
)

cat(
  "Observed polygon x species x year values:",
  sum(
    full_grid$is_observed
  ),
  "\n"
)

cat(
  "Allocation target values:",
  nrow(
    allocation_targets
  ),
  "\n"
)


# =====================================================================
# 8. DIAGNOSTIC: PARTIALLY OBSERVED POLYGON-YEARS
# =====================================================================
#
# This explicitly records the cases that were previously zero-completed.
# They are now allocation targets for the missing species.
# =====================================================================

partial_polygon_years <- full_grid %>%
  
  group_by(
    poly_id,
    prov,
    year
  ) %>%
  
  summarise(
    n_species_observed =
      sum(
        is_observed
      ),
    
    n_species_missing =
      sum(
        needs_allocation
      ),
    
    .groups =
      "drop"
  ) %>%
  
  filter(
    n_species_observed > 0,
    n_species_missing > 0
  )


fwrite(
  partial_polygon_years,
  file.path(
    output_dir,
    "partial_polygon_years_now_allocated.csv"
  )
)


# =====================================================================

# 9. REQUIRED REGIONAL/GROUP TOTALS FOR CURRENT POLYGONS
#    (ABSENT ROWS IN THE SPARSE SOURCE TABLE = TRUE ZERO)
# =====================================================================

required_group_keys <- full_grid %>%
  
  distinct(
    prov,
    allocation_group,
    species,
    year
  )


regional_required <- required_group_keys %>%
  
  left_join(
    regional_group,
    by = c(
      "prov",
      "allocation_group",
      "species",
      "year"
    )
  )


# IMPORTANT:
# Both original harvest tables are sparse count tables:
# rows with n = 0 are not stored at all (all stored n values are > 0).
#
# Therefore a missing allocation_group x species x year row in the
# regional table represents a TRUE ZERO, not an unknown value.
#
# We still write all such implicit zeros explicitly for auditing.

implicit_zero_regional_totals <- regional_required %>%
  
  filter(
    is.na(
      regional_total_n
    )
  ) %>%
  
  mutate(
    regional_total_n =
      0L,
    regional_total_source =
      "implicit_zero_absent_from_sparse_source"
  )


fwrite(
  implicit_zero_regional_totals,
  file.path(
    output_dir,
    "implicit_zero_regional_totals.csv"
  )
)


regional_required <- regional_required %>%
  
  mutate(
    regional_total_source = case_when(
      
      !is.na(
        regional_total_n
      ) ~
        "explicit_positive_source_row",
      
      TRUE ~
        "implicit_zero_absent_from_sparse_source"
    ),
    
    regional_total_n =
      coalesce(
        regional_total_n,
        0L
      )
  )


cat(
  "Implicit regional zeros from sparse source table:",
  nrow(
    implicit_zero_regional_totals
  ),
  "\n"
)


# =====================================================================
# 10. DIRECT MAPPED HARVEST BY GROUP x SPECIES x YEAR
# =====================================================================

direct_group_sum <- full_grid %>%
  
  filter(
    is_observed
  ) %>%
  
  group_by(
    prov,
    allocation_group,
    species,
    year
  ) %>%
  
  summarise(
    direct_mapped_n =
      sum(
        n_direct
      ),
    
    n_direct_polygons =
      n_distinct(
        poly_id
      ),
    
    .groups =
      "drop"
  )


target_group_count <- allocation_targets %>%
  
  group_by(
    prov,
    allocation_group,
    species,
    year
  ) %>%
  
  summarise(
    n_target_polygons =
      n_distinct(
        poly_id
      ),
    
    .groups =
      "drop"
  )


regional_balance <- regional_required %>%
  
  left_join(
    direct_group_sum,
    by = c(
      "prov",
      "allocation_group",
      "species",
      "year"
    )
  ) %>%
  
  left_join(
    target_group_count,
    by = c(
      "prov",
      "allocation_group",
      "species",
      "year"
    )
  ) %>%
  
  mutate(
    
    direct_mapped_n =
      coalesce(
        direct_mapped_n,
        0L
      ),
    
    n_direct_polygons =
      coalesce(
        n_direct_polygons,
        0L
      ),
    
    n_target_polygons =
      coalesce(
        n_target_polygons,
        0L
      ),
    
    residual_n =
      as.integer(
        regional_total_n -
          direct_mapped_n
      )
  )


negative_residuals <- regional_balance %>%
  
  filter(
    residual_n < 0
  )


fwrite(
  negative_residuals,
  file.path(
    output_dir,
    "negative_residuals.csv"
  )
)


if (
  nrow(
    negative_residuals
  ) > 0
) {
  
  print(
    negative_residuals,
    n = 100
  )
  
  stop(
    "Direct mapped harvest exceeds regional/group total."
  )
}


# =====================================================================
# 11. EXTRACT SPECIES-SPECIFIC SUITABILITY
# =====================================================================

suitability_by_polygon <- purrr::imap_dfr(
  
  suitability_files,
  
  ~ extract_suitability(
    raster_file =
      .x,
    species_name =
      .y,
    polygons =
      reviere
  )
)


fwrite(
  suitability_by_polygon,
  file.path(
    output_dir,
    "suitability_by_polygon.csv"
  )
)


missing_suitability <- suitability_by_polygon %>%
  
  filter(
    is.na(
      suitability_area_ha
    )
  )


fwrite(
  missing_suitability,
  file.path(
    output_dir,
    "missing_suitability.csv"
  )
)


if (
  nrow(
    missing_suitability
  ) > 0
) {
  
  print(
    missing_suitability,
    n = 100
  )
  
  stop(
    "At least one included polygon has missing suitability."
  )
}


# =====================================================================
# 12. INTEGER ALLOCATION TO TARGET POLYGONS
# =====================================================================

allocation_targets <- allocation_targets %>%
  
  left_join(
    suitability_by_polygon,
    by = c(
      "poly_id",
      "species"
    )
  ) %>%
  
  left_join(
    
    regional_balance %>%
      select(
        prov,
        allocation_group,
        species,
        year,
        regional_total_n,
        direct_mapped_n,
        residual_n,
        n_direct_polygons,
        n_target_polygons
      ),
    
    by = c(
      "prov",
      "allocation_group",
      "species",
      "year"
    )
  )


if (
  any(
    is.na(
      allocation_targets$residual_n
    )
  )
) {
  stop(
    "Allocation target without regional balance."
  )
}


allocation_targets <- allocation_targets %>%
  
  group_by(
    prov,
    allocation_group,
    species,
    year
  ) %>%
  
  arrange(
    poly_id,
    .by_group = TRUE
  ) %>%
  
  mutate(
    
    total_target_suitability_ha =
      sum(
        suitability_area_ha
      ),
    
    n_target_polygons_check =
      n_distinct(
        poly_id
      ),
    
    n_allocated =
      largest_remainder_allocate(
        residual_n =
          residual_n,
        suitability_area_ha =
          suitability_area_ha,
        poly_id =
          poly_id
      ),
    
    allocation_status = case_when(
      
      unique(
        residual_n
      ) == 0 ~
        "group_residual_zero",
      
      unique(
        residual_n
      ) > 0 &
        unique(
          total_target_suitability_ha
        ) > 0 ~
        "integer_suitability_weighted",
      
      unique(
        residual_n
      ) > 0 &
        unique(
          total_target_suitability_ha
        ) <= 0 ~
        "zero_target_suitability",
      
      TRUE ~
        "unexpected"
    )
  ) %>%
  
  ungroup()


target_count_mismatches <- allocation_targets %>%
  
  filter(
    n_target_polygons !=
      n_target_polygons_check
  )


if (
  nrow(
    target_count_mismatches
  ) > 0
) {
  
  fwrite(
    target_count_mismatches,
    file.path(
      output_dir,
      "ERROR_target_count_mismatches.csv"
    )
  )
  
  stop(
    "Target polygon count mismatch."
  )
}


# =====================================================================
# 13. CHECK INTEGER ALLOCATION
# =====================================================================

non_integer_allocations <- allocation_targets %>%
  
  filter(
    n_allocated !=
      as.integer(
        n_allocated
      )
  )


if (
  nrow(
    non_integer_allocations
  ) > 0
) {
  stop(
    "Non-integer allocation produced."
  )
}


allocation_check <- allocation_targets %>%
  
  group_by(
    prov,
    allocation_group,
    species,
    year
  ) %>%
  
  summarise(
    
    residual_n =
      first(
        residual_n
      ),
    
    total_target_suitability_ha =
      first(
        total_target_suitability_ha
      ),
    
    allocated_n =
      sum(
        n_allocated
      ),
    
    n_targets =
      n_distinct(
        poly_id
      ),
    
    .groups =
      "drop"
  ) %>%
  
  mutate(
    
    expected_polygon_allocation =
      if_else(
        total_target_suitability_ha > 0,
        residual_n,
        0L
      ),
    
    difference =
      allocated_n -
      expected_polygon_allocation
  )


allocation_mismatches <- allocation_check %>%
  
  filter(
    difference != 0
  )


fwrite(
  allocation_check,
  file.path(
    output_dir,
    "integer_allocation_check.csv"
  )
)


if (
  nrow(
    allocation_mismatches
  ) > 0
) {
  
  fwrite(
    allocation_mismatches,
    file.path(
      output_dir,
      "ERROR_integer_allocation_mismatches.csv"
    )
  )
  
  stop(
    "Integer allocation does not equal expected assignable residual."
  )
}


# =====================================================================
# 14. BUILD FALLBACK TABLE
# =====================================================================
#
# Fallback occurs when:
#
# A) a regional/group total belongs to a source-only group with no
#    current polygon at all;
#
# B) a current group has a positive residual but no target polygon;
#
# C) a current group has target polygons but all target suitability is 0.
# =====================================================================


# ---------------------------------------------------------------------
# A. Source-only groups
# ---------------------------------------------------------------------

current_groups <- reviere %>%
  
  st_drop_geometry() %>%
  
  distinct(
    prov,
    allocation_group =
      allocation_group_current
  )


source_only_fallback <- regional_group %>%
  
  anti_join(
    current_groups,
    by = c(
      "prov",
      "allocation_group"
    )
  ) %>%
  
  filter(
    regional_total_n > 0
  ) %>%
  
  transmute(
    prov,
    allocation_group,
    species,
    year,
    fallback_n =
      as.integer(
        regional_total_n
      ),
    fallback_reason =
      "source_only_group_no_current_polygon"
  )


# ---------------------------------------------------------------------
# B/C. Current groups
# ---------------------------------------------------------------------

target_suitability_by_group <- allocation_check %>%
  
  select(
    prov,
    allocation_group,
    species,
    year,
    total_target_suitability_ha
  )


current_group_fallback <- regional_balance %>%
  
  left_join(
    target_suitability_by_group,
    by = c(
      "prov",
      "allocation_group",
      "species",
      "year"
    )
  ) %>%
  
  mutate(
    fallback_n = case_when(
      
      residual_n <= 0 ~
        0L,
      
      n_target_polygons == 0 ~
        residual_n,
      
      n_target_polygons > 0 &
        coalesce(
          total_target_suitability_ha,
          0
        ) <= 0 ~
        residual_n,
      
      TRUE ~
        0L
    ),
    
    fallback_reason = case_when(
      
      fallback_n == 0 ~
        NA_character_,
      
      n_target_polygons == 0 ~
        "positive_residual_but_no_target_polygon",
      
      n_target_polygons > 0 &
        coalesce(
          total_target_suitability_ha,
          0
        ) <= 0 ~
        "positive_residual_but_zero_target_suitability",
      
      TRUE ~
        NA_character_
    )
  ) %>%
  
  filter(
    fallback_n > 0
  ) %>%
  
  select(
    prov,
    allocation_group,
    species,
    year,
    fallback_n,
    fallback_reason
  )


group_fallback <- bind_rows(
  source_only_fallback,
  current_group_fallback
) %>%
  
  arrange(
    prov,
    allocation_group,
    species,
    year
  )


fwrite(
  group_fallback,
  file.path(
    output_dir,
    "group_fallback.csv"
  )
)


# =====================================================================
# 15. CREATE FINAL COMPLETE LONG TABLE
# =====================================================================

allocated_values <- allocation_targets %>%
  
  select(
    poly_id,
    year,
    species,
    n_allocated,
    allocation_status,
    suitability_area_ha,
    total_target_suitability_ha,
    residual_n
  )


final_long <- full_grid %>%
  
  left_join(
    allocated_values,
    by = c(
      "poly_id",
      "year",
      "species"
    )
  ) %>%
  
  mutate(
    
    n = case_when(
      
      is_observed ~
        as.integer(
          n_direct
        ),
      
      needs_allocation ~
        as.integer(
          n_allocated
        ),
      
      TRUE ~
        NA_integer_
    ),
    
    n_source = case_when(
      
      is_observed ~
        "observed",
      
      needs_allocation &
        allocation_status ==
        "group_residual_zero" ~
        "allocated_zero",
      
      needs_allocation &
        allocation_status ==
        "zero_target_suitability" ~
        "estimated_zero_no_suitable_area",
      
      needs_allocation &
        allocation_status ==
        "integer_suitability_weighted" ~
        "allocated",
      
      TRUE ~
        "unresolved"
    )
  )


# =====================================================================

# 16. HARD FINAL CHECKS
# =====================================================================

# ---------------------------------------------------------------------
# Completeness
# ---------------------------------------------------------------------

unresolved <- final_long %>%
  
  filter(
    is.na(
      n
    ) |
      n_source ==
      "unresolved"
  )


fwrite(
  unresolved,
  file.path(
    output_dir,
    "unresolved_final_values.csv"
  )
)


if (
  nrow(
    unresolved
  ) > 0
) {
  stop(
    "Final polygon time series still contains unresolved values."
  )
}


# ---------------------------------------------------------------------
# Integer n
# ---------------------------------------------------------------------

if (
  any(
    final_long$n !=
    as.integer(
      final_long$n
    )
  )
) {
  stop(
    "Final n contains non-integer values."
  )
}


# ---------------------------------------------------------------------
# Observed raw values unchanged
# ---------------------------------------------------------------------

observed_change_check <- final_long %>%
  
  filter(
    n_source ==
      "observed"
  ) %>%
  
  filter(
    n !=
      as.integer(
        n_direct
      )
  )


fwrite(
  observed_change_check,
  file.path(
    output_dir,
    "observed_value_change_check.csv"
  )
)


if (
  nrow(
    observed_change_check
  ) > 0
) {
  stop(
    "At least one observed harvest value was changed."
  )
}


# =====================================================================
# 17. FINAL GROUP CONSERVATION CHECK
# =====================================================================

final_polygon_group_sum <- final_long %>%
  
  group_by(
    prov,
    allocation_group,
    species,
    year
  ) %>%
  
  summarise(
    polygon_n =
      sum(
        n
      ),
    .groups =
      "drop"
  )


fallback_group_sum <- group_fallback %>%
  
  group_by(
    prov,
    allocation_group,
    species,
    year
  ) %>%
  
  summarise(
    fallback_n =
      sum(
        fallback_n
      ),
    .groups =
      "drop"
  )


group_conservation <- regional_group %>%
  
  left_join(
    final_polygon_group_sum,
    by = c(
      "prov",
      "allocation_group",
      "species",
      "year"
    )
  ) %>%
  
  left_join(
    fallback_group_sum,
    by = c(
      "prov",
      "allocation_group",
      "species",
      "year"
    )
  ) %>%
  
  mutate(
    
    polygon_n =
      coalesce(
        polygon_n,
        0L
      ),
    
    fallback_n =
      coalesce(
        fallback_n,
        0L
      ),
    
    accounted_n =
      polygon_n +
      fallback_n,
    
    difference =
      accounted_n -
      regional_total_n
  )


group_conservation_mismatches <- group_conservation %>%
  
  filter(
    difference != 0
  )


fwrite(
  group_conservation,
  file.path(
    output_dir,
    "group_conservation_check.csv"
  )
)


if (
  nrow(
    group_conservation_mismatches
  ) > 0
) {
  
  fwrite(
    group_conservation_mismatches,
    file.path(
      output_dir,
      "ERROR_group_conservation_mismatches.csv"
    )
  )
  
  print(
    group_conservation_mismatches,
    n = 100
  )
  
  stop(
    "Final polygon + fallback harvest does not reproduce ",
    "regional/group totals."
  )
}


# =====================================================================
# 18. SAVE FINAL LONG CSV
# =====================================================================

final_long_out <- final_long %>%
  
  select(
    poly_id,
    hunt_site,
    name,
    prov,
    region_current,
    allocation_group,
    group_type,
    year,
    species,
    n,
    n_source,
    suitability_area_ha,
    residual_n
  ) %>%
  
  arrange(
    prov,
    poly_id,
    year,
    species
  )


fwrite(
  final_long_out,
  file.path(
    output_dir,
    "final_revier_timeseries_complete.csv"
  )
)


# =====================================================================
# 19. SAVE FINAL LONG GPKG
# =====================================================================

geometry_lookup <- reviere %>%
  
  select(
    poly_id
  )


final_long_sf <- geometry_lookup %>%
  
  left_join(
    final_long_out,
    by = "poly_id",
    relationship = "one-to-many"
  )


st_write(
  final_long_sf,
  file.path(
    output_dir,
    "final_revier_timeseries_complete.gpkg"
  ),
  layer =
    "revier_timeseries",
  delete_dsn = TRUE,
  quiet = TRUE
)


# =====================================================================
# 20. CREATE WIDE OUTPUT
# =====================================================================

wide_n <- final_long_out %>%
  
  select(
    poly_id,
    species,
    year,
    n
  ) %>%
  
  mutate(
    field =
      paste0(
        "n_",
        species,
        "_",
        year
      )
  ) %>%
  
  select(
    poly_id,
    field,
    n
  ) %>%
  
  pivot_wider(
    names_from =
      field,
    values_from =
      n
  )


wide_source <- final_long_out %>%
  
  select(
    poly_id,
    species,
    year,
    n_source
  ) %>%
  
  mutate(
    field =
      paste0(
        "src_",
        species,
        "_",
        year
      )
  ) %>%
  
  select(
    poly_id,
    field,
    n_source
  ) %>%
  
  pivot_wider(
    names_from =
      field,
    values_from =
      n_source
  )


wide_attributes <- final_long_out %>%
  
  distinct(
    poly_id,
    hunt_site,
    name,
    prov,
    region_current,
    allocation_group,
    group_type
  ) %>%
  
  left_join(
    wide_n,
    by = "poly_id"
  ) %>%
  
  left_join(
    wide_source,
    by = "poly_id"
  )


final_wide_sf <- geometry_lookup %>%
  
  left_join(
    wide_attributes,
    by = "poly_id"
  )


st_write(
  final_wide_sf,
  file.path(
    output_dir,
    "final_revier_unique_wide_complete.gpkg"
  ),
  layer =
    "reviere_complete",
  delete_dsn = TRUE,
  quiet = TRUE
)


# Province-specific wide GPKGs

st_write(
  final_wide_sf %>%
    filter(
      prov == "styria"
    ),
  file.path(
    output_dir,
    "final_revier_unique_wide_styria_1992_2024.gpkg"
  ),
  layer =
    "styria_reviere",
  delete_dsn = TRUE,
  quiet = TRUE
)


st_write(
  final_wide_sf %>%
    filter(
      prov == "sbg"
    ),
  file.path(
    output_dir,
    "final_revier_unique_wide_salzburg_1998_2024.gpkg"
  ),
  layer =
    "salzburg_reviere",
  delete_dsn = TRUE,
  quiet = TRUE
)


# =====================================================================
# 21. COMPLETENESS + SOURCE SUMMARY
# =====================================================================

completeness_summary <- final_long_out %>%
  
  group_by(
    prov
  ) %>%
  
  summarise(
    
    expected_values =
      n(),
    
    available_values =
      sum(
        !is.na(
          n
        )
      ),
    
    missing_values =
      sum(
        is.na(
          n
        )
      ),
    
    completeness_percent =
      100 *
      available_values /
      expected_values,
    
    .groups =
      "drop"
  )


fwrite(
  completeness_summary,
  file.path(
    output_dir,
    "completeness_summary.csv"
  )
)


source_summary <- final_long_out %>%
  
  count(
    prov,
    species,
    year,
    n_source,
    name =
      "n_values"
  )


fwrite(
  source_summary,
  file.path(
    output_dir,
    "data_source_by_year_species.csv"
  )
)


source_harvest_summary <- final_long_out %>%
  
  group_by(
    prov,
    species,
    year,
    n_source
  ) %>%
  
  summarise(
    harvest_n =
      sum(
        n
      ),
    .groups =
      "drop"
  )


fwrite(
  source_harvest_summary,
  file.path(
    output_dir,
    "harvest_by_source_year_species.csv"
  )
)


# =====================================================================
# 22. FIGURES — WHERE DO VALUES COME FROM BY YEAR?
# =====================================================================

source_levels <- c(
  "observed",
  "allocated",
  "allocated_zero",
  "estimated_zero_no_suitable_area"
)


pretty_source_labels <- c(
  observed =
    "Observed",
  allocated =
    "Suitability allocated",
  allocated_zero =
    "Allocated zero",
  estimated_zero_no_suitable_area =
    "Zero: no suitable target area"
)


plot_source_share <- function(
    province,
    province_label,
    filename
) {
  
  pdat <- source_summary %>%
    
    filter(
      prov ==
        province
    ) %>%
    
    group_by(
      species,
      year
    ) %>%
    
    mutate(
      share =
        n_values /
        sum(
          n_values
        )
    ) %>%
    
    ungroup() %>%
    
    mutate(
      n_source =
        factor(
          n_source,
          levels =
            source_levels
        )
    )
  
  
  p <- ggplot(
    pdat,
    aes(
      x =
        year,
      y =
        share,
      fill =
        n_source
    )
  ) +
    
    geom_col(
      width =
        0.9
    ) +
    
    facet_wrap(
      ~ species,
      ncol = 1
    ) +
    
    scale_y_continuous(
      labels =
        scales::percent_format(
          accuracy =
            1
        ),
      expand =
        expansion(
          mult =
            c(
              0,
              0.02
            )
        )
    ) +
    
    scale_fill_discrete(
      name =
        "Value source",
      labels =
        pretty_source_labels
    ) +
    
    labs(
      title =
        paste0(
          "Origin of hunt-site values by year — ",
          province_label
        ),
      subtitle =
        "Share of current polygon × species values",
      x =
        "Year",
      y =
        "Share of values"
    ) +
    
    theme_minimal(
      base_size =
        12
    ) +
    
    theme(
      legend.position =
        "bottom",
      panel.grid.minor =
        element_blank()
    )
  
  
  ggsave(
    filename =
      file.path(
        output_dir,
        filename
      ),
    plot =
      p,
    width =
      11,
    height =
      9,
    dpi =
      300
  )
}


plot_harvest_source <- function(
    province,
    province_label,
    filename
) {
  
  pdat <- source_harvest_summary %>%
    
    filter(
      prov ==
        province
    ) %>%
    
    mutate(
      n_source =
        factor(
          n_source,
          levels =
            source_levels
        )
    )
  
  
  p <- ggplot(
    pdat,
    aes(
      x =
        year,
      y =
        harvest_n,
      fill =
        n_source
    )
  ) +
    
    geom_col(
      width =
        0.9
    ) +
    
    facet_wrap(
      ~ species,
      ncol = 1,
      scales =
        "free_y"
    ) +
    
    scale_fill_discrete(
      name =
        "Value source",
      labels =
        pretty_source_labels
    ) +
    
    labs(
      title =
        paste0(
          "Harvest represented by data source — ",
          province_label
        ),
      subtitle =
        "Observed and suitability-allocated harvest in current polygons",
      x =
        "Year",
      y =
        "Harvest n"
    ) +
    
    theme_minimal(
      base_size =
        12
    ) +
    
    theme(
      legend.position =
        "bottom",
      panel.grid.minor =
        element_blank()
    )
  
  
  ggsave(
    filename =
      file.path(
        output_dir,
        filename
      ),
    plot =
      p,
    width =
      11,
    height =
      9,
    dpi =
      300
  )
}


plot_source_share(
  province =
    "sbg",
  province_label =
    "Salzburg",
  filename =
    "figure_source_share_by_year_salzburg.png"
)


plot_source_share(
  province =
    "styria",
  province_label =
    "Styria",
  filename =
    "figure_source_share_by_year_styria.png"
)


plot_harvest_source(
  province =
    "sbg",
  province_label =
    "Salzburg",
  filename =
    "figure_harvest_by_source_salzburg.png"
)


plot_harvest_source(
  province =
    "styria",
  province_label =
    "Styria",
  filename =
    "figure_harvest_by_source_styria.png"
)


# =====================================================================
# 23. FINAL SUMMARY
# =====================================================================

cat("\n")
cat("============================================================\n")
cat("STEP 3 COMPLETE — INTEGER SUITABILITY ALLOCATION\n")
cat("============================================================\n")

cat(
  "Regional rows outside study period ignored: ",
  nrow(
    regional_group_outside_study_period
  ),
  "\n"
)

cat(
  "Current polygons used:                      ",
  nrow(
    reviere
  ),
  "\n"
)

cat(
  "Excluded polygons:                          ",
  nrow(
    excluded_polygons
  ),
  "\n"
)

cat(
  "Expected polygon-species-year values:       ",
  nrow(
    final_long_out
  ),
  "\n"
)

cat(
  "Observed values:                            ",
  sum(
    final_long_out$n_source ==
      "observed"
  ),
  "\n"
)

cat(
  "Previously zero-completed values now allocated: ",
  nrow(
    partial_polygon_years
  ),
  " partial polygon-years\n"
)

cat(
  "Suitability-allocated values:               ",
  sum(
    final_long_out$n_source ==
      "allocated"
  ),
  "\n"
)

cat(
  "Allocated-zero values:                      ",
  sum(
    final_long_out$n_source ==
      "allocated_zero"
  ),
  "\n"
)

cat(
  "Zero values from no suitable target area:   ",
  sum(
    final_long_out$n_source ==
      "estimated_zero_no_suitable_area"
  ),
  "\n"
)

cat(
  "Total allocated harvest n:                  ",
  sum(
    final_long_out$n[
      final_long_out$n_source ==
        "allocated"
    ]
  ),
  "\n"
)

cat(
  "Fallback rows:                              ",
  nrow(
    group_fallback
  ),
  "\n"
)

cat(
  "Fallback harvest n:                         ",
  sum(
    group_fallback$fallback_n
  ),
  "\n"
)

cat(
  "Unresolved final values:                    ",
  nrow(
    unresolved
  ),
  "\n"
)

cat(
  "Negative residuals:                         ",
  nrow(
    negative_residuals
  ),
  "\n"
)

cat(
  "Observed values changed:                    ",
  nrow(
    observed_change_check
  ),
  "\n"
)

cat(
  "Integer allocation mismatches:              ",
  nrow(
    allocation_mismatches
  ),
  "\n"
)

cat(
  "Group conservation mismatches:              ",
  nrow(
    group_conservation_mismatches
  ),
  "\n"
)

cat("\nCompleteness by province:\n")

print(
  completeness_summary
)

cat("\nFallback by reason:\n")

print(
  group_fallback %>%
    group_by(
      prov,
      fallback_reason
    ) %>%
    summarise(
      n_rows =
        n(),
      fallback_n =
        sum(
          fallback_n
        ),
      .groups =
        "drop"
    )
)

cat("\nOutputs written to:\n")
cat(
  output_dir,
  "\n"
)

cat("============================================================\n")
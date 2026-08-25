library(terra)
library(sf)
library(dplyr)
library(viridisLite)


# =====================================================================
# 1. PATHS
# =====================================================================

raster_dir <-
  "/mnt/eo/WilDensity/output/continuous_harvest_density/red_deer/octagon_2000m_temporal_3yr"

output_gif <-
  "/mnt/eo/WilDensity/output/continuous_harvest_density/red_deer/red_deer_octagon_2000m_temporal_3y.gif"

frame_dir <-
  "/mnt/eo/WilDensity/output/continuous_harvest_density/red_deer/gif_frames"

dir.create(
  frame_dir,
  recursive = TRUE,
  showWarnings = FALSE
)


# =====================================================================
# 2. DISTRICT / PROVINCE OUTLINES
# =====================================================================

gpkg <-
  "/mnt/eo/WilDensity/output/final_integer_allocation/interpolated_missing_reviere/final_revier_timeseries_complete_interpolated.gpkg"

reviere <- st_read(
  gpkg,
  layer = "revier_timeseries",
  quiet = TRUE
) %>%
  arrange(poly_id) %>%
  filter(!duplicated(poly_id)) %>%
  select(poly_id, prov) %>%
  st_make_valid() %>%
  st_transform(3035)

outline <- reviere %>%
  group_by(prov) %>%
  summarise(.groups = "drop") %>%
  st_make_valid()

outline_v <- vect(outline)


# =====================================================================
# 3. FIND RASTERS
# =====================================================================

files <- list.files(
  raster_dir,
  pattern = "^red_deer_harvest_density_[0-9]{4}\\.tif$",
  full.names = TRUE
)

if (length(files) == 0) {
  stop(
    "No red deer rasters found in:\n",
    raster_dir
  )
}

years <- as.integer(
  sub(
    ".*_([0-9]{4})\\.tif$",
    "\\1",
    files
  )
)

ord <- order(years)

files <- files[ord]
years <- years[ord]

cat(
  "Found ",
  length(files),
  " rasters: ",
  min(years),
  "–",
  max(years),
  "\n",
  sep = ""
)


# =====================================================================
# 4. COMMON COLOUR SCALE
# =====================================================================

cat("Calculating common colour scale...\n")

p99 <- numeric(length(files))

for (i in seq_along(files)) {
  
  r <- rast(files[i])
  
  q <- global(
    r,
    fun = quantile,
    probs = 0.99,
    na.rm = TRUE
  )
  
  p99[i] <- as.numeric(q[1, 1])
}

scale_max <- max(
  p99,
  na.rm = TRUE
)

if (
  !is.finite(scale_max) ||
  scale_max <= 0
) {
  stop("Could not determine colour scale.")
}

cat(
  "Colour scale: 0–",
  round(scale_max, 2),
  " n/km²/year\n",
  sep = ""
)


# =====================================================================
# 5. COLOURS
# =====================================================================

n_colors <- 100

cols <- viridis(
  n_colors,
  option = "C"
)

breaks <- seq(
  0,
  scale_max,
  length.out = n_colors + 1
)


# =====================================================================
# 6. CREATE PNG FRAMES
# =====================================================================

cat("Creating frames...\n")

for (i in seq_along(files)) {
  
  yr <- years[i]
  
  cat(
    "  ",
    yr,
    "\n",
    sep = ""
  )
  
  r <- rast(files[i])
  
  # Cap only for plotting
  r_plot <- clamp(
    r,
    lower = 0,
    upper = scale_max,
    values = TRUE
  )
  
  # Sequential numbering is important for ffmpeg
  frame_file <- file.path(
    frame_dir,
    sprintf(
      "frame_%03d.png",
      i
    )
  )
  
  png(
    filename = frame_file,
    width = 1400,
    height = 1100,
    res = 150,
    bg = "white"
  )
  
  par(
    mar = c(2, 2, 4, 6)
  )
  
  plot(
    r_plot,
    col = cols,
    breaks = breaks,
    axes = FALSE,
    box = FALSE,
    legend = TRUE,
    plg = list(
      title = expression(
        "Harvest density" ~
          (n ~ km^{-2} ~ yr^{-1})
      ),
      cex = 1.15
    ),
    main = paste0(
      "Red deer harvest density — ",
      yr
    ),
    cex.main = 1.7
  )
  
  plot(
    outline_v,
    add = TRUE,
    border = "grey30",
    lwd = 1
  )
  
  mtext(
    "100 m | 2 km octagonal moving window | 3-year temporal window",
    side = 3,
    line = 0.4,
    cex = 1
  )
  
  dev.off()
}


# =====================================================================
# 7. CHECK FFMPEG
# =====================================================================

ffmpeg <- Sys.which("ffmpeg")

if (ffmpeg == "") {
  stop(
    "ffmpeg was not found on the system."
  )
}


# =====================================================================
# 8. CREATE GIF WITH FFMPEG
# =====================================================================

cat("Creating GIF with ffmpeg...\n")

frame_pattern <- file.path(
  frame_dir,
  "frame_%03d.png"
)

palette_file <- file.path(
  frame_dir,
  "palette.png"
)


# ---------------------------------------------------------------------
# First pass: generate optimized colour palette
# ---------------------------------------------------------------------

system2(
  ffmpeg,
  args = c(
    "-y",
    "-framerate", "2",
    "-start_number", "1",
    "-i", shQuote(frame_pattern),
    "-vf", "palettegen=stats_mode=diff",
    shQuote(palette_file)
  )
)


# ---------------------------------------------------------------------
# Second pass: generate GIF
# ---------------------------------------------------------------------

system2(
  ffmpeg,
  args = c(
    "-y",
    "-framerate", "2",
    "-start_number", "1",
    "-i", shQuote(frame_pattern),
    "-i", shQuote(palette_file),
    "-lavfi", "paletteuse=dither=sierra2_4a",
    "-loop", "0",
    shQuote(output_gif)
  )
)


# =====================================================================
# 9. CHECK OUTPUT
# =====================================================================

if (!file.exists(output_gif)) {
  stop("GIF creation failed.")
}


cat("\n")
cat("GIF created successfully:\n")
cat(output_gif, "\n")
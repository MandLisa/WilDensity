library(terra)
library(sf)
library(dplyr)
library(viridisLite)


# =====================================================================
# 1. PATHS
# =====================================================================

raster_dir <-
  "/mnt/eo/WilDensity/output/continuous_harvest_density_5y/red_deer/octagon_1880m_temporal_5yr"

output_gif <-
  "/mnt/eo/WilDensity/output/continuous_harvest_density_5y/red_deer/red_deer_octagon_1880m_temporal_5yr.gif"

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

cols <- viridisLite::rocket(n_colors)

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
    width = 1700,
    height = 1200,
    res = 150,
    bg = "white"
  )
  
  par(
    mar = c(2, 2, 4, 4)
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
      cex = 1.4
    ),
    main = paste0(
      "Red deer harvest density — ",
      yr
    ),
    cex.main = 2
  )
  
  plot(
    outline_v,
    add = TRUE,
    border = "grey30",
    lwd = 1
  )
  
  mtext(
    "",
    side = 3,
    line = 0.4,
    cex = 1
  )
  
  dev.off()
}


# =====================================================================
# 7. CREATE GIF WITH PYTHON / PILLOW
# =====================================================================

python <- Sys.which("python3")

if (python == "") {
  stop("python3 was not found on the system.")
}

cat("Creating GIF with Python/Pillow...\n")

python_script <- file.path(
  frame_dir,
  "create_gif.py"
)

python_code <- c(
  "from PIL import Image",
  "import glob",
  "import os",
  "",
  paste0(
    "frame_dir = r'", frame_dir, "'"
  ),
  paste0(
    "output_gif = r'", output_gif, "'"
  ),
  "",
  "files = sorted(glob.glob(os.path.join(frame_dir, 'frame_*.png')))",
  "",
  "if len(files) == 0:",
  "    raise RuntimeError('No PNG frames found')",
  "",
  "frames = [Image.open(f).convert('RGB') for f in files]",
  "",
  "frames[0].save(",
  "    output_gif,",
  "    save_all=True,",
  "    append_images=frames[1:],",
  "    duration=500,",
  "    loop=0,",
  "    optimize=True",
  ")",
  "",
  "print(f'GIF written to: {output_gif}')"
)

writeLines(
  python_code,
  python_script
)

status <- system2(
  python,
  python_script
)

if (status != 0) {
  stop("GIF creation with Python failed.")
}

if (!file.exists(output_gif)) {
  stop("GIF file was not created.")
}

cat("\nGIF successfully created:\n")
cat(output_gif, "\n")
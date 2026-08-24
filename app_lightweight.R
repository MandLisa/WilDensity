# =====================================================================
# SHINY APP — UNGULATE HARVEST
# FAST / LIGHTWEIGHT VERSION
# =====================================================================
#
# Browser:
#   - simplified geometries
#   - lower rendering resolution
#   - cached species / plot combinations
#
# Download:
#   - full polygon geometries
#   - 600 dpi PNG
#
# Metrics:
#   - Harvest density [n / km²]
#   - Absolute harvest [n]
#
# Displays:
#   - Single year
#   - Year series
#   - Mean over selected period
#
# 1992 excluded.
# =====================================================================


# =====================================================================
# 0. PACKAGES
# =====================================================================

library(shiny)
library(sf)
library(dplyr)
library(ggplot2)
library(scales)


# =====================================================================
# 1. SETTINGS
# =====================================================================

input_gpkg <-
  "/mnt/eo/WilDensity/output/final_integer_allocation/interpolated_missing_reviere/final_revier_timeseries_complete_interpolated.gpkg"

input_layer <-
  "revier_timeseries"


# Equal-area CRS
area_crs <- 3035


# Simplification tolerance for browser display [m]
# Increase to e.g. 150 if year series is still slow.
simplify_tolerance <- 100


species_labels <- c(
  roe_deer = "Roe deer",
  red_deer = "Red deer",
  chamois  = "Chamois"
)


# =====================================================================
# 2. READ DATA
# =====================================================================

cat("Reading input data...\n")


x <- st_read(
  input_gpkg,
  layer = input_layer,
  quiet = TRUE
) %>%
  
  mutate(
    poly_id = as.integer(poly_id),
    species = as.character(species),
    year    = as.integer(year),
    n       = as.numeric(n),
    prov    = as.character(prov)
  ) %>%
  
  filter(
    species %in% names(species_labels),
    
    # Exclude 1992
    year != 1992,
    
    # Study periods
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
# 3. UNIQUE FULL-RESOLUTION REVIER GEOMETRIES
# =====================================================================

cat("Preparing polygon geometries...\n")


reviere_geom <- x %>%
  
  arrange(poly_id) %>%
  
  filter(
    !duplicated(poly_id)
  ) %>%
  
  select(
    poly_id,
    prov
  )


if (anyDuplicated(reviere_geom$poly_id) > 0) {
  stop("poly_id is not unique.")
}


if (any(!st_is_valid(reviere_geom))) {
  
  reviere_geom <-
    st_make_valid(
      reviere_geom
    )
}


# =====================================================================
# 4. TRANSFORM ONCE TO EPSG:3035
# =====================================================================

# Keeping all map geometries in the final plotting CRS avoids repeated
# reprojection during Shiny rendering.

reviere_geom_full <- reviere_geom %>%
  
  st_transform(
    area_crs
  )


# =====================================================================
# 5. CALCULATE AREA [km²]
# =====================================================================

area_lookup <- reviere_geom_full %>%
  
  mutate(
    area_km2 =
      as.numeric(
        st_area(.)
      ) / 1e6
  ) %>%
  
  st_drop_geometry() %>%
  
  select(
    poly_id,
    area_km2
  )


invalid_area <- area_lookup %>%
  
  filter(
    is.na(area_km2) |
      area_km2 <= 0
  )


if (nrow(invalid_area) > 0) {
  
  stop(
    "At least one polygon has missing or invalid area."
  )
}


# =====================================================================
# 6. SIMPLIFIED GEOMETRY FOR BROWSER DISPLAY
# =====================================================================

cat(
  "Simplifying geometries for Shiny display (",
  simplify_tolerance,
  " m)...\n",
  sep = ""
)


reviere_geom_app <- reviere_geom_full %>%
  
  st_simplify(
    dTolerance = simplify_tolerance,
    preserveTopology = TRUE
  )


# =====================================================================
# 7. PREPARE ATTRIBUTE TABLE
# =====================================================================

# Important:
# Do NOT carry thousands of repeated geometries through the Shiny
# reactives. Keep attributes as an ordinary table and attach the
# geometry only immediately before plotting.

annual_data <- x %>%
  
  st_drop_geometry() %>%
  
  select(
    poly_id,
    any_of("hunt_site"),
    any_of("name"),
    prov,
    species,
    year,
    n,
    any_of("was_interpolated")
  ) %>%
  
  left_join(
    area_lookup,
    by = "poly_id"
  ) %>%
  
  mutate(
    harvest_density =
      n / area_km2
  )


# =====================================================================
# 8. CHECK UNIQUENESS
# =====================================================================

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
  
  print(
    duplicate_check
  )
  
  stop(
    "Duplicate poly_id x species x year combinations found."
  )
}


# =====================================================================
# 9. PROVINCE OUTLINES
# =====================================================================

# ---------------------------------------------------------------------
# Full-resolution province outlines
#
# IMPORTANT:
# Union the original valid polygons FIRST.
# Simplifying individual polygons before st_union() can introduce
# small topology problems along shared borders.
# ---------------------------------------------------------------------

prov_outline_full <- reviere_geom_full %>%
  
  st_make_valid() %>%
  
  group_by(
    prov
  ) %>%
  
  summarise(
    do_union = TRUE,
    .groups = "drop"
  ) %>%
  
  st_make_valid()


# ---------------------------------------------------------------------
# Simplified province outlines for browser
#
# Simplify only AFTER the polygons have been unioned.
# ---------------------------------------------------------------------

prov_outline_app <- prov_outline_full %>%
  
  st_simplify(
    dTolerance = simplify_tolerance,
    preserveTopology = TRUE
  ) %>%
  
  st_make_valid()


# ---------------------------------------------------------------------
# Common extent
# ---------------------------------------------------------------------

full_bbox <- st_bbox(
  prov_outline_full
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
        size = 17,
        face = "bold"
      ),
    
    plot.subtitle =
      element_blank(),
    
    plot.title.position =
      "plot",
    
    plot.margin =
      margin(
        0,
        5,
        5,
        5
      )
  )


# =====================================================================
# 11. UI
# =====================================================================

ui <- fluidPage(
  
  tags$head(
    
    tags$style(
      
      HTML(
        "
        .sticky-sidebar {
          position: sticky;
          top: 10px;
          align-self: flex-start;
        }

        .control-label {
          font-weight: 600;
        }

        .shiny-plot-output {
          width: 100%;
        }

        .container-fluid {
          padding-top: 5px;
        }

        h2 {
          margin-top: 5px;
          margin-bottom: 10px;
        }
        "
      )
    )
  ),
  
  
  titlePanel(
    "Ungulate harvest"
  ),
  
  
  sidebarLayout(
    
    sidebarPanel(
      width = 3,
      
      div(
        class = "sticky-sidebar",
        
        
        # -------------------------------------------------------------
        # Species
        # -------------------------------------------------------------
        
        selectInput(
          inputId = "species",
          label = "Species",
          
          choices = c(
            "Roe deer" = "roe_deer",
            "Red deer" = "red_deer",
            "Chamois"  = "chamois"
          ),
          
          selected = "roe_deer"
        ),
        
        
        # -------------------------------------------------------------
        # Metric
        # -------------------------------------------------------------
        
        radioButtons(
          inputId = "metric",
          label = "Metric",
          
          choices = c(
            "Harvest density (n / km²)" =
              "density",
            
            "Absolute harvest (n)" =
              "absolute"
          ),
          
          selected = "density"
        ),
        
        
        # -------------------------------------------------------------
        # Display
        # -------------------------------------------------------------
        
        radioButtons(
          inputId = "view_mode",
          label = "Display",
          
          choices = c(
            "Single year" =
              "single",
            
            "Year series" =
              "series",
            
            "Mean over period" =
              "mean"
          ),
          
          selected = "single"
        ),
        
        
        # -------------------------------------------------------------
        # Single year
        # -------------------------------------------------------------
        
        conditionalPanel(
          condition =
            "input.view_mode == 'single'",
          
          sliderInput(
            inputId = "year",
            label = "Year",
            min = 1993,
            max = 2024,
            value = 2004,
            step = 1,
            sep = ""
          )
        ),
        
        
        # -------------------------------------------------------------
        # Period
        # -------------------------------------------------------------
        
        conditionalPanel(
          condition =
            "input.view_mode != 'single'",
          
          sliderInput(
            inputId = "year_range",
            label = "Period",
            min = 1993,
            max = 2024,
            value = c(
              1993,
              2024
            ),
            step = 1,
            sep = ""
          )
        ),
        
        
        hr(),
        
        
        downloadButton(
          outputId = "download_map",
          label = "Download PNG"
        )
      )
    ),
    
    
    mainPanel(
      width = 9,
      
      uiOutput(
        "map_ui"
      )
    )
  )
)


# =====================================================================
# 12. SERVER
# =====================================================================

server <- function(input, output, session) {
  
  
  # ===================================================================
  # 12.1 SPECIES DATA
  # ===================================================================
  
  # Plain dataframe -> much cheaper than manipulating sf objects.
  
  species_data <- reactive({
    
    req(
      input$species
    )
    
    
    annual_data %>%
      
      filter(
        species ==
          input$species
      )
    
  }) %>%
    
    bindCache(
      input$species
    )
  
  
  # ===================================================================
  # 12.2 AVAILABLE YEARS
  # ===================================================================
  
  available_years <- reactive({
    
    species_data() %>%
      
      pull(year) %>%
      
      unique() %>%
      
      sort()
  })
  
  
  observeEvent(
    input$species,
    {
      
      yrs <-
        available_years()
      
      
      req(
        length(yrs) > 0
      )
      
      
      current_year <-
        isolate(
          input$year
        )
      
      
      if (
        is.null(current_year) ||
        current_year < min(yrs) ||
        current_year > max(yrs)
      ) {
        
        current_year <-
          max(yrs)
      }
      
      
      updateSliderInput(
        session,
        inputId = "year",
        min = min(yrs),
        max = max(yrs),
        value = current_year,
        step = 1
      )
      
      
      updateSliderInput(
        session,
        inputId = "year_range",
        min = min(yrs),
        max = max(yrs),
        value = c(
          min(yrs),
          max(yrs)
        ),
        step = 1
      )
    },
    
    ignoreInit = FALSE
  )
  
  
  # ===================================================================
  # 12.3 CONSTANT COLOUR SCALE PER SPECIES + METRIC
  # ===================================================================
  
  scale_max <- reactive({
    
    req(
      input$metric
    )
    
    
    dat <-
      species_data()
    
    
    if (
      input$metric ==
      "density"
    ) {
      
      vals <-
        dat$harvest_density
      
    } else {
      
      vals <-
        dat$n
    }
    
    
    out <-
      as.numeric(
        
        quantile(
          vals,
          probs = 0.99,
          na.rm = TRUE
        )
      )
    
    
    if (
      !is.finite(out) ||
      out <= 0
    ) {
      
      out <- 1
    }
    
    
    out
    
  }) %>%
    
    bindCache(
      input$species,
      input$metric
    )
  
  
  # ===================================================================
  # 12.4 PREPARE ATTRIBUTE DATA FOR CURRENT VIEW
  # ===================================================================
  
  plot_table <- reactive({
    
    req(
      input$view_mode
    )
    
    
    dat <-
      species_data()
    
    
    # ---------------------------------------------------------------
    # SINGLE YEAR
    # ---------------------------------------------------------------
    
    if (
      input$view_mode ==
      "single"
    ) {
      
      req(
        input$year
      )
      
      
      return(
        
        dat %>%
          
          filter(
            year ==
              input$year
          )
      )
    }
    
    
    # ---------------------------------------------------------------
    # YEAR SERIES
    # ---------------------------------------------------------------
    
    if (
      input$view_mode ==
      "series"
    ) {
      
      req(
        input$year_range
      )
      
      
      return(
        
        dat %>%
          
          filter(
            year >=
              input$year_range[1],
            
            year <=
              input$year_range[2]
          )
      )
    }
    
    
    # ---------------------------------------------------------------
    # MEAN OVER PERIOD
    # ---------------------------------------------------------------
    
    if (
      input$view_mode ==
      "mean"
    ) {
      
      req(
        input$year_range
      )
      
      
      return(
        
        dat %>%
          
          filter(
            year >=
              input$year_range[1],
            
            year <=
              input$year_range[2]
          ) %>%
          
          group_by(
            poly_id,
            prov
          ) %>%
          
          summarise(
            
            # Mean annual absolute harvest
            n =
              mean(
                n,
                na.rm = TRUE
              ),
            
            # Mean annual harvest density
            harvest_density =
              mean(
                harvest_density,
                na.rm = TRUE
              ),
            
            n_years =
              n_distinct(year),
            
            .groups =
              "drop"
          )
      )
    }
    
    
    NULL
  })
  
  
  # ===================================================================
  # 12.5 DYNAMIC MAP HEIGHT
  # ===================================================================
  
  map_height <- reactive({
    
    req(
      input$view_mode
    )
    
    
    # Single map:
    # avoid an unnecessarily tall output, otherwise coord_sf centres
    # the map vertically and creates white space above it.
    
    if (
      input$view_mode %in%
      c(
        "single",
        "mean"
      )
    ) {
      
      return(
        560
      )
    }
    
    
    # ---------------------------------------------------------------
    # YEAR SERIES
    # ---------------------------------------------------------------
    
    req(
      input$year_range
    )
    
    
    n_years <-
      input$year_range[2] -
      input$year_range[1] +
      1
    
    
    ncol_facets <-
      5
    
    
    n_rows <-
      ceiling(
        n_years /
          ncol_facets
      )
    
    
    max(
      560,
      n_rows * 235
    )
  })
  
  
  # ===================================================================
  # 12.6 MAP UI
  # ===================================================================
  
  output$map_ui <- renderUI({
    
    plotOutput(
      outputId = "map",
      
      height =
        paste0(
          map_height(),
          "px"
        )
    )
  })
  
  
  # ===================================================================
  # 12.7 FUNCTION TO BUILD MAP
  # ===================================================================
  
  # simplified = TRUE:
  #   fast browser map
  #
  # simplified = FALSE:
  #   full-resolution downloaded map
  
  build_plot <- function(
    simplified = TRUE
  ) {
    
    dat <-
      plot_table()
    
    
    req(
      !is.null(dat),
      nrow(dat) > 0,
      input$metric
    )
    
    
    sp_label <-
      species_labels[[input$species]]
    
    
    # ---------------------------------------------------------------
    # Select geometry
    # ---------------------------------------------------------------
    
    if (simplified) {
      
      geom_use <-
        reviere_geom_app
      
      outline_use <-
        prov_outline_app
      
    } else {
      
      geom_use <-
        reviere_geom_full
      
      outline_use <-
        prov_outline_full
    }
    
    
    # ---------------------------------------------------------------
    # Attach geometry only now
    # ---------------------------------------------------------------
    
    map_data <- geom_use %>%
      
      select(
        poly_id
      ) %>%
      
      inner_join(
        dat,
        by = "poly_id"
      )
    
    
    # ---------------------------------------------------------------
    # Metric
    # ---------------------------------------------------------------
    
    if (
      input$metric ==
      "density"
    ) {
      
      map_data$plot_value <-
        map_data$harvest_density
      
      
      metric_label <-
        "harvest density"
      
      
      if (
        input$view_mode ==
        "mean"
      ) {
        
        legend_title <-
          expression(
            "Mean harvest density" ~
              (n ~ km^{-2} ~ yr^{-1})
          )
        
      } else {
        
        legend_title <-
          expression(
            "Harvest density" ~
              (n ~ km^{-2} ~ yr^{-1})
          )
      }
      
      
    } else {
      
      map_data$plot_value <-
        map_data$n
      
      
      metric_label <-
        "harvest"
      
      
      if (
        input$view_mode ==
        "mean"
      ) {
        
        legend_title <-
          "Mean annual harvest (n)"
        
      } else {
        
        legend_title <-
          "Harvest (n)"
      }
    }
    
    
    # ---------------------------------------------------------------
    # Base map
    # ---------------------------------------------------------------
    
    p <- ggplot() +
      
      geom_sf(
        data = outline_use,
        fill = "grey94",
        linewidth = 0.10,
        color = "grey45"
      ) +
      
      geom_sf(
        data = map_data,
        
        aes(
          fill = plot_value
        ),
        
        linewidth = 0,
        color = NA
      ) +
      
      scale_fill_distiller(
        palette = "YlOrRd",
        direction = 1,
        
        limits = c(
          0,
          scale_max()
        ),
        
        oob =
          squish,
        
        na.value =
          "grey94",
        
        name =
          legend_title
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
          FALSE,
        
        # No graticule / geographic datum calculations needed
        datum =
          NA
      )
    
    
    # ---------------------------------------------------------------
    # SINGLE YEAR
    # ---------------------------------------------------------------
    
    if (
      input$view_mode ==
      "single"
    ) {
      
      p <- p +
        
        labs(
          title =
            paste0(
              sp_label,
              " ",
              metric_label,
              " — ",
              input$year
            )
        )
    }
    
    
    # ---------------------------------------------------------------
    # YEAR SERIES
    # ---------------------------------------------------------------
    
    if (
      input$view_mode ==
      "series"
    ) {
      
      p <- p +
        
        facet_wrap(
          ~ year,
          ncol = 5
        ) +
        
        labs(
          title =
            paste0(
              sp_label,
              " ",
              metric_label,
              " — ",
              input$year_range[1],
              "–",
              input$year_range[2]
            )
        )
    }
    
    
    # ---------------------------------------------------------------
    # MEAN OVER PERIOD
    # ---------------------------------------------------------------
    
    if (
      input$view_mode ==
      "mean"
    ) {
      
      p <- p +
        
        labs(
          title =
            paste0(
              sp_label,
              " mean ",
              metric_label,
              " — ",
              input$year_range[1],
              "–",
              input$year_range[2]
            )
        )
    }
    
    
    p +
      theme_map
  }
  
  
  # ===================================================================
  # 12.8 DISPLAY — FAST SIMPLIFIED GEOMETRY
  # ===================================================================
  
  output$map <- renderPlot(
    
    {
      build_plot(
        simplified = TRUE
      )
    },
    
    # Browser only — 80 dpi is sufficient and much faster
    res = 80
    
  ) %>%
    
    bindCache(
      input$species,
      input$metric,
      input$view_mode,
      input$year,
      input$year_range,
      map_height()
    )
  
  
  # ===================================================================
  # 12.9 DOWNLOAD — FULL GEOMETRY / 600 DPI
  # ===================================================================
  
  output$download_map <- downloadHandler(
    
    filename = function() {
      
      metric_name <-
        ifelse(
          input$metric == "density",
          "density",
          "absolute"
        )
      
      
      if (
        input$view_mode ==
        "single"
      ) {
        
        return(
          
          paste0(
            "harvest_",
            metric_name,
            "_",
            input$species,
            "_",
            input$year,
            ".png"
          )
        )
      }
      
      
      if (
        input$view_mode ==
        "series"
      ) {
        
        return(
          
          paste0(
            "harvest_",
            metric_name,
            "_",
            input$species,
            "_",
            input$year_range[1],
            "_",
            input$year_range[2],
            "_series.png"
          )
        )
      }
      
      
      paste0(
        "harvest_",
        metric_name,
        "_",
        input$species,
        "_",
        input$year_range[1],
        "_",
        input$year_range[2],
        "_mean.png"
      )
    },
    
    
    content = function(file) {
      
      # ---------------------------------------------------------------
      # Export dimensions
      # ---------------------------------------------------------------
      
      if (
        input$view_mode ==
        "series"
      ) {
        
        n_years <-
          input$year_range[2] -
          input$year_range[1] +
          1
        
        
        n_rows <-
          ceiling(
            n_years / 5
          )
        
        
        width_out <-
          24
        
        
        height_out <-
          max(
            10,
            n_rows * 3.8
          )
        
        
      } else {
        
        width_out <-
          10
        
        height_out <-
          8
      }
      
      
      ggsave(
        filename = file,
        
        # Full-resolution polygons for export
        plot =
          build_plot(
            simplified = FALSE
          ),
        
        width =
          width_out,
        
        height =
          height_out,
        
        units =
          "in",
        
        dpi =
          600,
        
        bg =
          "white",
        
        limitsize =
          FALSE
      )
    }
  )
}


# =====================================================================
# 13. RUN APP
# =====================================================================

cat("Starting Shiny app...\n")


shinyApp(
  ui = ui,
  server = server
)
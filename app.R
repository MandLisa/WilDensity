# =====================================================================
# SHINY APP — UNGULATE HARVEST
# =====================================================================

library(shiny)
library(sf)
library(dplyr)
library(ggplot2)
library(scales)


# =====================================================================
# 1. SETTINGS
# =====================================================================

# Use interpolated final dataset
input_gpkg <-
  "/mnt/eo/WilDensity/output/final_integer_allocation/interpolated_missing_reviere/final_revier_timeseries_complete_interpolated.gpkg"

input_layer <-
  "revier_timeseries"


# Equal-area CRS for polygon area calculation
area_crs <- 3035


species_labels <- c(
  roe_deer = "Roe deer",
  red_deer = "Red deer",
  chamois  = "Chamois"
)


# =====================================================================
# 2. READ DATA
# =====================================================================

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
# 3. UNIQUE REVIER GEOMETRIES
# =====================================================================

# Input is long: each polygon occurs repeatedly for species x year.

reviere_geom <- x %>%
  
  arrange(poly_id) %>%
  
  filter(
    !duplicated(poly_id)
  ) %>%
  
  select(
    poly_id,
    prov
  )


# =====================================================================
# 4. CALCULATE REVIER AREA [km²]
# =====================================================================

reviere_area <- st_transform(
  reviere_geom,
  area_crs
)


reviere_area$area_km2 <-
  as.numeric(
    st_area(reviere_area)
  ) / 1e6


area_lookup <- reviere_area %>%
  
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
    "At least one hunt-site polygon has missing or invalid area."
  )
}


# =====================================================================
# 5. PREPARE ANNUAL DATA
# =====================================================================

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


# Check uniqueness
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
  stop(
    "Duplicate poly_id x species x year combinations found."
  )
}


# Attach geometry
annual_sf <- reviere_geom %>%
  
  select(
    poly_id
  ) %>%
  
  left_join(
    annual_data,
    by = "poly_id",
    relationship = "one-to-many"
  )


# =====================================================================
# 6. PROVINCE OUTLINES
# =====================================================================

# sf::summarise() automatically unions geometries within province.

prov_outline <- reviere_geom %>%
  
  group_by(
    prov
  ) %>%
  
  summarise(
    .groups = "drop"
  ) %>%
  
  st_make_valid()


full_bbox <- st_bbox(
  prov_outline
)


# =====================================================================
# 7. MAP THEME
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
        size = 18,
        face = "bold"
      ),
    
    plot.subtitle =
      element_text(
        size = 11
      ),
    
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
# 8. UI
# =====================================================================

ui <- fluidPage(
  
  tags$head(
    
    tags$style(
      HTML(
        "
        .sticky-sidebar {
          position: sticky;
          top: 15px;
          align-self: flex-start;
        }

        .control-label {
          font-weight: 600;
        }

        .shiny-plot-output {
          width: 100%;
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
        # Display mode
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
# 9. SERVER
# =====================================================================

server <- function(input, output, session) {
  
  
  # ===================================================================
  # 9.1 SPECIES-SPECIFIC DATA
  # ===================================================================
  
  species_data <- reactive({
    
    req(input$species)
    
    annual_sf %>%
      
      filter(
        species == input$species
      )
  })
  
  
  # ===================================================================
  # 9.2 AVAILABLE YEARS
  # ===================================================================
  
  available_years <- reactive({
    
    species_data() %>%
      
      st_drop_geometry() %>%
      
      pull(year) %>%
      
      unique() %>%
      
      sort()
  })
  
  
  observeEvent(
    input$species,
    {
      
      yrs <- available_years()
      
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
        
        current_year <- max(yrs)
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
  # 9.3 COLOUR SCALE
  # ===================================================================
  
  # One constant colour scale per species + metric across all years.
  #
  # 99th percentile prevents a few extreme polygons from compressing
  # the useful colour range.
  
  scale_max <- reactive({
    
    req(
      input$metric
    )
    
    
    dat <- species_data()
    
    
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
    
    
    out <- as.numeric(
      
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
  })
  
  
  # ===================================================================
  # 9.4 PREPARE DATA FOR SELECTED VIEW
  # ===================================================================
  
  plot_data <- reactive({
    
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
    #
    # For density:
    #   mean annual n / km²
    #
    # For absolute harvest:
    #   mean annual n
    #
    # Both are calculated here so the metric can be switched freely.
    # ---------------------------------------------------------------
    
    if (
      input$view_mode ==
      "mean"
    ) {
      
      req(
        input$year_range
      )
      
      
      mean_dat <- dat %>%
        
        st_drop_geometry() %>%
        
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
          
          n =
            mean(
              n,
              na.rm = TRUE
            ),
          
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
      
      
      mean_sf <- reviere_geom %>%
        
        select(
          poly_id
        ) %>%
        
        left_join(
          mean_dat,
          by = "poly_id"
        )
      
      
      return(
        mean_sf
      )
    }
    
    
    NULL
  })
  
  
  # ===================================================================
  # 9.5 DYNAMIC MAP HEIGHT
  # ===================================================================
  
  map_height <- reactive({
    
    req(
      input$view_mode
    )
    
    
    # Single map and period mean
    if (
      input$view_mode %in%
      c(
        "single",
        "mean"
      )
    ) {
      
      return(
        650
      )
    }
    
    
    # Series
    req(
      input$year_range
    )
    
    
    n_years <-
      input$year_range[2] -
      input$year_range[1] +
      1
    
    
    ncol_facets <- 5
    
    
    n_rows <-
      ceiling(
        n_years /
          ncol_facets
      )
    
    
    max(
      650,
      n_rows * 260
    )
  })
  
  
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
  # 9.6 BUILD PLOT
  # ===================================================================
  
  make_plot <- reactive({
    
    dat <-
      plot_data()
    
    
    req(
      !is.null(dat),
      nrow(dat) > 0,
      input$metric
    )
    
    
    sp_label <-
      species_labels[[input$species]]
    
    
    # ---------------------------------------------------------------
    # Metric
    # ---------------------------------------------------------------
    
    if (
      input$metric ==
      "density"
    ) {
      
      dat$plot_value <-
        dat$harvest_density
      
      
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
      
      dat$plot_value <-
        dat$n
      
      
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
        data = prov_outline,
        fill = "grey94",
        linewidth = 0.12,
        color = "grey45"
      ) +
      
      geom_sf(
        data = dat,
        
        aes(
          fill = plot_value
        ),
        
        linewidth = 0.01,
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
          FALSE
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
            ),
          
          subtitle =
            ""
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
            ),
          
          subtitle =
            ""
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
            ),
          
          subtitle =
            ""
        )
    }
    
    
    p +
      theme_map
  })
  
  
  # ===================================================================
  # 9.7 DISPLAY MAP
  # ===================================================================
  
  output$map <- renderPlot(
    
    {
      make_plot()
    },
    
    res = 120
  )
  
  
  # ===================================================================
  # 9.8 DOWNLOAD HIGH-RESOLUTION PNG
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
      # Output dimensions
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
        plot = make_plot(),
        width = width_out,
        height = height_out,
        units = "in",
        dpi = 600,
        bg = "white",
        limitsize = FALSE
      )
    }
  )
}


# =====================================================================
# 10. RUN APP

# =====================================================================

shinyApp(
  ui = ui,
  server = server
)
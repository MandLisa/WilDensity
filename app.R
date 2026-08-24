# =====================================================================
# SHINY APP — HARVEST DENSITY
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
  "/mnt/eo/WilDensity/output/final_integer_allocation/final_revier_timeseries_complete.gpkg"

input_layer <- "revier_timeseries"

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
    species = as.character(species),
    year    = as.integer(year),
    n       = as.numeric(n),
    prov    = as.character(prov)
  ) %>%
  
  filter(
    species %in% names(species_labels),
    year != 1992,
    
    (prov == "sbg" &
       year >= 1998 &
       year <= 2024) |
      
      (prov == "styria" &
         year >= 1993 &
         year <= 2024)
  )


# =====================================================================
# 3. UNIQUE REVIER GEOMETRIES
# =====================================================================

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
# 4. AREA [km²]
# =====================================================================

area_lookup <- reviere_geom %>%
  
  st_transform(area_crs) %>%
  
  mutate(
    area_km2 =
      as.numeric(
        st_area(geometry)
      ) / 1e6
  ) %>%
  
  st_drop_geometry() %>%
  
  select(
    poly_id,
    area_km2
  )


# =====================================================================
# 5. ANNUAL DENSITY
# =====================================================================

annual_data <- x %>%
  
  st_drop_geometry() %>%
  
  select(
    poly_id,
    hunt_site,
    name,
    prov,
    species,
    year,
    n
  ) %>%
  
  left_join(
    area_lookup,
    by = "poly_id"
  ) %>%
  
  mutate(
    harvest_density =
      n / area_km2
  )


annual_sf <- reviere_geom %>%
  
  select(poly_id) %>%
  
  left_join(
    annual_data,
    by = "poly_id",
    relationship = "one-to-many"
  )


# =====================================================================
# 6. PROVINCE OUTLINE
# =====================================================================

prov_outline <- reviere_geom %>%
  
  group_by(prov) %>%
  
  summarise(
    geometry = st_union(geometry),
    .groups = "drop"
  ) %>%
  
  st_make_valid()


full_bbox <- st_bbox(prov_outline)


# =====================================================================
# 7. MAP THEME
# =====================================================================

theme_map <- theme_minimal(
  base_size = 12
) +
  
  theme(
    panel.grid = element_blank(),
    
    axis.title = element_blank(),
    axis.text  = element_blank(),
    axis.ticks = element_blank(),
    
    legend.position = "bottom",
    legend.box = "horizontal",
    
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
    
    plot.title.position = "plot"
  )


# =====================================================================
# 8. UI
# =====================================================================

ui <- fluidPage(
  
  titlePanel(
    "Ungulate harvest density"
  ),
  
  
  sidebarLayout(
    
    sidebarPanel(
      
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
      
      
      radioButtons(
        inputId = "view_mode",
        label = "Display",
        
        choices = c(
          "Single year" = "single",
          "Year series" = "series",
          "Mean over period" = "mean"
        ),
        
        selected = "single"
      ),
      
      
      conditionalPanel(
        condition = "input.view_mode == 'single'",
        
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
      
      
      conditionalPanel(
        condition = "input.view_mode != 'single'",
        
        sliderInput(
          inputId = "year_range",
          label = "Period",
          min = 1993,
          max = 2024,
          value = c(1993, 2024),
          step = 1,
          sep = ""
        )
      ),
      
      
      hr(),
      
      downloadButton(
        "download_map",
        "Download PNG"
      )
    ),
    
    
    mainPanel(
      
      plotOutput(
        "map",
        height = "1000px"
      )
    )
  )
)


# =====================================================================
# 9. SERVER
# =====================================================================

server <- function(input, output, session) {
  
  
  # -------------------------------------------------------------------
  # Species-specific data
  # -------------------------------------------------------------------
  
  species_data <- reactive({
    
    annual_sf %>%
      
      filter(
        species == input$species
      )
  })
  
  
  # -------------------------------------------------------------------
  # Available years
  # -------------------------------------------------------------------
  
  observe({
    
    yrs <- species_data() %>%
      st_drop_geometry() %>%
      pull(year) %>%
      unique() %>%
      sort()
    
    
    updateSliderInput(
      session,
      "year",
      min = min(yrs),
      max = max(yrs),
      value = min(
        max(input$year, min(yrs)),
        max(yrs)
      )
    )
    
    
    updateSliderInput(
      session,
      "year_range",
      min = min(yrs),
      max = max(yrs),
      value = c(
        min(yrs),
        max(yrs)
      )
    )
  })
  
  
  # -------------------------------------------------------------------
  # Common colour scale for selected species
  # -------------------------------------------------------------------
  
  scale_max <- reactive({
    
    quantile(
      species_data()$harvest_density,
      probs = 0.99,
      na.rm = TRUE
    )
  })
  
  
  # -------------------------------------------------------------------
  # Prepare data depending on selected mode
  # -------------------------------------------------------------------
  
  plot_data <- reactive({
    
    dat <- species_data()
    
    
    if (input$view_mode == "single") {
      
      dat %>%
        filter(
          year == input$year
        )
      
      
    } else if (input$view_mode == "series") {
      
      dat %>%
        filter(
          year >= input$year_range[1],
          year <= input$year_range[2]
        )
      
      
    } else {
      
      dat %>%
        
        filter(
          year >= input$year_range[1],
          year <= input$year_range[2]
        ) %>%
        
        group_by(
          poly_id,
          prov
        ) %>%
        
        summarise(
          
          harvest_density =
            mean(
              harvest_density,
              na.rm = TRUE
            ),
          
          .groups = "drop"
        )
    }
  })
  
  
  # -------------------------------------------------------------------
  # Build plot
  # -------------------------------------------------------------------
  
  make_plot <- reactive({
    
    dat <- plot_data()
    
    sp_label <-
      species_labels[[input$species]]
    
    
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
          fill = harvest_density
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
        
        oob = squish,
        na.value = "grey94",
        
        name =
          expression(
            "Harvest density" ~
              (n ~ km^{-2} ~ yr^{-1})
          )
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
        
        expand = FALSE
      )
    
    
    # ---------------------------------------------------------------
    # Single year
    # ---------------------------------------------------------------
    
    if (input$view_mode == "single") {
      
      p <- p +
        
        labs(
          title = paste0(
            sp_label,
            " harvest density — ",
            input$year
          )
        )
    }
    
    
    # ---------------------------------------------------------------
    # Year series
    # ---------------------------------------------------------------
    
    if (input$view_mode == "series") {
      
      p <- p +
        
        facet_wrap(
          ~ year,
          ncol = 5
        ) +
        
        labs(
          title = paste0(
            sp_label,
            " harvest density — ",
            input$year_range[1],
            "–",
            input$year_range[2]
          )
        )
    }
    
    
    # ---------------------------------------------------------------
    # Mean period
    # ---------------------------------------------------------------
    
    if (input$view_mode == "mean") {
      
      p <- p +
        
        labs(
          title = paste0(
            sp_label,
            " mean harvest density — ",
            input$year_range[1],
            "–",
            input$year_range[2]
          )
        )
    }
    
    
    p + theme_map
  })
  
  
  # -------------------------------------------------------------------
  # Display
  # -------------------------------------------------------------------
  
  output$map <- renderPlot({
    
    make_plot()
    
  }, res = 120)
  
  
  # -------------------------------------------------------------------
  # Download high-resolution PNG
  # -------------------------------------------------------------------
  
  output$download_map <- downloadHandler(
    
    filename = function() {
      
      if (input$view_mode == "single") {
        
        paste0(
          "harvest_density_",
          input$species,
          "_",
          input$year,
          ".png"
        )
        
      } else {
        
        paste0(
          "harvest_density_",
          input$species,
          "_",
          input$year_range[1],
          "_",
          input$year_range[2],
          "_",
          input$view_mode,
          ".png"
        )
      }
    },
    
    
    content = function(file) {
      
      if (input$view_mode == "series") {
        
        width_out  <- 24
        height_out <- 18
        
      } else {
        
        width_out  <- 9
        height_out <- 10
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
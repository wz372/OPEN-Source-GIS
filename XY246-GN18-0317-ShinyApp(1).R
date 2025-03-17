# Isonzo River Large Wood Analysis Interactive Map ShinyApp
# Author:
# Date: March 2025

# Load required libraries
library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(RColorBrewer)
library(raster)
library(htmltools)

# UI section
ui <- fluidPage(
  # Application title
  titlePanel("Isonzo River Large Wood Risk Analysis"),
  
  # Sidebar layout
  sidebarLayout(
    sidebarPanel(
      width = 3,
      # Data layer controls
      checkboxGroupInput("layers", "Select layers to display:",
                         choices = c("Isonzo River" = "river",
                                     "Bridges" = "bridges",
                                     "Large Wood" = "wood",
                                     "Wood Density Heatmap" = "heatmap",
                                     "Wood Clustering Hotspots" = "hotspot",
                                     "Recommended Retention Structures" = "retention"),
                         selected = c("river", "bridges", "wood")),
      
      # Information panel
      h4("Layer Description:"),
      p("Bridges: Shows all bridges across the Isonzo River"),
      p("Large Wood: Locations of large wood identified in satellite imagery"),
      p("Wood Density Heatmap: Shows the spatial distribution density of large wood"),
      p("Clustering Hotspots: Statistically significant clustering areas identified using hotspot analysis"),
      p("Recommended Retention Structures: Optimal locations for wood retention structures based on analysis"),
      
      # Divider
      hr(),
      
      # Risk analysis information
      h4("Risk Assessment Summary:"),
      p("High-risk bridges: Located in southern high-density and hotspot areas"),
      p("Medium-risk bridges: Located in the middle reaches, near local wood concentration points"),
      p("Low-risk bridges: Northern region bridges with limited upstream wood input"),
      
      # Project information
      hr(),
      p("GEOM184 - Open Source GIS Course Project"),
      p("Developer:"),
      a("GitHub Repository Link", href = "https://github.com/yourusername/IsonzoRiverWoodAnalysis")
    ),
    
    # Main panel - Map
    mainPanel(
      width = 9,
      leafletOutput("map", height = "700px"),
      p("Click on map elements for detailed information", style = "font-style: italic; margin-top: 5px;")
    )
  )
)

# Server section
server <- function(input, output, session) {
  
  # Create reactive values to store data
  rv <- reactiveValues(
    river = NULL,
    bridges = NULL,
    wood = NULL, 
    heatmap = NULL,
    hotspot = NULL,
    retention = NULL
  )
  
  # Try to load data when app starts
  observe({
    tryCatch({
      # Attempt to load actual data files
      rv$river <- st_read("Isonzo河.shp", quiet = TRUE)
      rv$bridges <- st_read("BridgesIsonzo.shp", quiet = TRUE)
      rv$wood <- st_read("Wood_Isonzo river.shp", quiet = TRUE) 
      rv$heatmap <- raster("热点分析结果.tif")
      rv$hotspot <- st_read("聚类分析结果.shp", quiet = TRUE)
      rv$retention <- st_read("position_wood retention structure.shp", quiet = TRUE)
    }, error = function(e) {
      # If data loading fails, use sample data
      message("Using sample data instead: ", e$message)
      
      # Sample river
      river_points <- data.frame(
        lon = c(13.25, 13.28, 13.30, 13.32, 13.35, 13.38, 13.40),
        lat = c(46.00, 45.95, 45.90, 45.85, 45.80, 45.75, 45.70)
      )
      rv$river <- st_as_sf(river_points, coords = c("lon", "lat"), crs = 4326) %>%
        summarise(geometry = st_combine(geometry)) %>%
        st_cast("LINESTRING")
      
      # Sample bridges
      rv$bridges <- st_as_sf(
        data.frame(
          id = 1:5,
          name = paste("Bridge", 1:5),
          lon = c(13.28, 13.32, 13.35, 13.38, 13.40),
          lat = c(45.95, 45.85, 45.80, 45.75, 45.70)
        ),
        coords = c("lon", "lat"), crs = 4326
      )
      
      # Sample wood points
      rv$wood <- st_as_sf(
        data.frame(
          id = 1:10,
          type = sample(c("Single", "Jam", "Blockage"), 10, replace = TRUE),
          lon = runif(10, 13.25, 13.40),
          lat = runif(10, 45.70, 46.00)
        ),
        coords = c("lon", "lat"), crs = 4326
      )
      
      # Sample retention structure locations
      rv$retention <- st_as_sf(
        data.frame(
          id = 1:2,
          name = c("Primary Structure", "Secondary Structure"),
          lon = c(13.36, 13.32),
          lat = c(45.78, 45.88)
        ),
        coords = c("lon", "lat"), crs = 4326
      )
      
      # Sample hotspot (just use a subset of wood points)
      rv$hotspot <- rv$wood[1:5,]
      
      # No sample raster data, just keep as NULL
      rv$heatmap <- NULL
    })
  })
  
  # Create map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%  # Add OpenStreetMap base layer
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite Imagery") %>%
      # Set initial view to Isonzo River area
      setView(lng = 13.35, lat = 45.85, zoom = 10) %>%
      # Add layer controls
      addLayersControl(
        baseGroups = c("OpenStreetMap", "Satellite Imagery"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })
  
  # Respond to layer selection changes
  observe({
    # Get currently selected layers
    selected_layers <- input$layers
    
    # Create base map
    map <- leafletProxy("map") %>%
      clearGroup(c("river", "bridges", "wood", "heatmap", "hotspot", "retention"))
    
    # Add river layer
    if ("river" %in% selected_layers && !is.null(rv$river)) {
      map <- map %>%
        addPolylines(
          data = rv$river, 
          color = "#3388ff", 
          weight = 3, 
          opacity = 0.8,
          group = "river",
          label = "Isonzo River"
        )
    }
    
    # Add bridges layer
    if ("bridges" %in% selected_layers && !is.null(rv$bridges)) {
      map <- map %>%
        addCircleMarkers(
          data = rv$bridges,
          radius = 6,
          color = "#FF1493",
          fillOpacity = 0.8,
          stroke = FALSE,
          group = "bridges",
          label = ~paste("Bridge ID:", ifelse(is.null(id), "Unknown", id))
        )
    }
    
    # Add wood points layer
    if ("wood" %in% selected_layers && !is.null(rv$wood)) {
      map <- map %>%
        addCircleMarkers(
          data = rv$wood,
          radius = 5,
          color = "#800000",
          fillOpacity = 0.7,
          stroke = FALSE,
          group = "wood",
          label = "Large Wood"
        )
    }
    
    # Add heatmap layer
    if ("heatmap" %in% selected_layers && !is.null(rv$heatmap)) {
      # Create color gradient
      pal <- colorNumeric(c("#FFFF00", "#FFA500", "#FF0000"), values(rv$heatmap),
                          na.color = "transparent")
      
      map <- map %>%
        addRasterImage(
          rv$heatmap,
          colors = pal,
          opacity = 0.7,
          group = "heatmap"
        ) %>%
        addLegend(
          position = "bottomright",
          pal = pal,
          values = values(rv$heatmap),
          title = "Wood Density",
          group = "heatmap"
        )
    }
    
    # Add hotspot layer
    if ("hotspot" %in% selected_layers && !is.null(rv$hotspot) && nrow(rv$hotspot) > 0) {
      map <- map %>%
        addCircleMarkers(
          data = rv$hotspot,
          radius = 4,
          color = "#FF0000",
          fillOpacity = 0.7,
          stroke = FALSE,
          group = "hotspot",
          label = "Clustering Hotspot"
        )
    }
    
    # Add recommended retention structure locations
    if ("retention" %in% selected_layers && !is.null(rv$retention)) {
      map <- map %>%
        addCircleMarkers(
          data = rv$retention,
          radius = 8,
          color = "#FF4500",
          fillOpacity = 0.8,
          stroke = TRUE,
          weight = 2,
          group = "retention",
          label = "Recommended Retention Structure"
        )
    }
  })
}

# Run the Shiny application
shinyApp(ui = ui, server = server)
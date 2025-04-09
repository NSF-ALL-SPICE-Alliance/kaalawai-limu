# Libraries ----

library(tidyverse)
library(here)
library(shiny)
library(sf)
library(ggmap)
library(janitor)
library(bslib)
library(fontawesome)

options(scipen = 99)

source("config.R")

lon <<- -157.7966
lat <<- 21.257

kaalawai_joined_sf <- st_read(here("data/kaalawai_data_limu_presence.gpkg"))

# Extract numeric columns for selection
numeric_vars <- kaalawai_joined_sf %>%
  select(turbidity_ntu, salinity_psu, pH, density_g_cm3, temperature_C, depth_ft,
         conductivity_mS_cm, resistivity_ohm_cm, total_dissolved_solids_ppt, 
         pressure_psi, specific_conductivity_mS_cm, orp_mV, external_voltage_V, 
         barometric_pressure_mbar, secondary_temperature_C, bga_pe_fluorescence_rfu_1056293, 
         gelidium_corneum, halimeda_discoidea, turbinaria_ornata, acanthophora_spicifera, 
         bryopsis_pennata) %>%
  colnames()

# Extract unique dates
dates <- unique(kaalawai_joined_sf$date_time)

ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "sandstone"),
  
  # Fallback FontAwesome
  tags$head(
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.2/css/all.min.css")
  ),
  
  tags$div(
    style = "display: flex; align-items: center; gap: 20px;",
    img(src = "SpiceLogo1.png", height = "100px", style = "margin-right: 10px;"), 
    img(src = "cuh_logo.png", height = "100px"),
    tags$a(
      href = "https://github.com/NSF-ALL-SPICE-Alliance/kaalawai-limu", 
      target = "_blank",
      fa("github", fill = "black", height = "50px")
    )
  ),
  
  titlePanel(""),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("var", "Select a variable:", choices = numeric_vars, selected = "bryopsis_pennata"),
      selectInput("date", "Select a date:", choices = dates)
    ),
    mainPanel(
      plotOutput("mapPlot", hover = hoverOpts(id = "plot_hover")),
      plotOutput("linePlot")
    )
  )
)

# Server ----
server <- function(input, output, session) {
  
  basemap <- reactive({
    get_map(location = c(lon = lon, lat = lat), zoom = 19, source = "google", maptype = "satellite")
  })
  
  filtered_data <- reactive({
    kaalawai_joined_sf %>%
      filter(date_time == input$date)
  })
  
  output$mapPlot <- renderPlot({
    req(input$var, input$date)
    
    ggmap(basemap()) +
      geom_sf(data = filtered_data(), aes_string(fill = input$var), color = "black", alpha = 0.5) +
      scale_fill_viridis_c() +
      labs(
        title = 'Visualize Data by Transect',
        subtitle = paste("Date:", input$date),
        fill = input$var
      ) +
      theme_minimal()
  })
  
  hovered_transect <- reactiveVal(NULL)
  
  observe({
    req(input$plot_hover)
    hover <- input$plot_hover
    hover_point <- st_sfc(st_point(c(hover$x, hover$y)), crs = 4326)
    kaalawai_joined_sf <- st_transform(kaalawai_joined_sf, crs = 4326)
    nearest_index <- st_nearest_feature(hover_point, kaalawai_joined_sf)
    if (!is.na(nearest_index)) {
      hovered_transect(kaalawai_joined_sf$transect[nearest_index])
    }
  })
  
  output$linePlot <- renderPlot({
    selected_transect <- hovered_transect()
    
    transect_data <- kaalawai_joined_sf %>%
      arrange(date_time) %>%
      mutate(highlight = "Other")
    
    if (!is.null(selected_transect)) {
      transect_data <- transect_data %>%
        mutate(highlight = ifelse(transect == selected_transect, "Highlighted", "Other"))
    }
    
    ggplot(transect_data, aes(x = date_time, y = .data[[input$var]], group = transect, color = highlight)) +
      geom_line() +
      geom_point() +
      scale_color_manual(values = c("Highlighted" = "steelblue", "Other" = "grey")) +
      labs(
        title = "Time Series for All Transects",
        subtitle = if (!is.null(selected_transect)) paste("Hovered Transect:", selected_transect) else "Hover over a transect to highlight",
        x = "Date",
        y = input$var
      ) +
      theme_minimal()
  })
}

# Launch ----
shinyApp(ui = ui, server = server)

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

lon <<- -157.7966  # Use `<<-` to ensure global scope
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

# Filter outlier variables



ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "sandstone"),
  
  # Load FontAwesome manually (fallback in case {fontawesome} package doesn't work)
  tags$head(
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.2/css/all.min.css")
  ),
  
  # Enlarge text, dropdowns, and adjust UI layout
  tags$style(HTML("
    body { font-size: 36px; }  
    .sidebarPanel, .mainPanel { font-size: 36px; } 
    .shiny-input-container { font-size: 36px !important; }  
    .selectize-input, .shiny-input-container select { font-size: 36px !important; height: 50px; }  
    .selectize-dropdown, .selectize-dropdown-content div { font-size: 36px !important; }  /* Enlarges dropdown options */
    h1, h2, h3 { font-size: 48px; }  
    .shiny-plot-output { width: 110%; height: auto; }  
")),
  
  # Logo + GitHub link section
  tags$div(
    style = "display: flex; align-items: center; gap: 20px;",
    
    # Spice & CUH Logos
    img(src = "SpiceLogo1.png", height = "100px", style = "margin-right: 10px;"), 
    img(src = "cuh_logo.png", height = "100px"),
    
    # GitHub Logo - Preferred Method using fontawesome::fa()
    tags$a(
      href = "https://github.com/NSF-ALL-SPICE-Alliance/kaalawai-limu", 
      target = "_blank",
      fa("github", fill = "black", height = "50px")
    ),
    
    # GitHub Logo - Fallback Method using raw FontAwesome
    # tags$a(
    #   href = "https://github.com/NSF-ALL-SPICE-Alliance/kaalawai-limu", 
    #   target = "_blank",
    #   tags$i(class = "fab fa-github")
    # )
  ),
  titlePanel(""),
  sidebarLayout(
    sidebarPanel(
      selectInput("var", "Select a variable:", choices = numeric_vars, selected = "bryopsis_pennata"),
      selectInput("date", "Select a date:", choices = dates),
      width = 3  # Adjust sidebar width
    ),
    mainPanel(
      plotOutput("mapPlot", hover = hoverOpts(id = "plot_hover"), height = "1200px"), # Make map larger
      plotOutput("linePlot", height = "800px")  # Make line plot larger
    )
  )
)

# Define Server
server <- function(input, output, session) {
  # Get Google Map
  # lon <- -157.7966
  # lat <- 21.257
  
  basemap <- reactive({
    # lon <- -157.7966  # Define inside the function
    # lat <- 21.257
    get_map(location = c(lon = lon, lat = lat), zoom = 19, source = "google", maptype = "satellite")
  })
  
  # Reactive filtered dataset based on selected date
  filtered_data <- reactive({
    kaalawai_joined_sf %>%
      filter(date_time == input$date)
  })
  
  # Render the map with ggmap
  output$mapPlot <- renderPlot({
    req(input$var, input$date)
    
    ggmap(basemap()) +
      geom_sf(data = filtered_data(), aes_string(fill = input$var), color = "black", alpha = 0.5) +
      scale_fill_viridis_c(
        guide = guide_colourbar(
          barwidth = 2,  # Increase width of color bar
          barheight = 20,  # Increase height of color bar
          title.theme = element_text(size = 36, face = "bold"),  # Enlarge legend title
          label.theme = element_text(size = 32)  # Enlarge legend labels
        )
      ) +
      labs(
        title = 'Visualize Data by Transect',
        subtitle = paste("Date:", input$date, "🔎 Hover over each transect to highlight line plot over time below 📈"),
        fill = input$var  # Legend title
      ) +
      theme_minimal(base_size = 36) +  # Increase base font size
      theme(
        plot.title = element_text(size = 40, face = "bold"),
        plot.subtitle = element_text(size = 36),
        axis.title = element_blank(),  # Remove axis labels
        axis.text = element_blank(),  # Remove axis ticks
        axis.ticks = element_blank(),  # Remove axis ticks
        legend.title = element_text(size = 36),  # Enlarge legend title
        legend.text = element_text(size = 32)  # Enlarge legend text
      )
  })
  
  
  # Reactive value to store hovered transect
  hovered_transect <- reactiveVal(NULL)
  
  # Observe hover event and update the hovered transect
  observe({
    req(input$plot_hover)
    
    hover <- input$plot_hover
    hover_point <- st_sfc(st_point(c(hover$x, hover$y)), crs = 4326)  # Ensure CRS matches
    
    # Ensure the dataset has the same CRS
    kaalawai_joined_sf <- st_transform(kaalawai_joined_sf, crs = 4326)
    
    # Find the nearest transect
    nearest_index <- st_nearest_feature(hover_point, kaalawai_joined_sf)
    
    # Update hovered transect only if valid
    if (!is.na(nearest_index)) {
      hovered_transect(kaalawai_joined_sf$transect[nearest_index])
    }
  })
  
  # Render time-series plot with all transects, highlight hovered transect
  output$linePlot <- renderPlot({
    selected_transect <- hovered_transect()  # Get the hovered transect
    
    # Initialize all transects as "Other" (grey)
    transect_data <- kaalawai_joined_sf %>%
      arrange(date_time) %>%
      mutate(highlight = "Other")  
    
    # If hovering, update only the selected transect to "Highlighted" (red)
    if (!is.null(selected_transect)) {
      transect_data <- transect_data %>%
        mutate(highlight = ifelse(transect == selected_transect, "Highlighted", "Other"))
    }
    
    # Generate time-series plot
    ggplot(transect_data, aes(x = date_time, y = .data[[input$var]], group = transect, color = highlight)) +
      geom_line(size = 2.5, alpha = 0.7) +  # Increase line thickness
      geom_point(size = 4) +  # Increase point size
      scale_color_manual(values = c("Highlighted" = "steelblue", "Other" = "grey")) +  
      labs(
        title = "Time Series for All Transects",
        subtitle = if (!is.null(selected_transect)) paste("Hovered Transect:", selected_transect) else "Hover over a transect to highlight",
        x = "Date",
        y = input$var
      ) +
      theme_minimal(base_size = 36) +  # Increase base font size
      theme(
        plot.title = element_text(size = 40, face = "bold"),
        plot.subtitle = element_text(size = 36),
        axis.title = element_text(size = 36),
        axis.text = element_text(size = 32),
        legend.title = element_text(size = 36),
        legend.text = element_text(size = 32),
        legend.position = "none"  # Hide legend for cleaner visualization
      )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
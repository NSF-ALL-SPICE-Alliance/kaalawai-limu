# Libraries ----

library(tidyverse)
library(here)
library(shiny)
library(sf)
library(ggmap)
library(janitor)

options(scipen = 99)

lon <- -157.7966
lat <- 21.257

kaalawai_joined_sf <- st_read(here("data/new_density_data_joined_sf.gpkg"))

class(kaalawai_joined_sf)


glimpse(kaalawai_joined_sf)

kaalawai_joined_sf <- kaalawai_joined_sf %>% 
  select(-x21, -x22, -x23, -description, -marked)

kaalawai_joined_sf <- kaalawai_joined_sf %>%
  mutate(across(-c(name, site, transect, date_time, geom), as.numeric))

kaalawai_joined_sf <- kaalawai_joined_sf %>%
  rename("Turbidity" = turbidity_ntu_895841,"Salinity" = salinity_psu_749968, 
          "PH" = p_h_p_h_723485, "Density" = density_g_cm_a3_749968, "Temperature" = temperature_a_c_732880,
          "Depth" = depth_ft_729320, "Conductivity" = actual_conductivity_am_s_cm_749968, "resistivity" = resistivity_i_c_a_cm_749968,
          "Dissolved_Solids" = total_dissolved_solids_ppt_749968, "pressure" = pressure_psi_729320, "Halimeda_discoidea" = halimeda_discoidea.y, "Gelidium_corneum" = gelidium_corneum,
          "Turbinaria_ornata" = turbinaria_ornata, "Acanthophora_specifera" = acanthophora_spicifera, "Bryopsis_pennata" = bryopsis_pennata)
######I want to select specifc colums to show on the app but im not sure naming the data frame the same thing will mess with retrieving the raw data again.
#kaalawai_joined_sf <- kaalawai_joined_sf %>%
  #select("Turbidity","Salinity", "PH", "Density", "Temperature","Depth", "Conductivity", "resistivity","Dissolved Solids" , "pressure", "Halimeda discoidea")


# Extract numeric columns for selection
numeric_vars <- kaalawai_joined_sf %>%
  select(Turbidity, Salinity, PH, Temperature, Depth, Conductivity,
         Dissolved_Solids, pressure, Halimeda_discoidea, 
         Acanthophora_specifera, Bryopsis_pennata) %>%
  colnames()

# Extract unique dates
dates <- unique(kaalawai_joined_sf$date_time)

# Filter outlier variables



# Define UI
ui <- fluidPage(
  titlePanel("Kaalawai Data Visualization"),
  sidebarLayout(
    sidebarPanel(
      selectInput("var", "Select a variable:", choices = numeric_vars, selected = "temperature_a_c_732880"),
      selectInput("date", "Select a date:", choices = dates)
    ),
    mainPanel(
      plotOutput("mapPlot", hover = hoverOpts(id = "plot_hover")), # Enable hover
      plotOutput("linePlot")  # Line graph for all transects
    )
  )
)

# Define Server
server <- function(input, output, session) {
  # Get Google Map
  lon <- -157.7966
  lat <- 21.257
  
  basemap <- reactive({
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
      scale_fill_viridis_c() +
      labs(
        title = 'Variable Visualization',
        subtitle = paste("Date:", input$date),
        fill = input$var
      ) +
      theme_minimal()
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
      geom_line(size = 1.2, alpha = 0.7) +
      geom_point(size = 2) +
      scale_color_manual(values = c("Highlighted" = "steelblue", "Other" = "grey")) +  
      labs(
        title = "Time Series for All Transects",
        subtitle = if (!is.null(selected_transect)) paste("Hovered Transect:", selected_transect) else "Hover over a transect to highlight",
        x = "Date",
        y = input$var
      ) +
      theme_minimal() +
      theme(legend.position = "none")  # Hide legend for cleaner visualization
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


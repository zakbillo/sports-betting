# app.R
library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(readr)
library(maps)
library(stringr)

# UI definition
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body { 
        background-color: #f8f9fa; 
        font-family: 'Arial', sans-serif;
        padding: 0 !important;
        margin: 0 !important;
      }
      .container-fluid {
        padding: 5px !important;
      }
      .shiny-output-error { color: #dc3545; }
      .shiny-output-error:before { content: 'Error: '; }
      .navbar { margin-bottom: 5px !important; }
    "))
  ),
  
  titlePanel("Sports Betting Revenue by State"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      style = "background-color: white; border-radius: 8px; padding: 20px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);",
      
      h4("Map Controls"),
      hr(),
      
      sliderInput(
        "year",
        "Select Year:",
        min = 2018,
        max = 2024,
        value = 2024,
        step = 1,
        sep = "",
        animate = animationOptions(interval = 2000, loop = FALSE)
      ),
      
      hr(),
      
      h5("Legend"),
      
      hr(),
      
      h5("Data Source"),
      p("State revenue data compiled from official reports. Although sports betting is legal in Florida, New Mexico, Missouri, North Dakota,
        and Washington, those states were missing from the dataset."),
      
      br(),
      
      actionButton("info", "About", icon = icon("info-circle"), 
                   class = "btn btn-info btn-sm"),
      
      br(),
      br(),
      downloadButton("downloadData", "Download Data", class = "btn btn-success btn-sm")
    ),
    
    mainPanel(
      width = 9,
      leafletOutput("revenue_map", height = "700px"),
      br(),
      fluidRow(
        column(12,
               wellPanel(
                 style = "background-color: white;",
                 h4("State Details"),
                 tableOutput("state_table")
               )
        )
      )
    )
  )
)

# Server logic
server <- function(input, output, session) {
  
  # Turn off spherical geometry
  sf::sf_use_s2(FALSE)
  
  # Load data - CSV is now in same folder as app.R
  total_state_revenue <- reactive({
    tryCatch({
      data <- read_csv("total_state_revenue.csv")
      
      required_cols <- c("State", "Year", "Revenue", "Taxes")
      if(!all(required_cols %in% names(data))) {
        missing_cols <- setdiff(required_cols, names(data))
        showNotification(
          paste("Missing columns in CSV:", paste(missing_cols, collapse = ", ")),
          type = "error",
          duration = 10
        )
        return(NULL)
      }
      
      data <- data %>%
        rename_all(~gsub("\\s+", "_", .)) %>%
        mutate(
          State = as.character(State),
          Year = as.numeric(Year),
          Revenue = as.numeric(Revenue),
          Taxes = as.numeric(Taxes)
        )
      
      return(data)
    }, error = function(e) {
      showNotification(
        paste("Error loading data:", e$message),
        type = "error",
        duration = 10
      )
      return(NULL)
    })
  })
  
  # Get state boundaries
  us_states_sf <- reactive({
    states <- maps::map("state", plot = FALSE, fill = TRUE)
    
    st_as_sf(states) %>%
      st_set_crs(4326) %>%
      mutate(ID = stringr::str_remove(ID, ":.*"))
  })
  
  # Prepare map data based on selected year
  map_data <- reactive({
    req(total_state_revenue())
    req(us_states_sf())
    
    selected_year <- input$year
    
    revenue_map_data <- total_state_revenue() %>%
      mutate(match_name = tolower(State))
    
    joined_data <- us_states_sf() %>%
      left_join(revenue_map_data, by = c("ID" = "match_name")) %>%
      filter(Year == selected_year)
    
    if(nrow(joined_data) == 0) {
      showNotification(
        paste("No data available for year", selected_year),
        type = "warning",
        duration = 5
      )
    }
    
    joined_data %>%
      mutate(
        State = stringr::str_to_title(ID),
        Revenue = as.numeric(Revenue),
        Taxes = as.numeric(Taxes),
        Revenue_formatted = ifelse(
          is.na(Revenue), 
          "No data", 
          paste0("$", format(round(Revenue, 0), big.mark = ",", scientific = FALSE))
        ),
        Taxes_formatted = ifelse(
          is.na(Taxes), 
          "No data", 
          paste0("$", format(round(Taxes, 0), big.mark = ",", scientific = FALSE))
        )
      )
  })
  
  # Create CONSISTENT color palette based on ALL years of data
  palette <- reactive({
    req(total_state_revenue())
    
    # Get ALL revenue values across ALL years, remove NAs
    revenue_values <- total_state_revenue()$Revenue
    revenue_values <- revenue_values[!is.na(revenue_values)]
    
    if(length(revenue_values) == 0) {
      revenue_values <- c(0, 1000000)
    }
    
    # Use the full range across all years for consistent legend
    min_val <- min(revenue_values, na.rm = TRUE)
    max_val <- max(revenue_values, na.rm = TRUE)
    
    # Use a more contrasting color palette
    colorNumeric(
      palette = "YlOrRd",  # Yellow-Orange-Red provides better visual contrast
      domain = c(min_val, max_val),
      na.color = "#cccccc"
    )
  })
  
  # Render the base map
  output$revenue_map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(
        providers$CartoDB.Positron,
        options = providerTileOptions(minZoom = 3, maxZoom = 10)
      ) %>%
      setView(lng = -96, lat = 37.8, zoom = 4) %>%
      setMaxBounds(lng1 = -130, lat1 = 20, lng2 = -60, lat2 = 50)
  })
  
  # Update map when data changes
  observe({
    req(map_data())
    req(palette())
    
    data <- map_data()
    pal <- palette()
    
    labels <- sprintf(
      "<strong>%s</strong><br/>
      Year: %s<br/>
      Revenue: %s<br/>
      Taxes: %s",
      data$State,
      data$Year,
      data$Revenue_formatted,
      data$Taxes_formatted
    ) %>% lapply(htmltools::HTML)
    
    leafletProxy("revenue_map", data = data) %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(
        fillColor = ~ifelse(is.na(Revenue), "#cccccc", pal(Revenue)),
        weight = 1,
        opacity = 1,
        color = "black",
        dashArray = "",
        fillOpacity = 0.7,  # Increased from 0.1 for better visibility
        highlightOptions = highlightOptions(
          weight = 5,
          color = "white",
          dashArray = "",
          fillOpacity = 0.9,  # Increased for better highlight
          bringToFront = TRUE
        ),
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"
        ),
        layerId = ~ID
      ) %>%
      addLegend(
        pal = pal,
        values = ~Revenue,
        opacity = 0.7,
        title = "Revenue ($)",
        position = "bottomright",
        na.label = "No data",
        labFormat = labelFormat(
          prefix = "$",
          big.mark = ",",
          digits = 0
        )
      )
  })
  
  # Show state details table
  output$state_table <- renderTable({
    req(map_data())
    
    data <- map_data()
    
    if(nrow(data) == 0) {
      return(data.frame(Message = "No data available for selected year"))
    }
    
    data %>%
      as.data.frame() %>%
      select(State, Year, Revenue, Taxes) %>%
      arrange(desc(Revenue)) %>%
      mutate(
        Revenue = ifelse(
          is.na(Revenue), 
          "No data", 
          paste0("$", format(round(Revenue, 0), big.mark = ",", scientific = FALSE))
        ),
        Taxes = ifelse(
          is.na(Taxes), 
          "No data", 
          paste0("$", format(round(Taxes, 0), big.mark = ",", scientific = FALSE))
        )
      ) %>%
      rename(
        "State" = State,
        "Year" = Year,
        "Revenue ($)" = Revenue,
        "Taxes ($)" = Taxes
      )
  }, striped = TRUE, hover = TRUE, width = "100%")
  
  # Download data handler
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("sports_betting_revenue_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(total_state_revenue())
      write_csv(total_state_revenue(), file)
    }
  )
  
  # Show info modal
  observeEvent(input$info, {
    showModal(modalDialog(
      title = "About This Map",
      tags$div(
        h4("Sports Betting Revenue Visualization"),
        p("This interactive map shows sports betting revenue by state for different years."),
        br(),
        h5("Features:"),
        tags$ul(
          tags$li("Use the slider to select different years"),
          tags$li("Click on any state to see detailed information"),
          tags$li("Hover over states to see revenue and tax data"),
          tags$li("Click the play button for automatic year-by-year animation"),
          tags$li("Download the data using the 'Download Data' button"),
          tags$li("Legend scale remains consistent across years for easy comparison")
        ),
        br(),
        h5("Data Notes:"),
        p("Revenue data is sourced from state gaming commission reports."),
        p("Only states with legalized sports betting are included."),
        br(),
        p("Created with R Shiny, Leaflet, and sf packages.")
      ),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  # Handle map clicks - show state details
  observeEvent(input$revenue_map_shape_click, {
    click <- input$revenue_map_shape_click
    if (!is.null(click)) {
      data <- map_data()
      state_data <- data[data$ID == click$id, ]
      
      if (nrow(state_data) > 0) {
        showModal(modalDialog(
          title = paste("Details for", stringr::str_to_title(click$id)),
          tags$div(
            h4("Financial Data"),
            tags$table(
              class = "table table-bordered",
              tags$tr(
                tags$th("Year"),
                tags$td(state_data$Year)
              ),
              tags$tr(
                tags$th("Total Revenue"),
                tags$td(state_data$Revenue_formatted)
              ),
              tags$tr(
                tags$th("Tax Revenue"),
                tags$td(state_data$Taxes_formatted)
              )
            ),
            br(),
            h5("Map Coordinates"),
            p(paste("Latitude:", round(click$lat, 4))),
            p(paste("Longitude:", round(click$lng, 4)))
          ),
          easyClose = TRUE,
          footer = modalButton("Close")
        ))
      }
    }
  })
}

# Run the app
shinyApp(ui = ui, server = server)
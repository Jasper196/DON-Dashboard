#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.

# load packages needed. If you have not installed them yet, you need to do this before running the code. 
library(leaflet)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(shiny)
library(DT)
library(dplyr)
library(ggplot2)
library(tidyr)
library(rsconnect)

# Important: the following line of code is personalized since it connects with the ShinyApps account I used to publish the Dashboard.
# If you want to create the dashboard yourself, you need to input your own credentials here. 
rsconnect::setAccountInfo(name='',
                          token='',
                          secret='')

#load file with data for the dashboard
load("Outbreaks2.RData")


# Ensure the Year column is numeric
Outbreaks2$Year <- as.numeric(as.character(Outbreaks2$Year))

# Load world map data
world <- ne_countries(scale = "medium", returnclass = "sf")

ui <- fluidPage(
  titlePanel("Disease Outbreak News Dashboard (1996 - March 2022)"),
  
  tabsetPanel(
    tabPanel("Bar Chart",
             sidebarLayout(
               sidebarPanel(
                 h4("Bar Chart Filters"),
                 selectInput("country_bar", "Select Country:", choices = c("All", sort(unique(Outbreaks2$Country))), selected = "All"),
                 selectInput("icd11l3_bar", "Select disease type according to ICD11:", choices = c("All", sort(unique(Outbreaks2$icd11l3))), selected = "All"),
                 sliderInput("year_bar", "Select Year Range for Bar Chart:", 
                             min = min(Outbreaks2$Year, na.rm = TRUE), 
                             max = max(Outbreaks2$Year, na.rm = TRUE), 
                             value = c(min(Outbreaks2$Year, na.rm = TRUE), max(Outbreaks2$Year, na.rm = TRUE)), 
                             step = 1,
                             sep = "")
               ),
               
               mainPanel(
                 h4("Disease Outbreaks by Year"),
                 plotOutput("barChart")
               )
             )
    ),
    
    tabPanel("Data Table",
             sidebarLayout(
               sidebarPanel(
                 h4("Data Table Filters"),
                 selectInput("country", "Select Country:", choices = c("All", sort(unique(Outbreaks2$Country))), selected = "All"),
                 selectInput("icd11l3_table", "Select disease type according to ICD11:", choices = c("All", sort(unique(Outbreaks2$icd11l3))), selected = "All"),
                 sliderInput("year", "Select Year Range for Data Table:", 
                             min = min(Outbreaks2$Year, na.rm = TRUE), 
                             max = max(Outbreaks2$Year, na.rm = TRUE), 
                             value = c(min(Outbreaks2$Year, na.rm = TRUE), max(Outbreaks2$Year, na.rm = TRUE)), 
                             step = 1,
                             sep = "")
               ),
               
               mainPanel(
                 h4("Filtered Data Table"),
                 dataTableOutput("table")
               )
             )
    ),
    
    tabPanel("World Map",
             sidebarLayout(
               sidebarPanel(
                 h4("Map Filters"),
                 selectInput("icd11l3_map", "Select disease type according to ICD11:", choices = c("All", sort(unique(Outbreaks2$icd11l3))), selected = "All"),
                 sliderInput("year_map", "Select Year Range for Map:", 
                             min = min(Outbreaks2$Year, na.rm = TRUE), 
                             max = max(Outbreaks2$Year, na.rm = TRUE), 
                             value = c(min(Outbreaks2$Year, na.rm = TRUE), max(Outbreaks2$Year, na.rm = TRUE)), 
                             step = 1,
                             sep = "")
               ),
               
               mainPanel(
                 h4("Global Disease Outbreaks"),
                 leafletOutput("map")
               )
             )
    )
  )
)

# Define server logic required to filter the dataset and display the table and bar chart
server <- function(input, output) {
  
  # Reactive data for bar chart
  filteredBarData <- reactive({
    data <- Outbreaks2
    if (input$country_bar != "All") {
      data <- data %>%
        filter(Country == input$country_bar)
    }
    if (input$icd11l3_bar != "All") {
      data <- data %>%
        filter(icd11l3 == input$icd11l3_bar)
    }
    data %>%
      filter(Year >= input$year_bar[1] & Year <= input$year_bar[2])
  })
  
  output$barChart <- renderPlot({
    # Create a complete sequence of years within the selected range
    all_years <- data.frame(Year = seq(input$year_bar[1], input$year_bar[2]))
    
    barData <- filteredBarData() %>%
      group_by(Year) %>%
      summarize(countSum = sum(count, na.rm = TRUE)) %>%
      right_join(all_years, by = "Year") %>%
      mutate(countSum = coalesce(countSum, 0))
    
    ggplot(barData, aes(x = Year, y = countSum)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      labs(title = "Disease Outbreaks by Year",
           x = "Year",
           y = "Number of Outbreaks") +
      scale_y_continuous(breaks = pretty(c(0, max(barData$countSum, na.rm = TRUE)), n = 5), 
                         labels = scales::number_format(accuracy = 1)) +
      theme_minimal()
  })
  
  # Reactive data for data table
  filteredData <- reactive({
    data <- Outbreaks2
    if (input$country != "All") {
      data <- data %>%
        filter(Country == input$country)
    }
    if (input$icd11l3_table != "All") {
      data <- data %>%
        filter(icd11l3 == input$icd11l3_table)
    }
    data %>%
      filter(Year >= input$year[1] & Year <= input$year[2]) %>%
      select(Country, Year, icd11l3, Disease)
  })
  
  output$table <- renderDataTable({
    datatable(filteredData())
  })
  
  # Reactive data for map
  filteredMapData <- reactive({
    data <- Outbreaks2
    if (input$icd11l3_map != "All") {
      data <- data %>%
        filter(icd11l3 == input$icd11l3_map)
    }
    data %>%
      filter(Year >= input$year_map[1] & Year <= input$year_map[2]) %>%
      group_by(iso3) %>%
      summarize(outbreakCount = sum(count, na.rm = TRUE))
  })
  
  output$map <- renderLeaflet({
    mapData <- filteredMapData()
    
    world <- world %>%
      left_join(mapData, by = c("iso_a3_eh" = "iso3")) %>%
      mutate(outbreakCount = coalesce(outbreakCount, 0))
    
    pal <- colorNumeric("YlOrRd", domain = world$outbreakCount, na.color = "transparent")
    
    leaflet(world) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~pal(outbreakCount),
        weight = 1,
        opacity = 1,
        color = 'white',
        dashArray = '3',
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 3,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE
        ),
        label = ~paste0(name, ": ", outbreakCount, " outbreaks"),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"
        )
      ) %>%
      addLegend(
        pal = pal,
        values = ~outbreakCount,
        title = "Number of Outbreaks",
        opacity = 0.7,
        labFormat = labelFormat(transform = function(x) floor(x)),
        bins = pretty(c(0, max(world$outbreakCount, na.rm = TRUE)), n = 5)
      )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
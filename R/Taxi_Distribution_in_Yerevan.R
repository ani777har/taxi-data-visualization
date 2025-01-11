library(shiny)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(shinydashboard)
library(rsconnect)
library(sf)
library(ggpubr)
library(shinythemes)



setwd("..") 
load("data/gg_processed_data.RData")
yerevan_districts <- st_read("data/Yerevan-Districts/Yerevan-Districts.shp", quiet=T)

rsconnect::deployApp(appName = "gg_taxi_in_yerevan")
rsconnect::setAccountInfo(name = "aniharutyunyan", 
                          token = "4E37D06BE09A8E5FD3F3235B6841A67A", 
                          secret = "Y/3Rkmr7AvcecNc6X8Mca+zJGX1hifsecuzHvOHB")



ui <- fluidPage(
  theme = shinytheme("flatly"),  
  dashboardHeader(title = "GG Taxi in Yerevan"),
  dashboardSidebar(
    radioButtons(inputId = "period", label = "Choose period of day",
                 choices = list(
                   "Morning (5am-12pm)" = "Morning",
                   "Afternoon (12pm-5pm)" = "Afternoon",
                   "Evening (5pm-12am)" = "Evening",
                   "Night (12am-5am)" = "Night")),
    sliderInput(inputId = "date", 
                label = "Select a Date Range:", 
                min = as.Date("2015-12-31"), 
                max = as.Date("2016-12-31"), 
                value = as.Date("2016-12-19"),
                timeFormat = "%Y-%m-%d")
  ),
  dashboardBody(
    plotOutput("map"),
    verbatimTextOutput("summary"),
    tableOutput("summary_table")
  )
)

server <- function(input, output) {
  filtered_df <- reactive({
    final_df[final_df$date == input$date[1] & final_df$period == input$period, ]
  })
  
  output$map <- renderPlot({
    origin <- ggplot() + 
      geom_sf(data = yerevan_districts, color = "black", fill = "darkseagreen1") +
      geom_point(data = filtered_df(), aes(x = originLng, y = oroginLat), 
                 size = 1, shape = 18, color = "navy") + 
      theme_minimal() + 
      labs(title = "Spatial Distribution of Taxi Rides in Yerevan",
           subtitle = "Origin locations", 
           x = "Longitude", y = "Latitude") + 
      geom_sf_text(data = yerevan_districts, aes(label = Name_en), color = "black") +
      theme(plot.title = element_text(size = 18), 
            plot.subtitle = element_text(size = 16))
    
    dest <- ggplot() + 
      geom_sf(data = yerevan_districts, color = "black", fill = "darkseagreen1") +
      geom_point(data = filtered_df(), aes(x = destLng, y = destLat), 
                 size = 1, shape = 18, color = "deeppink1") + 
      theme_minimal() + 
      labs(subtitle = "Destination Locations", 
           x = "Longitude", y = "Latitude") + 
      geom_sf_text(data = yerevan_districts, aes(label = Name_en), color = "black") +
      theme(plot.title = element_text(size = 18), 
            plot.subtitle = element_text(size = 16))
    
    ggarrange(origin, dest, ncol = 2)
  }) 
  
  output$summary <- renderPrint({
    total_cars <- nrow(filtered_df())
    paste0("Total number of cars in Yerevan on ", 
           format(input$date, "%B %d, %Y"), 
           " in the ", input$period, " is ", total_cars, ".")
  })
  
  output$summary_table <- renderTable({
    district_summary <- filtered_df() %>%
      group_by(origin_district) %>%
      summarise(Origins = n(), .groups = "drop")
    
    dest_summary <- filtered_df() %>%
      group_by(dest_district) %>%
      summarise(Destinations = n(), .groups = "drop")
    
    summary_table <- full_join(district_summary, dest_summary, 
                               by = c("origin_district" = "dest_district"))
    summary_table[is.na(summary_table)] <- 0  
    
    summary_table <- summary_table %>%
      arrange(desc(Origins))
    summary_table
  }, rownames = FALSE)
}

shinyApp(ui = ui, server = server)

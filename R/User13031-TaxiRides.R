library(shiny)
library(ggplot2)
library(gganimate)
library(grid)
library(sf)

ui <- fluidPage(
  dashboardHeader(title = ""),
  dashboardSidebar(disable = TRUE),  
  dashboardBody(
    imageOutput("map_animation")  
  )
)

server <- function(input, output) {
  output$map_animation <- renderImage({
    df_13031 <- final_df[final_df$userId == 13031, ]
    
    p <- ggplot() + 
      geom_sf(data = yerevan_districts, color = "black", fill = "darkseagreen2") +
      geom_point(data = df_13031, aes(x = originLng, y = oroginLat, group = date), 
                 color = "red", size = 2.5, shape = 18) + 
      geom_point(data = df_13031, aes(x = destLng, y = destLat, group = date), 
                 color = "black", size = 1.5, shape = 18) +
      geom_segment(data = df_13031, aes(x = originLng, y = oroginLat, 
                                        xend = destLng, yend = destLat, group = date), 
                   arrow = arrow(length = unit(0.2, "cm"), type = "closed")) +
      geom_sf_text(data = yerevan_districts, aes(label = Name_en), color = "black") +
      theme_minimal() + 
      labs(
        x = "Longitude", 
        y = "Latitude", colour = ""
      ) + 
      theme(
        plot.title = element_text(size = 18), 
        plot.subtitle = element_text(size = 16)
      ) +
      transition_time(as.Date(df_13031$date)) + 
      labs(title = "User-13031's Taxi Rides in Yerevan - {frame_time}") +
      shadow_mark(alpha = 0.5, size = 1)  
    
    gif_file <- "taxi_rides_animation1.gif"  
    anim <- animate(p, fps = 10, duration = 10, renderer = gifski_renderer())
    anim_save(gif_file, animation = anim)
    
    list(src = gif_file, contentType = 'image/gif', alt = "13031_Taxi_Rides")
  }, deleteFile = FALSE)  
}


shinyApp(ui = ui, server = server)

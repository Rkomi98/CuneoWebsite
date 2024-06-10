library(shiny)
library(datavolley)
library(ggplot2)
library(dplyr)
library(ovlytics)
library(plotly)

# Define UI for the application
ui <- fluidPage(
  titlePanel("Cuneo Data Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      # Add sidebar elements here if needed
    ),
    
    mainPanel(
      # Output plots and analysis results
      plotlyOutput("plot1"),
      # plotlyOutput("plot2"),
      # Add additional outputs as necessary
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Load and process the data
  data_plot <- reactive({
    filename <- "C:/Users/mirko/Documents/GitHub/CuneoWebsite.io/Assets/&##Backup##_R00 HONDA O-ALLIANZ V.dvw"
    teamName <- 'HONDA OLIVERO S.BERNARDO CUNEO'
    x <- dv_read(filename)
    serve_idx <- find_serves(plays(x))
    table(plays(x)$team[serve_idx])
    d <- dir("C:/Users/mirko/Documents/GitHub/CuneoWebsite.io/Assets/", pattern = "dvw$", full.names = TRUE)
    lx <- list()
    ## read each file
    for (fi in seq_along(d)) lx[[fi]] <- dv_read(d[fi], insert_technical_timeouts = FALSE)
    ## now extract the play-by-play component from each and bind them together
    px <- list()
    for (fi in seq_along(lx)) px[[fi]] <- plays(lx[[fi]])
    px <- do.call(rbind, px)
    
    table_data <- px %>% 
      dplyr::filter(skill == "Serve", team == teamName) %>% 
      group_by(player_name) %>% 
      dplyr::summarize(
        N_battute = n(),
        count_perfette = sum(evaluation_code == "#", na.rm = TRUE),
        count_positive = sum(evaluation_code == "+", na.rm = TRUE),
        count_errori = sum(evaluation_code == "=", na.rm = TRUE),
        positività = sum(count_positive + count_perfette) / N_battute,
        efficienza = sum(count_positive + count_perfette - count_errori) / N_battute
      )
    
    # Calculate cumulative statistics for the team
    team_total <- table_data %>%
      summarise(
        N_battute = sum(N_battute),
        count_perfette = sum(count_perfette),
        count_positive = sum(count_positive),
        count_errori = sum(count_errori),
        positività = sum(count_positive + count_perfette) / sum(N_battute),
        efficienza = sum(count_positive + count_perfette - count_errori) / sum(N_battute)
      ) %>%
      mutate(player_name = "TOT. Squadra")  # Add a player_name for the team total row
    
    # Combine the team total row with the original table data
    table_data_with_total <- bind_rows(table_data, team_total)
    
    return(table_data_with_total)
  })
  
  # Create plots
  output$plot1 <- renderPlotly({
    data <- data_plot()
    
    plot_ly(data = data, 
            x = ~positività * 100, 
            y = ~efficienza * 100,
            type = 'scatter', 
            mode = 'markers', 
            hovertemplate = paste('<i>Player</i>: %{text}',
                                  '<br><b>Positività (%)</b>: %{x})',
                                  '<br><b>Efficienza (%)</b>: %{y}',
                                  '<br><b>Battuta</b>: %{marker.size}<extra></extra>'),
            color = ~positività,
            marker = list(size = ~N_battute, sizemode = "area", sizeref = 0.005, opacity = 0.5),
            text = ~player_name
    )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

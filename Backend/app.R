library(shiny)
library(plotly)
library(datavolley)
library(dplyr)
library(DT)
library(scales)

ui <- fluidPage(
  titlePanel("Volleyball Stats Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("folderPath", "Enter folder path:", 
                value = "C:/Users/mirko/Documents/GitHub/CuneoWebsite.io/Assets/"),
      textInput("teamName", "Enter team name:", 
                value = "HONDA OLIVERO S.BERNARDO CUNEO"),
      selectInput("skillChoice", "Select skill:", 
                  choices = c("Serve", "Reception", "Attack"),#, "Block", "Dig"
                  selected = "Reception"),
      actionButton("updateData", "Update Data")
    ),
    
    mainPanel(
      plotlyOutput("plot1"),
      DTOutput("statsTable")
    )
  )
)

server <- function(input, output, session) {
  
  data_plot <- eventReactive(input$updateData, {
    # Get all .dvw files in the specified directory
    d <- dir(input$folderPath, pattern = "dvw$", full.names = TRUE)
    
    # Read and process each file
    px <- lapply(d, function(file) {
      x <- dv_read(file, insert_technical_timeouts = FALSE)
      plays(x)
    })
    px <- do.call(rbind, px)
    
    # Process data
    table_data <- px %>% 
      dplyr::filter(skill == input$skillChoice, team == input$teamName) %>% 
      group_by(player_name) %>% 
      dplyr::summarize(
        N_actions = n(),
        count_perfette = sum(evaluation_code == "#", na.rm = TRUE),
        count_positive = sum(evaluation_code == "+", na.rm = TRUE),
        count_errori = sum(evaluation_code == "=", na.rm = TRUE),
        positività = (count_positive + count_perfette) / N_actions,
        efficienza = (count_positive + count_perfette - count_errori) / N_actions
      )
    
    # Calculate team total
    team_total <- table_data %>%
      summarise(across(where(is.numeric), sum),
                positività = sum(count_positive + count_perfette) / sum(N_actions),
                efficienza = sum(count_positive + count_perfette - count_errori) / sum(N_actions)) %>%
      mutate(player_name = "TOT. Squadra")
    
    # Combine player data and team total
    bind_rows(table_data, team_total) %>%
      mutate(
        positività_percent = sprintf("%.0f%%", positività * 100),
        efficienza_percent = sprintf("%.0f%%", efficienza * 100)
      ) %>%
      #select(player_name, efficienza_percent, positività_percent, positività, efficienza, N_actions, count_perfette, count_positive, count_errori)
      select(player_name, efficienza, efficienza_percent, everything())
  })
  
  output$plot1 <- renderPlotly({
    req(data_plot())
    data <- data_plot()
    
    plot_ly(data = data, 
            x = ~positività * 100, 
            y = ~efficienza * 100,
            type = 'scatter', 
            mode = 'markers', 
            hovertemplate = paste('<i>Player</i>: %{text}',
                                  '<br><b>Positività</b>: %{x:.1f}%',
                                  '<br><b>Efficienza</b>: %{y:.1f}%',
                                  '<br><b>Actions</b>: %{marker.size}<extra></extra>'),
            color = ~positività,
            marker = list(size = ~N_actions, sizemode = "area", sizeref = 0.005, opacity = 0.5),
            text = ~player_name
    ) %>%
      layout(xaxis = list(title = "Positività (%)"),
             yaxis = list(title = "Efficienza (%)"),
             title = paste("Player Performance -", input$skillChoice))
  })
  
  output$statsTable <- renderDT({
    req(data_plot())
    data <- data_plot()
    
    colorFunc <- function(data, skill) {
      if (skill == "Serve") {
        styleInterval(c("25%", "30%"), c("lightcoral", "yellow", "lightgreen"))
      } else if (skill == "Reception") {
        case_when(
          data$player_name %in% c("Serena Scognamillo", "Federica Ferrario") ~ 
            styleInterval(c("48%", "56%"), c("lightcoral", "yellow", "lightgreen")),
          data$player_name %in% c("Lena Stigrot", "Anna Haak") ~ 
            styleInterval(c("31%", "37%"), c("lightcoral", "yellow", "lightgreen")),
          data$player_name %in% c("Alice Tanase", "Madison Kubik") ~ 
            styleInterval(c("37%", "43%"), c("lightcoral", "yellow", "lightgreen")),
          TRUE ~ styleInterval(c("41%", "42%"), c("red", "orange", "lightgreen"))
        )
      } else if (skill == "Attack") {
        case_when(
          data$player_name %in% c("Anna Adelusi", "Terry Ruth Enweonwu") ~ 
            styleInterval(c("24%", "27%"), c("lightcoral", "yellow", "lightgreen")),
          data$player_name %in% c("Anna Haak", "Lena Stigrot") ~ 
            styleInterval(c("30%", "34%"), c("lightcoral", "yellow", "lightgreen")),
          data$player_name %in% c("Alice Tanase", "Madison Kubik") ~ 
            styleInterval(c("20%", "24%"), c("lightcoral", "yellow", "lightgreen")),
          data$player_name %in% c("Saly Thior", "Amandha Sylves", "Anna Hall", "Beatrice Molinaro") ~ 
            styleInterval(c("38%", "44%"), c("lightcoral", "yellow", "lightgreen")),
          TRUE ~ styleInterval(c("41%", "42%"), c("red", "orange", "lightgreen"))
        )
      } else {
        styleInterval(c("25%", "30%"), c("lightcoral", "yellow", "lightgreen"))
      }
    }
    
    datatable(data, options = list(pageLength = 10)) %>%
      formatStyle(
        'efficienza_percent',
        backgroundColor = colorFunc(data, input$skillChoice)
      ) %>%
      formatStyle(
        'player_name',
        target = 'row',
        fontWeight = styleEqual("TOT. Squadra", 'bold')
      )
  })
}

shinyApp(ui = ui, server = server)
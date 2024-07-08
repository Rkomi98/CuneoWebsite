library(shiny)
library(plotly)
library(datavolley)
library(dplyr)
library(DT)
library(scales)
#library(rsconnect)

ui <- fluidPage(
  titlePanel("Volleyball Stats Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("folderPath", "Enter folder path:", 
                value = "Assets/"),
      textInput("teamName", "Enter team name:", 
                value = "HONDA OLIVERO S.BERNARDO CUNEO"),
      selectInput("skillChoice", "Select skill:", 
                  choices = c("Serve", "Reception", "Attack"),#, "Block", "Dig"
                  selected = "Reception"),
      actionButton("updateData", "Update Data")
    ),
    
    mainPanel(
      DTOutput("statsTable"),
      plotlyOutput("plot2"),
      plotlyOutput("plot1"),
    )
  )
)

server <- function(input, output, session) {
  
  data_plot <- eventReactive(input$updateData, {
    cat("Updating data...\n")
    # Get all .dvw files in the specified directory
    d <- dir(input$folderPath, pattern = "dvw$", full.names = TRUE)
    
    if (length(d) == 0) {
      cat("No .dvw files found in directory.\n")
      return(NULL)
    }
    
    # Read and process each file
    px <- lapply(d, function(file) {
      cat("Reading file:", file, "\n")
      x <- dv_read(file, insert_technical_timeouts = FALSE)
      plays(x)
    })
    px <- do.call(rbind, px)
    
    if (is.null(px)) {
      cat("No data processed from files.\n")
      return(NULL)
    }
    
    table_data <- px %>% 
      dplyr::filter(skill == input$skillChoice, team == input$teamName)
    
    if (nrow(table_data) == 0) {
      cat("No matching data for skill:", input$skillChoice, "and team:", input$teamName, "\n")
      return(NULL)
    }
    
    table_data <- table_data %>% 
      group_by(player_name) %>% 
      dplyr::summarize(
        N_actions = n(),
        count_perfette = sum(evaluation_code == "#", na.rm = TRUE),
        count_positive = sum(evaluation_code == "+", na.rm = TRUE),
        count_errori = sum(evaluation_code == "=", na.rm = TRUE),
        positività = round((count_positive + count_perfette) / N_actions, digits = 3),
        efficienza = round((count_positive + count_perfette - count_errori) / N_actions, digits = 3),
      )
    
    # Calculate team total
    team_total <- table_data %>%
      summarise(across(where(is.numeric), sum),
                positività = round(sum(count_positive + count_perfette) / sum(N_actions), digits = 3),
                efficienza = round(sum(count_positive + count_perfette - count_errori) / sum(N_actions),digits = 3)) %>%
      mutate(player_name = "TOT. Squadra")
    
    # Combine player data and team total
    result <- bind_rows(table_data, team_total) %>%
      mutate(
        positività_percent = sprintf("%.0f%%", positività * 100),
        Efficienza = sprintf("%.0f%%", efficienza * 100) #Efficienza in percent
      ) %>%
      select(player_name, Efficienza, everything())
    
    cat("Data processed successfully.\n")
    return(result)
  })
  
  colorFunc <- function(data, skill) {
    if (skill == "Serve") {
      return(styleInterval(c("25%", "30%"), c("lightcoral", "yellow", "lightgreen")))
    } else if (skill == "Reception") {
      intervals <- case_when(
        data$player_name %in% c("Serena Scognamillo", "Federica Ferrario") ~ c("48%", "56%"),
        data$player_name %in% c("Lena Stigrot", "Anna Haak") ~ c("31%", "37%"),
        data$player_name %in% c("Alice Tanase", "Madison Kubik") ~ c("37%", "43%"),
        TRUE ~ c("41%", "42%")
      )
      return(styleInterval(intervals, c("lightcoral", "yellow", "lightgreen")))
    } else if (skill == "Attack") {
      intervals <- case_when(
        data$player_name %in% c("Anna Adelusi", "Terry Ruth Enweonwu") ~ c("24%", "27%"),
        data$player_name %in% c("Anna Haak", "Lena Stigrot") ~ c("30%", "34%"),
        data$player_name %in% c("Alice Tanase", "Madison Kubik") ~ c("20%", "24%"),
        data$player_name %in% c("Saly Thior", "Amandha Sylves", "Anna Hall", "Beatrice Molinaro") ~ c("38%", "44%"),
        TRUE ~ c("41%", "42%")
      )
      return(styleInterval(intervals, c("lightcoral", "yellow", "lightgreen")))
    } else {
      return(styleInterval(c("25%", "30%"), c("blue", "yellow", "lightgreen")))
    }
  }
  
  
  output$plot1 <- renderPlotly({
    req(data_plot())
    data <- data_plot()
    
    if (is.null(data)) {
      cat("No data to plot.\n")
      return(NULL)
    }
    
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
  
  output$plot2 <- renderPlotly({
    req(data_plot())
    data <- data_plot()
    
    if (is.null(data)) {
      cat("No data to plot.\n")
      return(NULL)
    }
    
    data$color <- ifelse(data$efficienza < 0.25, "red",
                         ifelse(data$efficienza > 0.30, "green", "yellow"))
    
    plot_ly(
      data = data,
      x = ~player_name,
      y = ~round(efficienza, digits = 2),
      marker = list(color = ~color),
      name = "Efficienza in Battuta",
      type = "bar",
      text = ~round(efficienza, digits = 2), textposition = 'auto',
      marker = list(color = 'rgb(158,202,225)', line = list(color = 'rgb(8,48,107)', width = 1.5))
    ) %>%
      layout(title = paste("Efficienza in -", input$skillChoice),
             xaxis = list(title = ""),
             yaxis = list(title = ""))
  })
  
  output$statsTable <- renderDT({
    req(data_plot())
    data <- data_plot()
    
    if (is.null(data)) {
      cat("No data to display in table.\n")
      return(NULL)
    }
    
    datatable(data, options = list(pageLength = 10)) %>%
      formatStyle(
        'Efficienza',
        backgroundColor = styleInterval(c("25%", "30%"), c("lightcoral", "yellow", "lightgreen"))
      ) %>%
      formatStyle(
        'player_name',
        target = 'row',
        fontWeight = styleEqual("TOT. Squadra", 'bold')
      )
  })
}

shinyApp(ui = ui, server = server)
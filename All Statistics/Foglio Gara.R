library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(datavolley)

# Check and prepare the data
if(!"team" %in% names(px)) {
  stop("The 'team' column is missing from the data.")
}

if(!"player_name" %in% names(px)) {
  if("player" %in% names(px)) {
    final_table <- final_table %>% rename(player_name = player)
  } else {
    stop("Neither 'player_name' nor 'player' column found in the data.")
  }
}

d <- list.files("Scout/", pattern = "dvw$", full.names = TRUE)
lx <- list()
# Read each file with error handling
for (fi in seq_along(d)) {
  tryCatch({
    lx[[fi]] <- dv_read(d[fi], insert_technical_timeouts = FALSE)
  }, error = function(e) {
    message("Error reading file ", d[fi], ": ", e$message)
    # If the error is specifically about the [3SCOUT] section, try to read without it
    if (grepl("\\[3SCOUT\\]", e$message)) {
      message("Attempting to read file without [3SCOUT] section")
      file_content <- readLines(d[fi])
      scout_index <- grep("\\[3SCOUT\\]", file_content)
      if (length(scout_index) > 0) {
        file_content <- file_content[1:(scout_index-1)]
        temp_file <- tempfile(fileext = ".dvw")
        writeLines(file_content, temp_file)
        lx[[fi]] <- dv_read(temp_file, insert_technical_timeouts = FALSE)
        file.remove(temp_file)
      }
    }
  })
}

## now extract the play-by-play component from each and bind them together
px <- list()
for (fp in seq_along(lx)) px[[fp]] <- plays(lx[[fp]])
px <- do.call(rbind, px)

## take just the serves from the play-by-play data
xserves <- subset(px, skill == "Serve")
## if the file had been scouted with coordinate included, we could plot them directly
## this file has no coordinates, so we'll fake some up for demo purposes
coords <- dv_fake_coordinates("serve", xserves$evaluation)
xserves[, c("start_coordinate", "start_coordinate_x", "start_coordinate_y",
            "end_coordinate", "end_coordinate_x", "end_coordinate_y")] <- coords
## now we can plot these
xserves$evaluation[!xserves$evaluation %in% c("Ace", "Error")] <- "Other"

## calculate attack frequency by zone, per team
attack_rate <- px %>% dplyr::filter(skill == "Attack") %>%
  group_by(team, start_zone) %>% dplyr::summarize(n_attacks = n()) %>%
  mutate(rate = n_attacks/sum(n_attacks)) %>% ungroup

## add x, y coordinates associated with the zones
attack_rate <- cbind(attack_rate, dv_xy(attack_rate$start_zone, end = "lower"))

## for team 2, these need to be on the top half of the diagram
tm2i <- attack_rate$team == teams(px)[2]

# Start Dashboard

ui <- dashboardPage(
  dashboardHeader(title = "Volleyball Statistics"),
  dashboardSidebar(
    selectInput("team", "Select Team", choices = unique(final_table$team)),
    selectInput("skill", "Select Skill", choices = c("Serve", "Reception", "Attack", "Block")),
    uiOutput("player_select")
  ),
  dashboardBody(
    fluidRow(
      box(
        textOutput("message")  # Message to display when no team is selected
      )
    ),
    fluidRow(
      box(plotOutput("skill_plot"), width = 12)
    ),
    fluidRow(
      box(tableOutput("player_stats"), width = 12)
    )
  )
)

server <- function(input, output, session) {
  
  # Initial message when no team is selected
  output$message <- renderText({
    if (is.null(input$team)) {
      return("Select a team to see statistics.")
    } else {
      return(NULL)
    }
  })
  
  # Dynamic player selection based on team
  output$player_select <- renderUI({
    req(input$team, cancelOutput = TRUE)  # Ensure team is selected
    players <- final_table %>%
      filter(team == input$team) %>%
      pull(player_name) %>%
      unique()
    selectInput("player", "Select Player", choices = c("All Players", players))
  })
  
  # Reactive data filtering based on selected team and player
  filtered_data <- reactive({
    if (is.null(input$team)) return(NULL)  # Return NULL if no team selected
    data <- final_table %>%
      filter(team == input$team)
    
    if (input$player != "All Players") {
      data <- data %>% filter(player_name == input$player)
    }
    
    if (nrow(data) == 0) return(NULL)  # Check for empty filtered data
    
    data
  })
  
  # Plot generation
  output$skill_plot <- renderPlot({
    req(input$skill, input$team, input$player, filtered_data())
    
    if(input$skill == "Serve") {
      # Filter serves by the selected team and player
      filtered_serves <- px %>%
        filter(skill == "Serve",
               team == input$team,
               player_name == input$player)
      
      # If coordinates are not available, fake them (as in your example)
      if(!"start_coordinate" %in% names(filtered_serves)) {
        coords <- dv_fake_coordinates("serve", filtered_serves$evaluation)
        filtered_serves[, c("start_coordinate", "start_coordinate_x", "start_coordinate_y",
                            "end_coordinate", "end_coordinate_x", "end_coordinate_y")] <- coords
      }
      
      filtered_serves$evaluation[!filtered_serves$evaluation %in% c("Ace", "Error")] <- "Other"
      
      # Plot the serves with filtering applied
      ggplot(filtered_serves, aes(start_coordinate_x, start_coordinate_y,
                                  xend = end_coordinate_x, yend = end_coordinate_y, colour = evaluation)) +
        geom_segment(arrow = arrow(length = unit(2, "mm"), type = "closed", angle = 20)) +
        scale_colour_manual(values = c(Ace = "limegreen", Error = "firebrick", Other = "dodgerblue"),
                            name = "Evaluation") +
        ggcourt(labels = c("Serving team", "Receiving team")) +
        ggtitle(paste("Serve Analysis for", input$player, "from", input$team))
    } else if (input$skill == "Reception") {
      # Reception analysis: Calculate positivity rate for each player in each zone
      reception_rate <- px %>%
        filter(skill == "Reception", team == input$team, !is.na(end_zone))
      
      if (input$player != "All Players") {
        reception_rate <- reception_rate %>%
          filter(player_name == input$player)
      }
      
      # Calculate positivity and reception rate by zone
      reception_rate <- reception_rate %>%
        group_by(end_zone) %>%
        summarize(
          n_receptions = n(),
          positivity = sum(evaluation_code %in% c('#', '+')) / n_receptions  # Correct positivity calculation
        ) %>%
        mutate(rate = n_receptions / sum(n_receptions)) %>%
        ungroup()
      
      # Add coordinates for the reception zones
      reception_rate <- cbind(reception_rate, dv_xy(reception_rate$end_zone, end = "lower"))
      
      # Plot the heatmap for reception frequency with positivity values
      ggplot(reception_rate, aes(x, y, fill = rate)) +
        geom_tile() +
        geom_text(aes(label = scales::percent(positivity)), color = "white", size = 4) +  # Add positivity text
        ggcourt("lower", labels = NULL) +
        scale_fill_gradient2(name = "Rate: reception\nend location") +
        ggtitle(paste("Reception Analysis for", ifelse(input$player == "All Players", input$team, input$player)))
    
    } else if(input$skill == "Attack") {
      # Add attack plot here
    } else if(input$skill == "Block") {
      # Add block plot here
    }
  })
  
  
  # Player stats table
  output$player_stats <- renderTable({
    req(filtered_data())
    filtered_data()
  })
}

shinyApp(ui, server)

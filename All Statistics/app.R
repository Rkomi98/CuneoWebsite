library(shiny)
library(DT)

# Load the pre-processed data
final_table <- readRDS("final_table.rds")

# UI definition
ui <- fluidPage(
  titlePanel("Player Statistics"),
  selectInput("role_filter", "Filter by Role:",
              choices = c("All", unique(final_table$role)),
              selected = "All"),
  DTOutput("table")
)

# Server definition
server <- function(input, output, session) {
  filtered_table <- reactive({
    filtered_data <- final_table
    if (input$role_filter != "All") {
      filtered_data <- filtered_data[filtered_data$role == input$role_filter, ]
    }
    filtered_data <- filtered_data[complete.cases(filtered_data), ]
    return(filtered_data)
  })
  
  output$table <- renderDT({
    datatable(filtered_table(), options = list(pageLength = 10, autoWidth = TRUE))
  })
}

# Run the app
shinyApp(ui = ui, server = server)

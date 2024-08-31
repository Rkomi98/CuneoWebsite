library(datavolley)
library(dplyr)
library(tidyr)

setwd("C:/Users/mirko/Documents/GitHub/CuneoWebsite.io/All Statistics")

d <- dir("Scout/", pattern = "dvw$", full.names = TRUE)

lx <- list()
## read each file
for (fi in seq_along(d)) lx[[fi]] <- dv_read(d[fi], insert_technical_timeouts = FALSE)
## now extract the play-by-play component from each and bind them together
px <- list()
for (fp in seq_along(lx)) px[[fp]] <- plays(lx[[fp]])
px <- do.call(rbind, px)
px

unique_player_names <- unique(px$player_name)
unique_player_names <- unique_player_names[!is.na(unique_player_names)]

battuta_table <- px %>%
  filter(skill == "Serve") %>%
  group_by(player_name) %>%
  summarize(
    Bat_Positività = sum(evaluation_code == '#' | evaluation_code == '+' | evaluation_code == '!') / n(),
    Bat_Perfetta = sum(((evaluation_code == '#')) / n()),
    )

ricezione_table <- px %>%
  filter(skill == "Reception") %>%
  group_by(player_name) %>%
  summarize(
    Rice_Positivita = sum(evaluation_code == '#' | evaluation_code == '+') / n(),
    Rice_Errori = sum(evaluation_code == '=') / n(),
    Rice_Efficienza = (sum(evaluation_code == '#') + sum(evaluation_code == '+') - sum(evaluation_code == '=')) / n()
  )

attack_table <- px %>%
  filter(skill == "Attack") %>%
  group_by(player_name) %>%
  summarize(
    Att_Perfetto = sum(evaluation_code == '#') / n(),
    Att_Efficienza = (sum(evaluation_code == '#') + sum(evaluation_code == '+') - sum(evaluation_code == '=')) / n()
  )

muro_table <- px %>%
  filter(skill == "Block") %>%
  group_by(player_name) %>%
  summarize(
    Muro_positivo = sum(evaluation_code == '#' | evaluation_code == '+' | evaluation_code == '!') / n(),
    Muro_negativo = sum(evaluation_code == '-' | evaluation_code == '=' | evaluation_code == '/') / n(),
  )

punti_totali_table <- px %>%
  filter(skill %in% c("Block", "Attack", "Serve")) %>%
  group_by(player_name) %>%
  summarize(Punti_totali = sum(evaluation_code == '#'))

errori_table <- px %>%
  filter(skill %in% c("Block", "Attack", "Serve", "Reception", "Set")) %>%
  group_by(player_name) %>%
  summarize(Errori = sum(evaluation_code == '='))

efficiency_table <- punti_totali_table %>%
  left_join(errori_table, by = "player_name")

efficiency_table <- efficiency_table %>%
  mutate(eff_player = Punti_totali - Errori)

### Include role
# Directory containing the .sq files
directory_path <- "C:/Users/mirko/Documents/GitHub/CuneoWebsite.io/All Statistics/Elenco Giocatori Squadra/"

# List all .sq files in the directory
sq_files <- list.files(directory_path, pattern = "\\.sq$", full.names = TRUE)
# Initialize an empty list to store the dataframes
df_list <- list()

# Loop through each .sq file
for (file in sq_files) {
  file_content <- readLines(file)
  data_lines <- file_content[-c(1, 2)]
  split_data <- strsplit(data_lines, "\t")
  df <- do.call(rbind, lapply(split_data, function(x) c(x[3], x[9], x[10])))
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  colnames(df) <- c("Cognome", "Nome", "Ruolo")
  df$player_name <- paste(df$Nome, df$Cognome)
  df <- df %>%
    mutate(role = case_when(
      Ruolo == "1" ~ "Libero",
      Ruolo == "2" ~ "Schiacciatore",
      Ruolo == "3" ~ "Opposto",
      Ruolo == "4" ~ "Centrale",
      Ruolo == "5" ~ "Palleggiatore",
      TRUE ~ "Unknown"
    ))
  
  # Add the processed dataframe to the list
  df_list[[file]] <- df
}

# Combine all dataframes into one
combined_role_df <- do.call(rbind, df_list)

# Select only the relevant columns
role <- combined_role_df %>%
  select(player_name, role)

final_table <- battuta_table %>%
  left_join(ricezione_table, by = "player_name") %>%
  left_join(attack_table, by = "player_name") %>%
  left_join(muro_table, by = "player_name") %>%
  left_join(efficiency_table, by = "player_name")
#%>% left_join(role, by = "player_name") 

final_table[is.na(final_table)] <- 0

final_table <- final_table%>% left_join(role, by = "player_name") 

# Convert relevant columns to percentage
percentage_columns <- names(final_table)[2:(ncol(final_table)-4)]
# Convert the columns to numeric and then to percentages
final_table[percentage_columns] <- lapply(final_table[percentage_columns], function(x) {
  x <- as.numeric(as.character(x))  # Convert to numeric
  round(x * 100, 2)                 # Multiply by 100 and round to 2 decimal places
})

print(final_table)






### Dashboard online
library(shiny)
library(DT)

# Define UI
ui <- fluidPage(
  titlePanel("Player Statistics B1"),
  
  # Add a filter by role dropdown
  selectInput("role_filter", "Filter by Role:",
              choices = c("All", unique(final_table$role)),
              selected = "All"),
  
  DTOutput("table")
)

# Define Server
server <- function(input, output, session) {
  
  # Reactive expression to filter the table based on role and remove rows with NA values
  filtered_table <- reactive({
    filtered_data <- final_table
    
    # Filter by role if not 'All'
    if (input$role_filter != "All") {
      filtered_data <- filtered_data[filtered_data$role == input$role_filter, ]
    }
    
    # Remove rows with any NA values
    filtered_data <- filtered_data[complete.cases(filtered_data), ]
    
    return(filtered_data)
  })
  
  # Render the DataTable
  output$table <- renderDT({
    datatable(filtered_table(), options = list(pageLength = 10, autoWidth = TRUE))
  })
}

library(rsconnect)
#rsconnect::setAccountInfo(name='your_shinyapps_name', 
#                          token='your_token', 
#                          secret='your_secret')
#rsconnect::deployApp()


# Run the app
shinyApp(ui = ui, server = server)


library(reshape2)
library(ggplot2)
library(fmsb)
library(GGally)

# Melt the dataframe to long format
melted_table <- melt(final_table, id.vars = c("player_name", "role"))

ggplot(melted_table, aes(x = player_name, y = variable, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "red", high = "green", mid = "yellow", midpoint = 50, limit = c(0, 100), space = "Lab") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Performance Metrics Heatmap", x = "Player", y = "Metric")


# Function to create radar plot for a player
create_player_radar_plot <- function(final_table, player_name) {
  # Select the player's data
  player_data <- final_table %>%
    filter(player_name == !!player_name) %>%
    select(Bat_Positività, Rice_Efficienza, Att_Perfetto, Muro_positivo)
  
  # Replace NA values with 0
  player_data[is.na(player_data)] <- 0
  
  # Add max and min rows required by fmsb
  player_data <- rbind(rep(100,4) , rep(0,4) , player_data)
  
  # Set up the radar chart
  par(mar = c(1, 1, 1, 1))  # Adjust margins
  radarchart(
    player_data,
    axistype = 1,
    # Custom polygon
    pcol = "blue",
    pfcol = scales::alpha("blue", 0.5),
    plwd = 2,
    # Custom the grid
    cglcol = "grey",
    cglty = 1,
    cglwd = 0.8,
    # Custom the axis
    axislabcol = "grey",
    # Custom labels
    vlcex = 0.8,
    title = paste("Performance Radar for", player_name),
    caxislabels = seq(0, 100, 25)
  )
  
  # Add a legend
  legend(
    x = "topright",
    legend = c("Player Performance"),
    bty = "n", pch = 20, col = "blue", text.col = "black", cex = 0.8
  )
}

# Example usage:
create_player_radar_plot(final_table, "Nicole Piomboni")

performance_data <- final_table %>%
  select(Bat_Positività, Rice_Efficienza, Att_Perfetto, Muro_positivo)

ggpairs(performance_data)


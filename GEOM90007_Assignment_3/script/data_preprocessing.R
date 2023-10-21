library(shiny)
library(leaflet)
library(dplyr)
library(bslib)
library(leaflet.extras)
library(purrr)
library(ggplot2)
library(ggiraph)
library(geosphere)
library(jsonlite)
library(httr)
library(shinyjs)
# Prepare landmarks data
landmarks <- read.csv("../data/landmarks.csv", stringsAsFactors = FALSE)
# Data preprocessing to separate coordinates
coords <- strsplit(as.character(landmarks$`Coordinates`), ",")
landmarks$lat <- as.numeric(sapply(coords, `[`, 1))
landmarks$lng <- as.numeric(sapply(coords, `[`, 2))

preprocess_distance_data <- function() {
  # Get a list of all unique place names
  all_place_names <- unique(landmarks$Feature.Name)
  
  # Initialize an empty data frame to store the final aggregated data
  final_aggregated_data <- data.frame()
  
  # Loop through all place names
  for (place_name in all_place_names) {
    # Retrieve data for the current place
    selected_row <- landmarks[landmarks$Feature.Name == place_name,]
    
    # Check if a valid row is selected
    if (nrow(selected_row) > 0) {
      clicked_point <- c(selected_row$lng, selected_row$lat)
      distances <- distm(clicked_point, cbind(all_points$lng, all_points$lat), fun = distVincentySphere)
      max_break <- max(distances) + 1
      
      # Categorize distances
      distance_category <- cut(
        distances,
        breaks = c(0, 500, 1000, 5000, 10000, max_break),
        labels = c("Within 500m", "500m - 1km", "1km - 5km", "5km - 10km", "Beyond 10km"),
        include.lowest = TRUE
      )
      
      # Create a new dataframe
      df_distance <- data.frame(
        Place = rep(place_name, length(distance_category)),  # Add place_name column to identify the place
        Distance = distance_category,
        Theme = all_points$Theme,
        Category = all_points$Sub.Theme,
        Name = all_points$Feature.Name
      )
      
      # Remove the current place from df_distance
      df_distance <- df_distance[df_distance$Name != place_name, ]
      
      # Aggregate counts and concatenate place names
      df_aggregated <- df_distance %>%
        group_by(Place, Distance, Theme) %>%
        summarise(
          Count = n(),
          PlaceNames = paste(Name, collapse = ", ")
        ) %>%
        ungroup()
      
      # Append the aggregated data for the current place to the final data frame
      final_aggregated_data <- rbind(final_aggregated_data, df_aggregated)
    }
  }
  
  # Write the final aggregated data to a CSV file
  write.csv(final_aggregated_data, "distance_data.csv", row.names = FALSE)
}

# Call the function to preprocess the data and write it to a CSV file
preprocess_distance_data()

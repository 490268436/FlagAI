# Install and load required packages
library(shiny)
library(leaflet)
library(dplyr)
library(bslib)
library(leaflet.extras)
library(purrr)
library(ggplot2)
library(plotly)
library(ggiraph)
library(geosphere)
library(jsonlite)
library(httr)
library(tidyverse)
library(patchwork)
library(owmr)
library(zoo)
library(lubridate)
library(emojifont)
library(shinyjs)
library(shinydashboard)
library(shinyWidgets)
library(viridis)
library(sf)
library(geojsonsf)
source("tableau-in-shiny-v1.0.R")

# Prepare landmarks data
landmarks <- read.csv("../data/landmarks.csv", stringsAsFactors = FALSE)
# Data preprocessing to separate coordinates
coords <- strsplit(as.character(landmarks$`Coordinates`), ",")
landmarks$lat <- as.numeric(sapply(coords, `[`, 1))
landmarks$lng <- as.numeric(sapply(coords, `[`, 2))

theme_counts <- table(landmarks$Theme)
all_counts <- c(theme_counts)
df_bar <- data.frame(Category = names(all_counts), Counts = as.numeric(all_counts))

hotel_icons <- list(
  "4-star" = makeIcon(
    iconUrl = "www/hotel4.png",
    iconWidth = 28, iconHeight = 28
  ),
  "5-star" = makeIcon(
    iconUrl = "www/hotel5.png",
    iconWidth = 28, iconHeight = 28
  ),
  "3-star" = makeIcon(
    iconUrl = "www/hotel3.png",
    iconWidth = 28, iconHeight = 28
  ),
  "Restaurant" = makeIcon(
    iconUrl = "www/restaurant.png",
    iconWidth = 28, iconHeight = 28
  ),
  "Cafe" = makeIcon(
    iconUrl = "www/cafe.png",
    iconWidth = 28, iconHeight = 28
  )
)

theme_icons <- list(
  "Place of Worship" = "www/church.png",
  "Leisure and Recreation" = "www/beach.png",
  "Public and Institutional Facilities" = "www/office.png",
  "Place Of Assembly" = "www/assembly.png",
  "Health Services" = "www/hospital.png",
  "Retail" = "www/store.png",
  "Foodie" = "www/cafe.png",
  "Accommodation" = "www/hotel.png",
  "PublicTransport" = "www/transport.png",
  "Bus" = "www/bus.png",
  "Shuttle Bus" = "www/shuttle-bus.png",
  "Bike" = "www/bike.png",
  "Tram" = "www/tram.png",
  "Train" = "www/train.png"

)

icons_list <- lapply(theme_icons, function(url) {
  makeIcon(iconUrl = url, iconWidth = 40, iconHeight = 40)
})

# Combine the datasets
all_points <- select(landmarks, Theme, Feature.Name, Sub.Theme, lat, lng)

# prepare all transport datasets
bike_data <- read.csv('../data/bike-share-dock-filtered.csv')
bike_data$lat <- as.numeric(bike_data$lat)
bike_data$lng <- as.numeric(bike_data$lng)

bus_data <- read.csv("../data/bus-stops-filtered.csv")
bus_data$lat <- as.numeric(bus_data$lat)
bus_data$lng <- as.numeric(bus_data$lng)

bus_route <- st_read("../data/BUS_ROUTE") 
bus_sf <- st_transform(bus_route, "+proj=longlat +datum=WGS84")

tram_stops <- read.csv('../data/tram-stops-filtered.csv')
tram_stops$lat <- as.numeric(tram_stops$lat)
tram_stops$lng <- as.numeric(tram_stops$lng)

tram_route <- st_read("../data/VIC_TRAM_ROUTE") 
tram_sf <- st_transform(tram_route, "+proj=longlat +datum=WGS84")

train_data <- read.csv("../data/metro-train-filtered.csv")
train_data$lat <- as.numeric(train_data$lat)
train_data$lng <- as.numeric(train_data$lng)

shuttle_bus_data <- read.csv("../data/shuttle-bus-filtered.csv")
shuttle_bus_data$lat <- as.numeric(shuttle_bus_data$lat)
shuttle_bus_data$lng <- as.numeric(shuttle_bus_data$lng)

transport_distance_data <- read.csv("../data/transport_distance_data.csv")
transport_data <- read.csv("../data/transport_data.csv")

aggregated_df = read.csv("../data/aggregated_data.csv")

# Load the datasets
data_res <- read.csv("../data/data_without_commas.csv")

# Load hotel data
hotels <- read.csv("../data/output_data_with_recent_prices.csv")

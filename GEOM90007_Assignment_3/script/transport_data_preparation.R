library('shiny')
library('ggplot2')
library('ggiraph')
library('leaflet')
library('leaflet.extras')
library('dplyr')
library('tidyr')
library(viridis)
library(sf)
library(geojsonsf)

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
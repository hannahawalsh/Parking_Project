### Hannah Walsh ###
### Pasadena Parking Data ###
### Trying out using maps and stuff ###


# Close all open plots and clear the workspace
graphics.off()
rm(list = ls())


# Load any packages I want to use
library(leaflet)
library(shiny)

# Import the data
filename = "/Users/hannahwalsh/Desktop/Parking_Data/Citation_Data_CSV.csv"
Parking_Data = read.csv(filename, sep=",", header=TRUE)


# Find the min, max, and middle latitude and longitude
min_lat = min(Parking_Data$Latitude, na.rm = T)
max_lat = max(Parking_Data$Latitude, na.rm = T)

min_long = min(Parking_Data$Longitude, na.rm = T)
max_long = max(Parking_Data$Longitude, na.rm = T)


# Try making a map of Pasadena
map1 = leaflet() %>%
  addTiles() %>%
  fitBounds(min_long, min_lat, max_long, max_lat)
map1

# Try making a map of Pasadena with ticket locations from 1/20/16
lat = Parking_Data$Latitude[Parking_Data$Issue.Date == "1/20/16"]
long = Parking_Data$Longitude[Parking_Data$Issue.Date == "1/20/16"] 
location_1.20 = data.frame(long, lat)
location_1.20 = location_1.20[complete.cases(location_1.20),] # remove NAs

map2 = leaflet(data = location_1.20) %>%
  addTiles() %>%
  addCircles(~long, ~lat, radius = 20, color = "darkmagenta", fillOpacity = 1)
map2

map3 = leaflet(location_1.20) %>% addTiles() %>% 
  addMarkers(clusterOptions = markerClusterOptions()) # Clustered data from 1/20
map3


# Cluster all the data
lat = Parking_Data$Latitude
long = Parking_Data$Longitude
locationAll = data.frame(long, lat)
locationAll = locationAll[complete.cases(locationAll),] # remove NAs

map4 = leaflet(data = locationAll) %>%
  addTiles() %>%
  addMarkers(clusterOptions = markerClusterOptions())
map4


# Try having a pop-up
Parking_Data1 = Parking_Data
Parking_Data1 = Parking_Data1[complete.cases(Parking_Data1),] # remove NAs
locationAll1 = data.frame(Parking_Data1$Longitude, Parking_Data1$Latitude)
info = paste(sep = "<br/>", paste(Parking_Data1$Block, Parking_Data1$Location),
             paste(sep = "", "Code ", Parking_Data1$Code, ": ", 
                   Parking_Data1$Description), 
             paste("Officer:", Parking_Data1$Officer), 
             paste(Parking_Data1$Issue.Date, Parking_Data1$Issue.Time))
map5 = leaflet(data = locationAll1) %>%
  addTiles() %>%
  addMarkers(Parking_Data1$Longitude, Parking_Data1$Latitude, popup = info)
map5



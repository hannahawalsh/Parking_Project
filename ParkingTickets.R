### Hannah Walsh ###
### Pasadena Parking Data ###
### Larger Data Set UI ###


# Close all open plots and clear the workspace
graphics.off()
rm(list = ls())


# Load any packages I want to use
library(RDSTK)
library(leaflet)
library(shiny)
library(plyr)


# Import Data
setwd("/Users/hannahwalsh/Desktop/Parking_Data/cleanedData/")
files = list.files(pattern="*.csv")
Data = read.csv(files[1], sep = ",", header = T)
for (i in 2:length(files))
{
  dat = read.csv(files[i], sep = ",", header = T)
  Data = rbind(Data, dat)
}


# Need Latitude and Longitudes in order to map
Addresses = paste(Data$Block.Number, Data$Location, "Pasadena, CA")
Latitude = NA
Longitude = NA
for (i in 1:length(Addresses))
{
  info = street2coordinates(Addresses[i])
  Latitude[i] = info$latitude
  Longitude[i] = info$longitude
}


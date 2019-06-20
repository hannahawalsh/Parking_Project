### Hannah Walsh ###
### Pasadena Parking Data ###
### Distance Differences ###


# Close all open plots and clear the workspace
graphics.off()
rm(list = ls())


# Load any packages I want to use
library(RDSTK)
library(geosphere)


# Import Data
setwd("/Users/hannahwalsh/Desktop/Parking_Data/")
filename1 = "Citation_Data_CSV.csv"
Parking_Data = read.csv(filename1, sep=",", header=TRUE)

setwd("/Users/hannahwalsh/Desktop/Parking_Data/www/")
filename2 = "Lats&Longs.csv"
Location_Data = read.csv(filename2, sep=",", header = TRUE)

setwd("/Users/hannahwalsh/Desktop/Parking_Data/www/cleanedData/")
files = list.files(pattern="*.csv")
Complete_Data = read.csv(files[1], sep = ",", header = TRUE)
for (i in 2:length(files))
{
  dat = read.csv(files[i], sep = ",", header = TRUE)
  Complete_Data = rbind(Complete_Data, dat)
}


## The following was previously done and saved to "Lats&Longs.csv", loaded above
# # Make 2 new vectors for latitude and longitude
# AddLat = NA
# AddLong = NA
# 
# 
# # Make a string array that pastes on "Pasadena, CA"
# Addresses = paste(Parking_Data$Block, Parking_Data$Location, "Pasadena, CA")
# 
# 
# # Find the Latitude and Longitude using the RDSTK package
# # This is commented out because it takes a long time and I saved it to a file
# for (i in 1:length(Parking_Data$Location))
# {
#   info = street2coordinates(Addresses[i])
#   AddLat[i] = info$latitude
#   AddLong[i] = info$longitude
# }
# # Read in the CSV I wrote to earlier
# # coords = read.csv("Lats&Longs.csv", sep=",", header=TRUE)
# 
# # Calculate the difference in distance
# DiffLat = abs(AddLat - Parking_Data$Latitude)
# DiffLong = abs(AddLong - Parking_Data$Longitude)
# 
# 
# # Create a histogram for the differences
# hist(DiffLat)
# hist(DiffLong)
# 
# 
# # Make a data frame
# coordinates = data.frame(Parking_Data$Latitude, Parking_Data$Longitude,
#                          AddLat, AddLong, DiffLat, DiffLong)
# colnames(coordinates) = c("Ticket Latitude", "Ticket Longitude", 
#                           "Address Latitude", "Address Longitude", 
#                           "Difference Latitude", "Difference Longitude")
# 
# # Write data frame to a file
# write.csv(coordinates, file = "Lats&Longs.csv")

# ###
# ## Write a CSV of the lat and long of huge data set
# AddLat = NA
# AddLong = NA
# 
# Addresses = paste(Complete_Data$Block.Number, Complete_Data$Location,
#                   "Pasadena, CA")
# #skip 5333, 5334, 5335
# for (i in 5333:length(Complete_Data$Location))
# {
#   street2coordinates <- function(address, session=getCurlHandle())
#     {
#     api <- paste(getOption("RDSTK_api_base"), "/street2coordinates/", sep="")
#     get.addy <- getURL(paste(api, URLencode(address), sep=""), curl=session)
#     clean.addy <- lapply(fromJSON(get.addy), 
#                          lapply, 
#                          function(x) ifelse(is.null(x), NA, x))
#     result <- ldply(clean.addy, data.frame)
#     names(result)[1] <- "full.address"
#     return(result)
#   }
#   
#   info = street2coordinates(Addresses[i])
#   AddLat[i] = info$latitude
#   AddLong[i] = info$longitude
# }
# coords = data.frame(AddLong, AddLat)
# colnames(coords) = c("Longitude", "Latitude")
# 
# setwd("/Users/hannahwalsh/Desktop/")
# write.csv(coords, file="Locations")
# 
# ###

# Calculate the change in distance, as the crow flies
address_coords = matrix(c(Location_Data$Address.Longitude, 
                          Location_Data$Address.Latitude), ncol = 2)
gps_coords = matrix(c(Location_Data$Ticket.Longitude, 
                      Location_Data$Ticket.Latitude), ncol = 2)
diff_loc = distHaversine(gps_coords, address_coords) # in meters

# Make a histogram of the data
h1 = hist(diff_loc, main = "Histogram of Distance Between Recorded Location
          and GPS Location", xlab = "Difference of location (meters)",
          col = "darkgreen", border = FALSE)
# We can see from the histogram that we clearly have some outliers
diff_med = median(diff_loc, na.rm = TRUE)
diff_mean = mean(diff_loc, na.rm = TRUE)
diff_min = min(diff_loc, na.rm = TRUE)
diff_max = max(diff_loc, na.rm = TRUE)
diff_quant = quantile(diff_loc, na.rm = TRUE)

# Make a boxplot of the data
b1 = boxplot(diff_loc, main = "Boxplot of Location Difference in Meters")
# We can still see we have quite a few outliers

# Lets make a histogram without the outliers
diff_loc_noout = diff_loc[diff_loc < diff_quant[4]] # 0% to 75%
h2 = hist(diff_loc_noout, main="Histogram of Distance Between Recorded 
          Location and GPS Location Up To 3rd Quantile", 
          xlab="Difference of Location (meters)", col="navy", border=FALSE)
h3 = hist(diff_loc, plot=FALSE)
h3$counts = log10(h3$counts)
plot(h3, col="red", ylim = c(0,4.5),
     main = "Histogram of the Base 10 Logarithm of the Distance Between Recorded
     Location and GPS Location")

b2 = boxplot(diff_loc, outline = FALSE, main = "Boxplot of Location Differance
             Without Outliers (meters)")

## For the original three weeks of data:
# Look for trends by officers
officer_loc = data.frame(Parking_Data$Officer, diff_loc)


# Change the date string to a date, find weekday & quarter, convert to time
Parking_Data$Issue.Date = as.Date(Parking_Data$Issue.Date, "%m/%d/%y")
# for (i in 1:length(Parking_Data$Issue.Date))
# {
#   str = paste(toString(Parking_Data$Issue.Date[i]), 
#               toString(Parking_Data$Issue.Time[i]))
#   Parking_Data$DateTime[1]= strptime(str, "%m/%d/%y %I:%M %p")
# }
Parking_Data$Weekday = weekdays(Parking_Data$Issue.Date)
Parking_Data$Quarter = quarters(Parking_Data$Issue.Date)
# for (i in 1:length(Parking_Data$Issue.Time))
# {
#   str = toString(Parking_Data$Issue.Time[i])
#   Parking_Data$Issue.Time[i] = str #strptime(str, "%I:%M %p")
# }

## For all the data
# for (i in 1:length(Complete_Data$Issue.Date))
# {
#   str = toString(Complete_Data$Issue.Date[i])
#   datetime = strptime(str, "%m/%d/%Y %I:%M:%S %p")
#   datetime = toString(str)
#   Complete_Data$DateTime[i] = datetime
# }



# Plot Times and Dates
# Date = Complete_Data$Issue.Date[complete.cases(Parking_Data$Issue.Date)]
# TicketsPerDay = table(Date)
# TicketsPerDay = TicketsPerDay[with(TicketsPerDay, order(Issue.Date)),]








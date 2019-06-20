### Hannah Walsh ###
### BEM/Ec 150, Spring 2016 ###
### Initial Plots of Project Data ###


# Close all open plots and clear the workspace
graphics.off()
rm(list = ls())

# Import any libraries I want
library(plyr)
library(RgoogleMaps)
library(ggplot2)
library(ggmap)

# Import the data
filename = "/Users/hannahwalsh/Desktop/Parking_Data/Citation_Data_CSV.csv"
Parking_Data = read.csv(filename, sep=",", header=TRUE)


# Match code with its infraction name
codes = NA
descriptions = NA
for (KK in 1:length(Parking_Data$Code))
{
  if (!Parking_Data$Code[KK] %in% codes)
  {
    codes = c(codes, Parking_Data$Code[KK])
    descriptions = c(descriptions, toString(Parking_Data$Description[KK]))
  }
}
Key = data.frame(codes, descriptions)
Key = Key[-1,]

# Sort according to infraction code
Key = Key[with(Key, order(codes)),]

# Give each of the codes its own rainbow color
ColorRange = c(rainbow(length(Key$codes)/2), 
               rainbow(length(Key$codes)/2+1, s = .5, v = .5))
Key["Colors"] = ColorRange
Key["Symbols"] = c(0:25, 0:8)


## Tickets per officer
tickets_off = table(Parking_Data$Officer)
plot1 = barplot(tickets_off, col = "black", main="Tickets Given Per Officer", 
        ylab="Number of Tickets", border = NA, las = 2, ylim = c(0, 1200),
        cex.names = 0.6)
text(x = plot1, y = as.vector(tickets_off), label = as.vector(tickets_off), 
     pos = 3, col = "red")


## Tickets by type of ticket
ticket_type = table(Parking_Data$Description)
ticket_type1 = table(Parking_Data$Description)
ticket_code = table(Parking_Data$Code)

# Replace long labels with more manageable ones
names(ticket_type)[1] = "Yellow/Green/White"
names(ticket_type)[2] = "72 hour"
names(ticket_type)[4] = "HC sidwk access"
names(ticket_type)[8] = "18\"+ from curb"
names(ticket_type)[9] = "Fire hydrant-15 ft"
names(ticket_type)[11] = "Pref prkig dist"
names(ticket_type)[12] = "Bus zone"
names(ticket_type)[13] = "New years eve"
names(ticket_type)[14] = "No reg tab"
names(ticket_type)[16] = "7a-9a/4p-6p/3p-7p"
names(ticket_type)[17] = "7a-9a/7a-6p/9a-6p/3p-7p/4p-6p"
names(ticket_type)[18] = "Oversize veh 10p-6a"
names(ticket_type)[19] = "Overnight parking"
names(ticket_type)[20] = "Overtime parking"
names(ticket_type)[21] = "Blocking driveway"
names(ticket_type)[22] = "Handicapped space"
names(ticket_type)[23] = "Blocking fire hydrant"
names(ticket_type)[25] = "Front lawn"
names(ticket_type)[26] = "Parkway"
names(ticket_type)[27] = "Private Property"
names(ticket_type)[28] = "Non-taxi space"
names(ticket_type)[29] = "Comm. residential"
names(ticket_type)[30] = "Red zone"
names(ticket_type)[31] = "Restricted"
names(ticket_type)[32] = "Taxis only"
names(ticket_type)[33] = "No stopping/standing"
names(ticket_type)[35] = "City Property"

plot2 = barplot(ticket_type, col="black", main="Types of Tickets", 
                ylab="Number of Tickets", border=NA, ylim = c(0, 4000), 
                cex.names=0.3, las=2)
text(x = plot2, y = as.vector(ticket_type), label = as.vector(ticket_type),
     pos=3, col="red")


# Plot data for 1/20/16 by location
# Pull out the data for 1/20/16
lat20 = Parking_Data$Latitude[Parking_Data$Issue.Date == "1/20/16"] # Lattitude
long20 = Parking_Data$Longitude[Parking_Data$Issue.Date == "1/20/16"] #Longitude
code20 = Parking_Data$Code[Parking_Data$Issue.Date == "1/20/16"] # Infraction



# Color Code the plot based on infraction
PlotColors = rep(NA, length(code20)) 
PlotSymbols = rep(NA, length(code20))
for (i in 1:length(Key$Colors))
{
  for (j in 1:length(PlotColors))
  {
    if (code20[j] == Key$codes[i])
    {
      PlotColors[j] = Key$Colors[i]
      PlotSymbols[j] = Key$Symbols[i]
    }
  }
}




layout(matrix(1:2, ncol=2)) # Plot multiple graphs on same plot
# Location Plot
plot3 = plot(lat20, long20, main="Tickets on 1/20/16", ylab="Longitude",
             xlab="Latitude", col=PlotColors, pch=PlotSymbols)
# Bar Plot
plot4 = barplot(table(code20), ylim=c(0, 200), 
                main="Number of Tickets by Code on 1/20/16", 
                xlab="Infraction Code", ylab = "Number of Tickets", 
                col=Key$Colors) 
# Legend
legend(x="right", legend=Key$descriptions, col=Key$Colors, pch=Key$Symbols, 
       cex = .7, text.width=12)


  
# Find the latitude and longitude of the block and location listed by officer
Parking_Data["Address.Latitude"] = rep(NA, length(Parking_Data$Latitude))
Parking_Data["Address.Longitude"] = rep(NA, length(Parking_Data$Longitude))

#for (i in 1:length(Parking_Data$Latitude))
#{
#  if (!is.na(Parking_Data$Latitude)[i])
#  {
#  coords = geocode(paste(Parking_Data$Block, Parking_Data$Location, 
#                         "Pasadena, CA"), output = "latlon", source = "dsk")
#  }
#}

# Find the min, max, and middle latitude and longitude
min_lat = min(Parking_Data$Latitude, na.rm = T)
max_lat = max(Parking_Data$Latitude, na.rm = T)

min_long = min(Parking_Data$Longitude, na.rm = T)
max_long = max(Parking_Data$Longitude, na.rm = T)

mid_lat = (max_lat - min_lat) / 2 + min_lat
mid_long = (max_long - min_long) / 2 + min_long


# Plot number of tickets per day
TicketsDay = table(Parking_Data$Issue.Date)
# This is bad code but I need it for class ### Fix this ###
dates = c("1/1/16", "1/2/16", "1/3/16", "1/4/16", "1/5/16", "1/6/16",
          "1/7/16", "1/8/16", "1/9/16")



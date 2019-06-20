### Hannah Walsh ###
### Pasadena Parking Data ###
### UI through Shiny ###


# Load any packages I want to use
library(leaflet)
library(shiny)
library(plyr)

# Import Data
filename = "Citation_Data_CSV.csv"
Parking_Data = read.csv(filename, sep=",", header=TRUE)


# Simple definintions and calculations (outside loop)
min_lat = min(Parking_Data$Latitude, na.rm = T)
max_lat = max(Parking_Data$Latitude, na.rm = T)

min_long = min(Parking_Data$Longitude, na.rm = T)
max_long = max(Parking_Data$Longitude, na.rm = T)

cleaned_parking = data.frame(Parking_Data$Longitude, Parking_Data$Latitude, 
                             Parking_Data$Code, Parking_Data$Officer, 
                             Parking_Data$Issue.Date, Parking_Data$Issue.Time,
                             Parking_Data$Description)
cleaned_parking = cleaned_parking[complete.cases(cleaned_parking),] # Remove NAs

# Rename the columns:
names(cleaned_parking)[names(cleaned_parking) == "Parking_Data.Longitude"] = "Longitude"
names(cleaned_parking)[names(cleaned_parking) == "Parking_Data.Latitude"] = "Latitude"
names(cleaned_parking)[names(cleaned_parking) == "Parking_Data.Code"] = "Code"
names(cleaned_parking)[names(cleaned_parking) == "Parking_Data.Officer"] = "Officer"
names(cleaned_parking)[names(cleaned_parking) == "Parking_Data.Issue.Date"] = "Date"
names(cleaned_parking)[names(cleaned_parking) == "Parking_Data.Issue.Time"] = "Time"
names(cleaned_parking)[names(cleaned_parking) == "Parking_Data.Description"] = "Description"

# Change the date strings to dates
cleaned_parking["Date"] = as.Date(cleaned_parking$Date, "%m/%d/%y")

# Add Weekday
cleaned_parking["Weekday"] = weekdays(cleaned_parking$Date)

# Get the ticket count for the days
ticketsDay = count(cleaned_parking$Date)
names(ticketsDay)[names(ticketsDay) == "x"] = "Date"
ticketsDay = ticketsDay[with(ticketsDay, order(Date)),]

# Get the ticket count by day of the week
weekticket = count(cleaned_parking$Weekday)
weekticket$x = factor(weekticket$x, levels = c("Sunday", "Monday", "Tuesday", 
                                               "Wednesday", "Thursday", "Friday", 
                                               "Saturday"))
weekticket = weekticket[order(weekticket$x),]

others = 1:50 # The 'other' codes
others = others[others != 1 & others != 2 & others != 3 & others != 6] 



# Make the UI
ui <- navbarPage(
  title = "Parking Violations in Pasadena, CA",

  tabsetPanel(type = "tabs",
              # Plot all the data on a map
              tabPanel("Simple Map",
                       leafletOutput("SimpleMap")
              ),
              
              # Create a cluster map
              tabPanel("Cluster Map",
                       leafletOutput("ClusterMap")
              ),
              
              # Create an interactive map - select by code
              tabPanel("Map By Ticket",
                       leafletOutput("IntMap")
              ),
                       
              # Show some plots
              tabPanel("Plots",
                       plotOutput(outputId = "Week"),
                       plotOutput(outputId = "Day")
              )
  )

           
           # # Add checkboxes
           # checkboxGroupInput("checkGroup1", label = h3("Sort by Ticket Type"), 
           #                    choices = list("Expired Meter" = 1, 
           #                                   "Overnight Parking" = 2, 
           #                                   "Overtime Parking" = 3,
           #                                   "Restricted" = 6, 
           #                                   "Other" = 0))
)



# Set up the server
server <- function(input, output) 
{

  # Plots

      output$Week = renderPlot({barplot(weekticket$freq, names.arg = weekticket$x, 
                                        ylim = c(0,1600), col = "navyblue", 
                                        border = NA, ylab = "Number of Tickets",
                                        main = "Tickets by Weekday")})

      output$Day = renderPlot({plot(ticketsDay, type = "b", 
                                    ylab = "Number of Tickets", 
                                    main = "Tickets over Time", las=2)})


  # Create the simple map, with locations having low opacity
  output$SimpleMap = renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      fitBounds(min_long, min_lat, max_long, max_lat) %>%
      addCircles(data = cleaned_parking, radius = 50, opacity = 0.001, 
                 color = "darkmagenta", stroke = FALSE)
  })
  
  # Cluster Map
  info = paste(sep = "<br/>", 
               paste(sep = ", ", cleaned_parking$Latitude, cleaned_parking$Longitude),
               paste(sep = ": ", "Code ", cleaned_parking$Code, cleaned_parking$Description), 
               paste("Officer:", cleaned_parking$Officer), 
               paste(cleaned_parking$Date, cleaned_parking$Time))
  
  output$ClusterMap = renderLeaflet({
    leaflet(data = cleaned_parking) %>%
      addTiles() %>%
      addMarkers(clusterOptions = markerClusterOptions(), popup = info)
  })
  
  
  # Create the interactive map, use colored circles ### Need to know about layers ###
  output$value <- renderPrint({ input$checkGroup1 }) # Display what is checked
  output$IntMap = renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      fitBounds(min_long, min_lat, max_long, max_lat) %>%
      # Expired Meter
      addCircles(lng = cleaned_parking[cleaned_parking$Code == 1, "Longitude"], 
                 lat = cleaned_parking[cleaned_parking$Code == 1, "Latitude"], 
                 radius = 30, opacity = 0.2, color = "red", stroke = FALSE) %>%
      # Overnight Parking
      addCircles(lng = cleaned_parking[cleaned_parking$Code == 2, "Longitude"], 
                 lat = cleaned_parking[cleaned_parking$Code == 2, "Latitude"], 
                 radius = 30, opacity = 0.7, color = "darkorchid", stroke = FALSE) %>%
      # Overtime Parking
      addCircles(lng = cleaned_parking[cleaned_parking$Code == 3, "Longitude"], 
                 lat = cleaned_parking[cleaned_parking$Code == 3, "Latitude"], 
                 radius = 30, opacity = 0.7, color = "cyan", stroke = FALSE) %>%
      # Restricted Parking
      addCircles(lng = cleaned_parking[cleaned_parking$Code == 6, "Longitude"], 
                 lat = cleaned_parking[cleaned_parking$Code == 6, "Latitude"], 
                 radius = 30, opacity = 0.7, color = "forestgreen", stroke = FALSE) %>%
      # Other
      addCircles(lng = cleaned_parking[cleaned_parking$Code %in% others, "Longitude"], 
                 lat = cleaned_parking[cleaned_parking$Code %in% others, "Latitude"], 
                 radius = 30, opacity = 0.7, color = "lightslateblue", stroke = FALSE) %>%
      #Add Legend
    addLegend(position  = "bottomright", colors = c("red", "darkorchid",
                                                    "cyan", "forestgreen",
                                                    "lightslateblue"),
              labels = c("Expired Meter", "Overnight", "Overtime", "Restricted", 
                         "Other"))

  })
  
  
}



# Link the server and the UI
shinyApp(ui = ui, server = server)






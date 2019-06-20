### Hannah Walsh ###
### Pasadena Parking Data ###
### Trying out UI through Shiny ###


# Close all open plots and clear the workspace
graphics.off()
rm(list = ls())


# Load any packages I want to use
library(leaflet)
library(shiny)
library(geosphere)

# Import Data
filename1 = "Citation_Data_CSV.csv"
Parking_Data = read.csv(filename1, sep=",", header=TRUE)
location = Parking_Data$Block

filename2 = "Lats&Longs.csv"
Location_Data = read.csv(filename2, sep=",", header = TRUE)


# Make the UI
ui <- fluidPage(
  h1("Stupid UI"),
  sliderInput(inputId = "x", label = "Slide X", min = 0, 
                            max = 100, value = 50),
  sliderInput(inputId = "y", label = "Slide Y", min = 0, 
              max = 100, value = 50),
  plotOutput(outputId = "dot")
  )


# Set up the server
server <- function(input, output) 
{
  output$dot = renderPlot({plot(input$x, input$y, xlim = c(0, 100), 
                                ylim = c(0,100))})
  
  
  
}


# Link the server and the UI
shinyApp(ui = ui, server = server)






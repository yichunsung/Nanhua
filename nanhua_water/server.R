library(magrittr)
library(httr)
library(rvest)
library(stringr)
library(reshape2)
library(knitr)
library(ggplot2)
library(plotly)
Sys.setlocale(category = "LC_ALL", locale = "")

rwaData <- read.csv("data/0000.csv")
timeforRAWDATA <- as.POSIXct(rwaData[[2]], "%Y/%m/%d %HH:%MM:%SS")
temforRAWDATA <- as.numeric(as.vector(rwaData[[4]]))
TimeforTemperture <- data.frame(time=timeforRAWDATA, Temperture=temforRAWDATA)

shinyServer(function(input, output) {
  
  output$plotlyData <- renderPlotly({
    Nanhua_water <-plot_ly(data = TimeforTemperture, 
                           x = TimeforTemperture$time, 
                           y = TimeforTemperture$Temperture, 
                           type = "scatter", 
                           mode = "markers+lines"
                           )
    Nanhua_water
  })
  
})

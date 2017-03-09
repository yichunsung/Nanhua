library(magrittr)
library(httr)
library(rvest)
library(stringr)
library(reshape2)
library(knitr)
library(ggplot2)
library(plotly)
Sys.setlocale(category = "LC_ALL", locale = "cht")

shinyUI(fluidPage(
	titlePanel("南化站近三日水溫資料"),

      		mainPanel(
      		             plotlyOutput("plotlyData")
      		          )

))

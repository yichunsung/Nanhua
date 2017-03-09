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
	titlePanel("北寮站"),

      		mainPanel(
      		             plotlyOutput("plotlyData")
      		          )

))

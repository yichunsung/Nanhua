# Library Packages
library(magrittr)
library(httr)
library(rvest)
library(stringr)
library(reshape2)
library(knitr)
library(ggplot2)
library(plotly)
# Setwd
setwd('c:/Taiwan-CWB-Data')
Sys.setlocale(category = "LC_ALL", locale = "")

getDataformCWB <- function(station, timerange1, timerange2, iterm){
  # ---------- input Targat Station ---------- #
  # Load Station List
  stationList <- read.csv("data/new_Station_List.csv")
  inputStationName <- c(station) # "Location"
  # ---------- input Date ---------- #
  fromdate <- as.Date(timerange1) # "2017-01-06"
  todate <- as.Date(timerange2) # "2017-01-06"
  date <- seq.Date(fromdate, todate, "day")
  lengthDate <- as.numeric(length(date))
  lengthDatep <- as.numeric(lengthDate+1)
  # ---------- URL ---------- #
  url_1 <- "http://e-service.cwb.gov.tw/HistoryDataQuery/DayDataController.do?command=viewMain&station="
  url_2 <- "&stname=%25E7%25AB%25B9%25E6%259D%25B1&datepicker="
  url_1_1 <- paste(url_1, stationList$id, sep = "")
  url_all <- paste(url_1_1, url_2, sep = "")
  stationListnew <- cbind(stationList, url_all)
  substation <- data.frame(subset(stationListnew, stationListnew$engName == inputStationName))
  subdataframe <-data.frame(date=date, urldate = paste(substation$url_all, date ,sep=""))
  # ---------- Xpath ---------- #
  inputxpathName <- c(iterm) # "ex: press"
  xpathrain <- "//table[@id='MyTable']/tbody/tr/td[11]" # Xpath for rain data
  xpathHum <- "//table[@id='MyTable']/tbody/tr/td[6]" # Xpath for RH data
  xpathTtem <- "//table[@id='MyTable']/tbody/tr/td[4]" # Xpath for Temperature data
  xpathPres <- "//table[@id='MyTable']/tbody/tr/td[2]" # Xpath for StnPres data
  XpathName <- c("Rain", "Hum", "Tem", "Press")
  xpathurl <- c(xpathrain, xpathHum, xpathTtem, xpathPres)
  xpathList <- data.frame(XpathName, xpathurl)
  xpathselect_dataframe <- subset(xpathList, xpathList$XpathName == inputxpathName)
  xpathSelect_result <- as.vector(xpathselect_dataframe$xpathurl)
  #-----
  hr24 <- data.frame(Hour=1:24)
  for (i in 1:lengthDate){
    urlhtml <- as.vector(subdataframe$urldate[i])# as.vector(date_dataFrame$urldate[1])
    # doc <- read_html(urls)
    datadoc <-read_html(urlhtml)# read_html(as.vector(date_dataFrame$urldate[1]))
    data <- datadoc %>%
      html_nodes(., xpath = xpathSelect_result)%>%
      html_text
    data_renew <- str_trim(sub("<U+00A0>",replacement ="",data)) # Delete something we don't need
    hr24 <-cbind(hr24, data_renew)
  }
  names(hr24)[2:lengthDatep] <- as.vector(as.factor(date))
  hr24_all <- melt(hr24, id=c("Hour") ) # Let them for one column
  names(hr24_all) <- c("hour", "date", "data")
  POStime <- as.POSIXct(paste(hr24_all$date, hr24_all$hour, sep = " "), "%Y-%m-%d %H", tz="GMT")
  resultTable <- data.frame(time=POStime, data= hr24_all$data)
  names(resultTable)[2] <-c(iterm)
  return(resultTable)
}

Beiliao_R <- getDataformCWB("Beiliao", Sys.Date()-4, Sys.Date()-1, "Rain")
rain <- sub("X", replacement = "0.0", Beiliao_R$Rain)
Beiliao_R <- data.frame(time=Beiliao_R$time, rain)
Beiliao_H <- getDataformCWB("Beiliao", Sys.Date()-4, Sys.Date()-1, "Hum")
Beiliao_T <- getDataformCWB("Beiliao", Sys.Date()-4, Sys.Date()-1, "Tem")
Beiliao_P <- getDataformCWB("Beiliao", Sys.Date()-4, Sys.Date()-1, "Press")

Beiliao <- data.frame(time=Beiliao_R$time, 
                      rain=Beiliao_R$rain, 
                      Hum=Beiliao_H$Hum, 
                      Tem=Beiliao_T$Tem, 
                      press=Beiliao_P$Press)

Beiliao_R_Plotly <- plot_ly(
  data = Beiliao,
  x = Beiliao$time,
  y = Beiliao$rain,
  type = "scatter",
  mode = "markers+lines",
  name = "Rain"
                               
)
Beiliao_H_Plotly <- plot_ly(
  data = Beiliao,
  x = Beiliao$time,
  y = Beiliao$Hum,
  type = "scatter",
  mode = "markers+lines",
  name = "Hum"
  
)
Beiliao_T_Plotly <- plot_ly(
  data = Beiliao,
  x = Beiliao$time,
  y = Beiliao$Tem,
  type = "scatter",
  mode = "markers+lines",
  name = "Tem"
  
)
Beiliao_P_Plotly <- plot_ly(
  data = Beiliao,
  x = Beiliao$time,
  y = Beiliao$press,
  type = "scatter",
  mode = "markers+lines",
  name = "Press"
  
)

Beiliao_CWB <- subplot(Beiliao_T_Plotly, Beiliao_P_Plotly, Beiliao_H_Plotly, Beiliao_R_Plotly,
                       nrows = 4,
                       shareX = TRUE
                       )
Beiliao_CWB



#today <- Sys.Date()
#today <- as.Date("2017-03-08")
#yesterday <- today-1


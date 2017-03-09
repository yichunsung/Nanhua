# Library Packages
library(magrittr)
library(httr)
library(rvest)
library(stringr)
library(reshape2)
library(knitr)

setwd('c:/Taiwan-CWB-Data')
Sys.setlocale(category = "LC_ALL", locale = "")

getDataformCWB <- function(iterm){

  fromdate <- Sys.Date()-4 # "2017-01-06"
  todate <- Sys.Date()-1 # "2017-01-06"
  date <- seq.Date(fromdate, todate, "day")
  lengthDate <- as.numeric(length(date))
  lengthDatep <- as.numeric(lengthDate+1)
  
  # Beiliao id: C0O830
  C0O830_url <- "http://e-service.cwb.gov.tw/HistoryDataQuery/DayDataController.do?command=viewMain&station=C0O830&stname=%25E7%25AB%25B9%25E6%259D%25B1&datepicker="
  C0O830_date_dataFrame <- data.frame(date=date, urldate = paste(C0O830_url, date ,sep=""))
  
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
    urlhtml <- as.vector(C0O830_date_dataFrame$urldate[i])# as.vector(date_dataFrame$urldate[1])
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
DrawFigure <- function(iterm, nameforIterm, chooserangemode){

  Beiliao_Plotly <- plot_ly(
    data = Beiliao,
    x = Beiliao$time,
    y = iterm,
    type = "scatter",
    mode = "markers+lines",
    name = nameforIterm
  ) %>% layout(yaxis =list(title="", rangemode = chooserangemode), 
           xaxis = list(zeroline = TRUE, showline = TRUE))
  return(Beiliao_Plotly)
}



Beiliao <- data.frame(time = getDataformCWB("Rain")[[1]], 
                      rain = as.numeric(as.vector(getDataformCWB("Rain")[[2]])),
                      hum = as.numeric(as.vector(getDataformCWB("Hum")[[2]])),
                      Tem = as.numeric(as.vector(getDataformCWB("Tem")[[2]])),
                      Press = as.numeric(as.vector(getDataformCWB("Press")[[2]]))
                      ) 
  

Beiliao_CWB <- subplot(DrawFigure(Beiliao$rain, "Rain", "tozero"),
                       DrawFigure(Beiliao$hum, "hum", "tozero"),
                       DrawFigure(Beiliao$Tem, "Tem", "tozero"),
                       DrawFigure(Beiliao$Press, "Press", "auto"),
                       nrows = 4,
                       shareX = TRUE
                       )

Beiliao_CWB


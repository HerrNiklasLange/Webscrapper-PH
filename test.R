 LIB <- function(){
  library(tidyverse) #library to make programme run
  library(sf)
  library(leaflet)
  library(htmlwidgets)
  library(RColorBrewer)
  library(htmltools)
  library(formattable)
  library(readxl)
  library(dplyr)
  library(rvest)
  library(magrittr)
  library(ggmap)
  library(stringr)
  library(contactdata)
  library(ggplot2)
}

Top5PerCountryLastDays <- function(WhatCountry){
  PHWdt <- read_excel("/Users/nikla/OneDrive/Desktop/PHW/PWS/PWS/data/phwMaindt.xlsx")
  PHWdt[1] <- sapply(PHWdt[1], as.character)
  dfWebScrapper <- data.frame(PHWdt)
  #Visualizsation of scotland map with last data sets
  dfWebScrapper <- dfWebScrapper %>% filter(position < 6) %>% filter(mostViewed == "today") %>% filter(countryID == WhatCountry)
  len = 70

  dfWebScrapper <- dfWebScrapper[1:len,]
  for(i in 1:len){
      if(dfWebScrapper[i,11] == "AM"){
        dfWebScrapper[i,1] = paste(substr(x = dfWebScrapper[i,1], start = 9, stop = 10), " AM")
        
      }
      else if (dfWebScrapper[i,11] == "PM"){
        dfWebScrapper[i,1] = paste(substr(x = dfWebScrapper[i,1], start = 9, stop = 10), " PM")
      }
  }
 
  
  tryCatch(
    {
      plt <-  ggplot(dfWebScrapper, aes(x = date, y = position, group = title)) +
        #geom_line() +
        geom_path(aes(colour = title)) +
        geom_point(aes(colour = title)) +
        ylim("1","2","3","4","5") +
        ggtitle("Top 5 videos most viewed videos per week for the last 7 days") +
        scale_y_reverse()#,"6","7","8","9","10 
      tempStore = paste(WhatCountry,"dailytop5Last7Days.jpeg",sep = "-")
      plt + ggsave(filename = file.path("/Users/nikla/OneDrive/Desktop/PHWW-main/PHWW-main/static/images/main/daily",tempStore),width = 1000,
                   height = 500, units = "px", scale = 5)
    },
    error=function(error_message) {
      message("This is my custom message.")
      message("And below is the error message from R:")
      message(error_message)
      return(NA)
    }
  )
}


Visualisation <-function() {
  countryID <- c("ar","au","at","be","br","bg","ca","cl","hr","cz","dk","eg","fi","fr","de","gr","hu","in","ie","il","it","jp","kr","mx","ma","nl","nz","no","pk","pl","pt","ro","ru","rs","sk","es","se","ch","gb","ua","us","world")
  
  for (i in 1:length(countryID)){
    Top5PerCountryLastDays(countryID[i])
  }
}
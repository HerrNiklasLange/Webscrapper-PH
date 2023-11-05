#Need to follow the following video so that everything works
#https://www.youtube.com/watch?v=GnpJujF9dBw
#common error go into the java folder and remove the error file
remDr <- ""
n <- 10
AM_PM = ""
LoadingPH <- function(){
  print("Executing Webscrapper")
# Loading packages
  library(RSelenium)
  library(wdman)
  library(netstat)
  library(httr)
  library(rvest)
  
  #loading selium and preparing servers dfWebSCrapper[[1,2]]
  selenium()
  selenium_objext <- selenium(retcommand = T, check = F)
  #checking newest version of chrome
  binman::list_versions("chromedriver")
  #starting a server
  rD <- rsDriver(browser="firefox",
                 chromever = NULL,
                 verbose =  F,
                 port = free_port())
  #remDr <- rD[["client"]]
  assign("remDr", rD[["client"]], envir = .GlobalEnv)
   
  # Loading PH test <- identical(Title, character(0))
  remDr$navigate("https://www.pornhub.com")
  #Going past the 18+ btn             #
  Sys.sleep(10)                      #/html/body/div[8]/div/div/button
  
  
  tryCatch(
    {
      button_element <- remDr$findElement(using = "xpath","/html/body/div[8]/div/div/button")
      button_element$clickElement()
    },
    error=function(error_message) {
      
      button_element <- remDr$findElement(using = "xpath","/html/body/div[4]/div/div/button")
      button_element$clickElement()
      return(NA)
    }
  )
  #accepting cookie settings
  Sys.sleep(3)
  button_element <- remDr$findElement(using = "css",".cbSecondaryCTA")
  button_element$clickElement()
  
  #List of country ID
  countryID <- c("ar","au","at","be","br","bg","ca","cl","hr","cz","dk","eg","fi","fr","de","gr","hu","in","ie","il","it","jp","kr","mx","ma","nl","nz","no","pk","pl","pt","ro","ru","rs","sk","es","se","ch","gb","ua","us","world")
  #if doing test runs then true if not false
  testRun <- FALSE
  
  #loop that reads in the data
  for (i in 1:length(countryID)){
    ID <- countryID[i]
    #rnd bewtween 30s and 60s so we don't spam full the server of PH
    j <- runif(n=1, min=20, max=40)
    Sys.sleep(j)
    ReadingCountryToday(ID) # Reading most viewed today of country
    ReadingCountryWeekly(ID) #Reading most viewed this week of country
    date <- substr(x = Sys.Date(), start = 10, stop = 10) #getting today's date
    if (testRun | strtoi(date) %% 2 == 0 | strtoi(date) == 0){
      #Reading most viewed of the country on the monthly category for every day that is divisible by 2
      ReadingCountryMonthly(ID)
    }
    if (testRun | as.POSIXlt(Sys.Date())$wday == 0){
      #reading most viewed of the country on Yearly on every Sunday
      ReadingCountryYearly(ID)
    }
    if (testRun  | date == "01"){
      #reading country most viewed of all time at the beginning of the month 
      ReadingCountryAllTime(ID)
    }
    print(paste("Loop number", i, "done"))
  }
  #closing the server
  
  
  remDr$close()
  rD[["server"]]$stop()
}
Readinghtml <- function(mostviewed, ID, updated_page_source){
  dfWebScrapper <- data.frame(date=c(rep.int(Sys.Date(), 30)), title = "NA", views = "NA",likeRatio = "NA", duration = "NA", URL = "NA",featuredOn = "NA", countryID = "NA", mostViewed="NA", position=-1, MorningOrEvening = AM_PM)
  
  for (i in 1:30) {
    x <- i + 1
    print(i)
    #title
    {
      parsed_page <- read_html(updated_page_source)# #v439344751 > div:nth-child(1) > div:nth-child(3) > span:nth-child(1) > a:nth-child(1)
      ##v438771811 > div:nth-child(1) > div:nth-child(3) > span:nth-child(1) > a:nth-child(1)
      #/html/body/div[4]/div[2]/div[5]/div/div[1]/ul/li[1]/div/div[3]/span/a  html.language-en.supportsGridLayout.fluidContainer.firefox.windows.largeLayout body.logged-out div.wrapper div.container div.gridWrapper div.nf-videos div.sectionWrapper ul#videoCategory.nf-videos.videos.search-video-thumbs li#v439344751.pcVideoListItem.js-pop.videoblock.videoBox div.wrap.flexibleHeight div.thumbnail-info-wrapper.clearfix span.title a
      #/html/body/div[4]/div[2]/div[5]/div/div[1]/ul/li[2]/div/div[3]/span/a /html/body/div[4]/div[2]/div[4]/div/div[1]/ul/li[1]/div/div[3]/span/a
      title_xpath <- paste("/html/body/div[4]/div[2]/div[5]/div/div[1]/ul/li[", x,"]/div/div[3]/span/a")
      title_xpath <- gsub(" ", "", title_xpath)
      Title <- parsed_page %>%  #/html/body/div[4]/div[2]/div[5]/div/div[1]/ul/li[2]/div/div[3]/span/a
        html_elements(xpath = title_xpath) %>%
        html_text()
      Title <- gsub("  ", "", Title)
      Title <- gsub("\n", "", Title)
      print(Title)
      test <- identical(Title, character(0))
      if (test){
        title_xpath <- paste("/html/body/div[4]/div[2]/div[4]/div/div[1]/ul/li[", x,"]/div/div[3]/span/a")
        title_xpath <- gsub(" ", "", title_xpath)
        Title <- parsed_page %>% 
          html_elements(xpath = title_xpath) %>%
          html_text()
        Title <- gsub("  ", "", Title)
        Title <- gsub("\n", "", Title)
      }
      dfWebScrapper[[i,2]] <- Title
    }
    #views
    {
      parsed_page <- read_html(updated_page_source)
      title_xpath <- paste("/html/body/div[4]/div[2]/div[5]/div/div[1]/ul/li[", x,"]/div/div[3]/div[2]/div/span/var")
      title_xpath <- gsub(" ", "", title_xpath)
      views <- parsed_page %>% 
        html_elements(xpath = title_xpath) %>%
        html_text()
      views <- gsub("  ", "", views)
      views <- gsub("\n", "", views)
      print(views)
      if (test){
        title_xpath <- paste("/html/body/div[4]/div[2]/div[4]/div/div[1]/ul/li[", x,"]/div/div[3]/div[2]/div/span/var")
        title_xpath <- gsub(" ", "", title_xpath)
        views <- parsed_page %>% 
          html_elements(xpath = title_xpath) %>%
          html_text()
        views <- gsub("  ", "", views)
        views <- gsub("\n", "", views)
      }
      dfWebScrapper[[i,3]] <- views
    }
    #likeRatio
    {
      parsed_page <- read_html(updated_page_source) #/html/body/div[4]/div[2]/div[5]/div/div[1]/ul/li[2]/div/div[3]/div[2]/div/div/div
      title_xpath <- paste("/html/body/div[4]/div[2]/div[5]/div/div[1]/ul/li[", x,"]/div/div[3]/div[2]/div/div/div")
      title_xpath <- gsub(" ", "", title_xpath)
      likeRatio <- parsed_page %>% 
        html_elements(xpath = title_xpath) %>%
        html_text()
      likeRatio <- gsub("  ", "", likeRatio)
      likeRatio <- gsub("\n", "", likeRatio)
      print(likeRatio)
      if (test){
        title_xpath <- paste("/html/body/div[4]/div[2]/div[4]/div/div[1]/ul/li[", x,"]/div/div[3]/div[2]/div/div/div")
        title_xpath <- gsub(" ", "", title_xpath)
        likeRatio <- parsed_page %>% 
          html_elements(xpath = title_xpath) %>%
          html_text()
        likeRatio <- gsub("  ", "", likeRatio)
        likeRatio <- gsub("\n", "", likeRatio)
      }
      dfWebScrapper[[i,4]] <- likeRatio
    }
    #duration
    {
      parsed_page <- read_html(updated_page_source) #/html/body/div[4]/div[2]/div[5]/div/div[1]/ul/li[2]/div/div[1]/a/div/var
      title_xpath <- paste("/html/body/div[4]/div[2]/div[5]/div/div[1]/ul/li[", x,"]/div/div[1]/a/div/var")
      title_xpath <- gsub(" ", "", title_xpath)
      duration <- parsed_page %>% 
        html_elements(xpath = title_xpath) %>%
        html_text()
      duration <- gsub("  ", "", duration)
      duration <- gsub("\n", "", duration)
      print(duration)
      if (test){
        title_xpath <- paste("/html/body/div[4]/div[2]/div[4]/div/div[1]/ul/li[", x,"]/div/div[1]/a/div/var")
        title_xpath <- gsub(" ", "", title_xpath)
        duration <- parsed_page %>% 
          html_elements(xpath = title_xpath) %>%
          html_text()
        duration <- gsub("  ", "", duration)
        duration <- gsub("\n", "", duration)
      }
      dfWebScrapper[[i,5]] <- duration
    }
    #URL
    {
      parsed_page <- read_html(updated_page_source) #/html/body/div[4]/div[2]/div[5]/div/div[1]/ul/li[2
      title_xpath <- paste("/html/body/div[4]/div[2]/div[5]/div/div[1]/ul/li[", x,"]/div/div[3]/span/a")
      title_xpath <- gsub(" ", "", title_xpath)
      URL <- parsed_page %>% 
        html_elements(xpath = title_xpath) %>%
        html_attr('href')
      URL <- gsub("/", "https://www.pornhub.com/", URL)
      URL <- gsub(" ", "", URL)
      print(url)
      if (test){
        title_xpath <- paste("/html/body/div[4]/div[2]/div[4]/div/div[1]/ul/li[", x,"]/div/div[3]/span/a")
        title_xpath <- gsub(" ", "", title_xpath)
        URL <- parsed_page %>% 
          html_elements(xpath = title_xpath) %>%
          html_attr('href')
        URL <- gsub("/", "https://www.pornhub.com/", URL)
        URL <- gsub(" ", "", URL)
      }
      dfWebScrapper[[i,6]] <- URL
    }
    #featuredOn
    {
      parsed_page <- read_html(updated_page_source) #/html/body/div[4]/div[2]/div[5]/div/div[1]/ul/li[2]/div/div[3]/div[1]/div/a
      title_xpath <- paste("/html/body/div[4]/div[2]/div[5]/div/div[1]/ul/li[", x,"]/div/div[3]/div[1]/div")
      title_xpath <- gsub(" ", "", title_xpath)
      featuredOn <- parsed_page %>% 
        html_elements(xpath = title_xpath) %>%
        html_text()
      featuredOn <- gsub("  ", "", featuredOn)
      featuredOn <- gsub("\n", "", featuredOn)
      print(featuredOn)
      if (test){
        title_xpath <- paste("/html/body/div[4]/div[2]/div[4]/div/div[1]/ul/li[", x,"]/div/div[3]/div[1]/div")
        title_xpath <- gsub(" ", "", title_xpath)
        featuredOn <- parsed_page %>% 
          html_elements(xpath = title_xpath) %>%
          html_text()
        featuredOn <- gsub("  ", "", featuredOn)
        featuredOn <- gsub("\n", "", featuredOn)
      }
      dfWebScrapper[[i,7]] <- featuredOn
    }
    #countryID, mostViewed and position
    {
      dfWebScrapper[[i,8]] <- ID
      dfWebScrapper[[i,9]] <- mostviewed
      dfWebScrapper[[i,10]] <- i
      dfWebScrapper[[i,11]] <- AM_PM
    }
  }
  print(dfWebScrapper)
  WritingToExcel(dfWebScrapper)
}
ReadingCountryToday <- function(ID){
  #text to tell what is happening
  print(paste("starting country with", ID, "as ID for today"))
  
  #navigating to the website
  newID <- paste("https://www.pornhub.com/video?o=mv&t=t&cc=", ID)
  newID <- gsub(" ", "", newID)
  remDr$navigate(newID) 
  
  #Storing the Html of the website
  updated_page_source <- remDr$getPageSource()[[1]] 
  #Reading the data
  Readinghtml("today", ID, updated_page_source) 
  print("Today done")
}
ReadingCountryWeekly <- function(ID){
  #text to tell what is happening
  print(paste("starting country with", ID, "as ID for Weekly"))
  
  #navigating to the website
  newID <- paste("https://www.pornhub.com/video?o=mv&cc=", ID)
  newID <- gsub(" ", "", newID)
  remDr$navigate(newID)
  
  #Storing HTML of the website
  updated_page_source <- remDr$getPageSource()[[1]]
  
  #reading the data
  Readinghtml("weekly", ID, updated_page_source)
  print("Weekly done")
}
ReadingCountryMonthly <- function(ID){
  #text to tell what is happening
  print(paste("starting country with", ID, "as ID for monthly"))
  
  #navigate to the website
  newID <- paste("https://www.pornhub.com/video?o=mv&t=m&cc=", ID)
  newID <- gsub(" ", "", newID)
  remDr$navigate(newID)
  
  #getting the HTML code
  updated_page_source <- remDr$getPageSource()[[1]]
  
  #reading the data,
  Readinghtml("weekly", ID, updated_page_source)
  print("Monthly Done")
}
ReadingCountryYearly <- function(ID){
  #text to show what is happening
  print(paste("starting country with", ID, "as ID for yearly"))
  
  #navigating to the website
  newID <- paste("https://www.pornhub.com/video?o=mv&t=y&cc=", ID)
  newID <- gsub(" ", "", newID)
  remDr$navigate(newID)
  
  #getting HTML code
  updated_page_source <- remDr$getPageSource()[[1]]
  
  #reading the data,
  Readinghtml("yearly", ID, updated_page_source)
  print("Yearly done")
}
ReadingCountryAllTime <- function(ID){
  #text to show what is happening
  print(paste("starting country with", ID, "as ID for all time"))
  
  #navigating to the website
  newID <- paste("https://www.pornhub.com/video?o=mv&t=a&cc=", ID)
  newID <- gsub(" ", "", newID)
  remDr$navigate(newID)
  
  
  #getting the HTML code
  updated_page_source <- remDr$getPageSource()[[1]]
  
  #reading the data,
  Readinghtml("allTime", ID, updated_page_source)
  print("All time done")
}
WritingToExcel <- function(dfWebScrapper){
  library(openxlsx)
  library(readxl)
  data <- read_excel("/Users/nikla/OneDrive/Desktop/PHW/PWS/PWS/data/phwMaindt.xlsx")#C:\Users\nikla\OneDrive\Desktop\PHW\PWS\PWS\data\phwMaindt.xlsx
  dfWebScrapper <- rbind(dfWebScrapper, data)
  write.xlsx(dfWebScrapper, "/Users/nikla/OneDrive/Desktop/PHW/PWS/PWS/data/phwMaindt.xlsx")
} 

#creating graphs
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
top1TodayWorld <- function(){
  PHWdt <- read_excel("/Users/nikla/OneDrive/Desktop/PHW/PWS/PWS/data/phwMaindt.xlsx")
  PHWdt[1] <- sapply(PHWdt[1], as.character)
  dfWebScrapper <- data.frame(PHWdt)
  #Visualizsation of scotland map with last data sets
  dfWebScrapper <- dfWebScrapper %>% filter(position == 1) %>% filter(mostViewed == "today") %>% filter(date == Sys.Date()) %>% filter(countryID != "world")
  dfWebScrapper <- dfWebScrapper[1:41,]
  country <- c("USA","Ukraine","UK","Switzerland","Sweden","Spain","Slovakia","Serbia","Russia","Romania","Portugal","Poland","Pakistan","Norway", "New Zealand","Netherlands","Morocco","Mexico","South Korea","Japan","Italy","Israel","Ireland","India","Hungary","Greece","Germany","France","Finland","Egypt","Denmark","Czech Republic","Croatia","Chile","Canada","Bulgaria","Brazil","Belgium","Austria","Australia","Argentina")
  dfWebScrapper<- cbind.data.frame(dfWebScrapper, country)
  
  dfWebScrapper <- dfWebScrapper %>% select(country, title)
  world <- map_data("world")
  combined <- left_join(world, dfWebScrapper, by = c("region" = "country"))
  
  tryCatch(
    {
    plt <- ggplot(combined, aes(x = long, y = lat, group = group)) +
      geom_polygon(aes(fill = title)) +
      ggtitle("Top viewed video today per country on the ",  PHWdt$date[1]) +
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank())
      plt + ggsave(filename = file.path("/Users/nikla/OneDrive/Desktop/PHWW-main/PHWW-main/static/images/main","dailyWorldMap.jpeg"),width = 1000,
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
top1weeklyWorld <- function(){
  PHWdt <- read_excel("/Users/nikla/OneDrive/Desktop/PHW/PWS/PWS/data/phwMaindt.xlsx")
  PHWdt[1] <- sapply(PHWdt[1], as.character)
  dfWebScrapper <- data.frame(PHWdt)
  country <- c("USA","Ukraine","UK","Switzerland","Sweden","Spain","Slovakia","Serbia","Russia","Romania","Portugal","Poland","Pakistan","Norway", "New Zealand","Netherlands","Morocco","Mexico","South Korea","Japan","Italy","Israel","Ireland","India","Hungary","Greece","Germany","France","Finland","Egypt","Denmark","Czech Republic","Croatia","Chile","Canada","Bulgaria","Brazil","Belgium","Austria","Australia","Argentina")
  
  #Visualizsation of scotland map with last data sets
  dfWebScrapper <- dfWebScrapper %>% filter(position == 1) %>% filter(mostViewed == "weekly") %>% filter(date == Sys.Date()) %>% filter(countryID != "world")
  dfWebScrapper <- dfWebScrapper[1:41,]
  dfWebScrapper<- cbind.data.frame(dfWebScrapper, country)
  dfWebScrapper <- dfWebScrapper %>% select(country, title)
  world <- map_data("world")
  #world %>% filter(region != "Antarctica") 
  combined <- left_join(world, dfWebScrapper, by = c("region" = "country"))
  title_fill <- geom_polygon(aes(fill = title))
  tryCatch(
    {
    ggplot(combined, aes(x = long, y = lat, group = group)) +
      geom_polygon(aes(fill = title)) +
      ggtitle("Top viewed video this week per country on the",  PHWdt$date[1]) +
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank()) +
      ggsave(filename = file.path("/Users/nikla/OneDrive/Desktop/PHWW-main/PHWW-main/static/images/main","weeklyWorldMap.jpeg"),width = 1000,
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
  LIB()
  countryID <- c("ar","au","at","be","br","bg","ca","cl","hr","cz","dk","eg","fi","fr","de","gr","hu","in","ie","il","it","jp","kr","mx","ma","nl","nz","no","pk","pl","pt","ro","ru","rs","sk","es","se","ch","gb","ua","us","world")
  top1weeklyWorld()
  top1TodayWorld()
  for (i in 1:length(countryID)){
    Top5PerCountryLastDays(countryID[i])
  }
}

#infinity run
while (TRUE){
  x <- substr(x = Sys.time(), start = 12, stop = 16)
  if (x == "09:30"){
    AM_PM = "AM"
    LoadingPH()
    Visualisation()
    print(Sys.time())
  }
  else if (x == "21:30"){
    AM_PM = "PM"
    LoadingPH()
    Visualisation()
    print(Sys.time())
  }
  
}
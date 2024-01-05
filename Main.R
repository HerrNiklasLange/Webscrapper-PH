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
  library(openxlsx)
  library(readxl)
  
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
  date = "01"
  #loop that reads in the data
  for (i in 1:length(countryID)){
    ID <- countryID[i]
    #rnd bewtween 30s and 60s so we don't spam full the server of PH
    
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
    if (TRUE  | substr(x = Sys.Date(), start = 9, stop = 10) == "01"){
      #reading country most viewed of all time at the beginning of the month 
      ReadingCountryAllTime(ID)
    }
    print(paste("Loop number", i, "done"))
    j <- runif(n=1, min=25, max=45)
  }
  #closing the server
  
  
  remDr$close()
  rD[["server"]]$stop()
  
  #Back up
  
  
  data <- read_excel("/Users/nikla/OneDrive/Desktop/PHW/PWS/PWS/data/phwMaindt.xlsx")#C:\Users\nikla\OneDrive\Desktop\PHW\PWS\PWS\data\phwMaindt.xlsx
  write.xlsx(data, paste("/Users/nikla/OneDrive/Desktop/PHW/PWS/PWS/data/DailyBackUp/backup",Sys.Date(),AM_PM,".xlsx",sep = ""))
  
}
Readinghtml <- function(mostviewed, ID, updated_page_source){
  dfWebScrapper <- data.frame(date=c(rep.int(Sys.Date(), 30)), title = "NA", views = "NA",likeRatio = "NA", duration = "NA", URL = "NA",featuredOn = "NA", countryID = "NA", mostViewed="NA", position=-1, MorningOrEvening = AM_PM)
  
  for (i in 1:30) {
    x <- i + 1
    #print(i)
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
      #print(Title)
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
      #print(views)
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
      #print(likeRatio)
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
      #print(duration)
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
      #print(url)
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
      #print(featuredOn)
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
  #print(dfWebScrapper)
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
  Readinghtml("monthly", ID, updated_page_source)
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
  library(openxlsx)
  library(readxl)
}
top1TodayWorld <- function(dfWebScrapper){
  #Visualizsation of scotland map with last data sets
  dfWebScrapper <- dfWebScrapper %>% filter(position == 1) %>% filter(mostViewed == "today") %>% filter(countryID != "world")
  dfWebScrapper <- dfWebScrapper[1:41,]
  webscrapDate <- dfWebScrapper$date[1]
  country <- c("USA","Ukraine","UK","Switzerland","Sweden","Spain","Slovakia","Serbia","Russia","Romania","Portugal","Poland","Pakistan","Norway", "New Zealand","Netherlands","Morocco","Mexico","South Korea","Japan","Italy","Israel","Ireland","India","Hungary","Greece","Germany","France","Finland","Egypt","Denmark","Czech Republic","Croatia","Chile","Canada","Bulgaria","Brazil","Belgium","Austria","Australia","Argentina")
  dfWebScrapper<- cbind.data.frame(dfWebScrapper, country)
  
  dfWebScrapper <- dfWebScrapper %>% select(country, title)
  world <- map_data("world")
  combined <- left_join(world, dfWebScrapper, by = c("region" = "country"))
  dfWebScrapper <- arrange(dfWebScrapper, date)
  tryCatch(
    {
    plt <- ggplot(combined, aes(x = long, y = lat, group = group)) +
      geom_polygon(aes(fill = title)) +
      ggtitle(paste("The most viewed videos per country of the category DAY on the ",  webscrapDate)) +
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank())
       ggsave(filename = file.path("/Users/nikla/OneDrive/Desktop/PHWW-main/PHWW-main/static/images/main","dailyWorldMap.jpeg"),
              plot = plt,
              width = 1000,
              height = 500, units = "px", scale = 5)
    },
    error=function(error_message) {
      return(NA)
    }
  )
}
top1weeklyWorld <- function(dfWebScrapper){
  country <- c("USA","Ukraine","UK","Switzerland","Sweden","Spain","Slovakia","Serbia","Russia","Romania","Portugal","Poland","Pakistan","Norway", "New Zealand","Netherlands","Morocco","Mexico","South Korea","Japan","Italy","Israel","Ireland","India","Hungary","Greece","Germany","France","Finland","Egypt","Denmark","Czech Republic","Croatia","Chile","Canada","Bulgaria","Brazil","Belgium","Austria","Australia","Argentina")
  
  #Visualizsation of scotland map with last data sets
  dfWebScrapper <- dfWebScrapper %>% filter(position == 1) %>% filter(mostViewed == "weekly") %>% filter(countryID != "world")
  dfWebScrapper <- dfWebScrapper[1:41,]
  webscrapDate <- dfWebScrapper$date[1]
  dfWebScrapper<- cbind.data.frame(dfWebScrapper, country)
  dfWebScrapper <- dfWebScrapper %>% select(country, title)
  world <- map_data("world")
  #world %>% filter(region != "Antarctica") 
  combined <- left_join(world, dfWebScrapper, by = c("region" = "country"))
  title_fill <- geom_polygon(aes(fill = title))
  dfWebScrapper <- arrange(dfWebScrapper, date)
  tryCatch(
    {
      plt <- ggplot(combined, aes(x = long, y = lat, group = group, fill = title)) +
        geom_polygon() +
        ggtitle(paste("The most viewed videos per country of the category WEEK on the", webscrapDate)) +
        theme(
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank()
        )
        #scale_fill_manual(values = title ) +
        ggsave(
          filename = file.path("/Users/nikla/OneDrive/Desktop/PHWW-main/PHWW-main/static/images/main", "weeklyWorldMap.jpeg"),
          plot = plt,
          width = 1000,
          height = 500,
          units = "px",
          scale = 5
        )
    },
    error=function(error_message) {
      
      return(NA)
    }
  )
}
top1monthlyWorld <- function(dfWebScrapper){
  
  country <- c("USA","Ukraine","UK","Switzerland","Sweden","Spain","Slovakia","Serbia","Russia","Romania","Portugal","Poland","Pakistan","Norway", "New Zealand","Netherlands","Morocco","Mexico","South Korea","Japan","Italy","Israel","Ireland","India","Hungary","Greece","Germany","France","Finland","Egypt","Denmark","Czech Republic","Croatia","Chile","Canada","Bulgaria","Brazil","Belgium","Austria","Australia","Argentina")
  
  #Visualizsation of scotland map with last data sets
  dfWebScrapper <- dfWebScrapper %>% filter(position == 1) %>% filter(mostViewed == "monthly") %>% filter(countryID != "world")
  dfWebScrapper <- dfWebScrapper[1:41,]
  webscrapDate <- dfWebScrapper$date[1]
  dfWebScrapper<- cbind.data.frame(dfWebScrapper, country)
  dfWebScrapper <- dfWebScrapper %>% select(country, title)
  world <- map_data("world")
  combined <- left_join(world, dfWebScrapper, by = c("region" = "country"))
  title_fill <- geom_polygon(aes(fill = title))
  dfWebScrapper <- arrange(dfWebScrapper, date)
  tryCatch(
    {
      plt <- ggplot(combined, aes(x = long, y = lat, group = group, fill = title)) +
        geom_polygon() +
        ggtitle(paste("The most viewed videos per country of the category MONTH on the", webscrapDate)) +
        theme(
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank()
        )
      #scale_fill_manual(values = title ) +
      ggsave(
        filename = file.path("/Users/nikla/OneDrive/Desktop/PHWW-main/PHWW-main/static/images/main", "monthlyWorldMap.jpeg"),
        plot = plt,
        width = 1000,
        height = 500,
        units = "px",
        scale = 5
      )
    },
    error=function(error_message) {
      
      return(NA)
    }
  )
}
top1yearWorld <- function(dfWebScrapper){
  
  country <- c("USA","Ukraine","UK","Switzerland","Sweden","Spain","Slovakia","Serbia","Russia","Romania","Portugal","Poland","Pakistan","Norway", "New Zealand","Netherlands","Morocco","Mexico","South Korea","Japan","Italy","Israel","Ireland","India","Hungary","Greece","Germany","France","Finland","Egypt","Denmark","Czech Republic","Croatia","Chile","Canada","Bulgaria","Brazil","Belgium","Austria","Australia","Argentina")
  
  #Visualizsation of scotland map with last data sets
  dfWebScrapper <- dfWebScrapper %>% filter(position == 1) %>% filter(mostViewed == "yearly") %>% filter(countryID != "world")
  dfWebScrapper <- dfWebScrapper[1:41,]
  webscrapDate <- dfWebScrapper$date[1]
  dfWebScrapper<- cbind.data.frame(dfWebScrapper, country)
  dfWebScrapper <- dfWebScrapper %>% select(country, title)
  world <- map_data("world")
  combined <- left_join(world, dfWebScrapper, by = c("region" = "country"))
  title_fill <- geom_polygon(aes(fill = title))
  dfWebScrapper <- arrange(dfWebScrapper, date)
  for(i in 1:length(dfWebScrapper$title)){
    original_title <- dfWebScrapper$title
    
    # Set the maximum number of characters per line
    max_chars_per_line <- 20
    
    # Wrap the title to a specified width
    wrapped_title <- strwrap(original_title, width = max_chars_per_line, simplify = FALSE)
    
    # If the title is longer than the specified width, join the lines with "\n"
    if (length(wrapped_title) > 1) {
      new_title <- paste(wrapped_title, collapse = "\n")
    } else {
      new_title <- original_title
    }
    
    # Print the original and new titles
    cat("Original Title:\n", original_title, "\n\n")
    cat("New Title:\n", new_title, "\n")
  }
  tryCatch(
    {
      plt <- ggplot(combined, aes(x = long, y = lat, group = group, fill = title)) +
        geom_polygon() +
        ggtitle(paste("The most viewed videos per country of the category Year on the", webscrapDate)) +
        theme(
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          legend.box = "vertical",
          plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"))
      
        #theme(legend.key.size = unit(0.5, 'cm'), #change legend key size
         #     legend.key.height = unit(0.5, 'cm'), #change legend key height
          #    legend.key.width = unit(0.3, 'cm'), #change legend key width
           #   legend.title = element_text(size=14), #change legend title font size
            #  legend. = element_text(size=8)) #change legend text font size
      
      #scale_fill_manual(values = title ) +
      ggsave(
        filename = file.path("/Users/nikla/OneDrive/Desktop/PHWW-main/PHWW-main/static/images/main", "yearlyWorldMap.jpeg"),
        plot = plt,
        width = 1000,
        height = 500,
        units = "px",
        scale = 5
      )
    },
    error=function(error_message) {
      
      return(NA)
    }
  )
}
top1AllTimeWorld <- function(dfWebScrapper){
  
  country <- c("USA","Ukraine","UK","Switzerland","Sweden","Spain","Slovakia","Serbia","Russia","Romania","Portugal","Poland","Pakistan","Norway", "New Zealand","Netherlands","Morocco","Mexico","South Korea","Japan","Italy","Israel","Ireland","India","Hungary","Greece","Germany","France","Finland","Egypt","Denmark","Czech Republic","Croatia","Chile","Canada","Bulgaria","Brazil","Belgium","Austria","Australia","Argentina")
  
  #Visualizsation of scotland map with last data sets
  dfWebScrapper <- dfWebScrapper %>% filter(position == 1) %>% filter(mostViewed == "allTime") %>% filter(countryID != "world")
  dfWebScrapper <- dfWebScrapper[1:41,]
  webscrapDate <- dfWebScrapper$date[1]
  dfWebScrapper<- cbind.data.frame(dfWebScrapper, country)
  dfWebScrapper <- dfWebScrapper %>% select(country, title)
  world <- map_data("world")
  combined <- left_join(world, dfWebScrapper, by = c("region" = "country"))
  title_fill <- geom_polygon(aes(fill = title))
  dfWebScrapper <- arrange(dfWebScrapper, date)
  for(i in 1:length(dfWebScrapper$title)){
    original_title <- dfWebScrapper$title
    
    # Set the maximum number of characters per line
    max_chars_per_line <- 20
    
    # Wrap the title to a specified width
    wrapped_title <- strwrap(original_title, width = max_chars_per_line, simplify = FALSE)
    
    # If the title is longer than the specified width, join the lines with "\n"
    if (length(wrapped_title) > 1) {
      new_title <- paste(wrapped_title, collapse = "\n")
    } else {
      new_title <- original_title
    }
    
    # Print the original and new titles
    cat("Original Title:\n", original_title, "\n\n")
    cat("New Title:\n", new_title, "\n")
  }
  tryCatch(
    {
      plt <- ggplot(combined, aes(x = long, y = lat, group = group, fill = title)) +
        geom_polygon() +
        ggtitle(paste("The most viewed videos per country of the category ALL TIME on the", webscrapDate)) +
        theme(
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          legend.box = "vertical",
          plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"))
      
      #theme(legend.key.size = unit(0.5, 'cm'), #change legend key size
      #     legend.key.height = unit(0.5, 'cm'), #change legend key height
      #    legend.key.width = unit(0.3, 'cm'), #change legend key width
      #   legend.title = element_text(size=14), #change legend title font size
      #  legend. = element_text(size=8)) #change legend text font size
      
      #scale_fill_manual(values = title ) +
      ggsave(
        filename = file.path("/Users/nikla/OneDrive/Desktop/PHWW-main/PHWW-main/static/images/main", "allTimeWorldMap.jpeg"),
        plot = plt,
        width = 1000,
        height = 500,
        units = "px",
        scale = 5
      )
    },
    error=function(error_message) {
      
      return(NA)
    }
  )
}
Top5PerCountryLastDays <- function(WhatCountry,dfWebScrapper){
  

  #Visualizsation of scotland map with last data sets
  dfWebScrapper <- dfWebScrapper %>% filter(position < 6) %>% filter(mostViewed == "today") %>% filter(countryID == WhatCountry)
  
  Top5GeomLinePlot(dfWebScrapper, WhatCountry, "Days")
}
Top5PerCountryLastWeek <- function(WhatCountry,dfWebScrapper){

  #Visualizsation of scotland map with last data sets
  dfWebScrapper <- dfWebScrapper %>% filter(position < 6) %>% filter(mostViewed == "weekly") %>% filter(countryID == WhatCountry)
  len = 40
  
  Top5GeomLinePlot(dfWebScrapper, WhatCountry, "Week")
}
Top5PerCountryLastMonth <- function(WhatCountry,dfWebScrapper){

  #Visualizsation of scotland map with last data sets
  dfWebScrapper <- dfWebScrapper %>% filter(position < 6) %>% filter(mostViewed == "monthly") %>% filter(countryID == WhatCountry)
  len = 40
  
  Top5GeomLinePlot(dfWebScrapper, WhatCountry, "Month")
}
Top5PerCountryLastYear <- function(WhatCountry,dfWebScrapper){

  #Visualizsation of scotland map with last data sets
  dfWebScrapper <- dfWebScrapper %>% filter(position < 6) %>% filter(mostViewed == "yearly") %>% filter(countryID == WhatCountry)
  len = 40
  Top5GeomLinePlot(dfWebScrapper, WhatCountry, "Year")
}
Top5GeomLinePlot <- function(dfWebScrapper, WhatCountry,type){
  len = 40
  dfWebScrapper <- dfWebScrapper%>% slice(1:len)
  for(i in 1:len){
    if(!is.na(dfWebScrapper[i, 11]) &&dfWebScrapper[i,11] == "AM"){
      dfWebScrapper[i,1] = paste(substr(x = dfWebScrapper[i,1], start = 1, stop = 10), " AM")
      
    } else if (!is.na(dfWebScrapper[i, 11]) &&dfWebScrapper[i,11] == "PM"){
      dfWebScrapper[i,1] = paste(substr(x = dfWebScrapper[i,1], start = 1, stop = 10), " PM")
    }
  }
  dfWebScrapper <- arrange(dfWebScrapper, date)
  #dfWebScrapper <- dfWebScrapper[order(dfWebScrapper$date, decreasing = FALSE), ]
  if(type == "Days"){
    GraphTitle <- paste("Top 5 videos most viewed videos of the category DAY for the last 3 days for country with ID", WhatCountry)
  } else if(type == "Week"){
    GraphTitle <- paste("Top 5 videos most viewed videos of the category WEEK for the last 3 days for country with ID", WhatCountry)
  } else if(type == "Month"){
    GraphTitle <- paste("Top 5 videos most viewed videos of the category MONTH for the last 3 days for country with ID", WhatCountry)
  } else{
    GraphTitle <- paste("Top 5 videos most viewed videos of the category YEAR for the last 3 days for country with ID", WhatCountry)
  }
  tryCatch({
      plt <- ggplot(dfWebScrapper, aes(x = date, y = position, group = title)) +
      #geom_path() +
      geom_path(aes(colour = title)) +
      geom_point(aes(colour = title)) +
      #ylim("1","2","3","4","5") +
      ggtitle(GraphTitle) +
      scale_y_reverse()#,"6","7","8","9","10 
      tempStore = paste(WhatCountry,"-dailytop5Last7",type,".jpeg",sep = "")
      ggsave(filename = file.path('/Users/nikla/OneDrive/Desktop/PHWW-main/PHWW-main/static/images/main/daily/',tempStore),
             plot = plt,
             width = 1000,
             height = 500, units = "px", scale = 5)
  },error = function(e){
    cat("THIS IS THE BIG ERROR:", conditionMessage(e), "\n")
    Sys.sleep(60)})
}
Visualisation <-function() {
  LIB() #
  PHWdt <- read_excel("/Users/nikla/OneDrive/Desktop/PHW/PWS/PWS/data/phwMaindt.xlsx")
  PHWdt[1] <- sapply(PHWdt[1], as.character)
  dfWebScrapper <- data.frame(PHWdt)
  countryID <- c("ar","au","at","be","br","bg","ca","cl","hr","cz","dk","eg","fi","fr","de","gr","hu","in","ie","il","it","jp","kr","mx","ma","nl","nz","no","pk","pl","pt","ro","ru","rs","sk","es","se","ch","gb","ua","us","world")
  top1weeklyWorld(dfWebScrapper)
  top1TodayWorld(dfWebScrapper)
  top1monthlyWorld(dfWebScrapper)
  top1yearWorld(dfWebScrapper)
  top1AllTimeWorld(dfWebScrapper)
  tryCatch({
  for (i in 1:length(countryID)){
    if(countryID[i] %in% c("ma", "aeg")){
     
    }
    else{
      print(paste("Today", countryID[i]))
      Top5PerCountryLastDays(countryID[i],dfWebScrapper)
      print(paste("Week", countryID[i]))
      Top5PerCountryLastMonth(countryID[i],dfWebScrapper)
      Top5PerCountryLastWeek(countryID[i],dfWebScrapper)
      print(paste("Year", countryID[i]))
      Top5PerCountryLastYear(countryID[i],dfWebScrapper)
  }
  }}, error = function(e){
    cat("THIS IS THE BIG ERROR:", conditionMessage(e), "\n")
    Sys.sleep(60)
  }
  )
  
}
#infinity run
while (TRUE){
  x <- substr(x = Sys.time(), start = 12, stop = 16)
  if (x == "08:30"){
    AM_PM = "AM"
    LoadingPH()
    Visualisation()
    print(Sys.time())
  }
  else if (x == "20:30"){
    AM_PM = "PM"
    LoadingPH()
    Visualisation()
    print(Sys.time())
  }
}

resetData <-function(){
  library(RSelenium)
  library(wdman)
  library(netstat)
  library(httr)
  library(rvest)
  library(openxlsx)
  library(readxl)
  
  PHWdt <- read_excel("/Users/nikla/OneDrive/Desktop/PHW/PWS/PWS/data/phwMaindt.xlsx")
  PHWdt[1] <- sapply(PHWdt[1], as.character)
  dfWebScrapper <- data.frame(PHWdt)
  newDT <- data.frame(date, title = "NA", views = "NA",likeRatio = "NA", duration = "NA", URL = "NA",featuredOn = "NA", countryID = "NA", mostViewed="NA", position=-1, MorningOrEvening = AM_PM)
  type <- dfWebScrapper %>% distinct(mostViewed)
  ID <- c("ar","au","at","be","br","bg","ca","cl","hr","cz","dk","eg","fi","fr","de","gr","hu","in","ie","il","it","jp","kr","mx","ma","nl","nz","no","pk","pl","pt","ro","ru","rs","sk","es","se","ch","gb","ua","us","world")
  uniqueDates <- dfWebScrapper %>% distinct(date)
  for(i in 1:nrow(uniqueDates)){
    #print("We get here")
    for(j in 1:length(ID)){
      for(k in 1:nrow(type)){
        dfWebScrapper <- data.frame(PHWdt)
        dfWebScrapper <- dfWebScrapper %>% filter(mostViewed == type[k,1]) %>% filter(countryID == ID[j])%>% filter(date == uniqueDates[i, 1])
        if(count(dfWebScrapper %>% filter(position == 1) %>% filter(MorningOrEvening == "AM")) > 1){
          #print("True")
          for(x in 1:30){
            dfWebScrapper$MorningOrEvening[x] <- "PM"
          }
        }
        newDT <- rbind(dfWebScrapper, newDT)
      }
    }
  }
  write.xlsx(newDT, "/Users/nikla/OneDrive/Desktop/PHW/PWS/PWS/data/phwMaindt.xlsx")
  print("DONE")
}
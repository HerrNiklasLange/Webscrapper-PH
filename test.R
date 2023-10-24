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
}
top1TodayWorld <- function(){
PHWdt <- read_excel("/Users/nikla/OneDrive/Desktop/PHW/PWS/PWS/data/phwMaindt.xlsx")
PHWdt[1] <- sapply(PHWdt[1], as.character)
dfWebScrapper <- data.frame(PHWdt)
#Visualizsation of scotland map with last data sets
dfWebScrapper <- dfWebScrapper %>% filter(position == 1) %>% filter(mostViewed == "today") %>% filter(date == Sys.Date()) %>% filter(countryID != "world")
country <- c("USA","Ukraine","UK","Switzerland","Sweden","Spain","Slovakia","Serbia","Russia","Romania","Portugal","Poland","Pakistan","Norway", "New Zealand","Netherlands","Morocco","Mexico","South Korea","Japan","Italy","Israel","Ireland","India","Hungary","Greece","Germany","France","Finland","Egypt","Denmark","Czech Republic","Croatia","Chile","Canada","Bulgaria","Brazil","Belgium","Austria","Australia","Argentina")
dfWebScrapper<- cbind.data.frame(dfWebScrapper, country)

dfWebScrapper <- dfWebScrapper %>% select(country, title)
world <- map_data("world")
combined <- left_join(world, dfWebScrapper, by = c("region" = "country"))

ggplot(combined, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = title)) +
  ggtitle("Top viewed video today per country on the ",  PHWdt$date[1]) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  ggsave(filename = file.path("/Users/nikla/OneDrive/Desktop/PHWW-main/PHWW-main/static/images/main","dailyWorldMap.jpeg"),width = 1000,
         height = 500, units = "px", scale = 5)

}
top1weeklyWorld <- function(){
PHWdt <- read_excel("/Users/nikla/OneDrive/Desktop/PHW/PWS/PWS/data/phwMaindt.xlsx")
PHWdt[1] <- sapply(PHWdt[1], as.character)
dfWebScrapper <- data.frame(PHWdt)
country <- c("USA","Ukraine","UK","Switzerland","Sweden","Spain","Slovakia","Serbia","Russia","Romania","Portugal","Poland","Pakistan","Norway", "New Zealand","Netherlands","Morocco","Mexico","South Korea","Japan","Italy","Israel","Ireland","India","Hungary","Greece","Germany","France","Finland","Egypt","Denmark","Czech Republic","Croatia","Chile","Canada","Bulgaria","Brazil","Belgium","Austria","Australia","Argentina")

#Visualizsation of scotland map with last data sets
dfWebScrapper <- dfWebScrapper %>% filter(position == 1) %>% filter(mostViewed == "weekly") %>% filter(date == Sys.Date()) %>% filter(countryID != "world")

dfWebScrapper<- cbind.data.frame(dfWebScrapper, country)
dfWebScrapper <- dfWebScrapper %>% select(country, title)
world <- map_data("world")

combined <- left_join(world, dfWebScrapper, by = c("region" = "country"))

title_fill <- geom_polygon(aes(fill = title))
ggplot(combined, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = title)) +
  ggtitle("Top viewed video this week per country on the ",  PHWdt$date[1]) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  ggsave(filename = file.path("/Users/nikla/OneDrive/Desktop/PHWW-main/PHWW-main/static/images/main","weeklyWorldMap.jpeg"),width = 1000,
         height = 500, units = "px", scale = 5)
}
testing <- function(){
  PHWdt <- read_excel("/Users/nikla/OneDrive/Desktop/PHW/PWS/PWS/data/phwMaindt.xlsx")
  PHWdt[1] <- sapply(PHWdt[1], as.character)
  dfWebScrapper <- data.frame(PHWdt)
  country <- c("USA","Ukraine","UK","Switzerland","Sweden","Spain","Slovakia","Serbia","Russia","Romania","Portugal","Poland","Pakistan","Norway", "New Zealand","Netherlands","Morocco","Mexico","South Korea","Japan","Italy","Israel","Ireland","India","Hungary","Greece","Germany","France","Finland","Egypt","Denmark","Czech Republic","Croatia","Chile","Canada","Bulgaria","Brazil","Belgium","Austria","Australia","Argentina")
  EuropeAfrica <- c("Ukraine","UK","Switzerland","Sweden","Spain","Slovakia","Serbia","Russia","Romania","Portugal","Poland","Norway", "Netherlands","Morocco","Italy","Israel","Ireland","Hungary","Greece","Germany","France","Finland","Egypt","Denmark","Czech Republic","Croatia","Bulgaria","Belgium","Austria")
  
  #Visualizsation of scotland map with last data sets
  dfWebScrapper <- dfWebScrapper %>% filter(position == 1) %>% filter(mostViewed == "weekly") %>% filter(date == Sys.Date()) %>% filter(countryID != "world")
  
  dfWebScrapper<- cbind.data.frame(dfWebScrapper, country)
  dfWebScrapper <- dfWebScrapper %>% select(country, title)
  world <- map_data("world")
  
  combined <- left_join(world, dfWebScrapper, by = c("region" = "country"))
  all_countries_2020 <- list_countries(data_source = 2020)
  n_titles <- length(which(table(combined$title)>1)) 
  palette1_named = setNames(object = scales::hue_pal()(n_titles), nm = table(combined$title))
  EUAF <- combined %>% filter(region != c(EuropeAfrica))
  ggplot(combined, aes(x = long, y = lat, group = group)) +
    fill +
    ggtitle("Top viewed video this week per country on the ",  PHWdt$date[1]) +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank()) +
    ggsave(filename = file.path("/Users/nikla/OneDrive/Desktop/PHWW-main/PHWW-main/static/images/main","weeklyWorldMap.jpeg"),width = 1000,
           height = 500, units = "px", scale = 5)
}

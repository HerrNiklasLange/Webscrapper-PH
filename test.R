library(tidyverse) #library to make programme run
library(dplyr)
library(ggplot2)
library(readxl)
library(tm)
library(tidytext)
library(SnowballC)
PHWdt <- read_excel("/Users/nikla/OneDrive/Desktop/PHW/PWS/PWS/data/phwMaindt.xlsx")
PHWdt[1] <- sapply(PHWdt[1], as.character)
dfWebScrapper <- data.frame(PHWdt)
Dec <- dfWebScrapper %>% filter(substr(x = date, start = 6, stop = 7) == "12")

title_tokens <- dfWebScrapper$title %>% 
  ennest_tokens(output = "word")
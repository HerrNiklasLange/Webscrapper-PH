main <-function(){
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
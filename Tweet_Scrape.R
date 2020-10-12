# Web Scrape Code 

library("twitteR")
library("ROAuth")

library(tidyverse)
#library(devtools)
#library(httr)
library(lubridate)
library(ggrepel)



consumerKey = 	"C9nxDX0XeqRRrJBFa4axZTw9J"
consumerSecret = "fHIhojW5JRasUU7ROAR34StXvU7JxpApc1u77N7yC8kEqhhwYz"
accessToken = "924644133297426437-db7Puu7zk1WJdLo9nTVcVhEOvZ1NEn0"
accessSecret = "AZvnfGHTVsZVyX5KGIlHtmAQbw1DBGdj6iZ4AyRIBQgqK"
options(httr_oauth_cache=TRUE)

setup_twitter_oauth(consumer_key = consumerKey, consumer_secret = consumerSecret,
                    access_token = accessToken, access_secret = accessSecret)

senators <- read.csv("data/senior senators.csv") %>% 
  rename(screenName = senator_twitter)

senatornames <- tolower(senators$screenName)

most_liberal <- as.character(senators$screenName[1:10])
most_conservative <- as.character(senators$screenName[83:92])

extreme_senators <- c(most_conservative, most_liberal)

obamatweets<- userTimeline("potus44", n = 1)
binded <- tbl_df(map_df(obamatweets, as.data.frame))


read_senator_tweets <- function(list){
  for(i in list){
    list_tweet <- userTimeline(i, n = 250, includeRts=TRUE)
    tweets_df <- tbl_df(map_df(list_tweet, as.data.frame))
    binded <- bind_rows(binded, tweets_df)
    print(i)
  }
  return(binded)
}

extremities <- read_senator_tweets(extreme_senators)

extremes2 <- extremities %>% 
  filter(screenName != "POTUS") %>% 
  select(screenName, created) 



extremes2 <- extremes2 %>% 
  mutate(date = ymd_hms(created),
         after_01oct2017 = ifelse(date > mdy("10/1/2017"), TRUE, FALSE)) %>% 
  filter(after_01oct2017 == TRUE) %>% 
  merge(senators, by = "screenName") %>% 
  group_by(screenName) %>% 
  mutate(count = n()) 

unique_extremes2 <- extremes2 %>% 
  select(screenName, ideology, party, count) %>% 
  unique()

unique_extremes2$party <- factor(unique_extremes2$party, levels=c("R", "I", "D"), 
                                 labels=c("Republican", "Independent", "Democrat"))

#Changing Bernie Sanders to Democrat for the purposes the of the graph
unique_extremes2$party[17] = "Democrat"


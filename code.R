library(dslabs)
library(tidyverse)
library(lubridate)
library(scales)
library(plyr)

url <- 'http://www.trumptwitterarchive.com/data/realdonaldtrump/%s.json'

trump_tweets <- map(2015:2020, ~sprintf(url, .x)) %>%
  map_df(jsonlite::fromJSON, simplifyDataFrame = TRUE) %>%
  filter(!is_retweet & !str_detect(text, '^"')) %>%
  mutate(created_at = parse_date_time(created_at, 
                                      orders = "a b! d! H!:M!:S! z!* Y!",
                                      tz="EST")) 

head(trump_tweets)
names(trump_tweets)

tt <- trump_tweets %>% 
  filter(created_at >= ymd("2015-01-01") & 
           created_at < ymd("2015-01-31")) %>%
  mutate(week = week(created_at),
         year = year(created_at),
         month = month(created_at),
         extract(year_month, "created_at", ""))

tt_weeks <- count(tt, 'week' & 'year')

tt_weeks <- tt %>%
  group_by(year)


tweets <- trump_tweets %>%
  select(id_str, source, text, created_at) %>%
  extract(source, "source", "Twitter for (.*?)<") %>%
  filter(source %in% c("iPhone", "Android"))

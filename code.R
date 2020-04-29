library(plyr)
library(dslabs)
library(tidyverse)
library(lubridate)
library(scales)
library(caret)


url <- 'http://www.trumptwitterarchive.com/data/realdonaldtrump/%s.json'

trump_tweets <- map(2015:2020, ~sprintf(url, .x)) %>%
  map_df(jsonlite::fromJSON, simplifyDataFrame = TRUE) %>%
  filter(!is_retweet & !str_detect(text, '^"')) %>%
  mutate(created_at = parse_date_time(created_at, 
                                      orders = "a b! d! H!:M!:S! z!* Y!",
                                      tz="EST")) 

write.csv(trump_tweets, file = "trump.csv") #csv incase i cant figure out how to manipulate it in R

head(trump_tweets)
names(trump_tweets)


tweets_df <- trump_tweets %>%
  select(source, id_str, text, created_at, retweet_count)

tweets_df$created_at <- as.Date(tweets_df$created_at, format="%d/%m/%Y")

tweets_table <- tweets_df %>% 
  group_by(
    'Year' = lubridate::year(created_at),
    'Week' = lubridate::week(created_at)
  ) %>%
  tally()

#look at averages across the different year, weeks, and daily average
week_avg <- tweets_df %>% 
  group_by(
    'Week' = lubridate::week(created_at)
  ) %>%
  tally() #%>%
  #mutate(avg = mean())

#make confiendence intervals 

#Predicting
index <- createDataPartition(tweets_table$n, times = 1, p = 0.5, list = FALSE)
train_set <- tweets_table[index,]
test_set <- tweets_table[-index,]


#Linear Regression
ctrl <- trainControl(method = "repeatedcv", repeats = 3)

lm.fit <- train(n ~ ., data = train_set, 
                method = "lm",
                trControl = ctrl)

pred.lm <- predict(lm.fit, test_set)
Lm.RMSE <- RMSE(pred.lm, test_set$n) #37.61

submission <- test_set %>% 
  mutate(Predicted_Tweets = pred.lm)



#Elastic Net
#Had the best predictive ability 
library(elasticnet)
enetGrid <- expand.grid(.lambda = c(0,0.01, .1), .fraction = seq(.05, 1, length = 20))

enet.fit <- train(n ~., data = train_set, 
                  method = "enet", 
                  trControl = ctrl, 
                  preProc = c("center", "scale")) 
                  #tuneGrid = enetGrid) 

pred.enet <- predict(enet.fit, test_set)
Enet.RMSE <- RMSE(pred.enet, test_set$n) #1.6863


#MARS
library(earth)
marsGrid <- expand.grid(.degree = 1:2, .nprune = 2:34)

mars.fit <- train(fat ~ ., data = train , 
                  method = "earth",
                  tuneGrid = marsGrid,
                  trControl = ctrl)

pred.mars <- predict(mars.fit, test)
Mars.RMSE <- RMSE(pred.mars, test$fat) #3.4561


#Partial Least Squares
library(pls)
pls.fit <- train(fat ~ ., data = train , 
                 method = "pls",
                 trControl = ctrl, 
                 preProc = c("center", "scale"),
                 #tuneLength = 30)
                 tuneGrid = data.frame(ncomp=13))

pred.pls <- predict(pls.fit, test)
Pls.RMSE <- RMSE(pred.pls, test$fat) #2.5137

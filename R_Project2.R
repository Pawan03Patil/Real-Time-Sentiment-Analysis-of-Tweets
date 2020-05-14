library(rtweet)
library(tm)
library(ggplot2)
library(sentimentr)
library(dplyr)
library(lubridate)
library(tidyr)

#get tweets
twt <- search_tweets(q = "corona", n = 18000,
                               lang = "en",
                               include_rts = FALSE,
                     retryonratelimit = TRUE)


#preprocessing of tweet text, remove links, punctuation
twt$text <- gsub("http.*","",  twt$text)
twt$text <- gsub("https.*","", twt$text)
twt$text <- gsub("@\\w+", "", twt$text)
twt$text <- gsub("[[:punct:]]", "", twt$text)
twt$text <- gsub("[ |\t]{2,}", "", twt$text)
twt$text <- gsub("^ ", "", twt$text)
twt$text <- gsub(" $", "", twt$text)
twt$text <- tolower(twt$text)

View(twt$text)

#----- Analysis with Sentimentr package

temp <- twt

sent <- sentiment_by(temp$text)

View(sent)
#check stats of sentiments
summary(sent$ave_sentiment)

twt$sentiment <- sent$ave_sentiment
#Plot of Sentiment Score to each tweet

ggplot(data=sent, aes(x=element_id,y=ave_sentiment))+geom_bar(aes(fill=ave_sentiment), stat = "identity")+
  theme(legend.position="none")+
  xlab("Tweet")+ylab("scores")+ggtitle("Sentiment Analysis")


#Histogram showing frequency of the sentiment scores
hist(sent$ave_sentiment, breaks = 40,prob=TRUE,xlab = "sentiment scores", main="Tweet Sentiment Histogram" )
lines(density(sent$ave_sentiment), col="blue")


#Extracting positive, negative and neutral words of each sentence

key_words <- temp$text %>% extract_sentiment_terms()

View(key_words)

key_words_pos <- apply(key_words, 1, function(x)length(unlist(x$positive)))
key_words_neg <- apply(key_words, 1, function(x)length(unlist(x$negative)))
  

#Plotting Number of positive and negative sentiment words of each tweet

ggplot(key_words, aes(x=element_id)) +
  geom_smooth(aes(y = key_words_pos), color = "green") +
  geom_smooth(aes(y = key_words_neg), color = "red") +
  xlab("Tweet") + ylab("Count of positive and negative sentiment words")



#Variation of sentiment over time
twt = separate(twt,"created_at",c("date","time"),sep=" ")

twt <- twt %>% mutate(year = year(twt$date), 
                                  month = month(twt$date),
                                  day = day(twt$date))

twt <- twt %>% mutate(hour = hour(strptime(twt$time, format = "%H:%M:%S")), 
                      minute = minute(strptime(twt$time, format = "%H:%M:%S")),
                      second = second(strptime(twt$time, format = "%H:%M:%S")))


#hour
grouped_tweets <- twt %>%
  group_by(hour = twt$hour)
avg_sentiment_by_hour <-  summarise(grouped_tweets,avg_sentiment = mean(sentiment))
View(avg_sentiment_by_hour)

ggplot(data = avg_sentiment_by_hour) + 
  geom_smooth(mapping = aes(x=hour,
                            y=avg_sentiment))

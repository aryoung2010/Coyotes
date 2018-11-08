#My code
#install.packages("sentimentr")

library(zipcode)
library(tidyverse)
library(usmap)
library(ggplot2)
library(tidytext)
library(tm)
library(topicmodels)
library(wordcloud)
library(RColorBrewer)
library(readxl)
library(stringr)
library(tidyr)


sample2 <- read_excel(("Real Talk Data (1).xlsx"))

pub_data <- sample2[sample2$Subject == "puberty",]
pub_data <- pub_data[is.na(pub_data[,33]),]

pub_data$Story <- do.call(paste, c(pub_data[37:45], sep=" "))
pub_data$Story <- gsub("NA","",pub_data$Story)
colnames(pub_data)[2] <- c("Submission")


pub_data$Story <- trimws(pub_data$Story)

pub_data <- pub_data[pub_data$Story != "",]

#########
#grouping methods - sentiment analysis

undesirable_words <- c("the")


pub_tidy <- pub_data %>%
  unnest_tokens(word, Story) %>% #Break the lyrics into individual words
  filter(!word %in% undesirable_words) %>% #Remove undesirables
  filter(!nchar(word) < 2) %>% #Words like "on" or "bc" used in lingo
  anti_join(stop_words) #Data provided by the tidytext package

pub_bing <- pub_tidy %>%
  inner_join(get_sentiments("bing"))

#str(pub_bing)

ind_polarity_bing <- pub_bing %>%
  group_by(Submission, sentiment) %>%
  count(Submission, sentiment) %>%
  spread(sentiment, n) %>%
  mutate(polarity = positive - negative,
         ratio = polarity / (positive + negative))


########
# sentiment sentence wise

library(sentimentr)
text <- data.frame(id = c("12345"),
                   sentence = c("Talk shit and tried to fight"),
                   stringsAsFactors = FALSE)

checkscore <- sentiment(text$sentence)
score <- checkscore$sentiment/checkscore$word_count*100

sentiment_polarity_score <- sentiment(pub_data$Story)
#sentiment_polarity_score2 <- mutate(sentiment = get_sentiment(pub_data$Story))


sentiment_polarity_score_groupby <- sentiment_polarity_score %>% 
  group_by(element_id) %>% 
  summarise(sentiment = sum(sentiment),sentences_no=max(sentence_id),word_count=sum(word_count),positive=count(sentiment>0))

pub_data$ID <- seq.int(nrow(pub_data))
temp <- merge(x=pub_data,y=sentiment_polarity_score_groupby,by.x="ID",by.y="element_id",all.x = TRUE)
options(scipen = 999)
temp$average_sentiment_score <- temp$sentiment/temp$word_count*100

temp$average_sentiment_score_Sentences <- temp$sentiment/temp$sentences_no*100

View(head(pub_data$Story,n=20))

###############








###

temp<-pub_data$Story

colnames(temp)[1] <- "word"

trump_tweet_sentiment <- temp[1] %>%
  inner_join(get_sentiments("bing")) %>%
  count(sentiment) 

###


load(url("https://cbail.github.io/Trump_Tweets.Rdata"))

tidy_trump_tweets<- trumptweets %>%
  select(created_at,text) %>%
  unnest_tokens("word", text)

trump_tweet_sentiment <- tidy_trump_tweets %>%
  inner_join(get_sentiments("bing")) %>%
  count(created_at, sentiment) 

View(head(trump_tweet_se,n=20))

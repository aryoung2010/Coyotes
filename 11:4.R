
# Loading required packages ------------------------------------------------------------
library(zipcode)
library(tidyverse)
library(usmap)
library(ggplot2)
library(tidytext)
library(tm)
library(topicmodels)
library(wordcloud)
library(RColorBrewer)


# Reading samples and cleaning --------------------------------------------
sample <- read.csv("Cleaned Real Talk Data - Master Sheet copy.csv", header = T,stringsAsFactors = FALSE)
#sample <- sample[1:811,]
colnames(sample)[13] <- "zip"
sample$zip[sample$zip < 10000 & is.na(sample$zip) == F] <- 
  sprintf("%05d", sample$zip[sample$zip < 10000 & is.na(sample$zip) == F])
sample <- sample[ ,colSums(!is.na(sample)) != 0]
sample$Story <- do.call(paste, c(sample[18:127], sep=" "))
sample$Gender[sample$Gender=='female']<-'Female'
sample$Gender[sample$Gender=='male']<-'Male'
sample$Gender[sample$Gender=='male ']<-'Male'
sample$Gender[sample$Gender=='other']<-'Other'
sample$Gender[sample$Gender=='non-binary']<-'Non-binary'
sample[sapply(sample, is.character)]<-lapply(sample[sapply(sample, is.character)], as.factor)
summary(sample)


# Using Tidytext to unnest the dataframe ----------------------------------
sampledf <- sample %>% 
  select(Story) %>% 
  unnest_tokens("word", Story)

# Removing Stopwords ------------------------------------------------------
data("stop_words")
samplesort <- sampledf %>% 
  anti_join(stop_words) %>% 
  count(word) %>% 
  arrange(desc(n))


# Sentimental Analysis ----------------------------------------------------
sp_sentiment <- samplesort %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment)

ggplot(sp_sentiment, aes(index, sentiment, fill = word)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~word, ncol = 2, scales = "free_x")


# Graphing1: top 20 words in histogram ------------------------------------
top_20 <- samplesort[1:20,]
top_20$word <- factor(top_20$word,
                      levels = top_20$word[order(top_20$n, decreasing = TRUE)])
ggplot(top_20, aes(x=word, y=n, fill=word))+
  geom_bar(stat="identity")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ylab("Word freq in real talks sample")+
  xlab("")+
  guides(fill=FALSE)
# Graphing2: Corpus & Topic modeling --------------------------------------
# Creating Corpus
sample_corpus <- Corpus(VectorSource(as.vector(sample$Story))) 
sample_corpus <- tm_map(sample_corpus, removeWords, stopwords("english"))
sample_corpus <- tm_map(sample_corpus, content_transformer(removeNumbers))
sample_corpus <- tm_map(sample_corpus, content_transformer(removePunctuation))
sample_corpus <- tm_map(sample_corpus, content_transformer(tolower))
sample_corpus <- tm_map(sample_corpus, content_transformer(stemDocument), language = "english")
# Creating dtm
DTM <- DocumentTermMatrix(sample_corpus, control = list(wordLengths = c(2, Inf)))
TDM <- TermDocumentMatrix(sample_corpus, control = list(wordLengths = c(2, Inf)))
rowTotals <- apply(DTM , 1, sum)
DTM <- DTM[rowTotals> 0, ] 
topic_model<-LDA(DTM, k=10, control = list(seed = 321)) #### Not Working !!!!!!

# Tidying the model
topics <- tidy(topic_model, matrix = "beta")
top_terms <- 
  topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
# Ploting 
top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()
# Graphing3: Wordcloud ----------------------------------------------------
m <- as.matrix(TDM)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
# head(d, 10)
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=100, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
# Graphing4: visualization of state counts for the sample -----------------
# Building a zipprefix-state df
data(zipcode)
#zipprefix <- zipcode
#zipprefix$zip <- substr(zipprefix$zip, 0, 3) # If only using prefix 
zip <- zipcode %>% 
  select(zip, state) %>% 
  distinct(zip, .keep_all = TRUE)
# Creat a new df with state and state count
samplestate <- merge(sample, zip, by = "zip")
samplecount <- samplestate %>% 
  group_by(state) %>% 
  count(state)
# Plotting
plot_usmap(regions = "states", data = samplecount, values = "n", lines = "red") + 
  scale_fill_continuous(low = "white", high = "red", name = "Population (2015)", label = scales::comma) +
  labs(title = "US States", subtitle = "This is a map.") + 
  theme(panel.background = element_rect(colour = "black", fill = "white"), legend.position = "right")

# Using Tidytext to unnest the dataframe by two grams ---------------------
sampledf2 <- sample %>% 
  select(Story) %>% 
  unnest_tokens(ngram, Story, token = "ngrams",n=2)

sample_separated <- sampledf2 %>%
  separate(ngram, c("word1", "word2"), sep = " ")

data("stop_words")
sample_filtered <- sample_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

sample_counts <- sample_filtered %>% 
  count(word1, word2, sort = TRUE)

sample_united <- sample_filtered %>%
  unite(ngram, word1, word2, sep = " ")

sample_unitedcounts <- sample_united %>% 
  count(ngram, sort = TRUE)

sample_unitedcounts <- sample_unitedcounts[-1,]

top_20tgrams <- sample_unitedcounts[1:20,]

top_20tgrams$ngram <- factor(top_20tgrams$ngram,
                       levels = top_20tgrams$ngram[order(top_20tgrams$n, decreasing = TRUE)])
ggplot(top_20tgrams, aes(x=ngram, y=n, fill=ngram))+
  geom_bar(stat="identity")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ylab("Two grams freq in real talks sample")+
  xlab("")+
  guides(fill=FALSE)
# Export cleaned data for LSWC --------------------------------------------
#write.csv(sample,'cleaned_data.csv')
# Heat plots --------------------------------------------------------------
liwc<-read.csv('LIWC2015 Results (cleaned_data).csv',header = T)
liwc<-liwc[liwc$WC!=0,]
liwc<-liwc[-813,]
library(made4)
#Normalization
liwcscaled <- scale(liwc[,20:ncol(liwc)])
heatmap.2(as.matrix(liwc[,21:30]), trace="none")
heatmap.2(as.matrix(liwc[,31:ncol(liwc)]), trace="none")

heatplot(liwc[,21:ncol(liwc)])
hist(liwc[,20:21])

# Emoji -------------------------------------------------------------------
# Disclaimer: ugly yet woking code :)

library(rvest)
library(magrittr)
library(dplyr)

# reference website
url <- "http://apps.timwhitlock.info/emoji/tables/unicode"

# get emoticons
emoticons <- url %>%
  html() %>%
  html_nodes(xpath='/html/body/div[2]/div/div/table[1]') %>%
  html_table()
emoticons <- data.frame(emoticons[[1]]$Native, emoticons[[1]]$Bytes, 
                        emoticons[[1]]$Description, stringsAsFactors = FALSE)
names(emoticons) <- c("Native", "Bytes", "Description")

# get additional emoticons
addemoticons <- url %>%
  html() %>%
  html_nodes(xpath='/html/body/div[2]/div/div/table[6]') %>%
  html_table()
addemoticons <- data.frame(addemoticons[[1]]$Native, addemoticons[[1]]$Bytes, 
                           addemoticons[[1]]$Description, stringsAsFactors = FALSE)
names(addemoticons) <- c("Native", "Bytes", "Description")

# get dingbats
dingbats <- url %>%
  html() %>%
  html_nodes(xpath='/html/body/div[2]/div/div/table[2]') %>%
  html_table()
dingbats <- data.frame(dingbats[[1]]$Native, dingbats[[1]]$Bytes, 
                       dingbats[[1]]$Description, stringsAsFactors = FALSE)
names(dingbats) <- c("Native", "Bytes", "Description")

# get transports
transport <- url %>%
  html() %>%
  html_nodes(xpath='/html/body/div[2]/div/div/table[3]') %>%
  html_table()
transport <- data.frame(transport[[1]]$Native, transport[[1]]$Bytes, 
                        transport[[1]]$Description, stringsAsFactors = FALSE)
names(transport) <- c("Native", "Bytes", "Description")

# get additional transports
addtransport <- url %>%
  html() %>%
  html_nodes(xpath='/html/body/div[2]/div/div/table[7]') %>%
  html_table()
addtransport <- data.frame(addtransport[[1]]$Native, addtransport[[1]]$Bytes, 
                           addtransport[[1]]$Description, stringsAsFactors = FALSE)
names(addtransport) <- c("Native", "Bytes", "Description")

# get enclosed emoticons
enclosed <- url %>%
  html() %>%
  html_nodes(xpath='/html/body/div[2]/div/div/table[4]') %>%
  html_table()
enclosed <- data.frame(enclosed[[1]]$Native, enclosed[[1]]$Bytes, 
                       enclosed[[1]]$Description, stringsAsFactors = FALSE)
names(enclosed) <- c("Native", "Bytes", "Description")

# get uncategorized emoticons
uncategorized <- url %>%
  html() %>%
  html_nodes(xpath='/html/body/div[2]/div/div/table[5]') %>%
  html_table()
uncategorized <- data.frame(uncategorized[[1]]$Native, uncategorized[[1]]$Bytes, 
                            uncategorized[[1]]$Description, stringsAsFactors = FALSE)
names(uncategorized) <- c("Native", "Bytes", "Description")

# get additional other emoticons
addothers <- url %>%
  html() %>%
  html_nodes(xpath='/html/body/div[2]/div/div/table[8]') %>%
  html_table()
addothers <- data.frame(addothers[[1]]$Native, addothers[[1]]$Bytes, 
                        addothers[[1]]$Description, stringsAsFactors = FALSE)
names(addothers) <- c("Native", "Bytes", "Description")

# combine all dataframes to overall dataframe
alltogether <- bind_rows(list(emoticons, addemoticons, dingbats, transport, 
                              addtransport, enclosed, uncategorized, addothers))

#emoji sentiment analysis
emojisenti<-read.csv('emoji.csv',header = T)

library(dplyr)
emojisenti$Unicode.name->emojisenti$Description
emojijoin<-alltogether %>% left_join(emojisenti)

# emoji summary for all emojis in the whole data
emoji<-data.frame()
for (i in alltogether$Native){
  emo<-data.frame('unicode'=i,'n'=sum(grepl(i,sample$Story,fixed = F)),'sentiscore'= emojijoin$Sentiment.score[emojijoin$Native==i],'name'=emojijoin$Description[emojijoin$Native==i])
  emoji<-rbind(emoji,emo)
}
emoji$ave<-emoji$n/sum(emoji$n)

# emoji summary for all three subjects
bullying<-sample[sample$Subject == 'bullying',]
emojibullying<-data.frame()
for (i in emojijoin$Native){
  emo<-data.frame('unicode'=i,'n'=sum(grepl(i,bullying$Story,fixed = F)),'sentiscore'=emojijoin$Sentiment.score[emojijoin$Native==i],'name'=emojijoin$Description[emojijoin$Native==i])
  emojibullying<-rbind(emojibullying,emo)
}
emojibullying$ave<-emojibullying$n/nrow(sample[sample$Subject=='bullying',])

relationships<-sample[sample$Subject == 'relationships',]
emojirelationships<-data.frame()
for (i in emojijoin$Native){
  emo<-data.frame('unicode'=i,'n'=sum(grepl(i,relationships$Story,fixed = F)),'sentiscore'=emojijoin$Sentiment.score[emojijoin$Native==i],'name'=emojijoin$Description[emojijoin$Native==i],'ave'=emoji$ave[emoji$unicode==i])
  emojirelationships<-rbind(emojirelationships,emo)
}

puberty<-sample[sample$Subject == 'puberty',]
emojipuberty<-data.frame()
for (i in emojijoin$Native){
  emo<-data.frame('unicode'=i,'n'=sum(grepl(i,puberty$Story,fixed = F)),'sentiscore'=emojijoin$Sentiment.score[emojijoin$Native==i],'name'=emojijoin$Description[emojijoin$Native==i],'ave'=emoji$ave[emoji$unicode==i])
  emojipuberty<-rbind(emojipuberty,emo)
}

#emoji summary for different genders
female<-sample[sample$Gender == 'Female',]
emojifemale<-data.frame()
for (i in emojijoin$Native){
  emo<-data.frame('unicode'=i,'n'=sum(grepl(i,female$Story,fixed = F)),'sentiscore'=emojijoin$Sentiment.score[emojijoin$Native==i],'name'=emojijoin$Description[emojijoin$Native==i],'ave'=emoji$ave[emoji$unicode==i])
  emojifemale<-rbind(emojifemale,emo)
}

male<-sample[sample$Gender == 'Male',]
emojimale<-data.frame()
for (i in emojijoin$Native){
  emo<-data.frame('unicode'=i,'n'=sum(grepl(i,male$Story,fixed = F)),'sentiscore'=emojijoin$Sentiment.score[emojijoin$Native==i],'name'=emojijoin$Description[emojijoin$Native==i],'ave'=emoji$ave[emoji$unicode==i])
  emojimale<-rbind(emojimale,emo)
}

#Plotting
#http://kt.ijs.si/data/Emoji_sentiment_ranking/emojimap.html
#http://kt.ijs.si/data/Emoji_sentiment_ranking/index.html
#https://prismoji.com/2017/02/06/emoji-data-science-in-r-tutorial/#part3
## install.packages("devtools")
library(emoGG)
library(ggplot2)
ggplot(emoji, aes(ave,sentiscore))+ggtitle("Frequency of emoji used in the whole data set")+
  geom_emoji(data=emoji[emoji$unicode=='\U0001f601',],emoji='1f601')+
  geom_emoji(data=emoji[emoji$unicode=='\U0001f602',],emoji='1f602')+
  geom_emoji(data=emoji[emoji$unicode=='\U0001f603',],emoji='1f603')+
  geom_emoji(data=emoji[emoji$unicode=='\U0001f605',],emoji='1f605')+
  geom_emoji(data=emoji[emoji$unicode=='\U0001f609',],emoji='1f609')+
  geom_emoji(data=emoji[emoji$unicode=='\U0001f60a',],emoji='1f60a')+
  geom_emoji(data=emoji[emoji$unicode=='\U0001f60c',],emoji='1f60c')+
  geom_emoji(data=emoji[emoji$unicode=='\U0001f60d',],emoji='1f60d')+
  geom_emoji(data=emoji[emoji$unicode=='\U0001f612',],emoji='1f612')+
  geom_emoji(data=emoji[emoji$unicode=='\U0001f613',],emoji='1f613')+
  geom_emoji(data=emoji[emoji$unicode=='\U0001f614',],emoji='1f614')+
  geom_emoji(data=emoji[emoji$unicode=='\U0001f616',],emoji='1f616')+
  geom_emoji(data=emoji[emoji$unicode=='\U0001f618',],emoji='1f618')+
  geom_emoji(data=emoji[emoji$unicode=='\U0001f61d',],emoji='1f61d')+
  geom_emoji(data=emoji[emoji$unicode=='\U0001f622',],emoji='1f622')+
  geom_emoji(data=emoji[emoji$unicode=='\U0001f625',],emoji='1f625')+
  geom_emoji(data=emoji[emoji$unicode=='\U0001f628',],emoji='1f628')+
  geom_emoji(data=emoji[emoji$unicode=='\U0001f629',],emoji='1f629')+
  geom_emoji(data=emoji[emoji$unicode=='\U0001f62a',],emoji='1f62a')+
  geom_emoji(data=emoji[emoji$unicode=='\U0001f62b',],emoji='1f62b')+
  geom_emoji(data=emoji[emoji$unicode=='\U0001f62d',],emoji='1f62d')+
  geom_emoji(data=emoji[emoji$unicode=='\U0001f633',],emoji='1f633')+
  geom_emoji(data=emoji[emoji$unicode=='\U0001f64c',],emoji='1f64c')+
  geom_emoji(data=emoji[emoji$unicode=='\U0001f607',],emoji='1f607')+
  geom_emoji(data=emoji[emoji$unicode=='\U0001f610',],emoji='1f610')+
  geom_emoji(data=emoji[emoji$unicode=='\U0001f611',],emoji='1f611')+
  geom_emoji(data=emoji[emoji$unicode=='\U0001f615',],emoji='1f615')+
  geom_emoji(data=emoji[emoji$unicode=='\U0001f62c',],emoji='1f62c')+
  geom_emoji(data=emoji[emoji$unicode=='\U0001f308',],emoji='1f308')+
  geom_emoji(data=emoji[emoji$unicode=='\U0001f346',],emoji='1f346')+
  geom_emoji(data=emoji[emoji$unicode=='\U0001f351',],emoji='1f351')+
  geom_emoji(data=emoji[emoji$unicode=='\U0001f352',],emoji='1f352')+
  geom_emoji(data=emoji[emoji$unicode=='\U0001f36b',],emoji='1f36b')+
  geom_emoji(data=emoji[emoji$unicode=='\U0001f445',],emoji='1f445')+
  geom_emoji(data=emoji[emoji$unicode=='\U0001f480',],emoji='1f480')+
  geom_emoji(data=emoji[emoji$unicode=='\U0001f497',],emoji='1f497')+
  geom_emoji(data=emoji[emoji$unicode=='\U0001f4a6',],emoji='1f4a6')+
  geom_emoji(data=emoji[emoji$unicode=='\U0001f4a9',],emoji='1f4a9')+
  geom_emoji(data=emoji[emoji$unicode=='\U0001f4af',],emoji='1f4af')+
  geom_emoji(data=emoji[emoji$unicode=='❤',],emoji='2764')

ggplot(emojifemale, aes(ave,sentiscore))+ggtitle("Frequency of emoji used by female")+
  geom_emoji(data=emojifemale[emojifemale$unicode=='\U0001f601',],emoji='1f601')+
  geom_emoji(data=emojifemale[emojifemale$unicode=='\U0001f602',],emoji='1f602')+
  geom_emoji(data=emojifemale[emojifemale$unicode=='\U0001f603',],emoji='1f603')+
  geom_emoji(data=emojifemale[emojifemale$unicode=='\U0001f605',],emoji='1f605')+
  geom_emoji(data=emojifemale[emojifemale$unicode=='\U0001f609',],emoji='1f609')+
  geom_emoji(data=emojifemale[emojifemale$unicode=='\U0001f60a',],emoji='1f60a')+
  geom_emoji(data=emojifemale[emojifemale$unicode=='\U0001f60c',],emoji='1f60c')+
  geom_emoji(data=emojifemale[emojifemale$unicode=='\U0001f60d',],emoji='1f60d')+
  geom_emoji(data=emojifemale[emojifemale$unicode=='\U0001f612',],emoji='1f612')+
  geom_emoji(data=emojifemale[emojifemale$unicode=='\U0001f613',],emoji='1f613')+
  geom_emoji(data=emojifemale[emojifemale$unicode=='\U0001f614',],emoji='1f614')+
  geom_emoji(data=emojifemale[emojifemale$unicode=='\U0001f616',],emoji='1f616')+
  geom_emoji(data=emojifemale[emojifemale$unicode=='\U0001f618',],emoji='1f618')+
  geom_emoji(data=emojifemale[emojifemale$unicode=='\U0001f61d',],emoji='1f61d')+
  geom_emoji(data=emojifemale[emojifemale$unicode=='\U0001f622',],emoji='1f622')+
  geom_emoji(data=emojifemale[emojifemale$unicode=='\U0001f625',],emoji='1f625')+
  geom_emoji(data=emojifemale[emojifemale$unicode=='\U0001f628',],emoji='1f628')+
  geom_emoji(data=emojifemale[emojifemale$unicode=='\U0001f629',],emoji='1f629')+
  geom_emoji(data=emojifemale[emojifemale$unicode=='\U0001f62a',],emoji='1f62a')+
  geom_emoji(data=emojifemale[emojifemale$unicode=='\U0001f62b',],emoji='1f62b')+
  geom_emoji(data=emojifemale[emojifemale$unicode=='\U0001f62d',],emoji='1f62d')+
  geom_emoji(data=emojifemale[emojifemale$unicode=='\U0001f633',],emoji='1f633')+
  geom_emoji(data=emojifemale[emojifemale$unicode=='\U0001f64c',],emoji='1f64c')+
  geom_emoji(data=emojifemale[emojifemale$unicode=='\U0001f607',],emoji='1f607')+
  geom_emoji(data=emojifemale[emojifemale$unicode=='\U0001f610',],emoji='1f610')+
  geom_emoji(data=emojifemale[emojifemale$unicode=='\U0001f611',],emoji='1f611')+
  geom_emoji(data=emojifemale[emojifemale$unicode=='\U0001f615',],emoji='1f615')+
  geom_emoji(data=emojifemale[emojifemale$unicode=='\U0001f62c',],emoji='1f62c')+
  geom_emoji(data=emojifemale[emojifemale$unicode=='\U0001f308',],emoji='1f308')+
  geom_emoji(data=emojifemale[emojifemale$unicode=='\U0001f346',],emoji='1f346')+
  geom_emoji(data=emojifemale[emojifemale$unicode=='\U0001f351',],emoji='1f351')+
  geom_emoji(data=emojifemale[emojifemale$unicode=='\U0001f352',],emoji='1f352')+
  geom_emoji(data=emojifemale[emojifemale$unicode=='\U0001f36b',],emoji='1f36b')+
  geom_emoji(data=emojifemale[emojifemale$unicode=='\U0001f445',],emoji='1f445')+
  geom_emoji(data=emojifemale[emojifemale$unicode=='\U0001f480',],emoji='1f480')+
  geom_emoji(data=emojifemale[emojifemale$unicode=='\U0001f497',],emoji='1f497')+
  geom_emoji(data=emojifemale[emojifemale$unicode=='\U0001f4a6',],emoji='1f4a6')+
  geom_emoji(data=emojifemale[emojifemale$unicode=='\U0001f4a9',],emoji='1f4a9')+
  geom_emoji(data=emojifemale[emojifemale$unicode=='\U0001f4af',],emoji='1f4af')+
  geom_emoji(data=emojifemale[emojifemale$unicode=='❤',],emoji='2764')
  
ggplot(emojimale, aes(ave,sentiscore))+ggtitle("Frequency of emoji used by male")+
  geom_emoji(data=emojimale[emojimale$unicode=='\U0001f601',],emoji='1f601')+
  geom_emoji(data=emojimale[emojimale$unicode=='\U0001f602',],emoji='1f602')+
  geom_emoji(data=emojimale[emojimale$unicode=='\U0001f603',],emoji='1f603')+
  geom_emoji(data=emojimale[emojimale$unicode=='\U0001f605',],emoji='1f605')+
  geom_emoji(data=emojimale[emojimale$unicode=='\U0001f609',],emoji='1f609')+
  geom_emoji(data=emojimale[emojimale$unicode=='\U0001f60a',],emoji='1f60a')+
  geom_emoji(data=emojimale[emojimale$unicode=='\U0001f60c',],emoji='1f60c')+
  geom_emoji(data=emojimale[emojimale$unicode=='\U0001f60d',],emoji='1f60d')+
  geom_emoji(data=emojimale[emojimale$unicode=='\U0001f612',],emoji='1f612')+
  geom_emoji(data=emojimale[emojimale$unicode=='\U0001f613',],emoji='1f613')+
  geom_emoji(data=emojimale[emojimale$unicode=='\U0001f614',],emoji='1f614')+
  geom_emoji(data=emojimale[emojimale$unicode=='\U0001f616',],emoji='1f616')+
  geom_emoji(data=emojimale[emojimale$unicode=='\U0001f618',],emoji='1f618')+
  geom_emoji(data=emojimale[emojimale$unicode=='\U0001f61d',],emoji='1f61d')+
  geom_emoji(data=emojimale[emojimale$unicode=='\U0001f622',],emoji='1f622')+
  geom_emoji(data=emojimale[emojimale$unicode=='\U0001f625',],emoji='1f625')+
  geom_emoji(data=emojimale[emojimale$unicode=='\U0001f628',],emoji='1f628')+
  geom_emoji(data=emojimale[emojimale$unicode=='\U0001f629',],emoji='1f629')+
  geom_emoji(data=emojimale[emojimale$unicode=='\U0001f62a',],emoji='1f62a')+
  geom_emoji(data=emojimale[emojimale$unicode=='\U0001f62b',],emoji='1f62b')+
  geom_emoji(data=emojimale[emojimale$unicode=='\U0001f62d',],emoji='1f62d')+
  geom_emoji(data=emojimale[emojimale$unicode=='\U0001f633',],emoji='1f633')+
  geom_emoji(data=emojimale[emojimale$unicode=='\U0001f64c',],emoji='1f64c')+
  geom_emoji(data=emojimale[emojimale$unicode=='\U0001f607',],emoji='1f607')+
  geom_emoji(data=emojimale[emojimale$unicode=='\U0001f610',],emoji='1f610')+
  geom_emoji(data=emojimale[emojimale$unicode=='\U0001f611',],emoji='1f611')+
  geom_emoji(data=emojimale[emojimale$unicode=='\U0001f615',],emoji='1f615')+
  geom_emoji(data=emojimale[emojimale$unicode=='\U0001f62c',],emoji='1f62c')+
  geom_emoji(data=emojimale[emojimale$unicode=='\U0001f308',],emoji='1f308')+
  geom_emoji(data=emojimale[emojimale$unicode=='\U0001f346',],emoji='1f346')+
  geom_emoji(data=emojimale[emojimale$unicode=='\U0001f351',],emoji='1f351')+
  geom_emoji(data=emojimale[emojimale$unicode=='\U0001f352',],emoji='1f352')+
  geom_emoji(data=emojimale[emojimale$unicode=='\U0001f36b',],emoji='1f36b')+
  geom_emoji(data=emojimale[emojimale$unicode=='\U0001f445',],emoji='1f445')+
  geom_emoji(data=emojimale[emojimale$unicode=='\U0001f480',],emoji='1f480')+
  geom_emoji(data=emojimale[emojimale$unicode=='\U0001f497',],emoji='1f497')+
  geom_emoji(data=emojimale[emojimale$unicode=='\U0001f4a6',],emoji='1f4a6')+
  geom_emoji(data=emojimale[emojimale$unicode=='\U0001f4a9',],emoji='1f4a9')+
  geom_emoji(data=emojimale[emojimale$unicode=='\U0001f4af',],emoji='1f4af')+
  geom_emoji(data=emojimale[emojimale$unicode=='❤',],emoji='2764')
  
ggplot(emojibullying, aes(ave,sentiscore))+ggtitle("Frequency emoji used and not used in bullying")+
  geom_emoji(data=emoji[emoji$unicode=='\U0001f601',],emoji='1f601')+
  geom_emoji(data=emoji[emoji$unicode=='\U0001f602',],emoji='1f602')+
  geom_emoji(data=emoji[emoji$unicode=='\U0001f603',],emoji='1f603')+
  geom_emoji(data=emoji[emoji$unicode=='\U0001f605',],emoji='1f605')+
  geom_emoji(data=emoji[emoji$unicode=='\U0001f609',],emoji='1f609')+
  geom_emoji(data=emoji[emoji$unicode=='\U0001f60a',],emoji='1f60a')+
  geom_emoji(data=emoji[emoji$unicode=='\U0001f60c',],emoji='1f60c')+
  geom_emoji(data=emoji[emoji$unicode=='\U0001f60d',],emoji='1f60d')+
  geom_emoji(data=emoji[emoji$unicode=='\U0001f612',],emoji='1f612')+
  geom_emoji(data=emoji[emoji$unicode=='\U0001f613',],emoji='1f613')+
  geom_emoji(data=emoji[emoji$unicode=='\U0001f614',],emoji='1f614')+
  geom_emoji(data=emoji[emoji$unicode=='\U0001f616',],emoji='1f616')+
  geom_emoji(data=emoji[emoji$unicode=='\U0001f618',],emoji='1f618')+
  geom_emoji(data=emoji[emoji$unicode=='\U0001f61d',],emoji='1f61d')+
  geom_emoji(data=emoji[emoji$unicode=='\U0001f622',],emoji='1f622')+
  geom_emoji(data=emoji[emoji$unicode=='\U0001f625',],emoji='1f625')+
  geom_emoji(data=emoji[emoji$unicode=='\U0001f628',],emoji='1f628')+
  geom_emoji(data=emoji[emoji$unicode=='\U0001f629',],emoji='1f629')+
  geom_emoji(data=emoji[emoji$unicode=='\U0001f62a',],emoji='1f62a')+
  geom_emoji(data=emoji[emoji$unicode=='\U0001f62b',],emoji='1f62b')+
  geom_emoji(data=emoji[emoji$unicode=='\U0001f62d',],emoji='1f62d')+
  geom_emoji(data=emoji[emoji$unicode=='\U0001f633',],emoji='1f633')+
  geom_emoji(data=emoji[emoji$unicode=='\U0001f64c',],emoji='1f64c')+
  geom_emoji(data=emoji[emoji$unicode=='\U0001f607',],emoji='1f607')+
  geom_emoji(data=emoji[emoji$unicode=='\U0001f610',],emoji='1f610')+
  geom_emoji(data=emoji[emoji$unicode=='\U0001f611',],emoji='1f611')+
  geom_emoji(data=emoji[emoji$unicode=='\U0001f615',],emoji='1f615')+
  geom_emoji(data=emoji[emoji$unicode=='\U0001f62c',],emoji='1f62c')+
  geom_emoji(data=emoji[emoji$unicode=='\U0001f308',],emoji='1f308')+
  geom_emoji(data=emoji[emoji$unicode=='\U0001f346',],emoji='1f346')+
  geom_emoji(data=emoji[emoji$unicode=='\U0001f351',],emoji='1f351')+
  geom_emoji(data=emoji[emoji$unicode=='\U0001f352',],emoji='1f352')+
  geom_emoji(data=emoji[emoji$unicode=='\U0001f36b',],emoji='1f36b')+
  geom_emoji(data=emoji[emoji$unicode=='\U0001f445',],emoji='1f445')+
  geom_emoji(data=emoji[emoji$unicode=='\U0001f480',],emoji='1f480')+
  geom_emoji(data=emoji[emoji$unicode=='\U0001f497',],emoji='1f497')+
  geom_emoji(data=emoji[emoji$unicode=='\U0001f4a6',],emoji='1f4a6')+
  geom_emoji(data=emoji[emoji$unicode=='\U0001f4a9',],emoji='1f4a9')+
  geom_emoji(data=emoji[emoji$unicode=='\U0001f4af',],emoji='1f4af')+
  geom_emoji(data=emoji[emoji$unicode=='❤',],emoji='2764')
  
  

# Analysis by three subjects ----------------------------------------------
bullying<-sample[sample$Subject == 'bullying',]
relationships<-sample[sample$Subject == 'relationships',]
puberty<-sample[sample$Subject == 'puberty',]

# Using Tidytext to unnest the dataframe
bullyingdf <- bullying %>% 
  select(Story) %>% 
  unnest_tokens("word", Story)
# Stopwords
data("stop_words")
bullyingsort <- bullyingdf %>% 
  anti_join(stop_words) %>% 
  count(word) %>% 
  arrange(desc(n))

# #Sentimental Analysis
# sp_sentiment <- samplesort %>%
#   inner_join(get_sentiments("bing")) %>%
#   count(word, sentiment) 
# 
# ggplot(sp_sentiment, aes(index, sentiment, fill = word)) +
#   geom_col(show.legend = FALSE) +
#   facet_wrap(~word, ncol = 2, scales = "free_x")


# Graphing1: top 20 words in histogram
top_20 <- bullyingsort[1:20,]
top_20$word <- factor(top_20$word,
                      levels = top_20$word[order(top_20$n, decreasing = TRUE)])
ggplot(top_20, aes(x=word, y=n, fill=word))+
  geom_bar(stat="identity")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ylab("Word freq in real talks bullying")+
  xlab("")+
  guides(fill=FALSE)

# Graphing2: Corpus & Topic modeling
# Creating Corpus
bullying_corpus <- Corpus(VectorSource(as.vector(bullying$Story))) 
bullying_corpus <- tm_map(bullying_corpus, removeWords, stopwords("english"))
bullying_corpus <- tm_map(bullying_corpus, content_transformer(removeNumbers))
bullying_corpus <- tm_map(bullying_corpus, content_transformer(removePunctuation))
bullying_corpus <- tm_map(bullying_corpus, content_transformer(tolower))
bullying_corpus <- tm_map(bullying_corpus, content_transformer(stemDocument), language = "english")
# Creating dtm
DTM <- DocumentTermMatrix(bullying_corpus, control = list(wordLengths = c(2, Inf)))
TDM <- TermDocumentMatrix(bullying_corpus, control = list(wordLengths = c(2, Inf)))
rowTotals <- apply(DTM , 1, sum)
DTM <- DTM[rowTotals> 0, ] 
topic_model<-LDA(DTM, k=10, control = list(seed = 321)) #### Not Working !!!!!!

# Tidying the model
topics <- tidy(topic_model, matrix = "beta")
top_terms <- 
  topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
# Ploting 
top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

# Graphing3: Wordcloud
m <- as.matrix(TDM)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
# head(d, 10)
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=100, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

# Graphing4: visualization of state counts for the bullying
# Building a zipprefix-state df
data(zipcode)
#zipprefix <- zipcode
#zipprefix$zip <- substr(zipprefix$zip, 0, 3) # If only using prefix 
zip <- zipcode %>% 
  select(zip, state) %>% 
  distinct(zip, .keep_all = TRUE)
# Creat a new df with state and state count
bullyingstate <- merge(bullying, zip, by = "zip")
bullyingcount <- bullyingstate %>% 
  group_by(state) %>% 
  count(state)
# Plotting
plot_usmap(regions = "states", data = bullyingcount, values = "n", lines = "red") + 
  scale_fill_continuous(low = "white", high = "red", name = "Population (2015)", label = scales::comma) +
  labs(title = "US States", subtitle = "This is a map.") + 
  theme(panel.background = element_rect(colour = "black", fill = "white"), legend.position = "right")


###########################################Alan
#!diagnostics off
# Using Tidytext to unnest the dataframe by two grams
bullyingdf2 <- bullying %>%
  select(Story) %>%
  unnest_tokens(ngram, Story, token = "ngrams",n=2)

###########################################Using Tidytext to unnest the dataframe
relationshipsdf <- relationships %>% 
  select(Story) %>% 
  unnest_tokens("word", Story)
# Stopwords
data("stop_words")
relationshipssort <- relationshipsdf %>% 
  anti_join(stop_words) %>% 
  count(word) %>% 
  arrange(desc(n))

# #Sentimental Analysis
# sp_sentiment <- samplesort %>%
#   inner_join(get_sentiments("bing")) %>%
#   count(word, sentiment) 
# 
# ggplot(sp_sentiment, aes(index, sentiment, fill = word)) +
#   geom_col(show.legend = FALSE) +
#   facet_wrap(~word, ncol = 2, scales = "free_x")


# Graphing1: top 20 words in histogram
top_20 <- relationshipssort[1:20,]
top_20$word <- factor(top_20$word,
                      levels = top_20$word[order(top_20$n, decreasing = TRUE)])
ggplot(top_20, aes(x=word, y=n, fill=word))+
  geom_bar(stat="identity")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ylab("Word freq in real talks relationships")+
  xlab("")+
  guides(fill=FALSE)

# Graphing2: Corpus & Topic modeling
# Creating Corpus
relationships_corpus <- Corpus(VectorSource(as.vector(relationships$Story))) 
relationships_corpus <- tm_map(relationships_corpus, removeWords, stopwords("english"))
relationships_corpus <- tm_map(relationships_corpus, content_transformer(removeNumbers))
relationships_corpus <- tm_map(relationships_corpus, content_transformer(removePunctuation))
relationships_corpus <- tm_map(relationships_corpus, content_transformer(tolower))
relationships_corpus <- tm_map(relationships_corpus, content_transformer(stemDocument), language = "english")
# Creating dtm
DTM <- DocumentTermMatrix(relationships_corpus, control = list(wordLengths = c(2, Inf)))
TDM <- TermDocumentMatrix(relationships_corpus, control = list(wordLengths = c(2, Inf)))
rowTotals <- apply(DTM , 1, sum)
DTM <- DTM[rowTotals> 0, ] 
topic_model<-LDA(DTM, k=10, control = list(seed = 321)) #### Not Working !!!!!!

# Tidying the model
topics <- tidy(topic_model, matrix = "beta")
top_terms <- 
  topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
# Ploting 
top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

# Graphing3: Wordcloud
m <- as.matrix(TDM)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
# head(d, 10)
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=100, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

# Graphing4: visualization of state counts for the relationships
# Building a zipprefix-state df
data(zipcode)
#zipprefix <- zipcode
#zipprefix$zip <- substr(zipprefix$zip, 0, 3) # If only using prefix 
zip <- zipcode %>% 
  select(zip, state) %>% 
  distinct(zip, .keep_all = TRUE)
# Creat a new df with state and state count
relationshipsstate <- merge(relationships, zip, by = "zip")
relationshipscount <- relationshipsstate %>% 
  group_by(state) %>% 
  count(state)
# Plotting
plot_usmap(regions = "states", data = relationshipscount, values = "n", lines = "red") + 
  scale_fill_continuous(low = "white", high = "red", name = "Population (2015)", label = scales::comma) +
  labs(title = "US States", subtitle = "This is a map.") + 
  theme(panel.background = element_rect(colour = "black", fill = "white"), legend.position = "right")

# Using Tidytext to unnest the dataframe by two grams
relationshipsdf2 <- relationships %>%
  select(Story) %>%
  unnest_tokens(ngram, Story, token = "ngrams",n=2)

# Text networks -----------------------------------------------------------
library(devtools)
install_github("cbail/textnets")
library(textnets)
sample_first_speeches <- sample %>% 
  group_by(Gender)
prepped_sample <- PrepText(sample_first_speeches, groupvar = "Gender", textvar = "Story", node_type = 'words',tokenizer = 'words',pos = "nouns", remove_stop_words = TRUE, compound_nouns = TRUE)
sample_text_network <- CreateTextnet(prepped_sample)
VisTextNet(sample_text_network, label_degree_cut = 0)



library(htmlwidgets)
vis <- VisTextNetD3(sample_text_network, 
                    height=1000,
                    width=1400,
                    bound=FALSE,
                    zoom=FALSE,
                    charge=-30)
saveWidget(vis, "sample_textnet.html")
sample_communities <- TextCommunities(sample_text_network)
head(sample_communities)

# Textweb -----------------------------------------------------------------
sampleweb<-sample[,c(1,18)]
sampleweb$Submission..Code->sampleweb$X
sampleweb<-sampleweb[,-1]
unigram_probs <- sampleweb %>%
  unnest_tokens(word, Story) %>%
  count(word, sort = TRUE) %>%
  mutate(p = n / sum(n))
#probability of words appearing
sampleweb<-read.csv('cleaned_data.csv',header = T)

library(tidyverse)
library(widyr)
tidy_skipgrams <- sampleweb %>%
  unnest_tokens(ngram, Story, token = "ngrams", n = 8) %>%
  mutate(ngramID = row_number()) %>% 
  unite(skipgramID, X, ngramID) %>%
  unnest_tokens(word, ngram)

data("stop_words")
samplesort <- tidy_skipgrams %>% 
  anti_join(stop_words,by =  ) %>% 
  count(word) %>% 
  arrange(desc(n))

skipgram_probs <- tidy_skipgrams %>%
  pairwise_count(word, skipgramID, diag = TRUE, sort = TRUE) %>%
  mutate(p = n / sum(n))

normalized_prob <- skipgram_probs %>%
  filter(n > 20) %>%
  rename(word1 = item1, word2 = item2) %>%
  left_join(unigram_probs %>%
              select(word1 = word, p1 = p),
            by = "word1") %>%
  left_join(unigram_probs %>%
              select(word2 = word, p2 = p),
            by = "word2") %>%
  mutate(p_together = p / p1 / p2)

normalized_prob %>% 
  filter(word1 == "friend") %>%
  arrange(-p_together)

pmi_matrix <- normalized_prob %>%
  mutate(pmi = log10(p_together)) %>%
  cast_sparse(word1, word2, pmi)


library(irlba)
#remove missing data
pmi_matrix@x[is.na(pmi_matrix@x)] <- 0
#next we run SVD
pmi_svd <- irlba(pmi_matrix, 2, maxit = 500)
#next we output the word vectors:
word_vectors <- pmi_svd$u
rownames(word_vectors) <- rownames(pmi_matrix)

library(broom)

search_synonyms <- function(word_vectors, selected_vector) {
  
  similarities <- word_vectors %*% selected_vector %>%
    tidy() %>%
    as_tibble() %>%
    rename(token = .rownames,
           similarity = unrowname.x.)
  
  similarities %>%
    arrange(-similarity)    
}
like <- search_synonyms(word_vectors,word_vectors["like",])
like
friend <- search_synonyms(word_vectors,word_vectors["friend",])
friend
bully <- search_synonyms(word_vectors,word_vectors["bully",])
bully

# Readability -------------------------------------------------------------
library(quanteda)
test<-corpus(sample_corpus)
x<-textstat_readability(test,measure = c("Flesch.Kincaid",'Flesch'))
# Emoji by new data -------------------------------------------------------
 


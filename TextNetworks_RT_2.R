#####
# Setup
library(devtools)
#install_github("cbail/textnets")


library(textnets)
rt <- read.csv("C:/Users/ayoung/Desktop/Duke/Git Repositories/Coyotes/cleaned_data.csv", stringsAsFactors = F)
library(udpipe)

# will use X as the groupvar, only use nouns, and look for word overlap

# textdata= rt
# textvar= Story
# groupvar = x

#node_type = "words"
#pos = "nouns"
#udmodel_lang = model
names(rt)
rt_simple <- rt[c(1,8,10,12,18:19)]

str(rt_simple)

#####
# All Text
# x, words, nouns

prepped_rt <- PrepText(rt_simple, groupvar = "X", textvar = "Story", node_type = "words", pos = "nouns", remove_stop_words = TRUE, compound_nouns = TRUE)

rt_text_network <- CreateTextnet(prepped_rt)

VisTextNet(rt_text_network, label_degree_cut = 0)


library(igraph)
library(ggraph)
library(ggplot2)
library(htmlwidgets)
vis <- VisTextNetD3(rt_text_network, 
                                  height=1000,
                                  width=1400,
                                  bound=FALSE,
                                  zoom=TRUE,
                                  charge=-30)
saveWidget(vis, "rt_textnet.html")


##################################################### Relationships
relationships <- subset(rt_simple, Subject == "relationships")
relationships[1:50,5]

prepped_rt_relationships <- PrepText(relationships, groupvar = "X", textvar = "Story", node_type = "words", tokenizer = "words", pos = "nouns", remove_stop_words = TRUE)

rt_text_network_relationships <- CreateTextnet(prepped_rt_relationships)

VisTextNet(rt_text_network_relationships, label_degree_cut = 0)


library(igraph)
library(ggraph)
library(ggplot2)
library(htmlwidgets)
vis_relationships <- VisTextNetD3(rt_text_network_relationships, 
                    height=1000,
                    width=1400,
                    bound=FALSE,
                    zoom=TRUE,
                    charge=-30)
saveWidget(vis_relationships, "rt_textnet_relationships.html")

##### 
#Relationships
# X, words, nouns

relationships <- subset(rt_simple, Subject == "relationships")
relationships[1:50,5]

prepped_rt_relationships <- PrepText(relationships, groupvar = "X", textvar = "Story", node_type = "words", pos = "nouns", remove_stop_words = TRUE, compound_nouns = TRUE)

rt_text_network_relationships <- CreateTextnet(prepped_rt_relationships)

VisTextNet(rt_text_network_relationships, label_degree_cut = 0)


library(igraph)
library(ggraph)
library(ggplot2)
library(htmlwidgets)
vis_relationships <- VisTextNetD3(rt_text_network_relationships, 
                             height=1000,
                             width=1400,
                             bound=FALSE,
                             zoom=TRUE,
                             charge=-30)
saveWidget(vis_relationships, "rt_textnet_relationships.html")

#####
# Puberty
# X, Words, Nouns

puberty <- subset(rt_simple, Subject == "puberty")
puberty[1:50,5]

prepped_rt_puberty <- PrepText(puberty, groupvar = "X", textvar = "Story", node_type = "words", pos = "nouns", remove_stop_words = TRUE, compound_nouns = TRUE)

rt_text_network_puberty <- CreateTextnet(prepped_rt_puberty)

#VisTextNet(rt_text_network_puberty, label_degree_cut = 0)


library(igraph)
library(ggraph)
library(ggplot2)
library(htmlwidgets)
vis_puberty <- VisTextNetD3(rt_text_network_puberty, 
                                  height=1000,
                                  width=1400,
                                  bound=FALSE,
                                  zoom=TRUE,
                                  charge=-30)
saveWidget(vis_puberty, "rt_textnet_puberty.html")

rt_firsts_communities_puberty <- TextCommunities(rt_text_network_puberty)
rt_firsts_communities_puberty[1:100,]
View(rt_firsts_communities_puberty)


library(doBy)
summaryBy(group~modularity_class, data = rt_firsts_communities_puberty)
summary(rt_firsts_communities_puberty)

library(tidyr)
library(tidyverse)
spread(rt_firsts_communities_puberty$group,rt_firsts_communities_puberty$modularity_class)

class(rt_firsts_communities_puberty)

top_words_modularity_classes_P <- InterpretText(rt_text_network_puberty, prepped_rt_puberty)
head(top_words_modularity_classes_P)
View(top_words_modularity_classes_P)

text_centrality_P <- TextCentrality(rt_text_network_puberty)
View(text_centrality_P)

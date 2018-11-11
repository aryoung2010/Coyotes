
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
bullying <- subset(rt_simple, Subject == "bullying")
bullying[1:50,5]

prepped_rt_bullying <- PrepText(bullying, groupvar = "X", textvar = "Story", node_type = "words", pos = "nouns", remove_stop_words = TRUE, compound_nouns = TRUE)


rt_text_network_bullying <- CreateTextnet(prepped_rt_bullying)

#VisTextNet(rt_text_network_bullying, label_degree_cut = 0)
library(igraph)
library(ggraph)
library(ggplot2)
library(htmlwidgets)
vis_bullying <- VisTextNetD3(rt_text_network_bullying, 
                    height=1000,
                    width=1400,
                    bound=FALSE,
                    zoom=TRUE,
                    charge=-30)
saveWidget(vis_bullying, "rt_textnet_bullying.html")

#install.packages("networkD3")


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

prepped_rt <- PrepText(rt, groupvar = "X", textvar = "Story", node_type = "words", tokenizer = "words", pos = "nouns", remove_stop_words = TRUE, compound_nouns = TRUE)


rt_text_network <- CreateTextnet(prepped_rt)

VisTextNet(rt_text_network, label_degree_cut = 0)

install.packages("htmlwidgets")
library(htmlwidgets)
vis <- VisTextNetD3(rt_text_network, 
                    prune_cut=.50,
                    height=1000,
                    width=1400,
                    bound=FALSE,
                    zoom=TRUE,
                    charge=-30)
saveWidget(vis, "rt_textnet.html")

rt_communities <- TextCommunities(rt_text_network)
head(rt_communities)

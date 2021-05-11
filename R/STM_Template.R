############################################################################################
###########################STM #############################################################
############################################################################################

install.packages("stm")
install.packages("streamR")
install.packages("RCurl")
install.packages("bitops")
install.packages("rjson")
install.packages("NLP")
install.packages('tm')
install.packages('geometry')
install.packages('Rtsne')
install.packages('rsvd')
library(tm)
library(bitops)
library(RCurl)
library(rjson)
library(ndjson)
library(streamR)
library(tidytext)
library(stminsights)
library(stringr)
library (Rtsne)
library(rsvd)
library(geometry)
library(igraph)
library(dplyr)
library(tm)
library(ggplot2)
library(RColorBrewer)
library(stm)

####STM####

set.seed(0389475)

processed <- textProcessor(hanau_final$text,
                           removestopwords = TRUE, 
                           stem = TRUE,
                           language = "de",
                           metadata = hanau_final)

plotRemoved(processed$documents, lower.thresh = seq(1,100, by = 10))
out <- prepDocuments(processed$documents, processed$vocab, processed$meta, lower.thresh = 10)

#Removing 97311 of 109265 terms (186776 of 1567712 tokens) due to frequency 
#Removing 35 Documents with No Words 
#Your corpus now has 120832 documents, 11954 terms and 1380936 tokens.

docs<- out$documents
vocab<- out$vocab
meta <- out$meta
head(docs)
head(vocab)
head(meta)

#
Model1 <- stm(documents = out$documents, vocab = out$vocab, K= 55, 
                data = out$meta, init.type = "Spectral")

labelTopics (Model1)#55

plot(Model1, type = "summary", text.cex = 0.3)

save(Model1, file = "FinalModel1.RData") #This is the STM outcome, not the dataset 

run_stminsights()

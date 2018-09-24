library(twitteR)
library(tm)
library(wordcloud)

# load twitter data
load("C:/rdmTweets.RData")

df <- do.call("rbind", lapply(rdmTweets, as.data.frame))

# build a corpus, and specify the source to be character vectors
myCorpus <- Corpus(VectorSource(df$text))

myTdm <- TermDocumentMatrix(myCorpus, control=list(wordLengths=c(1,Inf)))
myTdm

m <- as.matrix(myTdm)
# calculate the frequency of words and sort it descendingly by frequency
wordFreq <- sort(rowSums(m), decreasing=TRUE)
# word cloud
set.seed(375) # to make it reproducible
grayLevels <- gray( (wordFreq+10) / (max(wordFreq)+10) )
wordcloud(words=names(wordFreq), freq=wordFreq, min.freq=3, random.order=F,
          colors=grayLevels)



#Testing my own word cloud
##########################
library(tm)
library(wordcloud)
library(twitteR)
library(ROAuth)
library(stringr)

tweets <- userTimeline("zfleeman",n=500,excludeReplies=TRUE) #excludeReplies=TRUE
tweets <- twListToDF(tweets)

write.csv(tweets,file="tweets.csv")
head(tweets)

tweets$text <- str_replace_all(tweets$text,"[^[:graph:]]", " ")
tweets$text <- gsub("http.*","",tweets$text)


b <- Corpus(VectorSource(tweets$text), readerControl = list(language = "eng"))

b <- tm_map(b, content_transformer(PlainTextDocument))
b <- tm_map(b, content_transformer(tolower))
b <- tm_map(b, content_transformer(stripWhitespace))
b <- tm_map(b, content_transformer(removePunctuation))

inspect(b)

tdm <- TermDocumentMatrix(b)
m1 <- as.matrix(tdm)
v1 <- sort(rowSums(m1),decreasing=TRUE)
d1 <- data.frame(word=names(v1), freq=v1)
wordcloud(d1$word,d1$freq,colors = brewer.pal(7,"Set1"))

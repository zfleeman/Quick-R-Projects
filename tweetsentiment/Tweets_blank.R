##TWITTER
#Step 1: Create a Twitter account
#Step 2: Create a Twitter application at https://apps.twitter.com/
#Step 3: Find trending topics

library(twitteR)
library(ROAuth)
#ACCESS URLS
reqURL <- "https://api.twitter.com/oauth/request_token";
accessURL <- "https://api.twitter.com/oauth/access_token";
authURL <- "https://api.twitter.com/oauth/authorize";

#ACCESS KEYS
consumerKey <- ""
consumerSecret <- ""
access_token=""
access_secret=''

#HANDSHAKE
setup_twitter_oauth(consumerKey,
                    consumerSecret,
                    access_token="",
                    access_secret='')






a=searchTwitter("Northern Iowa", n=2000)
tweets_dfa = twListToDF(a)
tweets_dfa
b=searchTwitter("Cincy", n=200)
tweets_dfb = twListToDF(b)
c=searchTwitter("Notre Dame", n=200)
tweets_dfc = twListToDF(c)
tweets=rbind(tweets_dfa,tweets_dfb,tweets_dfc)
#tweets
write.csv(tweets,file="tweets.csv")
head(tweets)
library(tm)
library(wordcloud)
b=Corpus(VectorSource(tweets$text), readerControl = list(language = "eng"))
b=tm_map(b, PlainTextDocument)
inspect(b)
b<- tm_map(b, content_transformer(tolower))
#Changes case to lower case
b<- tm_map(b, stripWhitespace) #Strips White Space
b <- tm_map(b, removePunctuation) #Removes Punctuation
inspect(b)
tdm <- TermDocumentMatrix(b)
m1 <- as.matrix(tdm)
v1<- sort(rowSums(m1),decreasing=TRUE)
d1<- data.frame(word=names(v1), freq=v1)
wordcloud(d1$word,d1$freq,colors =brewer.pal(7,"Set1"))

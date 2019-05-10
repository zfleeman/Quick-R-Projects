library(twitteR)
library(ROAuth)
library(stringr)
library(lubridate)
library(ggplot2)

#seeeeecret tokens
access_token <- ""
access_secret <- ""
Consumer_key <- ""
Consumer_secret <- ""

#ACCESS URLS
reqURL <- "https://api.twitter.com/oauth/request_token";
accessURL <- "https://api.twitter.com/oauth/access_token";
authURL <- "https://api.twitter.com/oauth/authorize";

#HANDSHAKE
setup_twitter_oauth(Consumer_key, Consumer_secret, access_token, access_secret)

#get our afinn sentiment with the emojis that I tacked on there
sentiment <- read.csv("~/Dropbox/R/twitter/afinn-111-emoji.csv", stringsAsFactors = FALSE)
colnames(sentiment) <- c('word','score')

#decoder ring for emojis with 
emDict <- read.csv("~/Dropbox/R/twitter/emojisent.csv", stringsAsFactors = FALSE)
emDict$Description <- gsub(" ", "", emDict$Description)
colnames(emDict) <- c('word',"native","bytes",'R.encoding','score')

#get tweets and place them in a data frame
tweets <- userTimeline("zfleeman",n=1000,excludeReplies=TRUE, includeRts = FALSE)
tweets <- twListToDF(tweets)

#convert the text to something more plyable. this changes the emojis in our data frame to things that look like <e9><b4>
tweets$text <- iconv(tweets$text, "latin1", "ASCII", "byte")

#get rid of http garbage
tweets$text <- gsub("http.*","",tweets$text)

#translate the emojis in our tweets. I'm pretty proud of this nested loop. Probably a better way to do it, though
for(i in 1:nrow(tweets)){
  for(j in 1:nrow(emDict)){
    tweets$text[i] <- gsub(emDict$R.encoding[j],paste(emDict$word[j],""),tweets$text[i])
  }
}

#assign a score to every tweet
tweets$sentscore <- unlist(lapply(tweets$text,function(X){
  score <- 0
  tokenizedlist <- unlist(str_split(X, pattern = " "))
  for (i in 1:length(tokenizedlist)){
    if (tokenizedlist[i] %in% sentiment$word){
      pos <- match(tokenizedlist[i], sentiment$word)
      score <- score + sentiment$score[pos]
    }
  }
  return(score)}))

#mess with dates to be aggregatable
tweets$created <- as.Date(substr(tweets$created,1,10))
tweets$month <- as.Date(cut(tweets$created, breaks = "1 month"))
tweets$week <- week(tweets$created)
tweets <- subset(tweets, month(tweets$month) > (month(Sys.Date()) - 3))

plottable <- aggregate(sentscore ~ week, data = tweets, FUN = 'sum')
plottable$negpos <- plottable$sentscore > 0

ggplot(data = plottable, aes(x = week, y = sentscore, fill = negpos)) + stat_summary(fun.y = "sum", geom = "bar") + labs(x = "Week", y = "Summed Tweet Score", title = "Tweet Happiness per Week") + guides(fill=FALSE) + theme_minimal()
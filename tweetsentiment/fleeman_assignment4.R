#ADM, BUS6301
#Assignment 4
#Zach Fleeman 04/15/2016

#load necessary libraries
library(arules)
library(arulesViz)
library(googleVis)
library(RSocrata)
library(tm)
library(wordcloud)
library(twitteR)
library(ROAuth)
library(stringr)

###
#Part 1
###

#read in data
bookbaskets <- read.transactions("bookdata.tsv.gz", format="single", sep="\t", cols=c("userid", "title"), rm.duplicates=T) 

#92,108 book purchases.
#220,447 user IDs.

arules::inspect(bookbaskets[1:5]) #Examine the first five transactions
basketSizes<-size(bookbaskets) #Calculate number of books purchased by "userID"
bookFreq<-itemFrequency(bookbaskets) #Calculate the support for each book title 
bookCount <- (bookFreq/sum(bookFreq))*sum(basketSizes) #Get the absolute count of book occurrences. 
bookbasket_use<-bookbaskets[basketSizes>1] #Only keep transactions with more than one book purchased. 
bookbasket_use

#explore the data a little bit

summary(bookCount)
frequency <- itemFrequency(bookbasket_use)
itemFrequencyPlot(bookbasket_use,topN = 10)

#looking at the item frequency plot reveals the top ten books

apriori(bookbasket_use) #no rules

bookrules <- apriori(bookbasket_use,parameter = list(support = 0.001,confidence = 0.8))
print(bookrules) #2923 rules

summary(bookrules)
arules::inspect(bookrules[1:15])

#create two rules
bookrules_liftsorted <- sort(bookrules, by = "lift")
bookrules_consorted <- sort(bookrules, by = "confidence")
arules::inspect(bookrules_liftsorted[1:20])
arules::inspect(bookrules_consorted[1:20])

#get lovely bones from rules
lovelybones_liftrules <- subset(bookrules_liftsorted, items %in% "The Lovely Bones: A Novel")
lovelybones_conrules <- subset(bookrules_consorted, items %in% "The Lovely Bones: A Novel")


#check out the top rules
arules::inspect(lovelybones_liftrules[1:18])
arules::inspect(lovelybones_conrules[1:15])

#Lift gave me the most diverse recommendations list, and the top ten book recomendations are "Two for the Dough", "Harry Potter: Goblet", "One for the Money", "Confessions of a Shopaholic", "Harry Potter: Prisoner", "Harry Potter: Chamber", "Harry Potter: Sorcerer".

#plot all of these
plot(lovelybones_liftrules,method = "graph")
plot(lovelybones_conrules,method = "graph")

#get lovely bones from rules
divine_liftrules <- subset(bookrules_liftsorted, items %in% "Divine Secrets of the Ya-Ya Sisterhood: A Novel")
arules::inspect(divine_liftrules[1:3])

#Divine Secrets of the Ya-Ya Sisterhood: A Novel had three rules. The books that the person should be are Confessions of a Shopaholic, Harry Potter and the Chamber of Secrets, and She's Come Undone.


detach("package:arulesViz", unload=TRUE)
detach("package:arules", unload=TRUE)

###
#Part 2
###

#SOCRATA

bad_buildings <- read.socrata("http://data.kcmo.org/resource/ax3m-jhxx.csv")

bad_buildings$latlong <- gsub("\n"," ",bad_buildings$Location)
bad_buildings$latlong <- gsub(".*\\(","",bad_buildings$latlong)
bad_buildings$latlong <- gsub(")","",bad_buildings$latlong)
bad_buildings$latlong <- gsub("\\, ",":",bad_buildings$latlong)

#test and see if we have a lat/long value
bad_buildings$test <- as.numeric(gsub("\\:.*","",bad_buildings$latlong))

#create a data frame with records having a lat/long value
clean_bad_buildings <- subset(bad_buildings,!is.na(bad_buildings$test))

#plot a google vis map based on lat/long
plot(gvisMap(clean_bad_buildings,locationvar = "latlong"))


#TWITTER

#APP NAME: ADM_ASSIGNMENT4

#ACCESS KEYS
access_token <- "15383523-duXULezep9kR7fejTcExJhKjFxkJtJ5irmrJSPqKU"
access_secret <- "9cGP9mSIJ3HlHYx87yPj4vopWn7zrDCbYCTZ9y6JqwhYe"
Consumer_key <- "ihT4sKOE2OWjCAgugdWAonyRS"
Consumer_secret <- "PVb4dqYdh33NcUg8y5yb9jbveXAoGIdUjX0YOGfTnH3hmodi5i"

#ACCESS URLS
reqURL <- "https://api.twitter.com/oauth/request_token";
accessURL <- "https://api.twitter.com/oauth/access_token";
authURL <- "https://api.twitter.com/oauth/authorize";

#HANDSHAKE
setup_twitter_oauth(Consumer_key, Consumer_secret, access_token, access_secret)

a <- searchTwitter("Batman", n=200)
tweets_dfa <- twListToDF(a)

b <- searchTwitter("Superman", n=200)
tweets_dfb <- twListToDF(b)

c <- searchTwitter("Daredevil", n=200)
tweets_dfc <- twListToDF(c)

tweets <- rbind(tweets_dfa,tweets_dfb,tweets_dfc)

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


###
#Part 3
###

library(rjson)
library(ggplot2)
library(grid)
library(jpeg)
library(RCurl)

playerID <- 201939
shoturl <- paste("http://stats.nba.com/stats/shotchartdetail?CFID=33&CFPARAMS=2014-15&ContextFilter=&ContextMeasure=FGA&DateFrom=&DateTo=&GameID=&GameSegment=&LastNGames=0&LeagueID=00&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerID=",playerID,"&PlusMinus=N&Position=&Rank=N&RookieYear=&Season=2014-15&SeasonSegment=&SeasonType=Regular+Season&TeamID=0&VsConference=&VsDivision=&mode=Advanced&showDetails=0&showShots=1&showZones=0", sep = "")

shotData <- fromJSON(file = shoturl, method="C")
shotdf <- data.frame(matrix(unlist(shotData$resultSets[[1]][[3]]), ncol=21,byrow = TRUE))

colnames(shotdf) <- shotData$resultSets[[1]][[2]]

shotdf$LOC_X <- as.numeric(as.character(shotdf$LOC_X))
shotdf$LOC_Y <- as.numeric(as.character(shotdf$LOC_Y))
shotdf$SHOT_DISTANCE <- as.numeric(as.character(shotdf$SHOT_DISTANCE))

ggplot(shotdf, aes(x=LOC_X, y=LOC_Y)) + geom_point(aes(colour = EVENT_TYPE))

courtImg.URL <- "https://thedatagame.files.wordpress.com/2016/03/nba_court.jpg"
court <- rasterGrob(readJPEG(getURLContent(courtImg.URL)), width=unit(1,"npc"), height=unit(1,"npc"))

# plot using NBA court background and colour by shot zone
ggplot(shotdf, aes(x=LOC_X, y=LOC_Y)) + annotation_custom(court, -250, 250, -50, 420) + geom_point(aes(colour = SHOT_ZONE_BASIC, shape = EVENT_TYPE)) + xlim(-250, 250) + ylim(-50, 420)

hd <- read.csv('~/Dropbox/colorado analysis/newhousing.csv')

hd <- cbind(hd[1:7],stack(hd[8:249]))

hd$date <- as.Date(paste(substr(hd$ind, 2,5), "-", substr(hd$ind,7,8), "-01", sep = ""))

colorado_present <- subset(hd, hd$date >= '2000-01-01' & hd$State == "CO")


write.csv(colorado_present,'colorado_092916.csv')

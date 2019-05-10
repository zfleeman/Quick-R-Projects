amiibo_list <- read.csv("amiibo-my-collection (1).csv",header = FALSE)

if("New Leaf Welcome amiibo cards" %in% amiibo_list$V1){
  welcomeamiibostart <- as.numeric(which(amiibo_list$V1 == "New Leaf Welcome amiibo cards"))
  welcomeamiibo <- read.csv("amiibo-my-collection.csv",header = TRUE, skip = welcomeamiibostart)
  firstwhitewelcome <- which(welcomeamiibo$Card.number == "")
  welcomeamiibo <- welcomeamiibo[1:(firstwhitewelcome[1]-1),]
}

if("Series 1-4 amiibo cards" %in% amiibo_list$V1){
  seriesstart <- as.numeric(which(amiibo_list$V1 == "Series 1-4 amiibo cards"))
  series <- read.csv("amiibo-my-collection (1).csv", header = TRUE, skip = seriesstart-1)
  firstwhiteseries <- which(series$Card.number == "")
  series <- series[1:(firstwhiteseries[1]-1),]
}

if("amiibo figures" %in% amiibo_list$V1){
  figuresstart <- as.numeric(which(amiibo_list$V1 == "amiibo figures"))
  figures <- amiibo_list[(figuresstart+2):(as.numeric(nrow(amiibo_list))-2),]
  
  colnames(figures) <- c("name", "scanned")
}
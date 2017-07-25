library(ggplot2)

years <- c(2012:2017)
months <- c(1:12)
days <- c(1:31)

hdr <- c("Time", "TempF","Dew.PointF","Humidity","Sea.Level.PressureIn","VisibilityMPH","Wind.Direction","Wind.SpeedMPH","Gust.SpeedMPH","Precipitation","Events","Conditions","WindDirDegrees","DateUTC","date")
df <- data.frame()

### Study for Kansas City, MO

for(year in years){
  for(month in months){
    for(day in days){
      if(!is.na(as.Date(paste0(year,"-",month,"-",day), format = "%Y-%m-%d"))){
        query <- paste0("https://www.wunderground.com/history/airport/KMKC/", year,"/",month,"/",day,"/DailyHistory.html?req_city=Kansas+City&req_state=MO&req_statename=Missouri&reqdb.zip=64111&format=1&format=1")
        weather <- read.csv(query)
        weather$date <- as.Date(paste0(year,"-",month,"-",day), format = "%Y-%m-%d")
        colnames(weather) <- hdr
        df <- rbind(df, weather)
      }
    }
  }
}

df$Humidity <- as.numeric(df$Humidity)
df$month <- as.Date(cut(df$date, "1 month"))

KC <- df
# write.csv(KC, "//ad/userfiles/u62096/public/weather/64111.csv", row.names = FALSE)
# KC <- read.csv("//ad/userfiles/u62096/public/weather/64111.csv", stringsAsFactors = FALSE)

KChighs <- aggregate(TempF ~ date + month, data = KC, FUN = "max")
KChighs <- KChighs[KChighs$TempF > 0,]
KChighs$type <- "Average Monthly High"
colnames(KChighs) <- c("date", "month", "value", "type")

KChumid <- aggregate(Humidity ~ date + month, data = KC, FUN = "mean")
KChumid$type <- "Average Humidity %"
colnames(KChumid) <- c("date", "month", "value", "type")

KCplottable <- rbind(KChighs, KChumid)

ggplot(data = KCplottable, aes(x = month, y = value, color = type)) + stat_summary(fun.y = "mean", geom = "line", size = 1) + labs(x="", y="Degrees/Humidity Percentage", title="Kansas City, MO Heat and Humidity") + theme_minimal() + ylim(c(35,100)) + ggsave("KC.png", width = 14, height = 8.5, units = "in")


### Study for Broomfield, CO

for(year in years){
  for(month in months){
    for(day in days){
      if(!is.na(as.Date(paste0(year,"-",month,"-",day), format = "%Y-%m-%d"))){
        query <- paste0("https://www.wunderground.com/history/airport/KBJC/", year,"/",month,"/",day,"/DailyHistory.html?req_city=Broomfield&req_state=CO&req_statename=Colorado&reqdb.zip=80020&format=1&format=1")
        weather <- read.csv(query)
        weather$date <- as.Date(paste0(year,"-",month,"-",day), format = "%Y-%m-%d")
        colnames(weather) <- hdr
        df <- rbind(df, weather)
      }
    }
  }
}

df$Humidity <- as.numeric(df$Humidity)
df$month <- as.Date(cut(df$date, "1 month"))

CO <- df

# write.csv(BR, "//ad/userfiles/u62096/public/weather/80020.csv", row.names = FALSE)
# BR <- read.csv("//ad/userfiles/u62096/public/weather/80020.csv", stringsAsFactors = FALSE)

COhighs <- aggregate(TempF ~ date + month, data = CO, FUN = "max")
COhighs <- COhighs[COhighs$TempF > 0,]
COhighs$type <- "Average Monthly High"
colnames(COhighs) <- c("date", "month", "value", "type")

COhumid <- aggregate(Humidity ~ date + month, data = CO, FUN = "mean")
COhumid$type <- "Average Humidity %"
colnames(COhumid) <- c("date", "month", "value", "type")

COplottable <- rbind(COhighs, COhumid)

ggplot(data = COplottable, aes(x = month, y = value, color = type)) + stat_summary(fun.y = "mean", geom = "line", size = 1) + labs(x="", y="Degrees/Humidity Percentage", title="Broomfield, CO Heat and Humidity") + theme_minimal() + ylim(c(35,100)) + ggsave("CO.png", width = 14, height = 8.5, units = "in")

### Comparisons

KChighs$City <- "Kansas City"
COhighs$City <- "Broomfield"
both <- rbind(KChighs, COhighs)
ggplot(data = both, aes(x = month, y = value, color = City)) + stat_summary(fun.y = "mean", geom = "line", size = 1) + labs(x = "", y = "Degrees Fahrenheit", title = "High Temperature Comparison") + theme_minimal() + ggsave("Heat.png", width = 14, height = 8.5, units = "in")

KChumid$City <- "Kansas City"
COhumid$City <- "Broomfield"
both <- rbind(KChumid, COhumid)
ggplot(data = both, aes(x = month, y = value, color = City)) + stat_summary(fun.y = "mean", geom = "line", size = 1) + labs(x = "", y = "Humidity Percentage", title = "Humidity Percentage Comparison") + theme_minimal() + ggsave("Humidity.png", width = 14, height = 8.5, units = "in")

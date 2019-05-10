library(ggplot2)

#read in our data
ops <- read.csv("ZF_RI_OPS_COMPLETE_FINAL-7658682.csv")

#add in new date fields
ops$datenumber <- as.numeric(as.POSIXct(ops$Dt.Tmstamp, format = "%m/%d/%Y %I:%M:%S%p"))
ops$date <- as.Date(substr(ops$Dt.Tmstamp,1,10),format = "%m/%d/%Y")
ops$month <- as.Date(cut(ops$date, breaks = "1 month"))
ops <- ops[order(ops$datenumber, decreasing = TRUE),]

#Limit the Data Frame to Q02 Planner Code parts that have transactions with quantities greater than zero
ops <- subset(ops, substr(ops$Planner.Cd,1,3) == "Q02" & ops$Compl.Qty > 0)

#A chart showing that Receiving Inspection has completed more operations as a department due to our increased workload over two years...
ggplot(data = ops, aes(x = month)) + geom_bar() + labs(x = "", y ="Completed Operations", title = "Count of Completed Operations on Q02 Parts") + scale_x_date(date_breaks = "1 month", date_labels = "%b '%y") + theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Subset our ops data frame to pull only my direct reports and something greater than thirty days ago
zach <- subset(ops, ops$Supv.ID == 00000 & ops$date > Sys.Date() - 30)
aggregate_count <- aggregate(date ~ Name, FUN = function(x){NROW(x)}, data = zach)

#make this annonymous
aggregate_count$Name <- c("Person 1","Person 2","Person 3","Person 4","Person 5","Person 6","Person 7","Person 8","Person 9","Person 10","Person 11","Person 12","Person 13","Person 14","Person 15","Person 16","Person 17","Person 18","Person 19","Person 20")

#A rolling 30-day look at each of my employee's operation comletions...
ggplot(data = aggregate_count, aes(x = reorder(Name, -date), y = date)) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + labs(x = "", y = "", title = "Count of Operation Completions for Zach Fleeman's Direct Reports")

#read in data
library(readxl)
sheet <- read_excel("MULTIPLE LINEAR REGRESSION AND OPTIMZATION.xlsx", skip = 1)
colnames(sheet) <- c("city","riders","price","population","monthly","parkingrate")

#generate linear model
model <- lm(riders ~ price + population + monthly + parkingrate, data = sheet)

#create our predictors data frame
price = c(5,6,7,8)
population = c(100000,90000,85000,110000)
monthly = c(5000,4000,3000,6000)
parkingrate = c(80,100,60,50)
predictors <- data.frame(price,population,monthly,parkingrate)

predict(model,predictors, interval = "prediction")

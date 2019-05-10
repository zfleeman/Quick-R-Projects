library(psych)

dp <- read.csv("death_penalty_data.csv", stringsAsFactors = FALSE)
dp$race_of_victim <- ifelse(dp$race_of_victim == "white", 1, 0)
dp$death_penalty <- ifelse(dp$death_penalty == "yes", 1, 0)

logit <- glm(death_penalty ~ severity_of_crime + race_of_victim, data = dp, family = binomial())
summary(logit)

-6.676 + 1.5397*5 + 1.8106*1
-6.676 + 1.5397*5

exp(2.8331)/(1 + exp(2.8331))
exp(1.0225)/(1 + exp(1.0225))

probability <- data.frame(fitted(logit))
predictions <- data.frame(ifelse(probability > 0.5, 1, 0))
comparison <- data.frame(dp$death_penalty, predictions)
table(comparison)
cm <- prop.table(table(comparison))
cm
0.01657459+0.04419890
logLik(logit)
table(dp$severity_of_crime,dp$race_of_victim)

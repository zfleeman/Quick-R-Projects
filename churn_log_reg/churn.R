# Churn Modeling
# Churn: Customers who cancelled contracts
#        Customers who could not renew contracts
#        Customers who had no activity for last month

# 3 Issues:
# a) probability of a customer "churning"
# b) clv
# c) structure of offers

library(psych)
library(outliers)
library(mlogit)
library(car)
library(DiscriMiner)

subscriber <- read.csv("subscriber_data.csv")
churn <- read.csv("churn_data.csv", stringsAsFactors = FALSE)

# probability of subscribing as a function of age
subscriber$subscribe <- ifelse(subscriber$subscribe == "yes", 1, 0)

# linear probability model
lpm <- lm(subscribe ~ age, data = subscriber)
predict(lpm, data.frame(age = 55))
# this isn't great due to the binary variable

# logistic function is better for binary outcomes
seg1 <- churn[1:5000,]
seg2 <- churn[5001:10000,]

# basic descriptive
describe(seg1)
describe(seg2)

hist(seg1$handset_age)
hist(seg2$handset_age)

# Hypothesis test on handset age

t.test(seg2$handset_age,seg1$handset_age)
# t is greater than 1.65

############ Do it again with overbundle mins

hist(seg1$avg_overbundle_mins)
hist(seg2$avg_overbundle_mins)

# Hypothesis test on handset age

t.test(seg2$avg_overbundle_mins,seg1$avg_overbundle_mins)
# t is less than 1.65

# LOGISTIC REGRESSION MODEL
churn_sub <- subset(churn, select = -c(customer, churn))

#binary
CHURN <- ifelse(churn$churn == "yes", 1, 0)
logitmodel <- glm(CHURN~., data = churn_sub, family = binomial())
summary(logitmodel)
logLik(logitmodel)

probability <- data.frame(fitted(logitmodel))
predictions <- data.frame(ifelse(probability > .5, "yes", "no"))
comparison <- data.frame(churn$churn, predictions)

table(comparison)

confusion_matrix <- prop.table(table(comparison))

logitmodel2 <- glm(churn ~ age + children + smart_phone + handset_age + avg_overbundle_mins + call_minutes_change_pct + bill_amount_change_pct, data = churn, family = binomial())
logLik(logitmodel2)
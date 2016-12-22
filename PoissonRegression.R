setwd("F:/PGDDS/PGDDS DAY 23 ACTIVITY DATA SETS")
callData <- read.csv("Complaint Counts Poisson Regression.csv")
compModel <- glm(ncomp~region+tier+age,data=callData,family = "poisson")
summary(compModel)
#West doesnt have much complains
#silver and gold members should be considered

#GLOBAL TESTING
library(lmtest)
null <- glm(ncomp~1,data=callData,family = "poisson")
lrtest(compModel,null)

#INDIVIDUAL TESTING


#predict fn requires model,object,data and type
callData$ncompPred <- round(predict(compModel,callData,type="response"))
head(callData)
callData$residual<- round(residuals(compModel))
head(callData)

rmse <- sqrt(mean(callData$residual^2))
rmse
#GOODNESS OF FIT (residual,deviance,df) performed because we have residuals here
pvalue <- 1-pchisq(116,0.01,106)
pvalue

#offset(log(N))
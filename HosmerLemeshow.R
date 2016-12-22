setwd("F:/PGDDS/PGDDS DAY 19 ACTIVITY DATA SETS")

bankloan <- read.csv("BANK LOAN.csv")

riskModel <- glm(DEFAULTER~EMPLOY+ADDRESS+DEBTINC+CREDDEBT,family = binomial,
                 data=bankloan)


install.packages("ResourceSelection")
library(ResourceSelection)

hltest <- hoslem.test(bankloan$DEFAULTER,fitted(riskModel),g=10)
hltest[] #this gives you what R is doing in the backend
hltest

#TABLE OF EXP AND OBSERVED FREQS:

cbind(hltest$expected,hltest$observed)
s<-summary(riskModel)
s[]
s$coefficients[,1]
riskModel$coefficients[2]


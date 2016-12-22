setwd("F:/PGDDS/PGDDS DAY 25 ACTIVITY DATA SETS")
bankloan <- read.csv("bankloan cox regression.csv")
library(survival)
surv.object <- Surv(bankloan$TIME,bankloan$STATUS)
timemodel <- coxph(surv.object~EMPLOY+ADDRESS+DEBTINC+CREDDEBT+OTHDEBT, data = bankloan)
summary(timemodel)

#check ppt for interpretation of the output

cox.zph()

install.packages("pec")
library(pec)
bankloantest<-read.csv("bankloan cox regression test data.csv")
#install.packages("prodlim")
#check for the customers who will last for 24 months, if less prob then less chances of surviving
bankloantest$prob24 <- predictSurvProb(timemodel,bankloantest,times=24)
head(bankloantest)

library(datasets)
library(car)
data(airmiles)
install.packages("insuranceData")
library(insuranceData)
data(AutoBi)
View(airmiles)

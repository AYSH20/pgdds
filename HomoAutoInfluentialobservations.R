install.packages("lmtest")
setwd("F:/PGDDS/PGDDS DAY 17 ACTIVITY DATA SETS")
perfInd <- read.csv("Performance Index.csv")
head(perfInd)
jpimodel <- lm(formula = jpi~aptitude+technical+general,data = perfInd)
library(car)
ncvTest(jpimodel,~aptitude+technical+general)
#p value >0.05 .:do not reject H0
#constant variance can be assumed
library(lmtest)
##Remedial Measures
#just for practice

se_correct <- hccm(jpimodel) #HETEROSCEDASTICITY-CORRECTED COVARIANCE MATRICES
se_correct
coeftest(jpimodel,vcov. = se_correct)


#AUTOCORRELATION
sales <- read.csv("sales vs marketing cost.csv")
salesmodel <- lm(formula = sales~print+online, data=sales)
summary(salesmodel)
durbinWatsonTest(salesmodel)

##DETECTING INFLUENTIAL OBSERVATIONS
influ <- influence.measures(jpimodel)
influ
influencePlot(jpimodel,id.method ="identity",main="influence plot",sub="" )
#study this again.

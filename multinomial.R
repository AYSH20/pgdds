setwd("F:/PGDDS/PGDDS DAY 22 ACTIVITY DATA SETS")
multi <- read.csv("BRAND PREF MULTINOMIAL LOGISTIC.csv")
install.packages("nnet")
multi$Brand<- relevel(multi$Brand,ref = "C")
library(nnet)
brandprefmodel <- multinom(Brand~Gender+Age,data=multi)
m <- summary(brandprefmodel)
m
#
#GLOBAL TESTING
library(lmtest)
null <- multinom(Brand~1,data=multi)
lrtest(brandprefmodel,null)
#reject null hypothesis

#individual testing
z <- m$coefficients/m$standard.errors
#table of pvalues
pvalue <- 1-pchisq(z^2,df=1)
pvalue
# only A brand is preferred by Males and is the only significant value. 
#Revise the model w/o Age
brandprefmodel <- multinom(Brand~Gender,data=multi)
n <- brandprefmodel
null <- multinom(Brand~1,data=multi)
null[]
lrtest(brandprefmodel,null)
predict<- predict(brandprefmodel, multi, type="class")
table(predict,multi$Brand)


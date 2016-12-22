install.packages("MASS")
setwd("F:/PGDDS/PGDDS DAY 23 ACTIVITY DATA SETS")
grad <- read.csv("Graduation App ordinal logistic.csv")
library(MASS)
summary(grad)
str(grad)
table(grad$apply,grad$gpa)
grad$apply <- as.ordered(grad$apply)
gradmodel <- polr(apply~pared+public+gpa,data=grad,Hess = T)
effect <- summary(gradmodel)
effect


#GLOBAL TESTING
library(lmtest)
null <- polr(apply~1,data=grad, Hess = T)
lrtest(gradmodel,null)

#INDIVIDUAL TESTING
ptable <- data.frame(effect$coefficients)
ptable$pvalue <- 1-pchisq(ptable$t.value^2,df = 1)
ptable$pvalue <- round(ptable$pvalue,4)
ptable

#we dont perform goodness of fit test as we dont observe residuals

expected<- predict(gradmodel,grad,type="class")
ctable<-table(grad$apply,expected)
ctable


\getwd()
setwd("F:/PGDDS/PGDDS DAY9 ACTIVITY DATA SETS")
mistime <- read.csv("ONE SAMPLE t TEST.csv")
#One sample t-test

t.test(mistime$Time, alternative = "greater", mu=90.5)

##INDEPENDENT SAMPLES t TEST
mistime2 <- read.csv("INDEPENDENT SAMPLES t TEST.csv",header=T)
attach(mistime2)
t.test(time_g1,time_g2,alternative = "two.sided", var.equal = T)

#CONCLUSION
#if the CI contains 0 then do not reject null hypothesis also p-value is 82.51 so do not reject Ho


##PAIRED t TEST
mistime3 <- read.csv("PAIRED t TEST.csv")

attach(mistime3)

t.test(time_before,time_after,alternative = "greater", paired = T)

#CONCLUSION
#reject Ho as p value is very less than LCL

#### F TEST

var.test(time_g1,time_g2,alternative = "two.sided")

var.test(time_g1,time_g2,alternative = "greater")

var.test(time_g1,time_g2,alternative = "less") #not to use less than 

# WELCH TWO SAMPLE TEST

t.test(time_g1,time_g2,alternative = "two.sided", var.equal = F)

setwd("F:/PGDDS/PGDDS DAY10 ACTIVITY DATA SETS")

#Single sample proportion
loyalty <- read.csv("Single sample proportion.csv")
attach(loyalty)
table(opinion) #freq of yes n No

prop.test(141,180,0.75, correct = F,alternative = "greater")


#Two sample proportion

loyalty_age <- read.csv("Two sample proportion.csv")
attach(loyalty_age)
prop.test(table(age,opinion),alternative = "two.sided", correct = F)

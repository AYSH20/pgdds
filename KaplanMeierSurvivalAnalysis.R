setwd("F:/PGDDS/PGDDS DAY 25 ACTIVITY DATA SETS")
kaplan_meier <- read.csv("Survival Analysis painrelief.csv")
install.packages("survival")
library(survival)
#
surv.obj <- Surv(kaplan_meier$time,kaplan_meier$status)
survdist <- survfit(surv.obj~1)
summary(survdist)

#plot survival distribution
plot(survdist,xlab="Time",ylab="Survival probability", col=c("green","blue","blue"))
survdist1 <- survfit((surv.obj)~treatment,data = kaplan_meier)
summary(survdist1)
plot(survdist1,xlab="Time",ylab="Survival probability", col=c("green","blue"),mark.time=T)

#is the difference b/w two survival distribution significant?
#LogRank
survdiff(surv.obj~treatment,data=kaplan_meier)
# Ho : the difference is same
#do not reject Ho, the difference is not significant 
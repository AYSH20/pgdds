setwd("F:/PGDDS/PGDDS DAY 17 ACTIVITY DATA SETS")
install.packages("leaps")
library(leaps)
bestmodel<-regsubsets(jpi~technical+tol+aptitude+general,data=perfInd,nbest = 10)
library(car)
subsets(bestmodel,statistic = "rsq")
#applicable only for linear models
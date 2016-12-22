setwd("F:/PGDDS/PGDDS DAY 19 ACTIVITY DATA SETS")

bankloan <- read.csv("BANK LOAN.csv")
install.packages("ROCR")
library(ROCR)
bankloan$predprob <- fitted(riskModel)
pred <- prediction()

perf
###INCOMPLETE
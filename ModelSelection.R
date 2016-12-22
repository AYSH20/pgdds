setwd("F:/PGDDS/PGDDS DAY 17 ACTIVITY DATA SETS")
perfInd
null <- lm(jpi~1,data = perfInd)
full <- lm(jpi~aptitude+tol+technical+general,data = perfInd)
step(null,scope = list(lower=null, upper=full),direction = "forward")
step(full,scope = list(lower=null,upper=full),direction = "backward")
step(full,scope = list(lower=null,upper=full),direction = "both")



setwd("F:/PGDDS/PGDDS DAY 26 ACTIVITY DATA SETS")
athletics <- read.csv("National Track Records For Men.csv")

athletics2 <- subset(athletics,select = c(-Country))
pc_athletics <- princomp(formula=~.,data = athletics2,cor=T)
summary(pc_athletics)
2.5740680^2/8
pc_athletics$loadings
#loadings gives A matrix values and the original data's variance,
#prop variance(1/8), and cum.  variance

#SCORES BASED ON PCA
athletics$performance <- pc_athletics$scores[,1]
athletics$performance2 <- pc_athletics$scores[,2]
head(athletics)
#CONC : HIGHER SCORE IMPLIES BETTER RECORD
#pc_athletics[]
plot(pc_athletics)
plot(athletics$performance)

text(athletics$performance,label=athletics$Country,col = "red",
     cex = 0.4)
plot(pc_athletics,type="lines")

ath1 <- head(athletics[order(-athletics$performance),],5)
ath2 <- head(athletics[order(athletics$performance),],5)

#VERIFY THAT PC'S ARE UNCORRELATED
round(cor(pc_athletics$score))

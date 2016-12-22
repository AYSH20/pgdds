setwd("F:/PGDDS/PGDDS DAY 26 ACTIVITY DATA SETS")
pcrdata <- read.csv("pcrdata.csv")
pred_sales <- lm(SALES~AD+PRO+SALEXP+ADPRE+PROPRE,data=pcrdata)
summary(pred_sales)
library(car)
vif(pred_sales)
pcrdata2 <- subset(pcrdata, select=c(-SRNO,-SALES))
round(cor(pcrdata2))
#step 2 : PCA to know ncomp

pca_model <-princomp(formula=~AD+PRO+SALEXP+ADPRE+PROPRE,data=pcrdata2,cor=T)
summary(pca_model)

#step 3 : PCR
install.packages("pls")
library(pls)
pcmodel <- pcr(SALES~AD+PRO+SALEXP+ADPRE+PROPRE,ncomp=3,
               data=pcrdata,scale=T)
#scale=T it will use correlation matrix, independent of scale,  
#ncomp = 3 is decided using PCA
pcrdata$pred1 <-predict(pred_sales,pcrdata)
pcrdata$pred2 <- predict(pcmodel,pcrdata,ncomp=3)
head(pcrdata)

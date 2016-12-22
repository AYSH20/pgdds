setwd("F:/PGDDS/PGDDS DAY14 ACTIVITY DATA SETS")
vehicle <- read.csv("Motor Insurance claim amount.csv")
pairs(~., data = vehicle)
attach(vehicle)
model_vehicle <- lm(claimamt~., data = vehicle)
summary(model_vehicle)

library(car)
vif(model_vehicle)

model_vehicle2 <- lm(claimamt~vehage+Length+CC,data = vehicle)
summary(model_vehicle2)

vehicle$predicted_claim <- fitted(model_vehicle2)
vehicle$residuals_claim <- residuals(model_vehicle2)

qqnorm(vehicle$residuals_claim)
qqline(vehicle$residuals_claim,col="red")

shapiro.test(vehicle$residuals_claim)
lillie.test(vehicle$residuals_claim)

setwd("F:/PGDDS/PGDDS DAY13 ACTIVITY DATA SETS")
per_index <- read.csv("Performance index.csv")
attach(per_index)
mod_jp <-lm(jpi~aptitude+tol+technical+general, data = per_index)
summary(mod_jp)
mod_jp1 <- lm(jpi~aptitude+technical+general, data = per_index)
summary(mod_jp1)
mod_jp2 <- lm(jpi~aptitude, data = per_index)
summary(mod_jp2)

#estimate of y
jpivalue <- -54.40644+0.33335*52+1.11663*50+0.54316*45
jpivalue

a=43.83
t=51.82
g=43.58

predicted_jpi <- -54.40644+0.33335*a+1.11663*t+0.54316*g
predicted_jpi

err = predicted_jpi-per_index$jpi[1]
err


#adding predicted values and residuals

per_index$predicted_jpi <- 
  fitted(mod_jp1) #gives y cap of original data
per_index$residual_jpi<-residuals(mod_jp1)
#

per_index_new <- read.csv("Performance index new.csv")

predict(mod_jp1)
#for new data using old data prediction
per_index_new$pred_jpi<-predict(mod_jp1,per_index_new)
#residuals = observed-predicted
per_index_new$residual_jpi <- per_index_new$jpi-per_index_new$pred_jpi

#confidence intervals for predicted values

#predict(mod_jp1,per_index_new, interval = "new") ???

install.packages("car")
library(car)
vif(mod_jp1)

plot(per_index$predicted_jpi,per_index$residual_jpi,col="blue")
#ideally we want random pattern

# so now check for normality of errors
#if normal our errors are normal

library(nortest)
version
shapiro.test(per_index$residual_jpi)
lillie.test(per_index$residual_jpi)


###UP TILL NOW ALL DEPENDENT VARS ARE CONTINUOUS
#NOW CATEGORICAL

restaurant_data <-read.csv("RESTAURANT SALES DATA.csv")
attach(restaurant_data)
str(restaurant_data)
summary(restaurant_data)
restaurant_data$Dh <- ifelse(LOCATION=="Highway")

model_restaurants <- lm(SALES~LOCATION+NOH,data = restaurant_data)
summary(model_restaurants)
#highway is taken as base, .:highway & street has no significance thus changing to from highway to 
#mall no value vice versa for mall n highway
#also estimates are  +ves : for malls are better sales

model.matrix(model_restaurants)


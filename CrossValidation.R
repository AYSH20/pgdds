setwd("F:/PGDDS/PGDDS DAY 17 ACTIVITY DATA SETS")
motor <- read.csv("Motor Insurance claim amount.csv")
head(motor,n=10)
library(caret)
install.packages("caret")
index <- createDataPartition(motor$claimamt,p=0.8,list=F)
traindata <- motor[index,]
testdata <- motor[-index,]
install.packages("stringi")
library(stringi)
library(ggplot2)
install.packages("lattice")
library(lattice)
dim(traindata)
dim(testdata)

motor_model <- lm(claimamt~Length+CC+vehage,data=traindata)
traindata$resi <- residuals(motor_model)
head(motor_model)
RMSEtrain <- sqrt(mean(traindata$resi**2))
RMSEtrain

#test data using model
testdata$pred <- predict(motor_model,testdata)
head(testdata)
testdata$resi <- testdata$claimamt-testdata$pred
RMSEtest <- sqrt(mean(testdata$resi**2))
RMSEtest

#working with entire data
motor$pred <- predict(motor_model,motor)
motor$resi <- motor$claimamt-motor$pred
RMSEmotor



#K fold partition

kfolds <- trainControl(method = "cv", number = 4)
model11 <- train(claimamt~vehage+CC+Length,data = motor,method="lm",trControl=kfolds)
model11

#Repeated K fold CV

kfolds1 <- trainControl(method = "repeatedcv",number = 4,repeats = 5)
model12 <- train(claimamt~vehage+CC+Length,data = motor,method="lm", trControl=kfolds1)
model12

#LEAVE ONE OUT
kfolds2 <- trainControl(method="LOOCV")
model13<-train(claimamt~vehage+CC+Length,data=motor,method="lm",trControl=kfolds2)
model13

kfolds3 <- trainControl(method = "boot", number = 100)
model14<- train(claimamt~vehage+CC+Length,data=motor,method="lm",trControl=kfolds3)
model14

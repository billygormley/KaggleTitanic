#Script in order to implement a simple model for the Titanic competition
#Created: 12/28/2015
#Last Updated: 12/28/2015

#Set working directory "C:\Users\Billy\SkyDrive\Coursera and Codes\Kaggle\Titanic"
setwd("C:\\Users\\Billy\\SkyDrive\\Coursera and Codes\\Kaggle\\Titanic")
getwd()

#Import the training data for the competition
trainingDataFrame <- data.frame(read.csv("train.csv"))
trainingDataFrame <- na.omit(trainingDataFrame)
head(trainingDataFrame, 5)

z <- table(trainingDataFrame$Survived, trainingDataFrame$Gender)

hist3D(z = z, border = "black")
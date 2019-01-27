

setwd("~/Data/Titanic Data")

library(glmnet)
library(randomForest)
library(stringr)

#Data Import
train_data <- read.csv("train.csv", header = TRUE, na.strings=c("","NA"))
test_data <- read.csv("test.csv", header = TRUE)

#Data Cleaning
length(which(is.na(train_data$Age)))
length(which(is.na(train_data$Cabin)))

#Clean up Cabin
train_data$Cabin <- stringr::str_split_fixed(train_data$Cabin, " ", 2)[,1]
cabin_string <- str_split(train_data$Cabin, pattern = "")

for(i in seq(1:length(train_data$Cabin))){
  train_data$Cabin[i] <- cabin_string[[i]][1]
}


#Other Feature Creation
train_data$CabinIND <- ifelse(is.na(train_data$Cabin), 0, 1)

x <- train_data[,c(3,5,6,7,8,10,11,12,13)]
y <- train_data$Survived

#Model Building
randomForest(x = x, y = y, type = "classification", n_tree = 500)



################################
# Set Seed                     #
################################
 
set.seed(4316346276)
 
################################
# Libraries                    #
################################
 
library(mgcv)
library(randomForest)
library(stringr)
 
################################
# Loading in Data              #
################################
 
train.data.rf <- read.csv("train.csv", header = TRUE)
train.data.mgcv <- read.csv("train.csv", header = TRUE)
 
################################
# Data Cleaning                #
################################
 
# Create cabin_type: Note "Z" is for N/A
for(i in 1:length(train.data.rf$Cabin)){
          
  train.data.rf$cabin_type[i] <- str_split(train.data.rf$Cabin[i], "")[[1]][1]
 
  ifelse(is.na(train.data.rf$cabin_type[i]),
  train.data.rf$cabin_type[i] <- "Z",
  train.data.rf$cabin_type[i] <- train.data.rf$cabin_type[i])
}
 
# Create Title based off Name
for(j in 1:length(train.data.rf$Name)){
  train.data.rf$Title[j] <- str_split(train.data.rf$Name, c(","))[[j]][2]
  train.data.rf$Title[j] <- str_split(train.data.rf$Title, c(" "))[[j]][2]
}
 
train.data.rf$Title[760] <- "Countess."
 
# Impute Age with Means based on Title category
table(train.data.rf$Title[which(is.na(train.data.rf$Age))])
 
# Create DF of non-missing Ages
age.exists.df <- train.data.rf[-c(which(is.na(train.data.rf$Age))),]
 
# Get Avg Age based on Title
dr.avg.age <- mean(age.exists.df$Age[which(age.exists.df$Title == "Dr.")])
master.avg.age <- mean(age.exists.df$Age[which(age.exists.df$Title == "Master.")])
miss.avg.age <- mean(age.exists.df$Age[which(age.exists.df$Title == "Miss.")])
mr.avg.age <- mean(age.exists.df$Age[which(age.exists.df$Title == "Mr.")])
mrs.avg.age <- mean(age.exists.df$Age[which(age.exists.df$Title == "Mrs.")])
 
for (k in 1:length(train.data.rf$Age)){
  if (is.na(train.data.rf$Age[k])){
            if (train.data.rf$Title[k] == "Dr.") {
              train.data.rf$Age[k] <- dr.avg.age
            } else if (train.data.rf$Title[k] == "Master.") {
              train.data.rf$Age[k] <- master.avg.age
            } else if (train.data.rf$Title[k] == "Miss.") {
              train.data.rf$Age[k] <- miss.avg.age
            } else if (train.data.rf$Title[k] == "Mr.") {
              train.data.rf$Age[k] <- mr.avg.age
            } else if (train.data.rf$Title[k] == "Mrs.") {
              train.data.rf$Age[k] <- mrs.avg.age
            }
  }
}
 
# Impute missing Embarked, just assigning "S"
train_data$Embarked[c(62, 830)] <- "S"
 
rf.model.df <- train.data.rf[-c(1,2,4,9,11)]
rf.response <- as.factor(train.data.rf$Survived)
 
pca.model.df <- mgcv.model.df <- rf.model.df
mgcv.response <- as.factor(train.data.mgcv$Survived)
 
 
#Convert to Factors
rf.model.df$Pclass <- as.factor(rf.model.df$Pclass)
rf.model.df$Sex <- as.factor(rf.model.df$Sex)
rf.model.df$Embarked <- as.factor(rf.model.df$Embarked)
rf.model.df$cabin_type <- as.factor(rf.model.df$cabin_type)
rf.model.df$Title <- as.factor(rf.model.df$Title)
 
 
################################
# Building MGCV Models         #
################################
 
#Set Number of splines (k) to 23
k <- 31
 
mgcv.gam.model.Smaller <- mgcv::gam(as.factor(Survived) ~
                                    Pclass + Sex + Age + SibSp,
                                    data = train.data.mgcv,
                                    family = binomial(link = logit),
                                    select = TRUE)
 
mgcv.gam.model.1 <- mgcv::gam(as.factor(Survived) ~
                                 Pclass + Sex + Age + SibSp + Parch +
                                 Fare + mgcv.model.df$cabin_type +
                                 Pclass*Fare + Sex*Age,
                                 data = train.data.mgcv,
                                 family = binomial(link = logit))
 
 
 
####################################
# Building Gradient Boosting Models#
####################################
 
 
 
 
 
 
####################################
# Building Random Forest Models    #
####################################
 
 
 
#######################################
# Building Principal Components Models#
#######################################
 
pca.model.df <- pca.model.df[, c(3,6)]
 
train.data.pca <- prcomp(pca.model.df, center = TRUE, scale. = TRUE)
 

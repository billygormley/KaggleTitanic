
library(randomForest)
library(stringr)
library(mgcv)

set.seed(123)

k <- 23
num_trees <- 5001

train_data <- read.csv("train.csv", header = TRUE, na.strings = c("NA", ""))
test_data <- read.csv("test.csv", header = TRUE, na.strings = c("NA", ""))


#Create 2 Cabin variables
for(i in 1:length(train_data$Cabin)){
  train_data$cabin_type[i] <- str_split(train_data$Cabin[i], "")[[1]][1]
  ifelse(is.na(train_data$cabin_type[i]), train_data$cabin_type[i] <- "Z", train_data$cabin_type[i] <- train_data$cabin_type[i])
}

#Create Title
for(j in 1:length(train_data$Name)){
  train_data$Title[j] <- str_split(train_data$Name, c(","))[[j]][2]
  train_data$Title[j] <- str_split(train_data$Title, c(" "))[[j]][2]
}

train_data$Title[760] <- "Countess."

#Impute Age with Normal Random draws based on Title category

table(train_data$Title[which(is.na(train_data$Age))])

#Create DF of non-missing Ages
age_exists_df <- train_data[-c(which(is.na(train_data$Age))),]

#Get Avg Age based on Title
dr_avg_age <- mean(age_exists_df$Age[which(age_exists_df$Title == "Dr.")])
master_avg_age <- mean(age_exists_df$Age[which(age_exists_df$Title == "Master.")])
miss_avg_age <- mean(age_exists_df$Age[which(age_exists_df$Title == "Miss.")])
mr_avg_age <- mean(age_exists_df$Age[which(age_exists_df$Title == "Mr.")])
mrs_avg_age <- mean(age_exists_df$Age[which(age_exists_df$Title == "Mrs.")])

for(k in 1:length(train_data$Age)){
  if (is.na(train_data$Age[k])){
    if (train_data$Title[k] == "Dr.") {train_data$Age[k] <- dr_avg_age}
    else if (train_data$Title[k] == "Master.") {train_data$Age[k] <- master_avg_age}
    else if (train_data$Title[k] == "Miss.") {train_data$Age[k] <- miss_avg_age}
    else if (train_data$Title[k] == "Mr.") {train_data$Age[k] <- mr_avg_age}
    else if (train_data$Title[k] == "Mrs.") {train_data$Age[k] <- mrs_avg_age}
  }
}

#Impute missing Embarked
train_data$Embarked[c(62, 830)] <- "S"

x <- train_data[-c(1,2,4,9,11)]
y <- as.factor(train_data$Survived)


#Convert all Strings to Factors
x$Pclass <- as.factor(x$Pclass)
x$Sex <- as.factor(x$Sex)
x$Embarked <- as.factor(x$Embarked)
x$cabin_type <- as.factor(x$cabin_type)
x$Title <- as.factor(x$Title)


#Build random forest
rf_model <- randomForest(x = x, y = y,
                         ntree = num_trees,
                         replace = TRUE,
                         importance = TRUE)

#Build Semiparametric Logistic Rregression Model
semilr_model <- gam(factor(Survived) ~ factor(Pclass) + factor(Sex) + s(Age, k = k) + SibSp +
                      Parch + s(Fare, k = k) + factor(Embarked) + factor(cabin_type) + factor(Title),
                    family = binomial(link = logit),
                    data = train_data)


#Predict the Test Data

#Create 2 Cabin variables
for(i in 1:length(test_data$Cabin)){
  test_data$cabin_type[i] <- str_split(test_data$Cabin[i], "")[[1]][1]
  ifelse(is.na(test_data$cabin_type[i]), test_data$cabin_type[i] <- "Z",
         test_data$cabin_type[i] <- test_data$cabin_type[i])
}

#Create Title
for(j in 1:length(test_data$Name)){
  test_data$Title[j] <- str_split(test_data$Name, c(","))[[j]][2]
  test_data$Title[j] <- str_split(test_data$Title, c(" "))[[j]][2]
  ifelse(test_data$Title[j] == "Ms.", test_data$Title[j] <- "Miss.", test_data$Title[j] <- test_data$Title[j])
  ifelse(test_data$Title[j] == "Dona.", test_data$Title[j] <- "Mrs.", test_data$Title[j] <- test_data$Title[j])
}


age_exists_df_test <- test_data[-c(which(is.na(test_data$Age))),]

dr_avg_age_test <- mean(age_exists_df_test$Age[which(age_exists_df_test$Title == "Dr.")])
master_avg_age_test <- mean(age_exists_df_test$Age[which(age_exists_df_test$Title == "Master.")])
miss_avg_age_test <- mean(age_exists_df_test$Age[which(age_exists_df_test$Title == "Miss.")])
mr_avg_age_test <- mean(age_exists_df_test$Age[which(age_exists_df_test$Title == "Mr.")])
mrs_avg_age_test <- mean(age_exists_df_test$Age[which(age_exists_df_test$Title == "Mrs.")])

for(k in 1:length(test_data$Age)){
  if (is.na(test_data$Age[k])){
    if (test_data$Title[k] == "Dr.") {test_data$Age[k] <- dr_avg_age_test}
    else if (test_data$Title[k] == "Master.") {test_data$Age[k] <- master_avg_age_test}
    else if (test_data$Title[k] == "Miss.") {test_data$Age[k] <- miss_avg_age_test}
    else if (test_data$Title[k] == "Mr.") {test_data$Age[k] <- mr_avg_age_test}
    else if (test_data$Title[k] == "Mrs.") {test_data$Age[k] <- mrs_avg_age_test}
  }
}



fare_test <- test_data$Fare[test_data$cabin_type == "Z"]
which(is.na(fare_test))
test_data$Fare[153] <- mean(fare_test[-118])


x_test <- test_data[-c(1,3,8,10)]


#Convert all Strings to Factors
x_test$Pclass <- as.factor(x_test$Pclass)
x_test$Sex <- as.factor(x_test$Sex)
x_test$Embarked <- as.factor(x_test$Embarked)
x_test$cabin_type <- as.factor(x_test$cabin_type)
x_test$Title <- as.factor(x_test$Title)


levels(x_test$cabin_type) <- levels(x$cabin_type)
levels(x_test$Title) <- levels(x$Title)


rf_prediction <- predict(rf_model, newdata = x_test, type = "response")
semilr_prediction <- predict.gam(semilr_model, newdata = test_data, type = "response")

submission <- cbind(test_data$PassengerId, rf_prediction)

write.csv(submission, "submission_5001.csv")

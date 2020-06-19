#import libray
library(Hmisc)
library(DataExplorer)
library(skimr)
library(party)
library(Amelia)
library(ggplot2)
library(tidyverse)
library(naivebayes)
library(mlr)
library(caret)
library(e1071)
library(rpart)
library(rpart.plot)
library(tree)

#Load data set
data_dir <- "E:/UM/Master/Sem 1/WQD7001 PRINCIPLES OF DATA SCIENCE/Assignment/Group/References/CleanData.csv"
clean_df <- read.csv(data_dir)
clean_df$cough <- as.factor(clean_df$cough)
clean_df$fever <- as.factor(clean_df$fever)
clean_df$sore_throat <- as.factor(clean_df$sore_throat)
clean_df$shortness_of_breath <- as.factor(clean_df$shortness_of_breath)
clean_df$head_ache <- as.factor(clean_df$head_ache)
clean_df$age_60_and_above <- as.factor(clean_df$age_60_and_above)
clean_df$gender <- as.factor(clean_df$gender)
clean_df$Abroad <- as.factor(clean_df$Abroad)
clean_df$Contact <- as.factor(clean_df$Contact)
clean_df$No_activite <- as.factor(clean_df$No_activite)
clean_df$corona_result <- as.factor(clean_df$corona_result)

#-------------------Data Processing------------------
#------------Naive Bayes Classification--------------
NBModel <- naive_bayes(corona_result ~., data = clean_df)
#Prediction on the dataset
NB_Predictions=predict(NBModel,clean_df)
#Confusion matrix to check accuracy
table(NB_Predictions,clean_df$corona_result)
NB_Predictions
pred_acc <- (29059+11609)/(29059+11609+3020+4310)
#Prediction accuracy of this classification task = 0.8473
#F1 Score = 0.76

#use naive bayes to do training 
#First splitting data, training set and testing set
indxTrain <- createDataPartition(y = clean_df$corona_result, p=0.75, list = FALSE)
train_data <- clean_df[indxTrain,]
test_data <- clean_df[-indxTrain,]

prop.table(table(clean_df$corona_result)) * 100

x <- train_data[,-11]
y <- train_data$corona_result

model = caret::train(x,y,'nb',trControl=trainControl(method='cv',number=10))
#save(model,file="E:/UM/Master/Sem 1/WQD7001 PRINCIPLES OF DATA SCIENCE/Assignment/Group/Final/Model/NB.rda")
#model evaluation (test on testing data)
Predict <- predict(model,newdata = test_data)
confusionMatrix(Predict, test_data$corona_result)
#Accuracy : 0.8475

model

#Plot Variable performance
X <- varImp(model)
plot(X)

testing <- read.csv("C:/Users/hp/Downloads/try.csv")
testing$cough <- factor(testing$cough, levels = c(0,1), labels = c("0","1"))
testing$fever <- factor(testing$fever, levels = c(0,1), labels = c("0","1"))
testing$sore_throat <- factor(testing$sore_throat, levels = c(0,1), labels = c("0","1"))
testing$shortness_of_breath <- factor(testing$shortness_of_breath, levels = c(0,1), labels = c("0","1"))
testing$head_ache <- factor(testing$head_ache, levels = c(0,1), labels = c("0","1"))
testing$age_60_and_above <- factor(testing$age_60_and_above, levels = c(0,1), labels = c("0","1"))
testing$gender <- factor(testing$gender, levels = c(0,1), labels = c("0","1"))
testing$Abroad <- factor(testing$Abroad, levels = c(0,1), labels = c("0","1"))
testing$Contact <- factor(testing$Contact, levels = c(0,1), labels = c("0","1"))
testing$No_activite <- factor(testing$No_activite, levels = c(0,1), labels = c("0","1"))
Predict <- predict(model,newdata=testing)
Predict

#-------------Decision Tree Algorithm----------------
prop.table(table(train_data$corona_result))
prop.table(table(test_data$corona_result))
dim(train_data)
dim(test_data)

tree_de <- rpart(corona_result~.,data=train_data, method = 'class')
rpart.plot(tree_de, extra= 106)

#Prediction on test data
model_tree <- predict(tree_de, test_data, type = "class")
confusionMatrix(model_tree, test_data$corona_result)
#Accuracy : 0.8817


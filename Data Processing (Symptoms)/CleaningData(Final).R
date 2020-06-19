#remove all variables stored previously
rm(list=ls())

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
#library(dplyr)

#Read Data set
data_dir <- "E:/UM/Master/Sem 1/WQD7001 PRINCIPLES OF DATA SCIENCE/Assignment/Group/References/corona_tested_individuals_ver_0013.csv"
df <- read.csv(data_dir)

#------------------------Data Description---------------------------------
describe(df)

#summary of the data set
describe(df)
head(df)
introduce(df)
plot_intro(df)

#Check the shape of the data
dim(df)
cat("Sample data:", nrow(df), "\nFeatures:", ncol(df))

# Check the structure of the data
str(df)

# Check all the feature name of the data
names(df)

#skim() is an alternative to summary(), quickly providing a broad overview of a data frame.
skim(df)

#Check the simple descriptive statistics of the data
summary(df)

# Check the datatype of each feature
table(sapply(df,class))

#------------------------Data Cleaning--------------------------------

#Dealing with missing data
#Obviously the loaded CSV in under UTF-8 with BOM and causing 1st column begin with Byte Order Mark (BOM)
names(df)[names(df) == "ï..test_date"] <- "test_date"

# since they make no meaning (all of the data in these variable are same or by order only)
df$test_date<-NULL

# Check whether any missing value in the dataset
sum(is.na(df))
sum(is.null(df))
sum(df$cough=="NULL")
sum(df$fever=="NULL")
sum(df$sore_throat=="NULL")
sum(df$shortness_of_breath=="NULL")
sum(df$head_ache=="NULL")
sum(df$corona_result=="NULL")
sum(df$age_60_and_above=="NULL")
sum(df$gender=="NULL")
sum(df$test_indication=="NULL")

#check the percentage of corona result = others
num_result_other <- as.integer(sum(df$corona_result=="Other"))
total_result <- as.integer(length(df$corona_result))
percent_result_other <- num_result_other/total_result
#percent_result_other only 0.01787 comparing to positive and negative results. Therefore, we can remove the data
df <- df[!df$corona_result=="Other",]
df$corona_result <- droplevels(df$corona_result)

#For those no symptoms and corona result as negative remove as well
sum(df$cough=="0"&df$fever=="0"&df$sore_throat=="0"&df$shortness_of_breath=="0"&df$head_ache=="0"&df$corona_result=="Negative")
df <- df[!(df$cough=="0"&df$fever=="0"&df$sore_throat=="0"&df$shortness_of_breath=="0"&df$head_ache=="0"&df$corona_result=="Negative"),]

#for corona result = negative, and symptoms results is NULL, we use mode to replace those NULL value
sum(df$cough=="NULL"&df$corona_result=="Negative")
sum(df$fever=="NULL"&df$corona_result=="Negative")
df$cough <- as.character(df$cough)
df$cough[df$cough == "NULL"&df$corona_result=="Negative"] <- "1"
df$cough <- as.factor(df$cough)
df$fever <- as.character(df$fever)
df$fever[df$fever == "NULL"&df$corona_result=="Negative"] <- "0"
df$fever <- as.factor(df$fever)

#check the symptoms = NULL but corona results = positive
#check the percentage of symptoms = NULL but corona results = positive
#cough
sum(df$cough=="1"&df$corona_result=="Positive")
sum(df$cough=="0"&df$corona_result=="Positive")
#using mode to fill in missing value 
df$cough <- as.character(df$cough)
df$cough[df$cough=="NULL"&df$corona_result=="Positive"] <- "0"
df$cough <- as.factor(df$cough)

#fever
sum(df$fever=="1"&df$corona_result=="Positive")
sum(df$fever=="0"&df$corona_result=="Positive")
#using mode to fill in missing value 
df$fever <- as.character(df$fever)
df$fever[df$fever=="NULL"&df$corona_result=="Positive"] <- "0"
df$fever <- as.factor(df$fever)

#sore_throat
sum(df$sore_throat=="1"&df$corona_result=="Positive")
sum(df$sore_throat=="0"&df$corona_result=="Positive")
#using mode to fill in missing value 
df$sore_throat <- as.character(df$sore_throat)
df$sore_throat[df$sore_throat=="NULL"&df$corona_result=="Positive"] <- "0"
df$sore_throat <- as.factor(df$sore_throat)

#shortness_of_breath
sum(df$shortness_of_breath=="1"&df$corona_result=="Positive")
sum(df$shortness_of_breath=="0"&df$corona_result=="Positive")
#using mode to fill in missing value 
df$shortness_of_breath <- as.character(df$shortness_of_breath)
df$shortness_of_breath[df$shortness_of_breath=="NULL"&df$corona_result=="Positive"] <- "0"
df$shortness_of_breath <- as.factor(df$shortness_of_breath)

#head_ache
sum(df$head_ache=="1"&df$corona_result=="Positive")
sum(df$head_ache=="0"&df$corona_result=="Positive")
#using mode to fill in missing value 
df$head_ache <- as.character(df$head_ache)
df$head_ache[df$head_ache=="NULL"&df$corona_result=="Positive"] <- "0"
df$head_ache <- as.factor(df$head_ache)

#ages
sum(df$age_60_and_above=="Yes"&df$corona_result=="Positive")
sum(df$age_60_and_above=="No"&df$corona_result=="Positive")
sum(df$age_60_and_above=="Yes"&df$corona_result=="Negative")
sum(df$age_60_and_above=="No"&df$corona_result=="Negative")
df$age_60_and_above <- as.character(df$age_60_and_above)
df$age_60_and_above[df$age_60_and_above=="NULL"&df$corona_result=="Positive"] <- "No"
df$age_60_and_above[df$age_60_and_above=="NULL"&df$corona_result=="Negative"] <- "No"
#change age to above 60 = 1, below =0
df$age_60_and_above[df$age_60_and_above=="Yes"] <- "1"
df$age_60_and_above[df$age_60_and_above=="No"] <- "0"
df$age_60_and_above <- as.factor(df$age_60_and_above)

#gender
sum(df$gender=="female"&df$corona_result=="Positive")
sum(df$gender=="male"&df$corona_result=="Positive")
sum(df$gender=="female"&df$corona_result=="Negative")
sum(df$gender=="male"&df$corona_result=="Negative")
df$gender <- as.character(df$gender)
df$gender[df$gender=="NULL"&df$corona_result=="Positive"] <- "male"
df$gender[df$gender=="NULL"&df$corona_result=="Negative"] <- "male"
#change gender to female = 0, male = 1
df$gender[df$gender=="male"] <- "1"
df$gender[df$gender=="female"] <- "0"
df$gender <- as.factor(df$gender)

#change corona results to Positive = 1, negative = 0
df$corona_result <- as.character(df$corona_result)
df$corona_result[df$corona_result=="Positive"] <- "1"
df$corona_result[df$corona_result=="Negative"] <- "0"
df$corona_result <- as.factor(df$corona_result)

#test_indication separate out into several column (Abroad, Contact, No_activites)
clean_df <- df
New_Col <- c("Abroad","Contact","No_activite")
clean_df[, New_Col] <- NA
#Abroad is the patient have been travel within 14 days
for (N in 1:nrow(clean_df)){
  if(clean_df$test_indication[N]=='Abroad'){
    clean_df[N,10]=1
  }else{
    clean_df[N,10]=0
  }
}
clean_df$Abroad <- as.factor(clean_df$Abroad)

#Contact is the patient have contact with covid patient before
for (N in 1:nrow(clean_df)){
  if(clean_df$test_indication[N]=='Contact with confirmed'){
    clean_df[N,11]=1
  }else{
    clean_df[N,11]=0
  }
}
clean_df$Contact <- as.factor(clean_df$Contact)

#No_activite is no interaction or other activite 
for (N in 1:nrow(clean_df)){
  if(clean_df$test_indication[N]=='Other'){
    clean_df[N,12]=1
  }else{
    clean_df[N,12]=0
  }
}
clean_df$No_activite <- as.factor(clean_df$No_activite)

#remove test_indication column
clean_df$test_indication <- NULL

#move the corona result to last column
colnames(clean_df)
clean_df <- clean_df[,c(1,2,3,4,5,7,8,9,10,11,6)]


#Generate clean csv file
write.csv(clean_df, file = "E:/UM/Master/Sem 1/WQD7001 PRINCIPLES OF DATA SCIENCE/Assignment/Group/References/CleanData.csv",  row.names=FALSE)

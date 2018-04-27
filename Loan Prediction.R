
#set your working directory
setwd("D:/Users/1015624/GE Internal/My Docs/Work/LoanPrediction")

#load required libraries
library(dplyr)
library(ggplot2)
library(reshape2)
library(plyr)
library(stringr)
library(randomForest)

#importing datasets
train <- read.csv("Loan Prediction_train.csv", stringsAsFactors = FALSE)
test <- read.csv("Loan Prediction_test.csv", stringsAsFactors = FALSE)
prop.table(table(train$Loan_Status))

#combine both datasets so that pre-processing is applied on both of them
test$Loan_Status <- NaN
loan_dat <- rbind(train,test)

summary(loan_dat)
#missing values present in Gender, Married, Dependents, Self_Employed, LoanAmount, 
#Loan_Amount_Term, Credit_History

#we have only 3 missing values for Married.
#filling these as 'No' looking at their other attributes
loan_dat$Married[loan_dat$Married == ""] <- "No"

#imputing missing values for Gender
#imputing Gender based on marital status and loan_amount
melten <- melt(loan_dat[, c('Gender','Married','LoanAmount')], id = c('Gender', 'Married'))
dcast(melten, Gender ~ Married , mean, na.rm = TRUE)

loan_dat$Gender <- ifelse(is.na(loan_dat$Gender) & loan_dat$Married == 'No' & loan_dat$LoanAmount <= )


str(loan_dat)

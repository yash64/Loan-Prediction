#set your working directory
setwd("E:/My Work/Dataset/Loan Prediction-III")

#load required libraries
library(dplyr)
library(ggplot2)
library(reshape2)
library(plyr)
library(stringr)
library(randomForest)

#importing datasets
train <- read.csv("train.csv", stringsAsFactors = FALSE)
test <- read.csv("test.csv", stringsAsFactors = FALSE)
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

##imputing missing values for Gender
#imputing Gender based on marital status and loan_amount
melten <- melt(loan_dat[, c('Gender','Married','LoanAmount')], id = c('Gender', 'Married'))
dcast(melten, Gender ~ Married , mean, na.rm = TRUE)

loan_dat$Gender <- ifelse(loan_dat$Gender == "" & loan_dat$Married == 'No' & loan_dat$LoanAmount <= 131, "Female",
                          ifelse(loan_dat$Gender == "" & loan_dat$Married == 'No' & loan_dat$LoanAmount > 131, "Male",
                                 ifelse(loan_dat$Gender == "" & loan_dat$Married == 'Yes' & loan_dat$LoanAmount <= 150, "Female",
                                        ifelse(loan_dat$Gender == "" & loan_dat$Married == 'Yes' & loan_dat$LoanAmount > 150, "Male",
                                               loan_dat$Gender))))

##imputing dependents column
#we have values 0,1,2,3+ as dependents. we shall change 3+ to 3
loan_dat$Dependents[loan_dat$Dependents == "3+"] <- 3
#generally dependents can be based on marital status of a person
#we shall check if this can give us any idea 
addmargins(table(loan_dat$Married, loan_dat$Dependents))
#so Married attribute doesnt provide us enough info to impute Dependents column
#we shall check with Gender and loan amount
addmargins(table(loan_dat$Gender, loan_dat$Dependents))
prop.table(table(loan_dat$Gender, loan_dat$Dependents),1)
loan_dat %>% group_by(Dependents) %>% 
  dplyr::summarise(Loan_amount = mean(LoanAmount, na.rm = TRUE), App_income = mean(ApplicantIncome, na.rm = TRUE), CoApp_income = mean(CoapplicantIncome, na.rm = TRUE))

#imputing dependents as 0 based on above summary
loan_dat$Dependents[loan_dat$Dependents == ""] <- 0

##missing values for self_employed

























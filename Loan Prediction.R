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

loan_dat$Self_Employed <- ifelse(loan_dat$Self_Employed == "" & loan_dat$Gender == "Female" & loan_dat$ApplicantIncome <= 8600,"No",
                                 ifelse(loan_dat$Self_Employed == "" & loan_dat$Gender == "Female" & loan_dat$ApplicantIncome > 8600, "Yes",
                                        ifelse(loan_dat$Self_Employed == "" & loan_dat$Gender == "Male" & loan_dat$ApplicantIncome <= 6700, "No",
                                               ifelse(loan_dat$Self_Employed == "" & loan_dat$Gender == "Male" & loan_dat$ApplicantIncome > 6700, "Yes",
                                                      loan_dat$Self_Employed))))

##filling missing values for loan_amount
#melten <- melt(loan_dat[,c('Gender','Education','LoanAmount')], id = c("Gender","Education") )
#casting <- dcast(melten, Gender ~ Education, mean, na.rm = TRUE)
#loan_dat %>% group_by(Gender, Education) %>% dplyr::summarise(mean = mean(LoanAmount, na.rm = TRUE))

mean_loanamount <- ddply(na.omit(loan_dat), ~Gender + Education, summarise, mean= round(mean(LoanAmount)))
loan_dat$LoanAmount <- ifelse(is.na(loan_dat$LoanAmount) & loan_dat$Gender == "Female" & loan_dat$Education == 'Graduate', with(mean_loanamount, mean[Gender == 'Female' & Education == "Graduate"]),
                              ifelse(is.na(loan_dat$LoanAmount) & loan_dat$Gender == "Female" & loan_dat$Education == 'Not Graduate', with(mean_loanamount, mean[Gender == 'Female' & Education == "Not Graduate"]),
                                     ifelse(is.na(loan_dat$LoanAmount) & loan_dat$Gender == "Male" & loan_dat$Education == 'Graduate', with(mean_loanamount, mean[Gender == 'Male' & Education == "Graduate"]),
                                            ifelse(is.na(loan_dat$LoanAmount) & loan_dat$Gender == "Male" & loan_dat$Education == 'Not Graduate', with(mean_loanamount, mean[Gender == 'Male' & Education == "Not Graduate"]),
                                                   loan_dat$LoanAmount))))
##missing values for loan term
#create new variable income by summing applicantincome and coapplicant income
loan_dat$Income <- loan_dat$ApplicantIncome + loan_dat$CoapplicantIncome
loan_term <- loan_dat %>% group_by(Loan_Amount_Term) %>% 
  dplyr::summarise(Avg_LoanAmt = mean(LoanAmount), Avg_Income = mean(Income))
ggplot(loan_term, aes(x = factor(Loan_Amount_Term), y = Avg_LoanAmt)) +
  geom_bar(stat = 'identity') + labs(title = "Average Loan Amount by Loan term", x = "Loan Term", y = "Average Loan Amount")

ggplot(loan_term, aes(x = factor(Loan_Amount_Term), y = Avg_Income)) + 
  geom_bar(stat = 'identity') + labs(title = "Average Income by Loan term", x = "Loan Term", y = "Average Income")
#the plots are giving us a good picture as how the loan term is distributed based on income and loan amount
#but looking at the distribution of the customers per term
table(loan_dat$Loan_Amount_Term)
ggplot(loan_dat, aes(factor(Loan_Amount_Term))) + geom_bar() + labs(x = "Loan Term")
#we can fill the missing loan term as 360 as ~86% of the total data has loan term as 360.
loan_dat$Loan_Amount_Term <- ifelse(is.na(loan_dat$Loan_Amount_Term), 360, loan_dat$Loan_Amount_Term)

##Credit history missing values
table(loan_dat$Credit_History)
ggplot(loan_dat, aes(factor(Credit_History))) + geom_bar() + labs(x = "Credit History")

#looking at the pattern of credit history with others features.
ggplot(loan_dat, aes(x = factor(Credit_History))) + geom_bar(aes(fill = Gender), width = 0.5)
ggplot(loan_dat, aes(x = factor(Credit_History))) + geom_bar(aes(fill = Married), width = 0.5)
ggplot(loan_dat, aes(x = factor(Credit_History))) + geom_bar(aes(fill = Dependents), width = 0.5)
ggplot(loan_dat, aes(x = factor(Credit_History))) + geom_bar(aes(fill = Education), width = 0.5)
ggplot(loan_dat, aes(x = factor(Credit_History))) + geom_bar(aes(fill = Self_Employed), width = 0.5)

loan_dat$Credit_History[is.na(loan_dat$Credit_History)] <- 1

#we are done with imputing missing values

###Exploratory data analysis###
#as we created total income we can delete the columns app_income, coapp_income
loan_dat$ApplicantIncome <- NULL
loan_dat$CoapplicantIncome <- NULL

ggplot(train) + geom_point(aes(Loan_Status))






















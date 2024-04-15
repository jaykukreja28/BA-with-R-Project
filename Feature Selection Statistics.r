#Result of Associations
#Cbal: Checking account balance | Strong Association
  #Reult: X-squared = 123.72, df = 3, p-value < 2.2e-16
#Cdur: Credit duration | Strong Association
  #Result: X-squared = 78.887, df = 32, p-value = 7.785e-06
#Chist: Credit history | Strong Association
  #Result: X-squared = 61.691, df = 3, p-value = 2.558e-13
#Cpur: Purpose of the loan | Strong Association
  #Result: X-squared = 33.356, df = 9, p-value = 0.000115  
#Camt: Loan Amount | Weak Association
  #Result: X-squared = 931.75, df = 920, p-value = 0.386
#Sbal: Savings Balance | Strong Association
  #Result: X-squared = 36.099, df = 4, p-value = 2.761e-07
#Edur : Employment Duration | Strong Association
  #Results: X-squared = 18.368, df = 4, p-value = 0.001045
#InRate : Installment Rate | Weak Association
  #Result: X-squared = 5.4768, df = 3, p-value = 0.14
#MSG | Marital Status and Gender | 0.02 p val almost Strong
  #Result: X-squared = 9.6052, df = 3, p-value = 0.02224
#Column 10 - Oparties | Other Parties Involved | Strong Association
  #Results : X-squared = 6.6454, df = 2, p-value = 0.03606
#Column 11 - Rdur | Residence Duration | WEAK ASSOCIATION
  #Results: X-squared = 0.7493, df = 3, p-value = 0.8616
#Column 12 - Prop | Property Type | Strong Association
  #Result: X-squared = 23.72, df = 3, p-value = 2.858e-05
#Column 13 - Age | Age | Strong Association
  #Results: X-squared = 57.477, df = 52, p-value = 0.2795
#Column 14 - inPlans | Existing Plans | Strong Association
  #Result: X-squared = 12.839, df = 2, p-value = 0.001629
#Column 15 - HType | Housing Type | Strong Association
  #Results: X-squared = 18.2, df = 2, p-value = 0.0001117
#Column 16 - NumCred | Number of existing credits | KIND OF WEAK ASSOCIATION
  #Results: X-squared = 2.6712, df = 3, p-value = 0.4451
#Column 17 - JobType | Job Type | WEAK ASSOCIATION
  #Result: X-squared = 1.8852, df = 3, p-value = 0.5966
#Column 18 - Ndepend | Number of Dependents | WEAK ASSOCIATION
  #Results: X-squared = 0, df = 1, p-value = 1
#Column 19 - telephone | Presence of a telephone | Strong Association
  #Results: X-squared = 1.1726, df = 1, p-value = 0.2789
#Column 20 - foreign | Foreign worker status | Strong Association
  #Results: X-squared = 5.8216, df = 1, p-value = 0.01583




#Stat Check for features in the dataset

# Clear the workspace
rm(list=ls())
cat("\014")

#Feature 1: Cbal - Checking Account Balance
# Load required package
install.packages("MASS")
library(MASS)


#packages
install.packages("readxl")
library(readxl)

#Set Working Directory
setwd("C:/Users/ABC/Desktop/BA with R - Spring 2024/BA with R Project/PROJECT")

# load in the data file into data.frame
creditDf <- read_excel("CreditWorthiness.xlsx")

# Column 1 - Cbal | Checking Account Balance
# Perform Chi-Square Test for Independence for the first column against the target variable
chisq_test <- chisq.test(table(creditDf$Cbal, creditDf$creditScore))
# Print the results
print(chisq_test)
#Reult: X-squared = 123.72, df = 3, p-value < 2.2e-16
#Therefore Strong Association



#Column 2 - Cdur | Credit Duration
# Perform Chi-Square Test for Independence for the second column against the target variable
chisq_test <- chisq.test(table(creditDf$Cdur, creditDf$creditScore))
# Print the results
print(chisq_test)
#Result: X-squared = 78.887, df = 32, p-value = 7.785e-06
#Strong Association


#Column 3 - Chist | Credit history
# Perform Chi-Square Test for Independence for the third column against the target variable
chisq_test <- chisq.test(table(creditDf$Chist, creditDf$creditScore))
# Print the results
print(chisq_test)
# Result: X-squared = 61.691, df = 3, p-value = 2.558e-13
#Strong Association


#Column 4 - Cpur | Purpose of the Loan
# Perform Chi-Square Test for Independence for the fourth column against the target variable
chisq_test <- chisq.test(table(creditDf$Cpur, creditDf$creditScore))
# Print the results
print(chisq_test)
#Result: X-squared = 33.356, df = 9, p-value = 0.000115
# Significant Association but p-value is more as compared to other variables


#Coumn 5 - Camt | Loan Amount
# Perform Chi-Square Test for Independence for the 'Camt' column against the target variable
chisq_test <- chisq.test(table(creditDf$Camt, creditDf$creditScore))
# Print the results
print(chisq_test)
#Result: X-squared = 931.75, df = 920, p-value = 0.386
# No Significant association since p-value > 0.05



#Column 6 - Sbal | Savings Balance Amount
# Perform Chi-Square Test for Independence for the 'Sbal' column against the target variable
chisq_test <- chisq.test(table(creditDf$Sbal, creditDf$creditScore))
# Print the results
print(chisq_test)
#Result: X-squared = 36.099, df = 4, p-value = 2.761e-07
# Significant Strong Association

#Column 7 - Edur | Employment duration
# Perform Chi-Square Test for Independence for the 'Edur' column against the target variable
chisq_test <- chisq.test(table(creditDf$Edur, creditDf$creditScore))
# Print the results
print(chisq_test)
#Results: X-squared = 18.368, df = 4, p-value = 0.001045
# Significant Strong Association


#Column 8 - InRate | Installment Rate
# Perform Chi-Square Test for Independence for the 'InRate' column against the target variable
chisq_test <- chisq.test(table(creditDf$InRate, creditDf$creditScore))
# Print the results
print(chisq_test)
#Result: X-squared = 5.4768, df = 3, p-value = 0.14
#Weak Association


#Column 9 - MSG | Marital Status and Gender
# Perform Chi-Square Test for Independence for the 'MSG' column against the target variable
chisq_test <- chisq.test(table(creditDf$MSG, creditDf$creditScore))
# Print the results
print(chisq_test)
#Result: X-squared = 9.6052, df = 3, p-value = 0.02224
#Almost Strong Association compared to other columns

#Column 10 - Oparties | Other Parties Involved
# Perform Chi-Square Test for Independence for the 'Oparties' column against the target variable
chisq_test <- chisq.test(table(creditDf$Oparties, creditDf$creditScore))
# Print the results
print(chisq_test)
#Results : X-squared = 6.6454, df = 2, p-value = 0.03606
# Strong Association



#Column 11 - Rdur | Residence Duration
# Perform Chi-Square Test for Independence for the 'Rdur' column against the target variable
chisq_test <- chisq.test(table(creditDf$Rdur, creditDf$creditScore))
# Print the results
print(chisq_test)
#Results: X-squared = 0.7493, df = 3, p-value = 0.8616
#Weak Association


#Column 12 - Prop | Property Type
# Perform Chi-Square Test for Independence for the 'Prop' column against the target variable
chisq_test <- chisq.test(table(creditDf$Prop, creditDf$creditScore))
# Print the results
print(chisq_test)
#Result: X-squared = 23.72, df = 3, p-value = 2.858e-05
#Strong Association



#Column 13 - Age | Age
# Perform Chi-Square Test for Independence for the 'Age' column against the target variable
chisq_test <- chisq.test(table(creditDf$age, creditDf$creditScore))
# Print the results
print(chisq_test)
#Results: X-squared = 57.477, df = 52, p-value = 0.2795
# Weak Association




#Column 14 - inPlans | Existing Plans
# Perform Chi-Square Test for Independence for the 'inPlans' column against the target variable
chisq_test <- chisq.test(table(creditDf$inPlans, creditDf$creditScore))
# Print the results
print(chisq_test)
#Result: X-squared = 12.839, df = 2, p-value = 0.001629
# Strong Association



#Column 15 - HType | Housing Type
# Perform Chi-Square Test for Independence for the 'Htype' column against the target variable
chisq_test <- chisq.test(table(creditDf$Htype, creditDf$creditScore))
# Print the results
print(chisq_test)
# Results: X-squared = 18.2, df = 2, p-value = 0.0001117


#Column 16 - NumCred | Number of existing credits
# Perform Chi-Square Test for Independence for the 'NumCred' column against the target variable
chisq_test <- chisq.test(table(creditDf$NumCred, creditDf$creditScore))
# Print the results
print(chisq_test)
# Results: X-squared = 2.6712, df = 3, p-value = 0.4451
# Weak Association



#Column 17 - JobType | Job Type
# Perform Chi-Square Test for Independence for the 'JobType' column against the target variable
chisq_test <- chisq.test(table(creditDf$JobType, creditDf$creditScore))
# Print the results
print(chisq_test)
#Result: X-squared = 1.8852, df = 3, p-value = 0.5966
#Weak Association


#Column 18 - Ndepend | Number of Dependents
# Perform Chi-Square Test for Independence for the 'Ndepend' column against the target variable
chisq_test <- chisq.test(table(creditDf$Ndepend, creditDf$creditScore))
# Print the results
print(chisq_test)
#Results: X-squared = 0, df = 1, p-value = 1
#Weakest Association


#Column 19 - telephone | Presence of a telephone
# Perform Chi-Square Test for Independence for the 'telephone' column against the target variable
chisq_test <- chisq.test(table(creditDf$telephone, creditDf$creditScore))
# Print the results
print(chisq_test)
#Results: X-squared = 1.1726, df = 1, p-value = 0.2789
#Showing little bit strong but X-Squared value is pretty less
#To be Decided


#COlumn 20 - foreign | Foreign worker status
# Perform Chi-Square Test for Independence for the 'foreign' column against the target variable
chisq_test <- chisq.test(table(creditDf$foreign, creditDf$creditScore))
# Print the results
print(chisq_test)
#Results: X-squared = 5.8216, df = 1, p-value = 0.01583
#Strong Association









































































# Clear the workspace
rm(list=ls())
cat("\014")

#packages
install.packages("readxl")
library(readxl)

#Set Working Directory
setwd("C:/Users/ABC/Desktop/BA with R - Spring 2024/BA with R Project/PROJECT")

# load in the data file into data.frame
creditDf <- read_excel("CreditWorthiness.xlsx")

#EDA
View(creditDf)
summary(creditDf)
str(creditDf)

#Step 2: Data Pre-processing

# sub-part 1
# Check for Missing Values

#how to check for 1 column
is.na(creditDf$age)
which(is.na(creditDf$age))
sum(is.na(creditDf$age))


# Check for missing values in each column
missing_values <- colSums(is.na(creditDf))
missing_values

# Display columns with missing values
names_with_missing <- names(missing_values[missing_values > 0])
print(names_with_missing)


#Step 2: Data-Pre-processing | Sub-Part 2: Encoding Categorical Variables
#Why? - because it is convenient for ML algorithms to do so

# Define a function to count unique categories in each column
count_categories <- function(data) {
  cat_counts <- lapply(data, function(column) table(column))
  return(cat_counts)
}

# Apply the function to your dataset
category_counts <- count_categories(creditDf)

# Print the counts for each column
for (i in seq_along(category_counts)) {
  cat_name <- names(category_counts)[i]
  cat_count <- category_counts[[i]]
  cat("\nColumn:", cat_name, "\n")
  print(cat_count)
}

#Encoding Variables Now - (This includes Feature Selection and Feature Engineering too )
#1 Cbal - Checking Account Balance
creditDf$Cbal <- factor(creditDf$Cbal, levels = c("no checking account", "Rs. < 0", "0 <= Rs. < 2000", "Rs. >= 2000"))
levels(creditDf$Cbal)


#2 Cdur - Credit Duration in Months ( Higher the number better it is for the creditworthiness as Experienced chap)
# doubt is should I keep it as numeric nrmal or should I convert it to factors having levels. That means
# if person has 9 months different factor level and someone having 54 months of credit duration will have higher credit worth.
#Since confused using statistics for the same
# Step 1: Summary statistics
summary(creditDf$Cdur)
# Step 2: Visualization - Histogram
hist(creditDf$Cdur, main = "Distribution of Credit Durations", xlab = "Credit Durations")
# If treating as numeric
creditDf$Cdur <- as.numeric(creditDf$Cdur)


#3 Chist - Credit History (confusion baaki 3 ka ordering kaise karege agar karege toh. Kiska value jyada hai and time bound hai kya 
#like all settled till now mein past mein kharab raha toh.. vaise ginna hai ya kya scene)
# Encoding Chist (Credit History)
creditDf$Chist <- factor(creditDf$Chist, levels = c("dues not paid earlier", "all settled", "none taken/all settled", "all settled till now"), ordered = TRUE)
# Checking the levels and encoded values
levels(creditDf$Chist)

#4 Cpur: Purpose of the loan
category_order <- c("electronics" = 0, "renovation" = 0, "second hand vehicle" = 0, 
                    "miscellaneous" = 0, "furniture" = 0, "retaining" = 0, 
                    "domestic needs" = 0, "new vehicle" = 1, "Business" = 2, 
                    "education" = 2)
creditDf$Cpur <- factor(creditDf$Cpur, levels = names(category_order), ordered = TRUE)
creditDf$Cpur_encoded <- as.numeric(creditDf$Cpur)
levels(creditDf$Cpur)

#5 Camt- Loan Amount
# No encoding needed for a continuous variable, but ensure it's in numeric format
creditDf$Camt <- as.numeric(creditDf$Camt)


#6 Sbal - Savings Account Balance
# Define the desired ordering of categories
category_order <- c("no savings account" = 0, 
                    "Rs. < 1000" = 1,
                    "1000 <= Rs. < 5,000" = 2,
                    "5000 <= Rs. < 10,000" = 3,
                    "Rs. >= 10,000" = 4)

# Convert the "Sbal" column to a factor and reorder the levels based on the category_order vector
creditDf$Sbal <- factor(creditDf$Sbal, levels = names(category_order), ordered = TRUE)
levels(creditDf$Sbal)

#7 Edur (Employment duration) - greater the duration more is the confidence that they will have credit 
creditDf$Edur <- factor(creditDf$Edur, levels = c("not employed", "less than 1 year", "1 to 4 years", "4 to 7 years", "more than 7 years"), ordered = TRUE)
levels(creditDf$Edur)

#8 InRate (Installment rate) - considering 1,2,3,4 as categories and not numerics
#check with someone regarding this.
creditDf$InRate <- factor(creditDf$InRate, levels = c(1, 2, 3, 4), ordered = TRUE)
levels(creditDf$InRate)

#9 MSG(Marital Status and gender) - Nahi aara

#10 Oparties (Other Parties Involved)
# Encode Oparties as an ordered factor
# I am assuming someone having no one as a guarantor will not be better than someone 
# having a guarantor together.
creditDf$Oparties <- factor(creditDf$Oparties, levels = c("no one", "yes, co-applicant", "yes, guarantor"), ordered = TRUE)
levels(creditDf$Oparties)

#11 Rdur (Residence duration) - Ordinal Categorical Variable
# Encode Rdur as an ordered factor
creditDf$Rdur <- factor(creditDf$Rdur, levels = c("less than a year", "1 to 2 years", "2 to 3 years", "more than 3 years"), ordered = TRUE)
levels(creditDf$Rdur)

#12 Prop (Property type) - nominal categorical variables
# I am considering this as a nominal one and not logically that usually people
# in real estate will be better at having creditworth. 
creditDf$Prop <- factor(creditDf$Prop)

#13 Age( Applicant's age)
# I am putting the breaks in levels because more the age and older the person 
# better will be the duration he works for and better will be his creditworthyness
summary(creditDf$age)
hist(creditDf$age, main = "Distribution of Credit Durations", xlab = "Credit Durations")
# Define age groups
age_groups <- cut(creditDf$age,
                  breaks = c(18, 25, 35, 45, 55, 65, Inf),
                  labels = c("18-24", "25-34", "35-44", "45-54", "55-64", "65+"))

# Encode age column as a factor
creditDf$age <- factor(age_groups)
levels(creditDf$age)

#14 inPlans(existing plans)
# Assuming any existing plans like for stores or banks and one having no existing
# plans are similar
creditDf$inPlans <- factor(creditDf$inPlans)

#15 Htype - Housing Type - free, pays rent, own
# factoring with levels as someone having a house and then taking a credit 
# can pay it of easily than someone with rent or that lives for free
# at any apartment or something.
creditDf$Htype <- factor(creditDf$Htype, levels = c("pays rent", "free", "own"))
levels(creditDf$Htype)

#16. NumCred (Number of existing credits)
# Convert NumCred to numeric
creditDf$NumCred <- as.numeric(as.character(creditDf$NumCred))

#17. Jobtype - Different jobs have different pay hence ordered factor
# Define the levels and their order
job_levels <- c("resident unskilled", "non-resident either unemployed or unskilled",
                "employee with official position", "employed either in management, self or in high position")

# Encode the "JobType" column as an ordered factor
creditDf$JobType <- factor(creditDf$JobType, levels = job_levels, ordered = TRUE)

#18. Ndepend (Number of dependents)
#no need of any conversion

#19. telephone
# converting it to binary 1&0's
#yet to decide whether this column decides a strong correlation with creditworthiness
creditDf$telephone <- ifelse(creditDf$telephone == "Yes", 1, 0)

#20 foreign
# if the person is a foreign worker there will be unstability and problem in
# creditworthiness. Value of No is higher than Yes hence No will get a value of 1
# Confirm on this.
creditDf$foreign <- ifelse(creditDf$foreign == "no", 1, 0)

#-----------------------------------------------------


# Perform chi-square test
chisq_result <- chisq.test(table(creditDf$creditScore, creditDf$Cbal))

# Print the result
print(chisq_result)
#------------------------------------------------------







# Install and load required packages
install.packages("rpart")
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)

tree_model <- rpart(creditScore ~ ., data = creditDf)
rpart.plot(tree_model, cex = 1.2)




























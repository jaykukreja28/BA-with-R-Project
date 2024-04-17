# Clear the workspace
rm(list=ls())
cat("\014")

# Load required libraries
library(ggplot2)
library(readxl)

# Set Working Directory
setwd("C:/Users/ABC/Desktop/BA with R - Spring 2024/BA with R Project/PROJECT")

# Import dataset
creditDf <- read_excel("CreditWorthiness.xlsx")

# View the first few rows of the dataset
head(creditDf)

# Summary statistics of the dataset
summary(creditDf)

# Visualize the dataset using ggplot2 (you can customize this based on the variables in your dataset)
# For example, to plot a histogram of one of the numerical variables:
ggplot(creditDf, aes(x = age)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black") +
  labs(title = "Histogram of Age", x = "Age", y = "Frequency")


#------------------------------------------------

#Stage 2: Pre-Processing

# Handling Missing Values
# Check for missing values
missing_values <- colSums(is.na(creditDf))

# Identify columns with missing values
cols_with_missing <- names(missing_values[missing_values > 0])

#Since no missing values let's go !!!!
cols_with_missing


#Converting to a factor
creditDf[] <- lapply(creditDf, function(x) if(is.factor(x)) x else factor(x))

#------------------------------------------

#Stage 3: Splitting the Data

library(caret)

# Set seed for reproducibility
set.seed(123)

# Create indices for splitting the data
train_indices <- createDataPartition(creditDf$creditScore, p = 0.7, list = FALSE)

# Split the data into training and testing sets
train_data <- creditDf[train_indices, ]
test_data <- creditDf[-train_indices, ]


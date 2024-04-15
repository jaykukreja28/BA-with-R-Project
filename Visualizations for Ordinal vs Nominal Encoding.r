# Clear the workspace
rm(list=ls())
cat("\014")


library(readxl)
#Set Working Directory
setwd("C:/Users/ABC/Desktop/BA with R - Spring 2024/BA with R Project/PROJECT")
# load in the data file into data.frame
creditDf <- read_excel("CreditWorthiness.xlsx")


# Column 1 | Cbal | Checking account balance

# Task 1: Distribution of Checking Account Balance
ggplot(creditDf, aes(x = Cbal)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Distribution of Checking Account Balance",
       x = "Checking Account Balance",
       y = "Frequency")

# Task 2: Distribution of Credit Scores by Checking Account Balance
ggplot(creditDf, aes(x = Cbal, fill = creditScore)) +
  geom_bar(position = "dodge") +
  labs(x = "Checking Account Balance", y = "Count", fill = "Credit Score") +
  ggtitle("Distribution of Credit Scores by Checking Account Balance") +
  theme_minimal()

# Task 3: Contingency Table for Cbal and creditScore
contingency_table_Cbal <- table(creditDf$Cbal, creditDf$creditScore)
print(contingency_table_Cbal)

# Task 4: Associated Stacked Bar Plot for the Contingency Table
barplot(as.matrix(contingency_table_Cbal), beside = TRUE, col = c("red", "green"), 
        main = "Stacked Bar Plot of Cbal vs. creditScore", 
        xlab = "Cbal", ylab = "Frequency", 
        legend.text = TRUE, args.legend = list(title = "creditScore", 
                                               c("bad", "good")))

# Task 5: Mosaic Plot for Cbal vs. creditScore
mosaicplot(contingency_table_Cbal, main = "Mosaic Plot of Cbal vs. creditScore")



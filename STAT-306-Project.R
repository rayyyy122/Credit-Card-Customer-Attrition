library(tidyverse)
library(repr)
library(infer)
library(cowplot)
library(GGally)
library(broom)
library(dplyr)
library(leaps)
library(glmnet)

# import our data 
BankChurners <- read.csv("~/Desktop/BankChurners.csv", header= TRUE)
bank_data <- BankChurners
View(bank_data)

# clean and wrangle the data
bank <- bank_data %>%
  select(Customer_Age, Gender, Marital_Status,Income_Category, Card_Category, Avg_Utilization_Ratio, Total_Trans_Ct,Attrition_Flag) %>%
  mutate(Attrition_Flag = if_else(Attrition_Flag == "Existing Customer", 1, 0))
View(bank) 

# model selection
# full model
Bank_binary_log_model <- glm(
  formula = Attrition_Flag~., data= bank,
  family = binomial)
summary(Bank_binary_log_model)

# BIC
BIC(Bank_binary_log_model)

# the model without Age
Bank_reduced_log_model <- glm(
  formula = Attrition_Flag ~ Gender + Marital_Status + Income_Category + Card_Category + Avg_Utilization_Ratio + Total_Trans_Ct,
  data = bank,
  family = binomial)
summary(Bank_reduced_log_model)

# BIC
BIC(Bank_reduced_log_model)

# the model without Age, Income
Bank_reduced_log_model2 <- glm(
  formula = Attrition_Flag ~ Gender + Marital_Status + Card_Category + Avg_Utilization_Ratio + Total_Trans_Ct,
  data = bank,
  family = binomial)
summary(Bank_reduced_log_model2)

# BIC
BIC(Bank_reduced_log_model2)

# the model without Income
Bank_reduced_log_model3 <- glm(
  formula = Attrition_Flag ~ Customer_Age + Gender + Marital_Status + Card_Category + Avg_Utilization_Ratio + Total_Trans_Ct,
  data = bank,
  family = binomial)
summary(Bank_reduced_log_model3)

# BIC
BIC(Bank_reduced_log_model3)

# the model with interaction
Bank_interaction_log_model <- glm(
  formula = Attrition_Flag ~  Customer_Age + Gender + Marital_Status + Income_Category * Card_Category + Avg_Utilization_Ratio + Total_Trans_Ct,
  data = bank,
  family = binomial)
summary(Bank_interaction_log_model)

# BIC
BIC(Bank_interaction_log_model)




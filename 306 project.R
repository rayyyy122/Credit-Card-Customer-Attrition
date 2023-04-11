BankChurners <- read.csv("~/Desktop/BankChurners.csv", header= TRUE)
bank_data <- BankChurners
View(bank_data)
library(tidyverse)
library(repr)
library(infer)
library(cowplot)
library(GGally)
library(broom)
library(dplyr)
library(leaps)
library(glmnet)

bank <- bank_data %>%
  select(Customer_Age, Gender, Marital_Status,Income_Category, Card_Category, Avg_Utilization_Ratio, Total_Trans_Ct,Attrition_Flag) %>%
  mutate(Attrition_Flag = if_else(Attrition_Flag == "Existing Customer", 1, 0))
View(bank)

Bank_binary_log_model <- glm(
  formula = Attrition_Flag~., data= bank,
  family = binomial)
summary(Bank_binary_log_model)

# If the significant level = 0.1, the Gender, Marital_Status(Married), Income_Category(Unknown), Card_Category(Gold),Card_Category(Platinum),
# Avg_Utilization_Ratio, and Total_Trans_Ct are significant
# With the significant level = 0.05, only Gender, Marital_Status(Married), Card_Category(Gold), Avg_Utilization_Ratio, and Total_Trans_Ct 
# are significant

Bank_reduced_log_model <- glm(
  formula = Attrition_Flag ~ Gender + Marital_Status + Income_Category + Card_Category + Avg_Utilization_Ratio + Total_Trans_Ct,
  data = bank,
  family = binomial)
summary(Bank_reduced_log_model)

Bank_reduced_log_model2 <- glm(
  formula = Attrition_Flag ~ Gender + Marital_Status + Card_Category + Avg_Utilization_Ratio + Total_Trans_Ct,
  data = bank,
  family = binomial)
summary(Bank_reduced_log_model2)

Bank_interaction_log_model <- glm(
  formula = Attrition_Flag ~  Customer_Age + Gender + Marital_Status + Income_Category * Card_Category + Avg_Utilization_Ratio + Total_Trans_Ct,
  data = bank,
  family = binomial)
summary(Bank_interaction_log_model)


# AIC are both larger than full model, so we use the full model

Bank_binary_log_model_results <- tidy(Bank_binary_log_model, conf.int = TRUE)

Bank_binary_log_model_results1 <- Bank_binary_log_model_results %>% mutate(
  exp.estimate = exp(estimate), exp.conf.low = exp(conf.low), exp.conf.high = exp(conf.high)) %>%
  mutate_if(is.numeric, round, 6)
Bank_binary_log_model_results1

set.seed(4)

bank$ID <- 1:nrow(bank)
bank_train <- 
  slice_sample(bank, prop = 0.70) 

bank_test <- 
  bank %>%
  anti_join(bank_train, by = "ID")

bank_train <- bank_train %>% select(- "ID")
bank_test <- bank_test %>% select(- "ID")

train_model <- glm(
  formula = Attrition_Flag~., data= bank_train,
  family = binomial)
summary(Bank_binary_log_model)

prediction <- predict(train_model,
                   tibble(Customer_Age = 20,
                   Gender = "M",
                   Marital_Status = "Single",
                   Income_Category = "Less than $40K",
                   Card_Category = "Gold",
                   Avg_Utilization_Ratio = 0.5,
                   Total_Trans_Ct = 50),
                   type = "response")

prediction

test_LR <- predict(train_model, newdata = bank_test)
head(test_LR)


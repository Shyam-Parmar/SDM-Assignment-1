# Statistical Data Mining
# Assignment 1


# Libraries
library(rio)  
library(moments)
library(dplyr) #
library(Rmisc)
library(ggplot2) #
library(pwr)
library(readxl) #
library(ggpubr)
library(tidyverse) #
library(broom)
library(AICcmodavg)
library(tidyr)
library(rsq)
library(mRMRe)
library(mlbench) #
library(caret) #
library(magrittr) #
library(leaps)
library(mltools) #
library(data.table)

# Load dataset
df <- read_excel("C:/Users/Shyam/Downloads/CreditRating.xlsx") 

# Analysis


# clean and normalize data
str(df)
df <- select(df, -ID)
cols <- c("Gender", "Student", "Married", "Ethnicity", "Education")
df %<>% mutate_at(cols, factor)

# normalize the data
norm_data <- df %>% mutate(Income_norm = Income/max(Income, na.rm = TRUE)) %>%
  mutate(Limit_norm = Limit/max(Limit, na.rm = TRUE)) %>%
  mutate(Cards_norm = Cards/max(Cards, na.rm = TRUE)) %>%
  mutate(Age_norm = Age/max(Age, na.rm = TRUE)) %>%
  mutate(Balance_norm = Balance/max(Balance, na.rm = TRUE))

select_df <- norm_data %>% select(Rating, Income_norm, Limit_norm, Cards_norm, Age_norm, Balance_norm, Gender, Education, Student, Married, Ethnicity)

# one-hot encoding
#dmy <- dummyVars(" ~ .", data = select_df)
#trsf <- data.frame(predict(dmy, newdata = select_df))
#trsf


lm <- lm(Rating ~., data = select_df)
summary(lm)


# model 3
m1 <- lm(Rating ~ I(Limit_norm/Cards_norm) + I(Age_norm^2) + Student + Married + Income_norm + Cards_norm + Ethnicity + Gender + Cards_norm, data = select_df)
summary(m1)

# model 2
m2 <- lm(Rating ~  I(Income_norm-Limit_norm) + I(Age_norm^2) + Student + Married + Income_norm + Balance_norm + Cards_norm + Ethnicity + Gender + Cards_norm, data = select_df)
summary(m2)

# model 1
m3 <- lm(Rating ~  Age_norm + Student + Married + Income_norm + Balance_norm + Cards_norm + Ethnicity + Gender + Cards_norm + Education  + Limit_norm, data = select_df)
summary(m3)

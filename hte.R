# set up workspace
rm(list=ls())
set.seed(1988)
setwd('/Users/vyasenov/Library/CloudStorage/OneDrive-Adobe/GitHub/personal/lasso-knockoffs-hte')

# load libraries
library(tidyverse)
library(knockoff)

# load data
data <- read_csv(file='titanic_dataset.csv')
colnames(data) <- tolower(colnames(data))

# clean data
data <- data %>%
  dplyr::mutate(age = replace(age, is.na(age), 99),
                male = ifelse(sex == 'male', 1, 0),
                cabin = substr(cabin, 1, 1),
                cabin = replace(cabin, is.na(cabin), 'Z')) %>%
  dplyr::select(-c(passengerid, name, ticket, sex))

# one-hot encode string vars
data <- data %>%
  mutate(embarkedC = as.integer(embarked == 'C'),
         embarkedQ = as.integer(embarked == 'Q'),
         embarkedS = as.integer(embarked == 'S'),
         cabinA = as.integer(cabin == 'A'),
         cabinB = as.integer(cabin == 'B'),
         cabinC = as.integer(cabin == 'C'),
         cabinD = as.integer(cabin == 'D'),
         cabinE = as.integer(cabin == 'E'),
         cabinF = as.integer(cabin == 'F'),
         ) %>%
  dplyr::select(-c(cabin,embarked))

# remove missing
dim(data)
data <- na.omit(data)
dim(data)

# women more likely to survive - 55.2 p.p. difference
summary(lm(survived~male, data=data))
data %>% group_by(male) %>% summarize(mean = mean(survived))

# format data
y <- data$survived
x <- data %>% dplyr::select(-c(survived, male))
x <- as.matrix(x)
n <- nrow(x)

# impute missing potential outcomes
model <- glm(y ~ male + x, family='binomial', data=data)
data$male <- 1 - data$male
data$survived_cntrfctl <- as.integer(predict(model, type = 'response', newdata = data) >= .7)

# calculate person-level treatment effect
data$te <- ifelse(data$male == 1, 
                  data$survived - data$survived_cntrfctl,
                  data$survived_cntrfctl - data$survived)
data %>% count(te)

# clean up
data <- data %>% 
  select(-c(male,survived,survived_cntrfctl)) %>%
  select(te, everything())

# knockoffs
x <- data[-1]
y <- data$te

# run knockoffs algorithm
result = knockoff.filter(x, 
                         y, 
                         fdr = .1,
                         statistic=stat.lasso_lambdasmax,
                         offset = 0)
print(result$selected)
result

# compare with OLS regression
summary(lm(te~.,data=data))

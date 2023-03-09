rm(list = ls())

library(dplyr)
library(fixest) # fixed effect estimation

# iris dataset
# https://hackmd.io/@mutolisp/SyowFbuAb?type=view
data <- iris

summary(data)
summary(data %>% filter(Species == "setosa"))
summary(data %>% filter(Species == "versicolor"))
summary(data %>% filter(Species == "virginica"))

# if I want to know the relationship of the length of petal and the length of sepal
# I may form a hypothesis that the greater the sepal is, the greater the petal is as well
# thus I expect to see a positive relationship between the two

cor(data$Sepal.Length, data$Petal.Length)
cor(data$Sepal.Length, data$Petal.Length)^2

# dependent variable ~ independent variables (variables of interest) + control variables + Fixed Effects
reg1 <- lm(Sepal.Length ~ Petal.Length, data = data)
reg2 <- lm(Petal.Length ~ Sepal.Length, data = data)
reg3 <- fixest::feols(Sepal.Length ~ Petal.Length, data = data)

summary(reg1)
summary(reg2)
summary(reg3)
reg3$coeftable$`Pr(>|t|)`


# export the reg table
etable(reg3)



?lm


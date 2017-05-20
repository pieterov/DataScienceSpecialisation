# QUIZ - REGRESSION MODELS - WEEK 1

# Set working directory
setwd('~/Documents/Github Repos/DataScienceSpecialisation/7.Regression Models/Quiz')

# Clear memory
rm(list=ls())

# QUESTION 1:
x <- c(0.18, -1.54, 0.42, 0.95)
w <- c(2, 1, 3, 1)
weighted.mean(x, w)
# 0.1471429

# QUESTION 2:
x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)
lm(y ~ x - 1)
plot(x,y,xlim=c(0,1))
# 0.8263

# QUESTION 3:
data(mtcars)
lm(mpg ~ wt, data=mtcars)
# -5.344

# QUESTION 4:
# b1 = cor(Y,X)*sd(Y)/sd(X)
# b1 = 0.5*1/0.5 = 1
# 1

# QUESTION 5:
# score2 = cor*score1 = 0.4 * 1.5 = 0.6
# 0.6

# QUESTION 6:
x <- c(8.58, 10.46, 9.01, 9.64, 8.86)
(x-mean(x))/sd(x)
# -0.9719

# QUESTION 7:
x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)
lm(y~x)
# 1.567

# QUESTION 8:
# 0

# QUESTION 9:
x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
mean(x)
# 0.573

# QUESTION 10:
# Not: 1, cor(y,x)
# Var(Y)/Var(X)
##############################################################
# QUIZ - REGRESSION MODELS - WEEK 3
##############################################################

# Set working directory
setwd('~/Documents/Github Repos/DataScienceSpecialisation/7.Regression Models/Quiz')

rm(list=ls())
library(datasets)
library(lmtest)
data("mtcars")

# QUESTION 1:
#----------------------------------------
fit <- lm(mpg ~ factor(cyl) + wt, mtcars)

summary(fit)$coef

# Answer: -6.07


# QUESTION 2:
#----------------------------------------
fit_adj <- lm(mpg ~ factor(cyl) + wt, mtcars)
fit_unadj <- lm(mpg ~ factor(cyl), mtcars)

summary(fit_adj)$coef
summary(fit_unadj)$coef

# Answer: Holding weight constant, cylinder appears
# to have less of an impact on mpg than if weight is 
# disregarded.

# QUESTION 3:
#----------------------------------------
fit_norm <- lm(mpg ~ factor(cyl) + wt, mtcars)
fit_inter <- lm(mpg ~ factor(cyl) + wt + factor(cyl)*wt, mtcars)

summary(fit_norm)$coef
summary(fit_inter)$coef

lrtest(fit_norm, fit_inter)

# Answer: The P-value is larger than 0.05. 
# So, according to our criterion, we would fail
# to reject, which suggests that the interaction 
# terms may not be necessary.

# QUESTION 4:
#----------------------------------------
lm(mpg ~ I(wt * 0.5) + factor(cyl), data = mtcars)
lm(mpg ~ wt + factor(cyl), data = mtcars)
lm(mpg ~ I(wt * 0.5), data = mtcars)

# Answer: The estimated expected change in MPG 
# per one ton increase in weight for a specific number
# of cylinders (4, 6, 8).

# WRONG ANSWER: The estimated expected change in MPG
# per half ton increase in weight.

# WRONG ANSWER: The estimated expected change in MPG per 
# half ton increase in weight for a specific number of 
# cylinders (4, 6, 8).

# QUESTION 5:
#----------------------------------------
x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
y <- c(0.549, -0.026, -0.127, -0.751, 1.344)

fit <- lm(y~x)

plot(x,y)
abline(fit$coef[1],fit$coef[2])

hatvalues(fit)

# Answer: 0.9946

# QUESTION 6:
#----------------------------------------
x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
y <- c(0.549, -0.026, -0.127, -0.751, 1.344)

fit_w <- lm(y~x)
fit_wo <- lm(y[-5]~x[-5])

plot(x,y)
abline(fit_w$coef[1],fit_w$coef[2], col="red")
abline(fit_wo$coef[1],fit_wo$coef[2], col="blue")

dfbeta(fit)[5,]

# Difference in slop
rcdiff <- fit_w$coef[2]-fit_wo$coef[2]
sldiff <- fit_w$coef[1]-fit_wo$coef[1]

# Mean Square Error:
mse <- summary(fit_wo)$sigma
cov <- diag(summary(fit_w)$cov.unscaled)[2]

# Answer,
rcdiff/sqrt(mse^2*cov)

# Alternatively,
influence.measures(fit_w)$infmat[5, 'dfb.x']

# Get unscaled covariance matrix, note solve() in this way gives inverse.
X <- cbind(rep(1,5), x)
solve((t(X) %*% X))

# Answer: -133.82

# QUESTION 7:
#----------------------------------------

# Answer: It is possible for the coefficient to reverse sign
# after adjustment. For example, it can be strongly significant 
# and positive before adjustment and strongly significant and 
# negative after adjustment.
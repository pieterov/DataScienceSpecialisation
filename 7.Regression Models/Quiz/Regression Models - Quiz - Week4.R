##############################################################
# QUIZ - REGRESSION MODELS - WEEK 4
##############################################################

# Set working directory
setwd('~/Documents/Github Repos/DataScienceSpecialisation/7.Regression Models/Quiz')

rm(list=ls())
library(datasets)

# QUESTION 1:
#----------------------------------------
# Give the estimated odds ratio for autolander use comparing head winds,
# labeled as "head" in the variable headwind (numerator) to tail winds
(denominator).
library(MASS)
data(shuttle)
head(shuttle)

fit <- glm(use~wind, data = shuttle, family = "binomial")

summary(fit)
exp(fit$coef)

# Answer: 0.969


# QUESTION 2:
#----------------------------------------
# Give the estimated odds ratio for autolander use comparing head winds
# (numerator) to tail winds (denominator) adjusting for wind strength
# from the variable magn.

fit <- glm(use~wind+magn, data = shuttle, family = "binomial")
summary(fit)
exp(fit$coef)

summary(fit)

# Answer: 0.968


# QUESTION 3:
#----------------------------------------

# If you fit a logistic regression model to a binary variable,
# for example use of the autolander, then fit a logistic regression 
# model for one minus the outcome (not using the autolander) what 
# happens to the coefficients?

# Answer: The coefficients reverse their signs.


# QUESTION 4:
#----------------------------------------

data(InsectSprays)

head(InsectSprays)

boxplot(count~spray, data=InsectSprays) 

fit <- glm(count ~ spray - 1, data = InsectSprays, family="poisson") 

summary(fit)

co <- exp(fit$coefficients)

co[1]/co[2]

# Answer: 0.9456522


# QUESTION 5:
#----------------------------------------

set.seed(1234)

t <- rnorm(72)
t1 <- log(10) + t

fit<- glm(count ~ factor(spray) + offset(t), family="poisson", data=InsectSprays)
fit1<- glm(count ~ factor(spray) + offset(t1), family="poisson", data=InsectSprays)

summary(fit)$coef[,1]
summary(fit1)$coef[,1]


# Answer: 


# QUESTION 6:
#----------------------------------------

x <- -5:5
y <- c(5.12, 3.93, 2.67, 1.87, 0.52, 0.08, 0.93, 2.05, 2.54, 3.87, 4.97)

knots <- c(0)

splineTerms <- sapply(knots, function(knot) (x>knot)*(x-knot))

xMat <- cbind(1, x, splineTerms)

fit <- lm(y ~ xMat - 1)

yhat <- predict(fit)

plot(x, y, frame=FALSE, pch=21, bg="lightblue", cex=2)

lines(x, yhat, col="red", lwd=2)

summary(fit)$coef

(yhat[11]-yhat[6])/5

# Answer: 1.013

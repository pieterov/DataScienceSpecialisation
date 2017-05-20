# QUIZ - REGRESSION MODELS - WEEK 2

# Set working directory
setwd('~/Documents/Github Repos/DataScienceSpecialisation/7.Regression Models/Quiz')

# Clear memory
rm(list=ls())

# QUESTION 1:
x <- c(0.61, 0.93, 0.83, 0.35, 0.54, 0.16, 0.91, 0.62, 0.62)
y <- c(0.67, 0.84, 0.6, 0.18, 0.85, 0.47, 1.1, 0.65, 0.36)

n <- length(y)
beta1 <- cor(y, x) * sd(y) / sd(x)
beta0 <- mean(y) - beta1 * mean(x)
e <- y - beta0 - beta1 * x
sigma <- sqrt(sum(e^2) / (n-2)) 
ssx <- sum((x - mean(x))^2)
seBeta0 <- (1 / n + mean(x) ^ 2 / ssx) ^ .5 * sigma 
seBeta1 <- sigma / sqrt(ssx)
tBeta0 <- beta0 / seBeta0; tBeta1 <- beta1 / seBeta1
pBeta0 <- 2 * pt(abs(tBeta0), df = n - 2, lower.tail = FALSE)
pBeta1 <- 2 * pt(abs(tBeta1), df = n - 2, lower.tail = FALSE)
coefTable <- rbind(c(beta0, seBeta0, tBeta0, pBeta0), c(beta1, seBeta1, tBeta1, pBeta1))
colnames(coefTable) <- c("Estimate", "Std. Error", "t value", "P(>|t|)")
rownames(coefTable) <- c("(Intercept)", "x")
coefTable
summary(lm(y~x))
# Answer: 0.05296439


# QUESTION 2:
sigma
sigma(lm(y~x))
sd(lm(y~x)$residuals)*sqrt(n-1)/sqrt(n-2)
# Answer: 0.223

# QUESTION 3:
data(mtcars)
x <- mtcars$wt; y <- mtcars$mpg; n <- length(y)
beta1 <- cor(y, x) * sd(y) / sd(x)
beta0 <- mean(y) - beta1 * mean(x)
e <- y - beta0 - beta1 * x
sigma <- sqrt(sum(e^2) / (n-2)) 
ssx <- sum((x - mean(x))^2)

#xVals <- seq(min(x),max(x),by=.01)
xVals <- mean(x)
yVals <- beta0+beta1*xVals
se1<-sigma*sqrt(1/n+(xVals-mean(x))^2/ssx)
c(yVals - 2*se1, yVals + 2*se1)

fit <- lm(mpg ~ I(wt-mean(wt)), data = mtcars)
confint(fit)
# Answer: 18.99

# QUESTION 4:
# Answer: The estimated expected change in mpg per 1,000 lb increase in weight.

# QUESTION 5:
xVals <- 3
yVals <- beta0+beta1*xVals
se2<-sigma*sqrt(1+1/n+(xVals-mean(x))^2/ssx)
c(yVals - 2*se2, yVals + 2*se2)

fit <- lm(mpg ~ wt, data = mtcars)
predict(fit, newdata = data.frame(wt = 3), interval = "prediction")
# Answer: 27.57 mpg 

# QUESTION 6:
data(mtcars)
x <- mtcars$wt/2 # weight in short ton
y <- mtcars$mpg; n <- length(y)
beta1 <- cor(y, x) * sd(y) / sd(x)
beta0 <- mean(y) - beta1 * mean(x)
e <- y - beta0 - beta1 * x
sigma <- sqrt(sum(e^2) / (n-2)) 
ssx <- sum((x - mean(x))^2)
seBeta0 <- (1 / n + mean(x) ^ 2 / ssx) ^ .5 * sigma 
seBeta1 <- sigma / sqrt(ssx)
c(beta1, beta1 - 2*seBeta1, beta1 + 2*seBeta1)

fit <- lm(mpg ~ wt, data = mtcars)
confint(fit)[2, ] * 2

fit <- lm(mpg ~ I(wt * 0.5), data = mtcars)
confint(fit)[2, ]
# Answer: -12.973 mpg per short ton.

# QUESTION 7:
# If X gets divided by 100 going from cm to m, the slope coefficient gets multiplied by 100.
# Answer: Multiplied by 100.

# QUESTION 8:
# The intercept would become ??0 minus the constant c times ??1. ??0-??1*c+??1*(X+c) = (??0-??1*c)+??1*(X+c)
# Answer: The new intercept would be ??0-c??1

# QUESTION 9:
data(mtcars)
x <- mtcars$wt; y <- mtcars$mpg; n <- length(y)
beta1 <- cor(y, x) * sd(y) / sd(x)
beta0 <- mean(y) - beta1 * mean(x)
yhat <- beta0 + beta1 * x
ymean <- mean(y)

RSS_data_tov_mean <- sum((y - mean(y))^2) # intercept / denominator
RSS_data_tov_model <- sum((y - yhat)^2) # intercept+slope / numerator
RSS_model_tov_mean <- sum((yhat - mean(y))^2)

RSS_data_tov_model/RSS_data_tov_mean # = Answer
RSS_model_tov_mean/RSS_data_tov_mean
RSS_data_tov_model/RSS_data_tov_mean + RSS_model_tov_mean/RSS_data_tov_mean
# Answer: 0.25

# QUESTION 10:
# Not right answers:
# - If an intercept is included, the residuals most likely won't sum to zero.
# - The residuals never sum to zero.
# Answer: If an intercept is included, then they will sum to 0.

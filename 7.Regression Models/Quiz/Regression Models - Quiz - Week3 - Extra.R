##############################################################
# QUIZ - REGRESSION MODELS - WEEK 3 - EXTRA
##############################################################

# Set working directory
setwd('~/Documents/Github Repos/DataScienceSpecialisation/7.Regression Models/Quiz')

rm(list=ls())

# QUESTION 1:
#----------------------------------------
file <- "https://d3c33hcgiwev3.cloudfront.net/_cf0fd3361e05f5be5304b07b771bad48_company_data.csv?Expires=1492560000&Signature=SmCwfqIqcxhGMLhw4HoiEnlNJjera4gpmg6sSYvvGHr2MEsm-uV7~qS9Gpbm1uxDwbMZxRVd-VNJ8jfqnXrrrPThdXE1raN8oQe~aWLbYrry0SaPpFdRAwMEVeyWohGgRuw73TrnsufZRhqsEJXYfv~mVsN-uZwTqRl8HgP6obE_&Key-Pair-Id=APKAJLTNE6QMUY6HBC5A"
df <- read.csv(file)

head(df)
plot(df$x1,df$y)

fit <- lm(y~x1, df)
summary(fit)$coef

# Answer: Yes


# QUESTION 2:
#----------------------------------------
# Answer: 4.131854


# QUESTION 3:
#----------------------------------------
confint(fit)
# Answer: (3.944728, 4.318981)


# QUESTION 4:
#----------------------------------------
# Answer: 2.781114e-171


# QUESTION 5:
#----------------------------------------
# Answer: 

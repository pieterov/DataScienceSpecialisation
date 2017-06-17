##############################################################
# QUIZ - Developing Data Product - WEEK 3
##############################################################

# QUESTION 1:
#----------------------------------------
# Which of the following items is required for an R package to pass 
# R CMD check without any warnings or errors?

# Answer: An explicit software license, or a description file.


# QUESTION 2:
#----------------------------------------
# Which of the following is a generic function in a fresh installation
# of R, with only the default packages loaded? (Select all that apply)

# Answer: mean, predict, show (all)


# QUESTION 3:
#----------------------------------------
# What function is used to obtain the function body for an
# S4 method function?

# Answer: getMethod()


# QUESTION 4:
#----------------------------------------
#' Please download the R package DDPQuiz3 from the course web site. 
#' Examine the 𝚌𝚛𝚎𝚊𝚝𝚎𝚖𝚎𝚊𝚗 function implemented in the R/ sub
#' directory. What is the appropriate text to place above the 𝚌𝚛𝚎𝚊𝚝
#' 𝚎𝚖𝚎𝚊𝚗 function for Roxygen2 to create a complete help file?

#' Answer:

#' This function calculates the mean
#' 
#' @param x is a numeric vector
#' @return the mean of x
#' @export
#' @examples 
#' x <- 1:10
#' createmean(x)
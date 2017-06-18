shinydiamonds
========================================================
        author: Reproducible Pitch for my Shiny App - Coursera Data Science Specialization
date: 17/06/2017
autosize: true
width: 1920
height: 1080

The Idea Behind The App
========================================================
        
        - For this assignment, I wanted to create an app **that was simple and that worked**
        - I decided to use the **'diamonds' data set** from the ggplot2 package that I had seen many times before in class examples
- I was curious to see whether I could actually **afford** any of the diamonds in the data set
- The **shinydiamonds** app lets you input your budget and see which diamonds you can afford


ui.r and server.R
========================================================
        
        Here is the core of the ui.r input:
        
        ```{r, eval=FALSE}
sliderInput("budget",
            "Enter your budget (USD):",
            min = 300,
            max = 10000,
            value = 350, step = 1000)
```

On the server.R side I tag each diamond as affordable (TRUE/FALSE) based in the input:
        
        ```{r, eval=FALSE}
# generate maximum diamond price based on input$budget from ui.R
maxprice <- input$budget

# tag diamonds as afforable based on maxprice
diamonds
diamonds$affordable <- as.logical(ifelse(
        diamonds$price < maxprice, 1, 0))
```


Mini example
========================================================
        
        Here is an example for a budget of 1000$.

*The App uses the full data set. I sampled it here to keep this slide from loading too slowly.*
        
        ```{r, echo=TRUE}
library(ggplot2) 
library(dplyr)
diamonds <- sample_n(diamonds,1000) 
maxprice <- 1000
diamonds$affordable <- as.logical(ifelse(
        diamonds$price < maxprice, 1, 0))
qplot <- qplot(y=diamonds$carat, 
               x=diamonds$affordable,
               colour=diamonds$affordable, 
               ylab="Carat of the diamond",
               xlab="Can you afford it?") + geom_jitter() + theme(legend.position="none")
```
Results on the next slide!
        
        Final Result
========================================================
        
        Here is the output graph for a budget of 1000$:
        
        ```{r, echo=TRUE}
qplot 
```

Try the REAL APP!: https://pierremourier.shinyapps.io/shinydiamonds/
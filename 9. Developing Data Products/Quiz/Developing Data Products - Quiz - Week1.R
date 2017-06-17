##############################################################
# QUIZ - Developing Data Product - WEEK 1
##############################################################

# QUESTION 1:
#----------------------------------------
# Which of the following are absolutely necessary for creating 
# a functioning shiny app? (Check all that apply)

# Answers: CORRECT
# A server.R file containing a call to shinyServer()
# A ui.R file containing a call to shinyUI()

# QUESTION 2:
#----------------------------------------
# What is incorrect about the following syntax in ui.R?

library(shiny)
shinyUI(pageWithSidebar(  
        headerPanel("Data science FTW!"),  
        sidebarPanel(    
                h2('Big text')    
                h3('Sidebar')  
        ),  
        mainPanel(      
                h3('Main Panel text')  
        )
))

# Answer: CORRECT
# Missing a comma in the sidebar panel


# QUESTION 3:
#----------------------------------------

# Consider the following in ui.R
shinyUI(pageWithSidebar(  
        headerPanel("Example plot"),  
        sidebarPanel(    
                sliderInput('mu', 'Guess at the mu',value = 70, min = 60, max = 80, step 
                            = 0.05,)  ), 
        mainPanel(    
                plotOutput('newHist')  
        )
))

# And the following in server.R
library(UsingR)
data(galton)
shinyServer(  
        function(input, output) {    
                output$myHist <- renderPlot({      
                        hist(galton$child, xlab='child height', col='lightblue',main
                             ='Histogram')      
                        mu <- input$mu      
                        lines(c(mu, mu), c(0, 200),col="red",lwd=5)      
                        mse <- mean((galton$child - mu)^2)      
                        text(63, 150, paste("mu = ", mu))      
                        text(63, 140, paste("MSE = ", round(mse, 2)))      
                })      }
)

# Why isn't it doing what we want? (Check all that apply.)

# Answer: CORRECT
# The server.R output name isn't the same as the plotOutput command used in ui.R.

# Not the answer:
# The limits of the slider are set incorrectly and giving an error.


# QUESTION 4:
#----------------------------------------
# What are the main differences between creating a Shiny Gadget and creating
# a regular Shiny App? (Check all that apply)

# Answer: CORRECT
# Shiny Gadgets are designed to have small user interfaces that fit on one page.
# Shiny Gadgets are designed to be used by R users in the middle of a data analysis.

# Not the answer:
# Shiny Gadgets are specially designed for use on mobile phones and tablet computers.
# Shiny Gadgets are smaller programs and therefore run faster than Shiny Apps.
# Shiny Gadgets can be run on a user???s personal computer, unlike a regular Shiny App which needs to be hosted online.

# QUESTION 5:
#----------------------------------------
# Consider the following R script:
library(shiny)
library(miniUI)

pickXY <- function() {
        ui <- miniPage(
                gadgetTitleBar("Select Points by Dragging your Mouse"),
                miniContentPanel(
                        plotOutput("plot", height = "100%", brush = "brush")
                )
        )
        
        server <- function(input, output, session) {
                output$plot <- renderPlot({
                        plot(data_frame$X, data_frame$Y, main = "Plot of Y versus X",
                             xlab = "X", ylab = "Y")
                })
                observeEvent(input$done, {
                        stopApp(brushedPoints(data_frame, input$brush,
                                              xvar = "X", yvar = "Y"))
                })
        }
        
        runGadget(ui, server)
}

my_data <- data.frame(X = rnorm(100), Y = rnorm(100))

pickXY(my_data)

# Answer: CORRECT
# No arguments are defined for pickXY()


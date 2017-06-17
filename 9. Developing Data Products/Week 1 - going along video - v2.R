##################################
# Lecture 1:  Shiny - Part 1
##################################

setwd("~/Documents/Github Repos/DataScienceSpecialisation/9. Developing Data Products")

install.packages("shiny")
library(shiny)

# ---------------------------------------
#ui.R
library(shiny)
shinyUI(fluidPage(
        titlePanel("Data science FTW!"),
        sidebarLayout(
                sidebarPanel(
                        h3("Sidebar Text")
                ),
                mainPanel(
                        h3("Main Panel Text")
                )
        )
))

#server.R
library(shiny)
shinyServer(function(input, output) {
        
})
# ---------------------------------------
# To run it type runApp().


# R Wrappers for HTML Tags
# ---------------------------------------
library(shiny)
shinyUI(fluidPage(
        titlePanel("HTML Tags"),
        sidebarLayout(
                sidebarPanel(
                        h1("H1 Text"),
                        h2("H2 Text"),
                        h3("H3 Text"),
                        em("Emphasized Text")
                ),
                mainPanel(
                        h3("Main Panel Text"),
                        code("Some Code!")
                )
        )
))

# See ?builder for more details.

# Slider App: ui.R
# ---------------------------------------
library(shiny)
shinyUI(fluidPage(
        titlePanel("Slider App"),
        sidebarLayout(
                sidebarPanel(
                        h1("Move the Slider!"),
                        sliderInput("slider1", "Slide Me!", 0, 100, 0)
                        ),
                mainPanel(
                        h3("Slider Value:"),
                        textOutput("text")
                        )
                )
        )
        )
# ---------------------------------------
# Slider App: server.R
library(shiny)
shinyServer(function(input, output) {
        output$text <- renderText(input$slider1)
})
# ---------------------------------------

# Slider App

# New Components in the Slider App

# ui.R
# sliderInput() specifies a slider that a user can manipulate
# testOutput() displays text that is rendered in server.R

# server.R
# renderText() transforms UI input into text that can be displayed.


# Plot App: ui.R
# ---------------------------------------
library(shiny)
shinyUI(fluidPage(
        titlePanel("Plot Random Numbers"),
        sidebarLayout(
                sidebarPanel(
                        numericInput("numeric", "How Many Random Numbers Should be Plotted?",
                                     value = 1000, min = 1, max = 1000, step = 1),
                        sliderInput("sliderX", "Pick Minimum and Maximum X Values", 
                                    -100, 100, value = c(-50, 50)),
                        sliderInput("sliderY", "Pick Minimum and Maximum Y Values",
                                    -100, 100, value = c(-50, 50)),
                        checkboxInput("show_xlab", "Show/Hide X Axis Label", value = TRUE),
                        checkboxInput("show_ylab", "Show/Hide Y Axis Label", value = TRUE),
                        checkboxInput("show_title", "Show/Hide Title")
                        ),
                mainPanel(
                        h3("Graph of Random Points"),
                        plotOutput("plot1")
                        )
                )
        ))

# Plot App: server.R
# ---------------------------------------
library(shiny)
shinyServer(function(input, output) {
        output$plot1 <- renderPlot({
                set.seed(2016-05-25)
                number_of_points <- input$numeric
                minX <- input$sliderX[1]
                maxX <- input$sliderX[2]
                minY <- input$sliderY[1]
                maxY <- input$sliderY[2]
                dataX <- runif(number_of_points, minX, maxX)
                dataY <- runif(number_of_points, minY, maxY)
                xlab <- ifelse(input$show_xlab, "X Axis", "")
                ylab <- ifelse(input$show_ylab, "Y Axis", "")
                main <- ifelse(input$show_title, "Title", "")
                plot(dataX, dataY, xlab = xlab, ylab = ylab, main = main,
                     xlim = c(-100, 100), ylim = c(-100, 100))
                })
        })
# ---------------------------------------

# Apps with Plots

# ui.R
# numericInput() allows the user to enter any number
# checkboxInput() creates boxes that can be checked
# plotOutput() displays a plot

# server.R
# renderPlot() wraps the creation of a plot so it can be displayed


##################################
# Lecture 2:  Shiny - Part 2
##################################

calc_sum <- reactive({
        input$box1 + input$box2
})

library(webshot)
appshot("app1", "app1.png")
# This application predicts the horsepower of a car given the fuel efficiency in miles per gallon
# for the car.

# Horsepower Prediction: ui.R
# ---------------------------------------
library(shiny)
shinyUI(fluidPage(
        titlePanel("Predict Horsepower from MPG"),
        sidebarLayout(
                sidebarPanel(
                        sliderInput("sliderMPG", "What is the MPG of the car?", 10, 35, value = 20),
                        checkboxInput("showModel1", "Show/Hide Model 1", value = TRUE),
                        checkboxInput("showModel2", "Show/Hide Model 2", value = TRUE)
                ),
                mainPanel(
                        plotOutput("plot1"),
                        h3("Predicted Horsepower from Model 1:"),
                        textOutput("pred1"),
                        h3("Predicted Horsepower from Model 2:"),
                        textOutput("pred2")
                )
        )
))

# Horsepower Prediction: server.R
# ---------------------------------------
library(shiny)
shinyServer(function(input, output) {
        mtcars$mpgsp <- ifelse(mtcars$mpg - 20 > 0, mtcars$mpg - 20, 0)
        model1 <- lm(hp ~ mpg, data = mtcars)
        model2 <- lm(hp ~ mpgsp + mpg, data = mtcars)
        
        model1pred <- reactive({
                mpgInput <- input$sliderMPG
                predict(model1, newdata = data.frame(mpg = mpgInput))
        })
        
        model2pred <- reactive({
                mpgInput <- input$sliderMPG
                predict(model2, newdata = 
                                data.frame(mpg = mpgInput,
                                           mpgsp = ifelse(mpgInput - 20 > 0,
                                                          mpgInput - 20, 0)))
        })
        
        output$plot1 <- renderPlot({
                mpgInput <- input$sliderMPG
                
                plot(mtcars$mpg, mtcars$hp, xlab = "Miles Per Gallon", 
                     ylab = "Horsepower", bty = "n", pch = 16,
                     xlim = c(10, 35), ylim = c(50, 350))
                if(input$showModel1){
                        abline(model1, col = "red", lwd = 2)
                }
                if(input$showModel2){
                        model2lines <- predict(model2, newdata = data.frame(
                                mpg = 10:35, mpgsp = ifelse(10:35 - 20 > 0, 10:35 - 20, 0)
                        ))
                        lines(10:35, model2lines, col = "blue", lwd = 2)
                }
                
                legend(25, 250, c("Model 1 Prediction", "Model 2 Prediction"), pch = 16, 
                       col = c("red", "blue"), bty = "n", cex = 1.2)
                points(mpgInput, model1pred(), col = "red", pch = 16, cex = 2)
                points(mpgInput, model2pred(), col = "blue", pch = 16, cex = 2)
        })
        
        output$pred1 <- renderText({
                model1pred()
        })
        
        output$pred2 <- renderText({
                model2pred()
        })
})
# ---------------------------------------

library(webshot)
appshot("app2", "app2.png")

# Reactive Horsepower: ui.R
# ---------------------------------------
        shinyUI(fluidPage(
                titlePanel("Predict Horsepower from MPG"),
                sidebarLayout(
                        sidebarPanel(
                                sliderInput("sliderMPG", "What is the MPG of the car?", 10, 35, value = 20),
                                checkboxInput("showModel1", "Show/Hide Model 1", value = TRUE),
                                checkboxInput("showModel2", "Show/Hide Model 2", value = TRUE),
                                submitButton("Submit") # New!
                        ))))
# ---------------------------------------


# Tabs: ui.R
# ---------------------------------------                        
library(shiny)
shinyUI(fluidPage(
        titlePanel("Tabs!"),
        sidebarLayout(
        sidebarPanel(
                textInput("box1", "Enter Tab 1 Text:", value = "Tab 1!"),
                textInput("box2", "Enter Tab 2 Text:", value = "Tab 2!"),
                textInput("box3", "Enter Tab 3 Text:", value = "Tab 3!")
        ),
        mainPanel(
                tabsetPanel(type = "tabs", 
                tabPanel("Tab 1", br(), textOutput("out1")), 
                tabPanel("Tab 2", br(), textOutput("out2")), 
                tabPanel("Tab 3", br(), textOutput("out3"))
                )
                )
        )
))

#Tabs: server.R
# ---------------------------------------                     
library(shiny)
shinyServer(function(input, output) {
        output$out1 <- renderText(input$box1)
        output$out2 <- renderText(input$box2)
        output$out3 <- renderText(input$box3)
        })
# ---------------------------------------                        

# Interactive Graphics: ui.R
# ---------------------------------------                        
library(shiny)
shinyUI(fluidPage(
        titlePanel("Visualize Many Models"),
        sidebarLayout(
                sidebarPanel(
                        h3("Slope"),
                        textOutput("slopeOut"),
                        h3("Intercept"),
                        textOutput("intOut")
                        ),
                mainPanel(
                        plotOutput("plot1", brush = brushOpts(
                                id = "brush1"
                                ))
                        )
                )
        ))

# Interactive Graphics: server.R
# ---------------------------------------                        
library(shiny)
shinyServer(function(input, output) {
        model <- reactive({
                brushed_data <- brushedPoints(trees, input$brush1,
                                              xvar = "Girth", yvar = "Volume")
                if(nrow(brushed_data) < 2){
                        return(NULL)
                        }
                lm(Volume ~ Girth, data = brushed_data)
                })
        output$slopeOut <- renderText({
                if(is.null(model())){
                        "No Model Found"
                        } else {
                                model()[[1]][2]
                                }
                })
        output$intOut <- renderText({
                if(is.null(model())){
                        "No Model Found"
                        } else {
                                model()[[1]][1]
                                }
                })
        output$plot1 <- renderPlot({
                plot(trees$Girth, trees$Volume, xlab = "Girth",
                     ylab = "Volume", main = "Tree Measurements",
                     cex = 1.5, pch = 16, bty = "n")
                if(!is.null(model())){
                        abline(model(), col = "blue", lwd = 2)
                        }
                })
        })


##################################
# Lecture 3:  Shiny Gadgets
##################################

library(shiny)
library(miniUI)

myFirstGadget <- function() {
        ui <- miniPage(
                gadgetTitleBar("My First Gadget")
        )
        server <- function(input, output, session) {
                # The Done button closes the app
                observeEvent(input$done, {
                        stopApp()
                })
        }
        runGadget(ui, server)
}
# --------------------------------------- 

# Gadgets with Arguments: Code Part 1
# --------------------------------------- 
library(shiny)
library(miniUI)

multiplyNumbers <- function(numbers1, numbers2) {
        ui <- miniPage(
                gadgetTitleBar("Multiply Two Numbers"),
                miniContentPanel(
                        selectInput("num1", "First Number", choices=numbers1),
                        selectInput("num2", "Second Number", choices=numbers2)
                        )
                )
        server <- function(input, output, session) {
                observeEvent(input$done, {
                        num1 <- as.numeric(input$num1)
                        num2 <- as.numeric(input$num2)
                        stopApp(num1 * num2)
        })
        }
        runGadget(ui, server)
}
# ---------------------------------------

# Gadgets with Interactive Graphics: Code Part 1
# ---------------------------------------
library(shiny)
library(miniUI)

pickTrees <- function() {
        ui <- miniPage(
                gadgetTitleBar("Select Points by Dragging your Mouse"),
                miniContentPanel(
                        plotOutput("plot", height = "100%", brush = "brush")
                )
        )
        
        server <- function(input, output, session) {
                output$plot <- renderPlot({
                        plot(trees$Girth, trees$Volume, main = "Trees!",
                             xlab = "Girth", ylab = "Volume")
                })
                observeEvent(input$done, {
                        stopApp(brushedPoints(trees, input$brush,
                                              xvar = "Girth", yvar = "Volume"))
                })
        }
        
        runGadget(ui, server)
}
# ---------------------------------------

# For more details about Shiny Gadgets visit the Shiny Gadgets website:
# http://shiny.rstudio.com/articles/gadgets.html
# http://shiny.rstudio.com/articles/gadget-ui.html


##################################
# Lecture 4:  googleVis / Google Vis API
##################################

# Need to allow Flash in Privacy settings in Google Chrome.

suppressPackageStartupMessages(library(googleVis))
M <- gvisMotionChart(Fruits, "Fruit", "Year",
                     options=list(width=600, height=400))
plot(M)

print(M,"chart")

# http://cran.r-project.org/web/packages/googleVis/googleVis.pdf

# Plots on maps
G <- gvisGeoChart(Exports, locationvar="Country",
                  colorvar="Profit",options=list(width=600, height=400))
print(G,"chart")


# Specifying a region
G2 <- gvisGeoChart(Exports, locationvar="Country",
                   colorvar="Profit",options=list(width=600, height=400,region="150"))
plot(G2)

print(G2,"chart")


# Finding parameters to set under options

# https://developers.google.com/chart/interactive/docs/gallery/geochart

# Setting more options

df <- data.frame(label=c("US", "GB", "BR"), val1=c(1,3,4), val2=c(23,12,32))
Line <- gvisLineChart(df, xvar="label", yvar=c("val1","val2"),
                      options=list(title="Hello World", legend="bottom",
                                   titleTextStyle="{color:'red', fontSize:18}",                         
                                   vAxis="{gridlines:{color:'red', count:3}}",
                                   hAxis="{title:'My Label', titleTextStyle:{color:'blue'}}",
                                   series="[{color:'green', targetAxisIndex: 0}, 
                                   {color: 'blue',targetAxisIndex:1}]",
                                   vAxes="[{title:'Value 1 (%)', format:'##,######%'}, 
                                   {title:'Value 2 (\U00A3)'}]",                          
                                   curveType="function", width=500, height=300                         
                      ))

plot(Line)

# https://github.com/mages/Introduction_to_googleVis/blob/gh-pages/index.Rmd

# Setting more options

# Combining multiple plots together

G <- gvisGeoChart(Exports, "Country", "Profit",options=list(width=200, height=100))
T1 <- gvisTable(Exports,options=list(width=200, height=270))
M <- gvisMotionChart(Fruits, "Fruit", "Year", options=list(width=400, height=370))
GT <- gvisMerge(G,T1, horizontal=FALSE)
GTM <- gvisMerge(GT, M, horizontal=TRUE,tableOptions="bgcolor=\"#CCCCCC\" cellspacing=10")

plot(GTM)

# demo(googleVis)
# http://cran.r-project.org/web/packages/googleVis/vignettes/googleVis.pdf
# http://cran.r-project.org/web/packages/googleVis/googleVis.pdf
# https://developers.google.com/chart/interactive/docs/gallery
# https://developers.google.com/chart/interactive/faq

##################################
# Lecture 5:  plotly
##################################


install.packages("plotly")
library(plotly)

# Basic Scatterplot
plot_ly(mtcars, x = ~wt, y = ~mpg, type = "scatter")

# Scatterplot Color
plot_ly(mtcars, x = ~wt, y = ~mpg, type = "scatter", color = ~factor(cyl))

# Continuous Color
plot_ly(mtcars, x = ~wt, y = ~mpg, type = "scatter", color = ~disp)

# Scatterplot Sizing
plot_ly(mtcars, x = ~wt, y = ~mpg, type = "scatter", color = ~factor(cyl), size = ~hp)

# 3D Scatterplot
set.seed(2016-07-21)
temp <- rnorm(100, mean = 30, sd = 5)
pressure <- rnorm(100)
dtime <- 1:100
plot_ly(x = temp, y = pressure, z = dtime,
        type = "scatter3d", mode = "markers", color = temp)

# Line Graph
data("airmiles")
plot_ly(x = ~time(airmiles), y = ~airmiles, type = "scatter", mode = "lines")

# Multi Line Graph
library(plotly)
library(tidyr)
library(dplyr)
data("EuStockMarkets")

stocks <- as.data.frame(EuStockMarkets) %>%
        gather(index, price) %>%
        mutate(time = rep(time(EuStockMarkets), 4))

plot_ly(stocks, x = ~time, y = ~price, color = ~index, type = "scatter", mode = "lines")

# Multi Line Graph
library(plotly)
library(tidyr)
library(dplyr)
data("EuStockMarkets")

# Histogram
plot_ly(x = ~precip, type = "histogram")

# Boxplot
plot_ly(iris, y = ~Petal.Length, color = ~Species, type = "box")

# Heatmap
terrain1 <- matrix(rnorm(100*100), nrow = 100, ncol = 100)
plot_ly(z = ~terrain1, type = "heatmap")

# 3D Surface
terrain2 <- matrix(sort(rnorm(100*100)), nrow = 100, ncol = 100)
plot_ly(z = ~terrain2, type = "surface")


# Choropleth Maps: Setup

# Create data frame
state_pop <- data.frame(State = state.abb, Pop = as.vector(state.x77[,1]))
# Create hover text
state_pop$hover <- with(state_pop, paste(State, '<br>', "Population:", Pop))
# Make state borders white
borders <- list(color = toRGB("red"))
# Set up some mapping options
map_options <- list(
        scope = 'usa',
        projection = list(type = 'albers usa'),
        showlakes = TRUE,
        lakecolor = toRGB('white')
)

plot_ly(z = ~state_pop$Pop, text = ~state_pop$hover, locations = ~state_pop$State, 
        type = 'choropleth', locationmode = 'USA-states', 
        color = state_pop$Pop, colors = 'Blues', marker = list(line = borders)) %>%
        layout(title = 'US Population in 1975', geo = map_options)
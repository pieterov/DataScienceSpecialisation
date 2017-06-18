library(shiny)

shinyUI(
        
fluidPage(
        titlePanel("Calculate Length and Angle of Box Diagonals"),
        sidebarLayout(
                sidebarPanel(
                        h4("Give box dimensions"),
                        sliderInput("l", "Length:", 
                                    1, 10, value = c(5)),
                        sliderInput("d", "Depth:", 
                                    1, 10, value = c(5)),
                        sliderInput("h", "Height:", 
                                    1, 10, value = c(5)),
                        h4("Give diagonal plane direction"),
                        selectInput("plane.option", "Is it from left to right, or front to back:",
                                    c("Left to Right" = "lr", "Front to Back" = "fb"))
                        ),
                mainPanel(
                        p("This app allows you to calculate the distance between two opposite corners of a box,
                        i.e., through the center of the box. First, choose the dimensions of the box. Then,
                          choose the direction of the diagonal plane in the box, from left to right, or from front to back."),
                        p("The left schematic below shows the 3 unique surfaces of the box, i.e., front, top and side surfaces.
                        The red line is the surface diagonal (Diagonal1)."),
                        p("The right schematic shows the diagonal plain through the box, either from left to right,
                          or from front to back - depending on your choice. The plane diagonal is shown in blue (Diagonal2).
                           The angles between the plane diagonal and the horizontal and vertical lines are given in blue."),
                        plotOutput("plot1")
                )
        )
)

)


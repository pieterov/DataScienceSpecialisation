shinyServer(
        
        function(input, output) {
                
                output$plot1 <- renderPlot({
                        
                        ###################################################################
                        # Initialisation of parameters.
                        ###################################################################
                        
                        # General parameters.
                        l <- input$l
                        h <- input$h
                        d <- input$d
                        plane.option <- input$plane.option

                        maxlim <- max(l+d, h+d, sqrt(l^2+h^2), sqrt(d^2+h^2)) + 4
                        
                        par(mfrow=c(1,2), mar=c(0,0,0,0), pty="s")
                        
                        # Plot 1 parameters (left).
                        xa <- c(0, l, l, 0, 0, l, l, 0, l, l+d, l+d, l)
                        ya <- c(0, 0, h, h, h, h, h+d, h+d, 0, 0, h, h)
                        
                        # Plot 2 parameters (right).
                        xb0 <- ifelse(plane.option == "lr", d, l)
                        yb0 <- ifelse(plane.option == "lr", sqrt(l^2+h^2), sqrt(d^2+h^2))
                        
                        xlab <- ifelse(plane.option == "lr", paste("Depth = ", d), paste("Length = ",l))
                        ylab <- paste("Diagonal1 = ", round(yb0, 2))
                        
                        xb <- c(0, xb0, xb0, 0)
                        yb <- c(0, 0, yb0, yb0)
                        
                        ###################################################################
                        # PLOT 1: Draw 3 sides of the box.
                        ###################################################################
                        
                        plot(xa, ya, type = "n", axes=FALSE, xlab="", ylab="", xlim=c(-1,maxlim), ylim=c(-1,maxlim))
                        polygon(xa[1:8], ya[1:8], col="gray", border = "blue", lwd = 3)
                        polygon(xa[9:12], ya[9:12], col="gray", border = "blue", lwd = 3)
                        
                        # Print the lables of the surfaces.
                        text(0.1, h-1, "FRONT", col = "blue", pos = 4)
                        text(0.1, h+d-1, "TOP", col = "blue", pos = 4)
                        text(l+0.1, h-1, "SIDE", col = "blue", pos = 4)
                        
                        # Print the dimensions of the box.
                        text(l/2, -0.75, paste("Length = ", l), col = "blue")
                        text(l+0.1, h+d/2, paste("Depth = ", d), col = "blue", pos = 4)
                        text(l+d+0.1, h/2, paste("Height = ", h), col = "blue", pos = 4)
                        
                        # Print the length of the surface diagonal.
                        text(l+0.1, h+0.75, ylab, col = "red", pos = 4)
                        
                        if (plane.option == "lr") { xlines <- c(0, l) } else { xlines <- c(l, l+d) }
                        lines(xlines, c(0, h), lwd=3, col="red")
                        
                        ###################################################################
                        # PLOT 2: Draw plane inside the box.
                        ###################################################################
                        
                        plot(xb, yb, type = "n", axes=FALSE, xlab="", ylab="", xlim=c(-1,maxlim), ylim=c(-1,maxlim))
                        polygon(xb, yb, col="gray", border = "blue", lwd = 3)
                        
                        # Print the dimensions of the plane.
                        text(xb0/2, -0.75, xlab, col = "blue")
                        text(xb0+0.1, yb0/2, ylab, col = "red", pos = 4)
                        
                        # Plot surface (red) and plane (blue) diagonal.
                        lines(c(0, xb0), c(0, yb0), lwd=3, col="blue")
                        lines(c(xb0, xb0), c(0, yb0), lwd=3, col="red")
                        
                        # Print length plane diagonal and angle.
                        text(xb0+0.1, yb0-1, paste("Diagonal2 = ", round(sqrt(xb0^2+yb0^2), 2)), col = "blue", pos = 4)
                        text(0.5, 0.5, paste(round(atan(yb0/xb0)/pi*180,1),"deg"), col = "blue", pos = 4)
                
                })                
        }

)
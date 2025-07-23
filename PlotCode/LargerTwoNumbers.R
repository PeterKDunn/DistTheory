

plot( x = 1:6,  ### The values for which pmf > 0
      y = c(0, 0.1, 0.2, 0.3, 0.4, 0),
      xlim = c(0.5, 6.6), ylim = c(0, 0.45),
      type = "h",  ### type = "h": vertical lines
      las = 1, lty = 3,  ### lty = 3: Dotted lines 
      axes = FALSE,      ### Supress drawing labelled axes
      col = "grey",
      main = expression( 
        paste( "The probability distribution of ", italic(X)) 
        ),
      xlab = expression( 
        paste("Values of the random variable ", italic(X)) 
        ),
      ylab = expression( 
        paste( "Probability function ", italic(p)[italic(X)](italic(x)) )
      )
)
points( x = 1:6,  ### Adds the points on top of the vertical lines
        y = c(0, 0.1, 0.2, 0.3, 0.4, 0),
        pch = 19)

axis(side = 1, at = 1:6) ### Add axis on bottom (side = 1)
axis(side = 2,           ### Add axis at left (side = 2)
     at = seq(0, 0.4, by = 0.1),
     las = 1)
box() ### Surround plot with a box


plot( x = 1:6,                           # The values for which PMF > 0
      y = c(0, 0.1, 0.2, 0.3, 0.4, 0),   # The values of f(y)
      xlim = c(0.5, 6.6), ylim = c(0, 0.45),
      type = "h",             # type = "h": vertical lines
      lty = 3,                # lty = 3: Dotted lines 
      las = 1,                # las = 1: Axis labels horizontal 
      col = "grey",           # Use the colour grey
      main = "The probability distribution of X",
      xlab = "x",             # Label for horizontal axis
      ylab = "PMF"            # Label for vertical axis
)
points( x = 1:6,  ### Adds the points on top of the vertical lines
        y = c(0, 0.1, 0.2, 0.3, 0.4, 0),
        pch = 19)

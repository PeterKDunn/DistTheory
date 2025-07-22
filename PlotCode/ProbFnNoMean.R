z <- seq(1, 6, 
         length = 100)
plot(x = z,
     y = z^(-2),
     type = "l",
     lwd = 2,
     xlim = c(0, 6),
     ylim = c(-0.025, 1),
     las = 1,
     xlab = expression( italic(z)),
     ylab = "Density",
     main = expression(paste(The~probability~"function for"~italic(Z))))
lines( x = c(-1, 1),
       y = c(0, 0),
       lwd = 2) # Line for x = 0 to x = 1
abline(v = 1,
       lwd = 1,
       col = "grey",
       lty = 2) # Dotted vertical line
points(x = 1,
       y = 0,
       pch = 1) # Show open point
  
  

# Define values of z > 1 to plot over
z <- seq(1, 6, length.out = 100)

# Plot for z > 1
plot(x = z, y = z^(-2),
     type = "l", lwd = 2, las = 1,
     xlim = c(0, 6), ylim = c(-0.025, 1),
     xlab = expression( italic(z)),
     ylab = "Density",
     main = expression(paste(The~probability~"function for"~italic(Z))))

# Line for x = 0 to x = 1
lines( x = c(-1, 1),
       y = c(0, 0),
       lwd = 2) ### lwd = 2: Thicker line width

# Dotted vertical line
abline(v = 1, lty = 2, ### lty = 2: means 'dotted lines' 
       col = "grey") 

# Show open point
points(x = 1, y = 0,
       pch = 1) ### pch = 1: open circle


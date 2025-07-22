dhbsecant <- function(x, mean){
  # Hyperbolic secant distribution

  z <- (x - mean)
  0.5 / cosh(pi * z / 2)
  }


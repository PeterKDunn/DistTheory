dtriangular <- function(x, xMin = 0, xMax = 2, xMode = 1){
  
  if ( (xMin > xMax) | (xMin > xMode) ) stop("Min  must be the smallest parameter.")
  if ( xMax < xMode) stop("Max  must be the largest parameter.")

  xRange <- xMax - xMin
  yHeight <- 2 / xRange

  y <- rep(0, length = length(x) )
  
  xLeft  <- (x > xMin) & (x < xMode)
  xRight <- (x < xMax) & (x >= xMode)
  
  y[ xLeft ]  <- yHeight/(xMode - xMin) * (x[ xLeft ] - xMin)
  y[ xRight ] <- yHeight/(xMax - xMode) * (xMax - x[ xRight ])
  
  return(y = y)
}
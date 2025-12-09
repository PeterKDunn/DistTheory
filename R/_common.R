# Load libraries
library("plot3D")
library("rgl")
library("kableExtra")
library("plotrix")
library("mnormt") # Multivariate normal
library("mnorm")  # Multivariate normal
library("countries") # Sets of countries
library("shape")
library("diagram")
library("plotrix") # draw.circle()
library("fitdistrplus")
library("tweedie")
library("downlit")
library("bookdown")
library("mnormt")

# example R options set globally
options(width = 50)

# example chunk options set globally
knitr::opts_chunk$set(
  comment = "#>",
  size = 'scriptsize',
  class.source = "mycode",
  class.output = "myoutput",
  collapse = TRUE
  )


# Translucent colour:
#plotColour <- rgb(red = 47/256, 
#                  green = 105/256, 
#                  blue = 113/256, 
#                  alpha = 0.6, 
#                  maxColorValue = 1)
plotColour <- "cyan4" # 0, 139, 139

# Solid colour:
#plotColour1 <- rgb(red = 129/256, 
#                   green = 194/256, 
#                   blue = 202/256, 
#                   alpha = 1, 
#                   maxColorValue = 1)
#plotColour1 <- "cyan4" # 0, 139, 139
plotColour1 <- adjustcolor(plotColour, 
                           alpha.f = 0.6)

###
# Redefine  MASS::truehist  to the default colours used
truehist <- function(x, ...) {
  MASS::truehist(x, 
                 col = plotColour1, 
                 ...)
}

###
source("R/triangular.R")
source("R/gnormal.R")
source("R/hbsecant.R")

source("R/kurtosis.R")

source("R/myDeparse.R")
source("R/showHTMLCode.R")

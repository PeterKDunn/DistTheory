# Load libraries
library("plot3D")
library("plotly")
library("plotrix") # draw.circle()
library("diagram")
library("rgl")
library("kableExtra")
library("mnormt") # Multivariate normal
library("mnorm")  # Multivariate normal
library("countries") # Sets of countries
library("shape")
library("fitdistrplus")
library("tweedie")
library("downlit")
library("bookdown")
library("mnormt")
library("sf")
library("MASS")
library("tweedie")


# example R options set globally
options(width = 65)

# example chunk options set globally
knitr::opts_chunk$set(
  comment = "#>",
  size = 'scriptsize',
  class.source = "mycode",
  class.output = "myoutput",
  collapse = TRUE
  )

###
# Redefine  MASS::truehist  to the default colours used
truehist <- function(x, ...) {
  MASS::truehist(x, 
                 col = ColourOpaque, 
                 ...)
}

###
source("R/triangular.R")
source("R/gnormal.R")
source("R/hbsecant.R")

source("R/kurtosis.R")

source("R/myDeparse.R")
source("R/showHTMLCode.R")

source("R/plotDiscrete.R") # Load fn to plot discrete DF and SF


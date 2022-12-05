# Load libraries
library(plot3D)
library(rgl)
library(kableExtra)
library(plotrix)


# example R options set globally
options(width = 50)

# example chunk options set globally
knitr::opts_chunk$set(
  comment = "#>",
  collapse = TRUE
  )


# Translucent colour:
plotColour <- rgb(red = 47/256, 
                  green = 105/256, 
                  blue = 113/256, 
                  alpha = 0.6, 
                  maxColorValue = 1)

# Solid colour:
plotColour1 <- rgb(red = 129/256, 
                   green = 194/256, 
                   blue = 202/256, 
                   alpha = 1, 
                   maxColorValue = 1)

shadeNormal <- function(xx, yy, lo, hi, col="blue"){
  
  xshade <- xx[ (xx >= lo) & ( xx <= hi ) ]
  yshade <- yy[ (xx >= lo) & ( xx <= hi ) ]
  y0 <- rep(0, length(yshade))
  
  polygon( x = c(xshade, rev(xshade) ),
           y = c( yshade, y0),
           col = col)
}
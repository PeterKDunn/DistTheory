plotDiscrete <- function(x, px, type = "DF", pchIN = 19, pchOUT = 1, ...) {
  # Compute CDF and Survival:
  # "DF": distribution fn
  # "SF": survival function
  # "MF": PMF
  
  if ( !(type %in% c("DF", "MF", "SF") ) ) {
    stop('type  must be one of "DF", "SF", "MF"')
  }
  
  Fx <- cumsum(px)
  Sx <- 1 - Fx
  
  # Check for some given parameters supplied via ...
  plot_defaults <- list(
    col = "black",
    las = 1,
    main = if (type == "DF") {
              "Distribution function"
            } else if (type=="MF") {
              "Prob. mass function"
            } else if (type=="SF") {
              "Survival function"
            }, 
    xlab = expression(italic(x)),
    ylab =  if (type == "DF") {
              "Dist. Fn"
            } else if (type=="MF") {
              "Prob. fn"
            } else if (type=="SF") {
              "Survival fn"
            }
  )
  point_defaults <- list(col = "black",
                         cex = 1)
  line_defaults  <- list(col = "black", 
                         lwd = 2)
  
  plot_dots <- modifyList(plot_defaults, 
                          list(...))
  point_dots <- modifyList(point_defaults, 
                           list(...))
  line_dots  <- modifyList(line_defaults, 
                           list(...))
  

  if (type == "DF") {
    y          <- Fx
    y0         <- 0
  } 
  if (type == "SF") {
    y          <- Sx
    y0         <- 1
  }
  if (type == "MF") {
    y          <- px
    y0         <- NA
  }
  
  if ( (type == "DF" ) | ( type == "SF") ) {
    
    do.call( plot,
             c(list( 
               x    = x,
               y    = y,
               type = "n"
               ),
               plot_dots) )
    
    
    # Left tail
    do.call( lines,
             c(list(
               x   = c(min(x) - 2, 
                       min(x)),
               y   = rep(y0, 2)
             ),
             line_dots) )
  }  
  # Point at right end of left tail
  if (type == "DF") {
    do.call(points,
            c(list(
              x   = min(x),
              y   = y0,
              pch = pchOUT
            ),
            point_dots) )
  }
  if (type == "SF") {
    do.call( points,
             c(list(
               x   = min(x),
               y   = y0,
               pch = pchIN
             ),
             point_dots) )
  }
  
  if ( (type == "DF") | (type == "SF") )  {
    for (i in seq_along(x)) {
      # Horizontal segment
      step <- x[i+1] - x[i]
      if (i > 1) {
        do.call(lines,
                c(list(
                  x   = c(x[i - 1], 
                          x[i]),
                  y   = rep(y[i - 1], 2)
                ),
                line_dots) )
      }
      
      if (type == "DF") {
        # Closed point at top of jump
        do.call(points,
                c(list(
                  x   = x[i],
                  y   = y[i],
                  pch = pchIN
                ),
                point_dots) )
        # Open point at right end of segment (not needed after last jump)
        if (i < length(x)) {
          do.call(points,
                  c(list(
                    x   = x[i] + step,
                    y   = y[i],
                    pch = pchOUT
                  ),
                  point_dots) )
                  }
        # Right tail
        do.call( lines,
                 c(list(
                   x   = c(max(x), 
                           max(x) + 2),
                   y   = rep(y[length(y)], 2)
                 ),
              line_dots) ) 
      } 
      if (type == "SF") {
        # Closed point at left of jump
        if (i > 1) {
          do.call(points,
                  c(list(
                    x   = x[i],
                    y   = y[i - 1],
                    pch = pchIN
                  ),
                 point_dots) )
        }
        # Open point at right of jump
        do.call( points,
                 c(list(x   = x[i],
                        y   = y[i],
                        pch = pchOUT
                        ),
                   point_dots) )
        # Right tail
        do.call(lines,
                c(list(
                  x   = c(max(x),
                          max(x) + 2),
                  y   = rep(y[length(y)], 2)
                  ),
                  line_dots) )
      }
    }
  }
  if (type == "MF") {
    do.call(plot,
            c(list(
              x    = x,
              y    = y,
              type = "n"
            ),
            plot_dots) )
    
    do.call(lines,
            c(list( x   = x,
                    y   = y,
                    type = "h",
                    lty = 2,
                    pch = pchIN
            ),
          line_dots) )
    do.call(points,
            c(list(
              x   = x,
              y   = y,
              pch = pchIN
            ),
           line_dots) )
  }
  
}

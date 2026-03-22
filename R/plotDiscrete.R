plotDiscrete <- function(x, px, type = "DF", main = NULL, ylim = NULL, xlim = NULL, ...) {
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
    main = if (type == "DF") {
              "Distribution function"
            } else if (type=="MF") {
              "Prob. mass function"
            } else if (type=="SF") {
              "Survival function"
            }, 
    xlab = expression(x),
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
                         lwd = 1)
  
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
    
    plot(x    = x,
         y    = y,
         type = "n",
         main = main_title,
         las  = 1,
         plot_dots)
    
    
    # Left tail
    lines(x   = c(min(x) - 2, 
                  min(x)),
          y   = rep(y0, 2),
          lwd = 2,
          line_dots)
  }  
  # Point at right end of left tail
  if (type == "DF") {
    points(x   = min(x),
           y   = y0,
           pch = 1,
           point_dots)
  }
  if (type == "SF") {
    points(x   = min(x),
           y   = y0,
           pch = 19,
           point_dots)
  }
  
  if ( (type == "DF") | (type == "SF") )  {
    for (i in seq_along(x)) {
      # Horizontal segment
      step <- x[i+1] - x[i]
      if (i > 1) {
        lines(x   = c(x[i - 1], 
                      x[i]),
              y   = rep(y[i - 1], 2),
              line_dots)
      }
      
      if (type == "DF") {
        # Closed point at top of jump
        points(x   = x[i],
               y   = y[i],
               pch = 19)
        # Open point at right end of segment (not needed after last jump)
        if (i < length(x)) {
          points(x   = x[i] + step,
                 y   = y[i],
                 pch = 1)
        }
        # Right tail
        lines(x   = c(max(x), 
                      max(x) + 2),
              y   = rep(y[length(y)], 2),
              line_dots)
      } 
      if (type == "SF") {
        # Closed point at left of jump
        if (i > 1) {
          points(x   = x[i],
                 y   = y[i - 1],
                 pch = 19,
                 point_dots)
        }
        # Open point at right of jump
        points(x   = x[i],
               y   = y[i],
               pch = 1,
               point_dots)
        # Right tail
        lines(x   = c(max(x), 
                      max(x) + 2),
              y   = rep(y[length(y)], 2),
              lwd = 2)
      }
    }
  }
  if (type == "MF") {
    plot(x    = x,
         y    = y,
         type = "n",
         main = main_title,
         las  = 1,
         plot_dots)
    
    lines(x   = x,
          y   = y,
          type = "h",
          pch = 19,
          line_dots)
    points(x   = x,
           y   = y,
           pch = 19,
           line_dots)
  }
  
}

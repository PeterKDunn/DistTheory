plotDiscreteCDF <- function(x, px, type = "DF", main = NULL,...) {
  # Compute CDF and Survival:
  # "DF": distribution fn
  # "SF": survival function
  # "MF": PMF
  
  Fx <- cumsum(px)
  Sx <- 1 - Fx
  
  if (type == "DF") {
    y          <- Fx
    y0         <- 0
    main_title <- ifelse(is.null(main), 
                         "Distribution function", 
                         main)
    ylab_text  <- expression(F(x))
  } 
  if (type == "SF") {
    y          <- Sx
    y0         <- 1
    main_title <- ifelse(is.null(main), 
                         "Distribution function", 
                         main)
    ylab_text  <- expression(S(x))
  }
  if (type == "MF") {
    y          <- px
    y0         <- NA
    main_title <- ifelse(is.null(main), 
                         "Prob. mass function", 
                         main)
    ylab_text  <- expression(p(x))
  }
  
  if ( (type == "DF" ) | ( type == "SF") ) {
    
    plot(x    = x,
         y    = y,
         type = "n",
         xlab = "x",
         ylab = ylab_text,
         main = main_title,
         las  = 1,
         xlim = c(min(x) - 1, 
                  max(x) + 1),
         ylim = c(0, 1),
         ...)
    
    
    # Left tail
    lines(x   = c(min(x) - 2, 
                  min(x)),
          y   = rep(y0, 2),
          lwd = 2)
  }  
  # Point at right end of left tail
  if (type == "DF") {
    points(x   = min(x),
           y   = y0,
           pch = 1)
  }
  if (type == "SF") {
    points(x   = min(x),
           y   = y0,
           pch = 19)
  }
  
  if ( (type == "DF") | (type == "SF") )  {
    for (i in seq_along(x)) {
      # Horizontal segment
      step <- x[i+1] - x[i]
      if (i > 1) {
        lines(x   = c(x[i - 1], 
                      x[i]),
              y   = rep(y[i - 1], 2),
              lwd = 2)
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
              lwd = 2)
      } 
      if (type == "SF") {
        # Closed point at left of jump
        if (i > 1) {
          points(x   = x[i],
                 y   = y[i - 1],
                 pch = 19)
        }
        # Open point at right of jump
        points(x   = x[i],
               y   = y[i],
               pch = 1)
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
         xlab = "x",
         ylab = ylab_text,
         main = main_title,
         las  = 1,
         xlim = c(min(x) - 1, 
                  max(x) + 1),
         ylim = c(0, 1),
         ...)
    
    lines(x   = x,
          y   = y,
          type = "h",
          lty = 2,
          pch = 19)
    points(x   = x,
           y   = y,
           type = "p",
           pch = 19)
  }
  
}

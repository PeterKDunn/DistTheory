plotDiscrete <- function(x, 
                         px, 
                         type = "DF", 
                         pchIN = 19, 
                         pchOUT = 1, ...) {
  # Compute CDF and Survival:
  # "DF": distribution fn
  # "SF": survival function
  # "MF": PMF
  # "QF": quantile function
  # pchIN, pchOUT: the ploiting characters to show that an exact value
  #                INcluded or OUTcluded 
  
  if ( !(type %in% c("DF", "MF", "SF", "QF") ) ) {
    stop('type  must be one of "DF", "SF", "MF" "QF"')
  }
  
  # Parameters valid for plot() but NOT for lines()/points()
  plot_only_params <- c("axes", "asp", "main", "sub", "xlab", "ylab",
                        "xlim", "ylim", "log", "las", "frame.plot",
                        "xaxt", "yaxt")
  
  dots <- list(...)
  
  Fx <- cumsum(px)
  Sx <- 1 - Fx
  
  # Check for some given parameters supplied via ...
  plot_defaults <- list(
    col = "black",
    las = 1,
    ylim = switch(type,
                  DF = c(0, 1.05),
                  SF = c(0, 1.05),
                  MF = c(0,
                         max(px, na.rm = TRUE) * 1.05),
                  QF = c( min(x) * 0.95,
                          max(x) * 1.05)
    ),
    xlim = switch(type,
                  DF = c(min(x) * 0.95,
                         max(x) * 1.05),
                  SF = c(min(x) * 0.95,
                         max(x) * 1.05),
                  MF = c(min(x) * 0.95,
                         max(x) * 1.05),
                  QF = c(-0.05, 1.05)
    ),
    main = switch(type,
                  DF = "Distribution function",
                  MF = "Prob. mass function",
                  SF = "Survival function",
                  QF = "Quantile function"
    ),
    ylab = switch(type,
                  DF = "Dist. fn",
                  MF = "Prob. fn",
                  SF = "Survival fn",
                  QF = "Quantile fn"
    ),
    xlab = expression(italic(x))
  )
  point_defaults <- list(col = "black",
                         cex = 1)
  line_defaults  <- list(col = "black", 
                         lwd = 2)
  
  plot_dots  <- modifyList(plot_defaults, 
                           dots)
  
  # Strip plot-only params before passing to lines() / points()
  safe_dots      <- dots[ setdiff(names(dots), 
                                  plot_only_params) ]
  point_dots <- modifyList(point_defaults, 
                           safe_dots)
  line_dots  <- modifyList(line_defaults, 
                           safe_dots)

  if (type == "DF") {
    y    <- Fx
    y0   <- 0
  } 
  if (type == "SF") {
    y    <- Sx
    y0   <- 1
  }
  if (type == "MF") {
    y    <- px
    y0   <- NA
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
  
  if (type == "QF") {
    # p-values at which the quantile function jumps
    # Q(p) = min{x : F(x) >= p}
    p_vals <- c(0, Fx)   # jump points on probability axis
    
    do.call(plot,
            c(list(
              x    = c(0, 1),
              y    = c(min(x), max(x)),
              type = "n",
              xlab = expression(italic(p)),
              ylab = plot_dots$ylab,
              main = plot_dots$main,
              las  = plot_dots[["las"]]
            ),
            list()) )
    
    # Left tail: Q(p) = -Inf for p = 0, just show flat line at min(x)
    do.call(lines,
            c(list(x = c(0, p_vals[1] + 1e-9),
                   y = c(x[1], x[1])),
              line_dots))
    
    for (i in seq_along(x)) {
      p_lo <- p_vals[i]
      p_hi <- p_vals[i + 1]
      
      # Horizontal segment at height x[i]
      do.call(lines,
              c(list(x = c(p_lo, p_hi),
                     y = c(x[i], x[i])),
                line_dots))
      
      # Closed point at left of segment (Q is left-continuous... 
      # actually right-continuous for the standard definition)
      do.call(points,
              c(list(x = p_lo,
                     y = x[i],
                     pch = pchIN),
                point_dots))
      
      # Open point at right end (except last)
      if (i < length(x)) {
        do.call(points,
                c(list(x = p_hi,
                       y = x[i],
                       pch = pchOUT),
                  point_dots))
      }
    }
    
    # Right tail: closed point at p = 1
    do.call(points,
            c(list(x = 1,
                   y = max(x),
                   pch = pchIN),
              point_dots))
  }
}

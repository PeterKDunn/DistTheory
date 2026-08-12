plotContinuous <- function(pdf_fn,   # density function
                           df_fn,    # distribution function
                           q_fn,     # quantile function
                           type = "DF",
                           showx = c(-4, 4),
                           n = 2000,  # Number of points between range(showx)
                           fill = NA,
                           lo = min(showx), hi = max(showx), # Shade between lo and hi 
                           ...) {
  
  if (!(type %in% c("DF", "PDF", "SF", "QF"))) {
    stop('type must be one of "DF", "PDF", "SF", "QF"')
  }
  
  if (type == "PDF" && is.null(pdf_fn)) stop('type = "PDF" requires pdf_fn')
  if (type %in% c("DF", "SF") && is.null(df_fn)) stop('type = "DF"/"SF" requires df_fn')
  if (type == "QF" && is.null(q_fn)) stop('type = "QF" requires q_fn')
  
  plot_only_params <- c("axes", "asp", "main", "sub", "xlab", "ylab",
                        "xlim", "ylim", "log", "las", "frame.plot",
                        "xaxt", "yaxt")
  
  dots <- list(...)
  
  xx <- seq(showx[1],
            showx[2],
            length.out = n)
  plot_defaults <- list(
    col  = "black",
    lwd  = 2,
    las  = 1,
    
    xlim = switch(type,
                  PDF = showx,
                  DF  = showx,
                  SF  = showx,
                  QF  = c(0, 1)),
    
    ylim = switch(type,
                  PDF = c(0,
                          max(pdf_fn(xx), na.rm = TRUE) * 1.05),
                  DF  = c(0, 1.05),
                  SF  = c(0, 1.05),
                  QF  = range(q_fn(seq(0.001,
                                       0.999,
                                       length.out = n)),
                              na.rm = TRUE)),
    
    main = switch(type,
                  PDF = "Density function",
                  DF  = "Distribution function",
                  SF  = "Survival function",
                  QF  = "Quantile function"),
    
    xlab = if (type == "QF")
      expression(italic(p))
    else
      expression(italic(x)),
    
    ylab = switch(type,
                  PDF = "Density",
                  DF  = "Dist. fn",
                  SF  = "Survival fn",
                  QF  = "Quantile fn")
  )
  
  plot_dots <- modifyList(plot_defaults,
                          dots)
  
  if (type == "PDF") yy <- pdf_fn(xx)
  if (type == "DF")  yy <- df_fn(xx)
  if (type == "SF")  yy <- 1 - df_fn(xx)
  if (type == "QF"){
    pp <- seq(0, 1, length.out = n)
    qq <- q_fn(pp)
  }
  
  
  ### Canvas
  if (type == "QF"){
    do.call(plot,
            c(list(x = pp,
                   y = qq,
                   type = "n"),
              plot_dots))
  } else {
    do.call(plot,
            c(list(x = xx,
                   y = yy,
                   type = "n"),
              plot_dots))
  }
  
  
  ### Shading: do first to overplot lines
  if ( !(is.na(lo) & is.na(hi)) ) {               # If both are NA, then don't proceed
    if ( is.na(lo) & !is.na(hi)) lo <- min(showx) # If only hi, take lo as the smallest value to show
    if ( !is.na(lo) & is.na(hi)) hi <- max(showx) # If only lo, take hi as the largest value to show

    inner <- (xx >= lo) & (xx <= hi)

    if (type == "QF") {
      yyy <- qq 
    } else {
      yyy <- yy
    }
    
    polygon( x = c( xx[inner], 
                    rev(xx[inner])),
             y = c( yyy[inner], 
                    rep(0, sum(inner))),
             col = fill)
  }
  
  
  ### Now the lines
  if (type == "PDF") {
    
    do.call(lines,
            c(list(x = xx,
                   y = yy,
                   type = "l"),
              plot_dots))
    if (!is.na(fill)) {
      polygon(
        x = c(xx, rev(xx)),
        y = c(yy, rep(0, length(xx))),
        col = NA,
        border = NA
      )
    }
    
    lines(xx, yy,
          col = plot_dots$col,
          lwd = plot_dots$lwd)
    
  } else if (type == "DF") {
    
    do.call(lines,
            c(list(x = xx,
                   y = yy,
                   type = "l"),
              plot_dots))
    
    if (!is.na(fill)) {
      polygon(
        x = c(xx, rev(xx)),
        y = c(yy, rep(0, length(xx))),
        col = NA,
        border = NA
      )
    }
    
    lines(xx, yy,
          col = plot_dots$col,
          lwd = plot_dots$lwd)
    
  } else if (type == "SF") {
    
    do.call(lines,
            c(list(x = xx,
                   y = yy,
                   type = "l"),
              plot_dots))
    if (!is.na(fill)) {
      polygon(
        x = c(xx, rev(xx)),
        y = c(yy, rep(0, length(xx))),
        col = NA,
        border = NA
      )
    }
    
    lines(xx, yy,
          col = plot_dots$col,
          lwd = plot_dots$lwd)
    
  } else if (type == "QF") {
    
    do.call(lines,
            c(list(x = pp,
                   y = qq,
                   type = "l"),
              plot_dots))
    
    if (!is.na(fill)) {
      polygon(
        x = c(pp, rev(pp)),
        y = c(qq, rep(min(qq), length(pp))),
        col = NA,
        border = NA
      )
    }
    
    lines(pp, qq,
          col = plot_dots$col,
          lwd = plot_dots$lwd)
  }
  

  
  invisible(NULL)
}


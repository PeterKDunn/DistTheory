plotContinuous <- function(pdf_fn,   # density function
                           df_fn,    # distribution function
                           q_fn,     # quantile function
                           type = "DF",
                           support = c(-4, 4),
                           n = 1000,
                           fill = NA,
                           ...) {
  
  if (!(type %in% c("DF", "PDF", "SF", "QF"))) {
    stop('type must be one of "DF", "PDF", "SF", "QF"')
  }
  
  plot_only_params <- c("axes", "asp", "main", "sub", "xlab", "ylab",
                        "xlim", "ylim", "log", "las", "frame.plot",
                        "xaxt", "yaxt")
  
  dots <- list(...)
  
  xx <- seq(support[1],
            support[2],
            length.out = n)
  
  plot_defaults <- list(
    col  = "black",
    lwd  = 2,
    las  = 1,
    
    xlim = switch(type,
                  PDF = support,
                  DF  = support,
                  SF  = support,
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
  
  if (type == "PDF") {
    
    yy <- pdf_fn(xx)
    
    do.call(plot,
            c(list(x = xx,
                   y = yy,
                   type = "l"),
              plot_dots))
    if (!is.na(fill)) {
      polygon(
        x = c(xx, rev(xx)),
        y = c(yy, rep(0, length(xx))),
        col = fill,
        border = NA
      )
    }
    
    lines(xx, yy,
          col = plot_dots$col,
          lwd = plot_dots$lwd)
    
  } else if (type == "DF") {
    
    yy <- df_fn(xx)
    
    do.call(plot,
            c(list(x = xx,
                   y = yy,
                   type = "l"),
              plot_dots))
    
    if (!is.na(fill)) {
      polygon(
        x = c(xx, rev(xx)),
        y = c(yy, rep(0, length(xx))),
        col = fill,
        border = NA
      )
    }
    
    lines(xx, yy,
          col = plot_dots$col,
          lwd = plot_dots$lwd)
    
  } else if (type == "SF") {
    
    yy <- 1 - df_fn(xx)
    
    do.call(plot,
            c(list(x = xx,
                   y = yy,
                   type = "l"),
              plot_dots))
    if (!is.na(fill)) {
      polygon(
        x = c(xx, rev(xx)),
        y = c(yy, rep(0, length(xx))),
        col = fill,
        border = NA
      )
    }
    
    lines(xx, yy,
          col = plot_dots$col,
          lwd = plot_dots$lwd)
    
  } else if (type == "QF") {
    
    pp <- seq(0, 1, length.out = n)
    qq <- q_fn(pp)
    
    do.call(plot,
            c(list(x = pp,
                   y = qq,
                   type = "l"),
              plot_dots))
    
    if (!is.na(fill)) {
      polygon(
        x = c(pp, rev(pp)),
        y = c(qq, rep(min(qq), length(pp))),
        col = fill,
        border = NA
      )
    }
    
    lines(pp, qq,
          col = plot_dots$col,
          lwd = plot_dots$lwd)
  }
  
  invisible(NULL)
}


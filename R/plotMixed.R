plotMixed <- function(pdf_fn = NULL,
                      df_fn  = NULL,
                      q_fn   = NULL,
                      p0,
                      type = "DF",
                      support = c(-4, 4),
                      n = 2000,
                      fill = NA,
                      ...) {
  if (!(type %in% c("DF", "PDF", "SF", "QF"))) {
    stop('type must be one of "DF", "PDF", "SF", "QF"')
  }
  if (type == "PDF" && is.null(pdf_fn)) stop('type = "PDF" requires pdf_fn')
  if (type %in% c("DF", "SF") && is.null(df_fn)) stop('type = "DF"/"SF" requires df_fn')
  if (type == "QF" && is.null(q_fn)) stop('type = "QF" requires q_fn')
  
  dots <- list(...)
  xx <- seq(support[1], support[2], length.out = n)
  
  plot_defaults <- list(
    col = "black",
    lwd = 2,
    las = 1,
    xlim = switch(type,
                  PDF = support,
                  DF  = support,
                  SF  = support,
                  QF  = c(0, 1)
    ),
    ylim = switch(type,
                  PDF = c(0, max(pdf_fn(xx[xx != 0]), na.rm = TRUE) * 1.05),
                  DF  = c(0, 1.05),
                  SF  = c(0, 1.05),
                  QF  = range(q_fn(seq(0.001, 0.999, length.out = n)), na.rm = TRUE)
    ),
    main = switch(type,
                  PDF = "Density function",
                  DF  = "Distribution function",
                  SF  = "Survival function",
                  QF  = "Quantile function"
    ),
    xlab = if (type == "QF") expression(italic(p)) else expression(italic(x)),
    ylab = switch(type,
                  PDF = "Density",
                  DF  = "Dist. fn",
                  SF  = "Survival fn",
                  QF  = "Quantile fn"
    )
  )
  plot_dots <- modifyList(plot_defaults, dots)
  
  if (type == "PDF") {
    xx_cont <- xx[xx != 0]
    yy <- pdf_fn(xx_cont)
    do.call(plot, 
            c(list(x = xx_cont, 
                   y = yy, 
                   type = "l"), plot_dots))
    if (!is.na(fill)) {
      polygon(
        x = c(xx_cont, rev(xx_cont)),
        y = c(yy, rep(0, length(xx_cont))),
        col = fill, border = NA
      )
    }
    lines(xx_cont, 
          yy, 
          col = plot_dots$col, 
          lwd = plot_dots$lwd)
    points(0, 
           p0, 
           pch = 16, 
           col = plot_dots$col)
    
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
        col = fill, border = NA
      )
    }
    lines(xx[xx < 0],  
          yy[xx < 0],  
          col = plot_dots$col, 
          lwd = plot_dots$lwd)
    lines(xx[xx >= 0], 
          yy[xx >= 0], 
          col = plot_dots$col, 
          lwd = plot_dots$lwd)
    y_left  <- df_fn(0) - p0
    y_right <- df_fn(0)
    points(0, 
           y_left,  
           pch = 1,  
           col = plot_dots$col)
    points(0, 
           y_right, 
           pch = 16, 
           col = plot_dots$col)
    
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
        col = fill, border = NA
      )
    }
    lines(xx[xx < 0],  
          yy[xx < 0],  
          col = plot_dots$col, 
          lwd = plot_dots$lwd)
    lines(xx[xx >= 0], 
          yy[xx >= 0], 
          col = plot_dots$col, 
          lwd = plot_dots$lwd)
    y_left  <- 1 - (df_fn(0) - p0)
    y_right <- 1 - df_fn(0)
    points(0, 
           y_left,  
           pch = 16, 
           col = plot_dots$col)
    points(0, 
           y_right, 
           pch = 1,  
           col = plot_dots$col)
    
  } else if (type == "QF") {
    pp <- seq(0, 1, length.out = n)
    F0_minus <- df_fn(0) - p0
    F0_plus  <- df_fn(0)
    qq <- numeric(n)
    below <- pp < F0_minus
    flat  <- pp >= F0_minus & pp <= F0_plus
    above <- pp > F0_plus
    qq[below] <- q_fn(pp[below])
    qq[flat]  <- 0
    qq[above] <- q_fn(pp[above])
    do.call(plot, 
            c(list(x = pp, 
                   y = qq, 
                   type = "l"), 
              plot_dots))
    if (!is.na(fill)) {
      polygon(
        x = c(pp, rev(pp)),
        y = c(qq, rep(min(qq), length(pp))),
        col = fill, border = NA
      )
    }
    lines(pp, 
          qq, 
          col = plot_dots$col, 
          lwd = plot_dots$lwd)
  }
  
  invisible(NULL)
}
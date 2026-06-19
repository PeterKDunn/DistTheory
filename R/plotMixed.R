plotMixed <- function(pdf_fn,    # density function of the continuous part (conditional on X != 0)
                      df_fn,    # distribution function of the continuous part
                      q_fn,     # quantile function of the continuous part
                      p0,       # P(X = 0), the point mass at zero
                      type = "DF",
                      support = c(-4, 4),
                      n = 1000,
                      fill = NA,
                      ...) {
  if (!(type %in% c("DF", "PDF", "SF", "QF"))) {
    stop('type must be one of "DF", "PDF", "SF", "QF"')
  }
  if (p0 < 0 || p0 > 1) {
    stop("p0 must be between 0 and 1")
  }
  
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
                  PDF = c(0, max((1 - p0) * pdf_fn(xx[xx != 0]), na.rm = TRUE) * 1.05),
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
    # continuous part scaled by (1 - p0); point mass at 0 shown separately
    xx_cont <- xx[xx != 0]
    yy <- (1 - p0) * pdf_fn(xx_cont)
    do.call(plot, c(list(x = xx_cont, y = yy, type = "l"), plot_dots))
    if (!is.na(fill)) {
      polygon(
        x = c(xx_cont, rev(xx_cont)),
        y = c(yy, rep(0, length(xx_cont))),
        col = fill, border = NA
      )
    }
    lines(xx_cont, yy, col = plot_dots$col, lwd = plot_dots$lwd)
    # point mass at 0, drawn as a vertical segment + point
    segments(x0 = 0, y0 = 0, x1 = 0, y1 = p0, col = plot_dots$col, lwd = plot_dots$lwd)
    points(0, p0, pch = 16, col = plot_dots$col)
    
  } else if (type == "DF") {
    # F(x) = (1-p0)*F_cont(x) for x < 0; jumps by p0 at x = 0; continues for x > 0
    yy <- (1 - p0) * df_fn(xx)
    yy[xx >= 0] <- yy[xx >= 0] + p0
    do.call(plot, c(list(x = xx, y = yy, type = "l"), plot_dots))
    if (!is.na(fill)) {
      polygon(
        x = c(xx, rev(xx)),
        y = c(yy, rep(0, length(xx))),
        col = fill, border = NA
      )
    }
    lines(xx, yy, col = plot_dots$col, lwd = plot_dots$lwd)
    # mark the jump at 0
    y_left  <- (1 - p0) * df_fn(0)
    y_right <- y_left + p0
    points(0, y_left,  pch = 1,  col = plot_dots$col)
    points(0, y_right, pch = 16, col = plot_dots$col)
    
  } else if (type == "SF") {
    yy <- 1 - ((1 - p0) * df_fn(xx))
    yy[xx >= 0] <- yy[xx >= 0] - p0
    do.call(plot, c(list(x = xx, y = yy, type = "l"), plot_dots))
    if (!is.na(fill)) {
      polygon(
        x = c(xx, rev(xx)),
        y = c(yy, rep(0, length(xx))),
        col = fill, border = NA
      )
    }
    lines(xx, yy, col = plot_dots$col, lwd = plot_dots$lwd)
    y_left  <- 1 - (1 - p0) * df_fn(0)
    y_right <- y_left - p0
    points(0, y_left,  pch = 16, col = plot_dots$col)
    points(0, y_right, pch = 1,  col = plot_dots$col)
    
  } else if (type == "QF") {
    pp <- seq(0, 1, length.out = n)
    # the jump at 0 occupies the interval [F(0-), F(0)] in p-space,
    # i.e. probabilities between (1-p0)*F_cont(0) and (1-p0)*F_cont(0)+p0
    F0_minus <- (1 - p0) * df_fn(0)
    F0_plus  <- F0_minus + p0
    qq <- numeric(n)
    below <- pp < F0_minus
    flat  <- pp >= F0_minus & pp <= F0_plus
    above <- pp > F0_plus
    qq[below] <- q_fn(pp[below] / (1 - p0))
    qq[flat]  <- 0
    qq[above] <- q_fn((pp[above] - p0) / (1 - p0))
    do.call(plot, c(list(x = pp, y = qq, type = "l"), plot_dots))
    if (!is.na(fill)) {
      polygon(
        x = c(pp, rev(pp)),
        y = c(qq, rep(min(qq), length(pp))),
        col = fill, border = NA
      )
    }
    lines(pp, qq, col = plot_dots$col, lwd = plot_dots$lwd)
  }
  
  invisible(NULL)
}
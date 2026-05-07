### DEFINE COLOURS

if ( knitr::is_latex_output() ) {
  plotColour  <- "black"
  plotColour1 <- adjustcolor(plotColour, 
                             alpha.f = 0.6)
} else {
  plotColour <- "cyan4" # 0, 139, 139
  plotColour1 <- adjustcolor(plotColour, 
                             alpha.f = 0.6)
}


### DEFINE COLOURS

if ( knitr::is_latex_output() ) {
  ColourSolid  <- "black"
  ColourLight  <- "darkgrey"
  ColourOpaque <- adjustcolor(ColourSolid, 
                              alpha.f = 0.6)
} else {
  ColourSolid <- "cyan4" # 0, 139, 139
  ColourLight <- "darkslategray3"
  ColourOpaque <- adjustcolor(ColourSolid, 
                              alpha.f = 0.6)
}

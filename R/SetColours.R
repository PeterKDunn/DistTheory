### DEFINE COLOURS

if ( knitr::is_latex_output() ) {
  ColourSolid  <- "black"
  ColourOpaque <- adjustcolor(ColourSolid, 
                             alpha.f = 0.6)
} else {
  ColourSolid <- "cyan4" # 0, 139, 139
  ColourOpaque <- adjustcolor(ColourSolid, 
                             alpha.f = 0.6)
}


### DEFINE COLOURS

if ( knitr::is_latex_output() ) {
  ColourSolid  <- "black"
  ColourLight  <- "darkgrey"
  ColourVeryLight  <- "lightgrey"
} else {
  ColourSolid <- "cyan4" # 0, 139, 139
  ColourLight <- "lightcyan3"
  ColourVeryLight <- "lightcyan"
}
ColourOpaque <- adjustcolor(ColourSolid, 
                            alpha.f = 0.6)
ColourLightOpaque <- adjustcolor(ColourLight, 
                            alpha.f = 0.6)
ColourVeryLightOpaque <- adjustcolor(ColourVeryLight, 
                                 alpha.f = 0.6)


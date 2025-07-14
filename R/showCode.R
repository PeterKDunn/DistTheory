# Place this function definition in a setup chunk, e.g., in index.Rmd
# or in a file like _common.R that is sourced by your book.

show_Code <- function(code_to_display) {
  if( knitr::is_latex_output()){
    # Capture the expression passed to the function without evaluating it
    code_expr <- substitute(code_to_display)
    
    # Convert the expression into a character string,
    # preserving line breaks and indentation
    code_string <- paste(deparse(code_expr), collapse = "\n")
    
    # Remove the outer curly braces if the code was passed in {}
    # (which is good practice for multi-line code)
    if (startsWith(trimws(code_string), "{") && endsWith(trimws(code_string), "}")) {
      code_string <- substr(code_string, 2, nchar(code_string) - 1)
    }
    
    # Use cat() with results='asis' to print raw Markdown directly to the output.
    # The 'r' after the backticks indicates it's an R code block for highlighting.
    cat("```r\n")
    cat(code_string)
    cat("\n```\n")
  }
}


#########################


plot_text_in_figure <- function(text, bg_col = "grey90", text_col = "black",
                                cex = 1, mar = c(2,2,2,2), indent = 2, ps = 10, ...) {
  # Split text into lines
  lines <- strsplit(text, "\n")[[1]]
  
  # Trim leading/trailing whitespace of each line
  lines <- trimws(lines, which = "right")
  
  # Indent all but the first line by 'indent' spaces
  if(length(lines) > 1) {
    lines[-1] <- paste0(strrep(" ", indent), lines[-1])
  }
  
  # Setup plot area
  op <- par(mar = mar, 
            ps = ps)
  on.exit(par(op))
  
  plot.new()
  plot.window(xlim = c(0,1), ylim = c(0,1) )
  
  # Draw background rectangle
  rect(0, 0, 1, 1, col = bg_col, border = NA)
  
  # Calculate y positions for lines to center vertically
  n <- length(lines)
  y_positions <- seq(from = 1 - 0.1, to = 0.1, length.out = n)
  
  # Draw text lines, left aligned with a small x offset for indent
  x_indent <- 0.0
  for(i in seq_along(lines)) {
    # For first line, no indent, so smaller x
    x_pos <- if(i == 1) x_indent / 2 else x_indent
    text(x = x_pos, y = y_positions[i], labels = lines[i], cex = cex,
         col = text_col, adj = 0, family = "mono", ...)
  }
  
  invisible(NULL)
}


###################################################

compute_heights_for_code_plot <- function(code_text,
                                          total_height_in = 4.5, 
                                          min_code_height_in = 1.5, 
                                          line_height_in = 0.3,
                                          padding_in = 0.3) {
  # Count lines in code
  lines <- strsplit(code_text, "\n")[[1]]
  line_count <- length(lines)
  
  # Calculate needed code height
  code_height <- max(min_code_height_in, line_count * line_height_in + padding_in)
  
  # Plot height = total height - code height
  plot_height <- total_height_in - code_height
  
  if(plot_height <= 0) {
    warning("Total height too small for given code text. Adjusting plot height to minimum 1 inch.")
    plot_height <- 1
    code_height <- total_height_in - plot_height
  }
  
  list(total_height = total_height_in,
       heights = c(plot_height, code_height))
}



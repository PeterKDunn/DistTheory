wrap_Code <- function(code_string, max_line_length = 60, indent = "  ") {
  # Pre-clean any leading or trailing whitespace and prompts
  code_string <- gsub("^\\s*>\\s*", "", code_string)
  code_string <- trimws(code_string)
  
  # Split into expressions
  exprs <- unlist(strsplit(code_string, ";|(?<=\\))\\s*\n", perl = TRUE))
  exprs <- unlist(strsplit(exprs, "\n(?=\\S)", perl = TRUE))  # handle multiline
  
  result <- character()
  
  for (expr in exprs) {
    expr <- trimws(expr)
    if (nchar(expr) == 0) next
    
    # Remove any trailing prompt from the expression
    expr <- sub("\\s*>\\s*$", "", expr)
    
    # Split at commas, keeping commas
    parts <- unlist(strsplit(expr, "(?<=,)", perl = TRUE))
    lines <- character()
    current <- ""
    
    for (part in parts) {
      part <- trimws(part)
      
      # ✅ Remove > at end of each part
      part <- sub("\\s*>\\s*$", "", part)
      
      candidate <- if (nchar(current) == 0) part else paste(current, part)
      
      if (nchar(candidate) > max_line_length) {
        lines <- c(lines, current)
        current <- part
      } else {
        current <- candidate
      }
    }
    
    if (nchar(current) > 0) {
      lines <- c(lines, current)
    }
    
    # Indent continuation lines
    if (length(lines) > 1) {
      lines <- c(lines[1], paste0(indent, lines[-1]))
    }
    
    result <- c(result, lines)
  }
  
  # Final cleanup
  result <- result[nzchar(result)]
  result <- sub("\\s*>\\s*$", "", result)             # Remove > from any lines
  result[length(result)] <- sub("\\s*>\\s*$", "", result[length(result)])  # Final guard
  
  return(result)
}




#####################################################################################################

draw_code_box <- function(code_lines,
                          line_height = 1.2,
                          font_size = 0.75,
                          bg_color = "grey95",
                          text_color = "black",
                          border_color = "grey70",
                          padding = 0.5) {
  n_lines <- length(code_lines)
  box_height <- n_lines * line_height + 2 * padding
  box_width <- max(nchar(code_lines)) * 0.12 + 2 * padding
  
  # Set up plot area
  old_par <- par(mar = c(0, 0, 0, 0))
  on.exit(par(old_par), add = TRUE)
  
  plot.new()
  plot.window(xlim = c(0, box_width), ylim = c(0, box_height))
  
  # Draw background box
  rect(0, 0, box_width, box_height, col = bg_color, border = border_color)
  
  # Plot each line of text
  y_start <- box_height - padding - line_height/2
  for (i in seq_along(code_lines)) {
    y <- y_start - (i - 1) * line_height
    text(x = padding, y = y, labels = code_lines[i],
         adj = c(0, 0.5), family = "mono", cex = font_size, col = text_color)
  }
}


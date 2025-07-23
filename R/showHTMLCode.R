# Helper functions to format code for HTML <pre><code> blocks

show_Code <- function(path) {
  lines <- readLines(path, 
                     warn = FALSE)
  
  # Escape HTML special chars
  lines <- gsub("&", 
                "&amp;", 
                lines)
  lines <- gsub("<", 
                "&lt;", 
                lines)
  lines <- gsub(">", 
                "&gt;", 
                lines)
  
  lines <- c('<pre><code class="r">\n', 
             lines, 
             '\n</code></pre>\n')

  return(lines)
}




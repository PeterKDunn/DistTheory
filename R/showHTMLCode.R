# Helper functions to format code for HTML <pre><code> blocks

show_code_from_file <- function(path) {
  lines <- readLines(path, warn = FALSE)
  #code <- paste(lines, collapse = "\n")
  lines <- c('<pre><code class="r">\n', lines, '\n</code></pre>\n')
  return(lines)
}




# Place this function definition in a setup chunk, e.g., in index.Rmd
# or in a file like _common.R that is sourced by your book.

show_Code <- function(code_to_display) {
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
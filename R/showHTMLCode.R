# Helper functions to format code for HTML <pre><code> blocks

show_md_Code <- function(code_expr, language = "r") {
  code_lines <- deparse(code_expr)
  code_block <- paste0("```", 
                       language, 
                       "\n", 
                       paste(code_lines, 
                             collapse = "\n"), 
                       "\n```")
  knitr::asis_output(code_block)
}


# Notes to self

To publish:

bookdown::publish_book()



## Find non-ASCII


```{r}
lines <- readLines("yourfile.Rmd")
bad <- grep("[^\x01-\x7F]", lines)
bad                    # Line numbers
lines[bad]             # See the offending lines
```

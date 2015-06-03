
convert_factor <- function(df, ...) {
  convert_func <- function(col) {
    col = eval(quote(col))
    fcall = substitute(mutate(df))
    fcall[[deparse(col)]] = bquote(as.factor(.(col)))
    df <<- eval(fcall)
  }
  lapply(eval(substitute(alist(...))), convert_func)
  df
}


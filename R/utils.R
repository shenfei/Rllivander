
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

split_data_label <- function(x, fold) {
  n_test = floor(length(x) / fold)
  n_train = length(x) - n_test
  label = c(rep(TRUE, n_train), rep(FALSE, n_test))
  return(sample(label, length(label)))
}


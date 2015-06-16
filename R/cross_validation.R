
split_data_label <- function(m, fold) {
  q = m %/% fold
  r = m %% fold
  count = c(rep(q + 1, r), rep(q, fold - r))
  sample(rep(seq_len(fold), count))
}


# Split a data.frame into k-fold w.r.t a column name.
# Output k csv files into a directory.
cv_split <- function(df, fold, outpath, col_name = NULL) {
  col = eval(substitute(quote(col_name)))
  if (is.null(col)) {
    df$cv_label = split_data_label(nrow(df), fold)
  } else {
    index = eval(substitute(select(df, col), list(col = col)))
    cv_label = eval(substitute(
        index %>% group_by(col) %>%
        mutate(cv_label = split_data_label(length(col), fold)),
      list(col = col)))
    df$cv_label = cv_label$cv_label
  }

  dump_cv <- function(idx) {
    outname = sprintf('%s/%s.csv', outpath, idx)
    df %>% filter(cv_label == idx) %>% select(-cv_label) %>%
      write.table(outname, sep = '\t', quote = FALSE,
                  row.names = FALSE, col.names = FALSE)
  }
  invisible(lapply(seq_len(fold), dump_cv))
}


cv_load <- function(inpath, fold, all = TRUE, index = 1) {
  read_cv <- function(idx) {
    fname = sprintf('%s/%s.csv', inpath, idx)
    read.table(fname, sep = '\t', stringsAsFactors = FALSE)
  }
  cv_data = lapply(seq_len(fold), read_cv)
  if (all) {
    return(bind_rows(cv_data))
  } else {
    test = cv_data[[index]]
    train = bind_rows(cv_data[-index])
    return(list(train = train, test = test))
  }
}


cv_run <- function(inpath, fold, func, ...) {
  run <- function(idx) {
    data = cv_load(inpath, fold, all = FALSE, index = idx)
    train = data$train
    test = data$test
    func(train, test, ...)
  }
  lapply(seq_len(fold), run)
}


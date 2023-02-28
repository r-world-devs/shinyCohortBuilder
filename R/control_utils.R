if_null_default <- function(val, default) {
  if (is.null(val)) {
    return(default)
  }
  return(val)
}

is_none <- function(x) {
  identical(x, "none")
}

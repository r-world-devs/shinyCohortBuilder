if_null_default <- function(val, default) {
  if (is.null(val)) {
    return(default)
  }
  return(val)
}

if_na_default <- function(val, default) {
  ifelse(is.na(val), default, val)
}

is_none <- function(x) {
  identical(x, "none")
}

modify_list <- function(x, y) {
  if (is.null(x)) {
    return(y)
  }
  return(
    utils::modifyList(x, y, keep.null = TRUE)
  )
}

suff <- function(x, suffix) {
  paste0(x, "-", suffix)
} 
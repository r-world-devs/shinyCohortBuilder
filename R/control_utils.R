if_null_default <- function(val, default) {
  if (is.null(val)) {
    return(default)
  }
  return(val)
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

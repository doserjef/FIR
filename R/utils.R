# Internal helper to get the names of one or more columns in data supplied as
# unquoted names. A single column can be supplied directly (e.g., Species) and
# multiple columns can be combined with c() (e.g., c(Species, DIA_Class)).
# data_name is the name of the data argument used in error messages.
# Returns a character vector of the column names (empty if col_expr is NULL).
getColNames <- function(col_expr, arg_name, data, data_name = 'data') {
  if (is.null(col_expr)) {
    return(character(0))
  }
  if (rlang::is_symbol(col_expr)) {
    col_exprs <- list(col_expr)
  } else if (rlang::is_call(col_expr, 'c') &&
             all(vapply(rlang::call_args(col_expr), rlang::is_symbol, logical(1)))) {
    col_exprs <- rlang::call_args(col_expr)
  } else {
    stop(paste0(arg_name, ' must be the unquoted name of a column in ', data_name,
                ', or multiple unquoted names combined with c() (e.g., ',
                arg_name, ' = c(col1, col2))'), call. = FALSE)
  }
  col_names <- vapply(col_exprs, rlang::as_string, character(1))
  for (col_name in col_names) {
    if (!(col_name %in% colnames(data))) {
      stop(paste0('column "', col_name, '" supplied to ', arg_name, ' is not in ', data_name), call. = FALSE)
    }
  }
  unname(col_names)
}

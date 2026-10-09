treeVolume <- function(data, dbh, mht, type, mht_units = 'log', gfc = 78, ...) {

  # Some initial checks ---------------------------------------------------
  if (missing(data)) {
    stop('data must be provided')
  }
  if (!is.data.frame(data)) {
    stop('data must be a data frame where each row corresponds to an individual tree')
  }
  if (missing(dbh)) {
    stop('dbh must be specified')
  }
  if (missing(mht)) {
    stop('merchantable height (mht) must be specified')
  }
  if (missing(type)) {
    stop('type must be specified')
  }
  # Capture the unquoted column names
  col_exprs <- list(dbh = rlang::enexpr(dbh), mht = rlang::enexpr(mht))
  for (arg_name in names(col_exprs)) {
    if (!rlang::is_symbol(col_exprs[[arg_name]])) {
      stop(paste0(arg_name, ' must be the unquoted name of a column in data'))
    }
    col_name <- rlang::as_string(col_exprs[[arg_name]])
    if (!(col_name %in% colnames(data))) {
      stop(paste0('column "', col_name, '" supplied to ', arg_name, ' is not in data'))
    }
  }
  dbh_vals <- data[[rlang::as_string(col_exprs$dbh)]]
  mht_vals <- data[[rlang::as_string(col_exprs$mht)]]
  n_trees <- nrow(data)
  # type can be an unquoted column in data or a single value used for all trees
  type_quo <- rlang::enquo(type)
  type_expr <- rlang::quo_get_expr(type_quo)
  type_in_data <- rlang::is_symbol(type_expr) && rlang::as_string(type_expr) %in% colnames(data)
  type_vals <- tryCatch(rlang::eval_tidy(type_quo, data = data),
                        error = function(e) stop('type must be the unquoted name of a column in data (e.g., type = VolType) or a single quoted volume type (e.g., type = "doyle")'))
  if (!(is.character(type_vals) || is.factor(type_vals))) {
    stop('type must be the unquoted name of a column in data (e.g., type = VolType) or a single quoted volume type (e.g., type = "doyle")')
  }
  type_vals <- as.character(type_vals)
  if (length(type_vals) != n_trees & length(type_vals) != 1) {
    stop(paste0('type must be a column name in data, a single volume type, or a vector with ', n_trees, ' values.'))
  }
  if (length(type_vals) == 1) {
    type_vals <- rep(type_vals, n_trees)
  }
  if (!is.numeric(dbh_vals)) {
    stop('the dbh column must be numeric values containing the tree dbh in inches')
  }
  if (!is.numeric(mht_vals)) {
    stop('the mht column must be numeric values containing the tree merchantable height in either 16-ft logs or ft')
  }
  if (length(mht_units) != 1 || !(mht_units %in% c('log', 'ft', 'feet'))) {
    stop('merchantable height units (mht_units) must be either 16-ft logs ("log") or "ft"')
  }
  # gfc can be an unquoted column in data or numeric value(s)
  gfc_quo <- rlang::enquo(gfc)
  if (is.character(rlang::quo_get_expr(gfc_quo))) {
    stop('gfc must be the unquoted name of a column in data (e.g., gfc = GFC) or a numeric value indicating the girard form class')
  }
  gfc <- tryCatch(rlang::eval_tidy(gfc_quo, data = data),
                  error = function(e) stop('gfc must be the unquoted name of a column in data (e.g., gfc = GFC) or a numeric value indicating the girard form class'))
  if (!is.numeric(gfc)) {
    stop('gfc must be a numeric value indicating the girard form class factor')
  }
  if (length(gfc) != n_trees & length(gfc) != 1) {
    stop(paste0('gfc must be a column name in data, a single numeric value, or a vector with ', n_trees, ' values.'))
  }
  if (length(gfc) == 1) {
    gfc <- rep(gfc, n_trees)
  }
  if (any(!(unique(tolower(type_vals)) %in% c('international', 'doyle', 'scribner',
                                              'mesavage_cubic_ft', 'huber')))) {
    stop('type values must be one of the following: "international", "doyle", "scribner", "mesavage_cubic_ft", "huber"')
  }
  # When type is not a column in data, it is added to the output as a 'type' column
  new_cols <- c('volume', 'units', if (!type_in_data) 'type')
  existing_cols <- intersect(new_cols, colnames(data))
  if (length(existing_cols) > 0) {
    warning(paste0('data already contains the column(s) ',
                   paste0('"', existing_cols, '"', collapse = ', '),
                   ', which will be overwritten'))
  }

  # Set up ----------------------------------------------------------------
  if (mht_units %in% c('ft', 'feet')) {
    message('mht provided in feet. Converting the heights to 16-ft logs, rounding down to the nearest half log. These values are used for volume calculation')
    mht_vals <- trunc(mht_vals / 16 / .5) * .5
  }

  # Make the calculations -------------------------------------------------
  out <- vector(mode = 'numeric', length = n_trees)
  units_vec <- character(n_trees)

  # Board foot log rules
  doyle_indx <- which(tolower(type_vals) == 'doyle')
  scribner_indx <- which(tolower(type_vals) == 'scribner')
  int_indx <- which(tolower(type_vals) == 'international')
  board_indx <- c(doyle_indx, scribner_indx, int_indx)

  if (length(board_indx) > 0) {
    gfc_cor <- 1.0 + ((gfc - 78) * 0.03)
    a_doyle <- -29.37337 + 41.51275 * mht_vals + 0.55743 * mht_vals^2
    b_doyle <- (2.78043 - 8.77272 * mht_vals - 0.04516 * mht_vals^2) * dbh_vals
    c_doyle <- (0.04177 + 0.59042 * mht_vals - 0.01578 * mht_vals^2) * dbh_vals^2
    a_scribner <- -22.50365 + 17.53508 * mht_vals - 0.59242 * mht_vals^2
    b_scribner <- (3.02988 - 4.34381 * mht_vals - 0.02302 * mht_vals^2) * dbh_vals
    c_scribner <- (-0.01969 + 0.51593 * mht_vals - 0.02035 * mht_vals^2) * dbh_vals^2
    a_int <- -13.35212 + 9.58615 * mht_vals + 1.52968 * mht_vals^2
    b_int <- (1.7962 - 2.59995 * mht_vals - 0.27465 * mht_vals^2) * dbh_vals
    c_int <- (0.04482 + 0.45997 * mht_vals - 0.00961 * mht_vals^2) * dbh_vals^2
    out[doyle_indx] <- (a_doyle + b_doyle + c_doyle)[doyle_indx] * gfc_cor[doyle_indx]
    out[scribner_indx] <- (a_scribner + b_scribner + c_scribner)[scribner_indx] * gfc_cor[scribner_indx]
    out[int_indx] <- (a_int + b_int + c_int)[int_indx] * gfc_cor[int_indx]
    units_vec[board_indx] <- 'board_ft'
  }

  # Cubic foot volume types
  mesavage_indx <- which(tolower(type_vals) == 'mesavage_cubic_ft')
  huber_indx <- which(tolower(type_vals) == 'huber')

  if (length(mesavage_indx) > 0) {
    mht_mesavage <- trunc(mht_vals / .5) * .5
    # Notice you're truncating diameters to the nearest integer.
    tmp_dat <- data.frame(DBH = trunc(dbh_vals[mesavage_indx]), Height = mht_mesavage[mesavage_indx])
    final_dat <- dplyr::left_join(tmp_dat, mesavageCubicFt, by = c('DBH', 'Height'))
    out[mesavage_indx] <- final_dat$Volume
    units_vec[mesavage_indx] <- 'cubic_ft'
  }

  if (length(huber_indx) > 0) {
    mht_ft <- mht_vals[huber_indx] * 16
    ba_sqft <- (pi / 4) * (dbh_vals[huber_indx] / 12)^2
    out[huber_indx] <- ba_sqft * mht_ft
    units_vec[huber_indx] <- 'cubic_ft'
  }

  data$volume <- out
  data$units <- units_vec
  if (!type_in_data) {
    data$type <- type_vals
  }
  data
}

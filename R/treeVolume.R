treeVolume <- function(dbh, mht, mht_units = 'log', gfc = 78, type, ...) {

  # Some initial checks ---------------------------------------------------
  if (missing(dbh)) {
    stop('dbh must be specified')
  }
  if (any(!is.numeric(dbh))) {
    stop('dbh must be numeric values containing the tree dbh in inches')
  }
  if (missing(mht)) {
    stop('merchantable height (mht) must be specified')
  }
  if (any(!is.numeric(mht))) {
    stop('mht must be numeric values containing the tree merchantable height in either 16-ft logs or ft')
  }
  if (!(mht_units %in% c('log', 'ft', 'feet'))) {
    stop('merchantable height units (mht_units) must be either 16-ft logs ("log") or "ft"')
  }
  if (!is.numeric(gfc)) {
    stop('gfc must be a numeric value indicating the girard form class factor')
  }
  if (length(gfc) != length(dbh) | length(gfc) != 1) {
    if (length(dbh) != 1) {
      stop(paste0('gfc must be a vector with ', length(dbh), ' values or 1 value.'))
    }
    if (length(dbh) == 1) {
      stop('gfc must be a numeric vector with 1 value')
    }
  }
  if (!(tolower(type) %in% c('international', 'doyle', 'scribner'))) {
    stop('type must be one of the following: "international", "doyle", "scribner"') 
  }
  
  # Set up ----------------------------------------------------------------
  if (mht_units %in% c('ft', 'feet')) {
    message('mht provided in feet. Converting the heights to 16-ft logs, rounding down to the nearest half log. These values are used for volume calculation')
    mht <- trunc(mht / 16 / .5) * .5
  }
  
  # Make the calculations -------------------------------------------------
  # Correction factor for different GFCs. 
  gfc.cor <- 1.0 + ((gfc - 78) * 0.03) 
  if (tolower(type) == 'doyle') {
    a <- -29.37337 + 41.51275 * mht + 0.55743 * mht^2
    b <- (2.78043 - 8.77272 * mht - 0.04516 * mht^2) * dbh 
    c <- (0.04177 + 0.59042 * mht - 0.01578 * mht^2) * dbh^2
  }
  if (tolower(type) == 'scribner') {
    a <- -22.50365 + 17.53508 * mht - 0.59242 * mht^2
    b <- (3.02988 - 4.34381 * mht - 0.02302 * mht^2) * dbh
    c <- (-0.01969 + 0.51593 * mht - 0.02035 * mht^2) * dbh^2
  }
  if (tolower(type) == 'international') {
    a <- -13.35212 + 9.58615 * mht + 1.52968 * mht^2
    b <- (1.7962 - 2.59995 * mht - 0.27465 * mht^2) * dbh
    c <- (0.04482 + 0.45997 * mht - 0.00961 * mht^2) * dbh^2
  }

  out <- (a + b + c) * gfc.cor

  out
}

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
  if (length(gfc) != length(dbh) & length(gfc) != 1) {
    if (length(dbh) != 1) {
      stop(paste0('gfc must be a vector with ', length(dbh), ' values or 1 value.'))
    }
    if (length(dbh) == 1) {
      stop('gfc must be a numeric vector with 1 value')
    }
  }
  if (length(gfc) == 1) {
    gfc <- rep(gfc, length(dbh))
  }
  if (any(!(unique(tolower(type)) %in% c('international', 'doyle', 'scribner', 
                                         'mesavage_cubic_ft')))) {
    stop('type must be one of the following: "international", "doyle", "scribner", "mesavage_cubic_ft"') 
  }
  # If using the cubic ft formulas, all values in type must be cubic ft. 
  if (('mesavage_cubic_ft' %in% unique(tolower(type))) & (length(tolower(unique(type))) > 1)) {
    stop('If generating volumes using the Mesavage cubic foot volumes, all tree volumes must be calculated in cubic ft. You are not allowed to mix board ft log rules and cubic ft log rules.')
  }
  # Get an indicator value indicating if doing log rule table or cubic ft.
  if ('mesavage_cubic_ft' %in% unique(tolower(type))) {
    bdFt <- FALSE
  } else {
    bdFt <- TRUE
  }
  # TODO: check for specifying Mesavage and gfc
  if (length(type) != length(dbh) & length(type) != 1) {
    if (length(dbh) != 1) {
      stop(paste0('type must be a vector with ', length(dbh), ' values or 1 value.'))
    }
    if (length(dbh) == 1) {
      stop('type must be a numeric vector with 1 value')
    }
  }
  if (length(type) == 1) {
    type <- rep(type, length(dbh))
  }
  
  # Set up ----------------------------------------------------------------
  if (mht_units %in% c('ft', 'feet')) {
    message('mht provided in feet. Converting the heights to 16-ft logs, rounding down to the nearest half log. These values are used for volume calculation')
    mht <- trunc(mht / 16 / .5) * .5
  }
  
  # Make the calculations -------------------------------------------------
  # Correction factor for different GFCs. 
  if (bdFt) {
    gfc.cor <- 1.0 + ((gfc - 78) * 0.03) 
    aDoyle <- -29.37337 + 41.51275 * mht + 0.55743 * mht^2
    bDoyle <- (2.78043 - 8.77272 * mht - 0.04516 * mht^2) * dbh 
    cDoyle <- (0.04177 + 0.59042 * mht - 0.01578 * mht^2) * dbh^2
    aScribner <- -22.50365 + 17.53508 * mht - 0.59242 * mht^2
    bScribner <- (3.02988 - 4.34381 * mht - 0.02302 * mht^2) * dbh
    cScribner <- (-0.01969 + 0.51593 * mht - 0.02035 * mht^2) * dbh^2
    aInt <- -13.35212 + 9.58615 * mht + 1.52968 * mht^2
    bInt <- (1.7962 - 2.59995 * mht - 0.27465 * mht^2) * dbh
    cInt <- (0.04482 + 0.45997 * mht - 0.00961 * mht^2) * dbh^2
    out <- vector(mode = 'numeric', length = length(dbh))
    doyleIndx <- which(tolower(type) == 'doyle')
    scribnerIndx <- which(tolower(type) == 'scribner')
    intIndx <- which(tolower(type) == 'international')
    out[doyleIndx] <- (aDoyle + bDoyle + cDoyle)[doyleIndx] * gfc.cor[doyleIndx] 
    out[scribnerIndx] <- (aScribner + bScribner + cScribner)[scribnerIndx] * gfc.cor[scribnerIndx] 
    out[intIndx] <- (aInt + bInt + cInt) * gfc.cor[intIndx] 
  } else {
    # Round heights to the nearest half foot if they aren't already
    mht <- trunc(mht / .5) * .5
    tmp.dat <- data.frame(DBH = dbh, Height = mht)                           
    final.dat <- dplyr::left_join(tmp.dat, mesavageCubicFt, by = c('DBH', 'Height'))
    out <- final.dat$Volume
  }
  out
}

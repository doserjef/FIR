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
                                         'mesavage_cubic_ft', 'huber')))) {
    stop('type must be one of the following: "international", "doyle", "scribner", "mesavage_cubic_ft", "huber"')
  }
  # Cannot mix board ft log rules with cubic ft volume types.
  cubic_types <- c('mesavage_cubic_ft', 'huber')
  board_types <- c('international', 'doyle', 'scribner')
  if (any(unique(tolower(type)) %in% cubic_types) & any(unique(tolower(type)) %in% board_types)) {
    stop('You cannot mix cubic foot volume types (Mesavage, Huber) with board foot log rules.')
  }
  # Get an indicator value indicating if doing log rule table or cubic ft.
  if (any(unique(tolower(type)) %in% cubic_types)) {
    bd_ft <- FALSE
  } else {
    bd_ft <- TRUE
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
  if (bd_ft) {
    gfc_cor <- 1.0 + ((gfc - 78) * 0.03)
    a_doyle <- -29.37337 + 41.51275 * mht + 0.55743 * mht^2
    b_doyle <- (2.78043 - 8.77272 * mht - 0.04516 * mht^2) * dbh
    c_doyle <- (0.04177 + 0.59042 * mht - 0.01578 * mht^2) * dbh^2
    a_scribner <- -22.50365 + 17.53508 * mht - 0.59242 * mht^2
    b_scribner <- (3.02988 - 4.34381 * mht - 0.02302 * mht^2) * dbh
    c_scribner <- (-0.01969 + 0.51593 * mht - 0.02035 * mht^2) * dbh^2
    a_int <- -13.35212 + 9.58615 * mht + 1.52968 * mht^2
    b_int <- (1.7962 - 2.59995 * mht - 0.27465 * mht^2) * dbh
    c_int <- (0.04482 + 0.45997 * mht - 0.00961 * mht^2) * dbh^2
    out <- vector(mode = 'numeric', length = length(dbh))
    doyle_indx <- which(tolower(type) == 'doyle')
    scribner_indx <- which(tolower(type) == 'scribner')
    int_indx <- which(tolower(type) == 'international')
    out[doyle_indx] <- (a_doyle + b_doyle + c_doyle)[doyle_indx] * gfc_cor[doyle_indx]
    out[scribner_indx] <- (a_scribner + b_scribner + c_scribner)[scribner_indx] * gfc_cor[scribner_indx]
    out[int_indx] <- (a_int + b_int + c_int) * gfc_cor[int_indx]
  } else {
    mesavage_indx <- which(tolower(type) == 'mesavage_cubic_ft')
    huber_indx <- which(tolower(type) == 'huber')
    out <- vector(mode = 'numeric', length = length(dbh))

    if (length(mesavage_indx) > 0) {
      mht_mesavage <- trunc(mht / .5) * .5
      # Notice you're truncating diameters to the nearest integer.
      tmp_dat <- data.frame(DBH = trunc(dbh[mesavage_indx]), Height = mht_mesavage[mesavage_indx])
      final_dat <- dplyr::left_join(tmp_dat, mesavageCubicFt, by = c('DBH', 'Height'))
      out[mesavage_indx] <- final_dat$Volume
    }

    if (length(huber_indx) > 0) {
      mht_ft <- mht[huber_indx] * 16
      ba_sqft <- (pi / 4) * (dbh[huber_indx] / 12)^2
      out[huber_indx] <- ba_sqft * mht_ft
    }
  }
  out
}

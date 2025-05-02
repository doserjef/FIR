treeMerch <- function(data, pricing, mht_units = 'log', dbh_units = 'in', ...) {
  # Some initial checks ---------------------------------------------------
  if (missing(data)) {
    stop('data must be provided')
  }
  if (!is.data.frame(data)) {
    stop('data must be specified as a data frame containing the individual tree measurements')
  }
  tmp.names <- c('Product_Type', 'Height', 'Species', 'DBH', 'GFC')
  if (sum(tmp.names %in% colnames(data)) != length(tmp.names)) {
    stop('treeData must contain the following columns: Product_Type, Height, Species, DBH, GFC')
  }
  if (missing(pricing)) {
    stop('pricing must be provided')
  }
  if (!is.data.frame(pricing)) {
    stop('pricing must be specified as a data frame containing the price information for each product class')
  }
  if (ncol(pricing) != 3) {
    stop('pricing must be a data frame with two columns: Product_type, Price, and Vol_Type') 
  }
  if (!all.equal(sort(colnames(pricing)), sort(c('Product_Type', 'Price', 'Vol_Type')))) {
    stop('the column names in pricing must be: Product_Type, Price, Vol_Type')
  }

  # Initial prep ----------------------------------------------------------
  # If dbh given in cm, convert to inches
  if (dbh_units == 'cm') {
    data$DBH <- data$DBH * 2.54
  }

  # Join data -------------------------------------------------------------
  comb_dat <- dplyr::left_join(data, pricing, by = 'Product_Type')

  # Determine board foot volume of tree -----------------------------------
  comb_dat$Volume <- treeVolume(dbh = comb_dat$DBH, mht = comb_dat$Height, 
                                mht_units = mht_units, gfc = comb_dat$GFC, 
                                type = comb_dat$Vol_Type) 
  
  # Determine price for each tree -----------------------------------------
  out <- comb_dat %>%
    dplyr::mutate(Value = round(Price * Volume, digits = 2)) %>%
    dplyr::select(-Price)

  out
}

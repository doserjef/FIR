treeMerch <- function(data, pricing, mht_units = 'log', dbh_units = 'in', volume, ...) {
  # Some initial checks ---------------------------------------------------
  if (missing(data)) {
    stop('data must be provided')
  }
  if (!is.data.frame(data)) {
    stop('data must be specified as a data frame containing the individual tree measurements')
  }
  # volume is an optional unquoted column in data with pre-computed tree volumes
  vol_supplied <- !missing(volume)
  if (vol_supplied) {
    vol_expr <- rlang::enexpr(volume)
    if (!rlang::is_symbol(vol_expr)) {
      stop('volume must be the unquoted name of a column in data')
    }
    vol_name <- rlang::as_string(vol_expr)
    if (!(vol_name %in% colnames(data))) {
      stop(paste0('column "', vol_name, '" supplied to volume is not in data'))
    }
    if (!is.numeric(data[[vol_name]])) {
      stop('the volume column must contain numeric values')
    }
    if (!('Product_Type' %in% colnames(data))) {
      stop('data must contain the following column: Product_Type')
    }
  } else {
    tmp.names <- c('Product_Type', 'Height', 'Species', 'DBH', 'GFC')
    if (sum(tmp.names %in% colnames(data)) != length(tmp.names)) {
      stop('treeData must contain the following columns: Product_Type, Height, Species, DBH, GFC')
    }
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
  if (!vol_supplied && dbh_units == 'cm') {
    data$DBH <- data$DBH / 2.54
  }

  # Join data -------------------------------------------------------------
  # Columns in pricing take precedence over columns of the same name in data
  # (e.g., a Vol_Type column left over from a previous treeVolume call)
  dup_cols <- intersect(c('Price', 'Vol_Type'), colnames(data))
  if (length(dup_cols) > 0) {
    pricing_vals <- dplyr::left_join(data['Product_Type'], pricing, by = 'Product_Type')
    differ_cols <- dup_cols[sapply(dup_cols, function(a) {
      x <- data[[a]]
      y <- pricing_vals[[a]]
      if (is.character(x) || is.factor(x)) {
        x <- tolower(as.character(x))
        y <- tolower(as.character(y))
      }
      !isTRUE(all(x == y | (is.na(x) & is.na(y)), na.rm = FALSE))
    })]
    if (length(differ_cols) > 0) {
      warning(paste0('data contains the column(s) ',
                     paste0('"', differ_cols, '"', collapse = ', '),
                     ' with values that differ from pricing. The values in pricing will be used'))
    }
    data <- data[, !(colnames(data) %in% dup_cols), drop = FALSE]
  }
  comb_dat <- dplyr::left_join(data, pricing, by = 'Product_Type')

  # Determine volume of tree ----------------------------------------------
  if (vol_supplied) {
    comb_dat$Volume <- comb_dat[[vol_name]]
    vol_types <- tolower(comb_dat$Vol_Type)
    valid_types <- c('international', 'doyle', 'scribner', 'mesavage_cubic_ft', 'huber')
    if (any(!(unique(vol_types) %in% valid_types))) {
      stop('Vol_Type values in pricing must be one of the following: "international", "doyle", "scribner", "mesavage_cubic_ft", "huber"')
    }
    comb_dat$Vol_Units <- ifelse(vol_types %in% c('international', 'doyle', 'scribner'),
                                 'board_ft', 'cubic_ft')
  } else {
    vol_result <- treeVolume(data = comb_dat, dbh = DBH, mht = Height,
                             type = Vol_Type, mht_units = mht_units, gfc = GFC)
    comb_dat$Volume <- vol_result$volume
    comb_dat$Vol_Units <- vol_result$units
  }

  # Determine price for each tree -----------------------------------------
  out <- comb_dat %>%
    dplyr::mutate(Value = round(Price * Volume, digits = 2)) %>%
    dplyr::select(-Price)

  out
}

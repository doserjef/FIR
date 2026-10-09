importanceValue <- function(treeData, plotID, baColumn, species, 
                            standID = NULL, ...) {

  # Some initial checks ---------------------------------------------------
  if (missing(treeData)) {
    stop('treeData must be provided')
  }
  if (!is.data.frame(treeData)) {
    stop('treeData must be a data frame where each row corresponds to an individual tree')
  }
  if (missing(plotID)) {
    stop('you need to specify the column name in treeData containing the plot ID (plotID)')
  }
  if (missing(baColumn)) {
    stop('you need to specify the column name in treeData containing the basal area (baColumn)')
  }
  if (missing(species)) {
    stop('you need to specify the column with the species names (species)')
  }
  # Capture the unquoted column names
  col_exprs <- list(plotID = rlang::enexpr(plotID), baColumn = rlang::enexpr(baColumn),
                    species = rlang::enexpr(species))
  for (arg_name in names(col_exprs)) {
    if (!rlang::is_symbol(col_exprs[[arg_name]])) {
      stop(paste0(arg_name, ' must be the unquoted name of a column in treeData'))
    }
    col_name <- rlang::as_string(col_exprs[[arg_name]])
    if (!(col_name %in% colnames(treeData))) {
      stop(paste0('column "', col_name, '" supplied to ', arg_name, ' is not in treeData'))
    }
  }
  # standID can be one or more unquoted column names (e.g., standID = c(Stand, County))
  standID_names <- getColNames(rlang::enexpr(standID), 'standID', treeData, 'treeData')
  species_name <- rlang::as_string(col_exprs$species)

  # Prep the data for use with dplyr --------------------------------------
  plotIDSyms <- col_exprs$plotID
  baColumnSyms <- col_exprs$baColumn
  speciesSyms <- col_exprs$species
  standIDSyms <- rlang::syms(standID_names)

  # Do the calculations ---------------------------------------------------
  n.plots.df <- treeData %>%
    dplyr::group_by(!!!standIDSyms) %>%
    dplyr::select(!!plotIDSyms, !!!standIDSyms) %>%
    dplyr::summarize(n.plots = dplyr::n_distinct(!!plotIDSyms))

  frequency <- treeData %>%
    dplyr::group_by(!!speciesSyms, !!plotIDSyms, !!!standIDSyms) %>%
    dplyr::summarize(pa = ifelse(dplyr::n() > 0, 1, 0), .groups = 'drop') %>%
    tidyr::complete(!!speciesSyms, !!!standIDSyms, fill = list(pa = 0)) %>%
    dplyr::group_by(!!speciesSyms, !!!standIDSyms) %>% 
    dplyr::summarize(frequency = sum(pa), .groups = 'drop')

  if (length(standID_names) > 0) {
    frequency <- dplyr::left_join(frequency, n.plots.df, by = standID_names)
  } else {
    frequency$n.plots <- n.plots.df$n.plots
  }

  frequency <- frequency %>%
    dplyr::mutate(frequency = frequency / n.plots) %>%
    dplyr::select(-n.plots)

  abundance <- treeData %>%
    dplyr::group_by(!!speciesSyms, !!!standIDSyms) %>%
    dplyr::summarize(abundance = dplyr::n(), 
                     dominance = sum(!!baColumnSyms), .groups = 'drop') %>%
    tidyr::complete(!!speciesSyms, !!!standIDSyms, fill = list(abundance = 0, 
                                                               dominance = 0))
  
  abundance <- abundance %>%
    dplyr::mutate(abundance = abundance / sum(abundance), 
                  dominance = dominance / sum(dominance))
  

  if (length(standID_names) > 0) {
    out <- dplyr::left_join(frequency, abundance, by = c(species_name, standID_names))
  } else {
    out <- dplyr::left_join(frequency, abundance, by = c(species_name))
  }
  
  out <- out %>%
    dplyr::mutate(importance = 100 * (frequency + abundance + dominance)) %>%
    as.data.frame()

  out
}

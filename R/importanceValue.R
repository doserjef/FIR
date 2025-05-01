importanceValue <- function(treeData, plotID, baColumn, species, 
                            standID = NULL, ...) {

  # Some initial checks ---------------------------------------------------
  if (missing(treeData)) {
    stop('treeData must be provided')
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

  # Prep the data for use with dplyr --------------------------------------
  plotIDSyms <- rlang::sym(plotID)
  baColumnSyms <- rlang::sym(baColumn)
  speciesSyms <- rlang::sym(species)
  standIDSyms <- rlang::syms(standID)

  # Do the calculations ---------------------------------------------------
  n.plots.df <- treeData %>%
    dplyr::group_by(!!!standIDSyms) %>%
    dplyr::select(!!plotIDSyms, !!!standIDSyms) %>%
    dplyr::summarize(n.plots = dplyr::n_distinct(!!plotIDSyms))

  frequency <- pef_dat %>%
    dplyr::group_by(!!speciesSyms, !!plotIDSyms, !!!standIDSyms) %>%
    dplyr::summarize(pa = ifelse(dplyr::n() > 0, 1, 0), .groups = 'drop') %>%
    tidyr::complete(!!speciesSyms, !!!standIDSyms, fill = list(pa = 0)) %>%
    dplyr::group_by(!!speciesSyms, !!!standIDSyms) %>% 
    dplyr::summarize(frequency = sum(pa), .groups = 'drop')

  if (!is.null(standID)) {
    frequency <- dplyr::left_join(frequency, n.plots.df, by = standID)
  } else {
    frequency$n.plots <- n.plots.df$n.plots
  }

  frequency <- frequency %>%
    dplyr::mutate(frequency = frequency / n.plots) %>%
    dplyr::select(-n.plots)

  abundance <- pef_dat %>%
    dplyr::group_by(!!speciesSyms, !!!standIDSyms) %>%
    dplyr::summarize(abundance = dplyr::n(), 
                     dominance = sum(!!baColumnSyms), .groups = 'drop') %>%
    tidyr::complete(!!speciesSyms, !!!standIDSyms, fill = list(abundance = 0, 
                                                               dominance = 0))
  
  abundance <- abundance %>%
    dplyr::mutate(abundance = abundance / sum(abundance), 
                  dominance = dominance / sum(dominance))
  

  if (!is.null(standID)) {
    out <- dplyr::left_join(frequency, abundance, by = c(species, standID))
  } else {
    out <- dplyr::left_join(frequency, abundance, by = c(species))
  }
  
  out <- out %>%
    dplyr::mutate(importance = 100 * (frequency + abundance + dominance)) %>%
    as.data.frame()

  out
}

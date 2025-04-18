standStock <- function(treeData, plotID, variable, grpBy, plotType, 
                       plotSize, BAF, baColumn, ...) {

  # Some initial checks ---------------------------------------------------
  if (missing(treeData)) {
    stop('treeData must be provided')
  }
  if (missing(variable)) {
    stop('you need to specify the variable you want to summarize in the stand/stock table (variable)')
  }
  if (missing(grpBy)) {
    stop('you need to specify the variable(s) to group the stand/stock table by (grpBy)') 
  }
  if (missing(plotID)) {
    stop('you need to specify the column in treeData that contains the plot/point ID (plotID)')
  }
  if (!(plotType %in% c('fixed', 'variable'))) {
    stop('plotType must be either "fixed" or "variable"')
  }
  if (plotType == 'fixed' & missing(plotSize)) {
    stop('the size of the fixed radius plot (plotSize) must be specified when plotType == "fixed"')
  }
  if (plotType == 'variable' & !missing(plotSize)) {
    message('plotSize is specified but plotType == "variable". plotSize is ignored for variable radius plots')
  }
  if (plotType == 'variable' & missing(BAF)) {
    stop('the Basal Area Factor (BAF) must be specified when using variable radius plots')
  }
  if (plotType == 'variable' & missing(baColumn)) {
    stop('a column must be included in treeData that contains the basal area (baColumn)')
  }

  # Prep the data for summarizing -----------------------------------------
  # Convert supplied characters to symbols
  plotIDSyms <- rlang::sym(plotID)
  variableSyms <- rlang::sym(variable)
  grpBySyms <- rlang::syms(grpBy)
  if (plotType == 'variable') {
    baColumnSyms <- rlang::sym(baColumn)
  } else {
    baColumnSyms <- NULL
  }
  # Shrink the size of treeData for ease
  treeData <- treeData %>%
    dplyr::select(!!plotIDSyms, !!variableSyms, !!!grpBySyms, !!baColumnSyms)

  # Complete the tree-level data
  treeData <- treeData %>%
    tidyr::complete(!!plotIDSyms, !!!grpBySyms) %>%
    dplyr::mutate(dplyr::across(c(!!variableSyms, !!baColumnSyms), function(a) ifelse(is.na(a), 0, a)))

  # Calculate the expansion factors
  if (plotType == 'fixed') {
    treeData <- treeData %>%
      dplyr::mutate(TF = 1 / plotSize) 
  } else {
    treeData <- treeData %>%
      dplyr::mutate(TF = ifelse(!!baColumnSyms > 0, BAF / !!baColumnSyms, 0))
  }

  # Calculate point/plot level data ---------------------------------------
  plotData <- treeData %>%
    dplyr::group_by(!!plotIDSyms, !!!grpBySyms) %>%
    dplyr::summarize(variable = sum(!!variableSyms * TF), .groups = 'drop')

  # Stand-level data ------------------------------------------------------
  standData <- plotData %>%
    dplyr::group_by(!!!grpBySyms) %>%
    dplyr::summarize(variable = mean(variable), .groups = 'drop')

  if (length(grpBy) == 1) {
    return(standData)
  }

  # Reformat the stand table in different ways depending on how many 
  # grouping variables there are.
  if (length(grpBy) > 1) {
    standWide <- standData %>%
      pivot_wider(id_cols = !!grpBySyms[[1]],
                  names_from = !!grpBySyms[[2]],
                  values_from = variable)

    # Get totals across rows and columns
    standWide <- standWide %>%
      mutate(Totals = rowSums(across(-!!grpBySyms[[1]]))) %>%
      as.data.frame()

    totals <- as.numeric(apply(standWide[, -1], 2, sum))
    standWide[nrow(standWide) + 1, ] <- NA
    standWide[nrow(standWide), 1] <- 'Totals'
    standWide[nrow(standWide), -1] <- totals

    standWide <- standWide %>%
      as_tibble()
    return(list(longTable = standData, wideTable = standWide))
  }

}

calcEsts <- function(treeData, plotID, variable, grpBy = NULL, plotType, 
                     plotSize, BAF, baColumn, confIntLevel = 0.95, design = 'sys', 
                     returnPlotSummaries = FALSE, ...) {

  # Some initial checks ---------------------------------------------------
  if (missing(treeData)) {
    stop('treeData must be provided')
  }
  if (missing(variable)) {
    stop('you need to specify the variable you want to estimate (variable)')
  }
  if (missing(grpBy)) {
    stop('you need to specify the variable(s) to group the estimates by (grpBy)')
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
  if (!(tolower(design) %in% c('sys', 'srswor'))) {
    stop('calcEsts currently only supports systematic (sys) or simple random samples without replacement (srswor)')
  }

  if (!is.logical(returnPlotSummaries)) {
    stop('returnPlotSummaries must be either TRUE or FALSE.')
  }
  if (confIntLevel <= 0 | confIntLevel >= 1) {
    stop('confIntLevel must be a numeric value between 0 and 1')
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

  # Stand-level estimates -------------------------------------------------
  alpha <- 1 - confIntLevel
  ests <- plotData %>%
    dplyr::group_by(!!!grpBySyms) %>%
    dplyr::summarize(n = n(), 
                     t = qt(p = 1 - alpha / 2, df = n - 1), 
                     estimate = mean(variable),
                     standardError = sd(variable) / sqrt(n), 
                     ciLower = estimate - t * standardError, 
                     ciUpper = estimate + t * standardError, 
                     ciLevel = confIntLevel,
                     .groups = 'drop')

  out <- ests
  if (returnPlotSummaries) {
    out <- list(plotSummaries = plotData, ests = ests)
  }

  out

}

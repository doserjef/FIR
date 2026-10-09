calcEsts <- function(treeData, plotID, variable, grpBy = NULL, plotType, 
                     plotSize, BAF, baColumn, confLevel = 0.95, design = 'sys', 
                     returnPlotSummaries = FALSE, standID = NULL, ...) {

  # Some initial checks ---------------------------------------------------
  if (missing(treeData)) {
    stop('treeData must be provided')
  }
  if (!is.data.frame(treeData)) {
    stop('treeData must be a data frame where each row corresponds to an individual tree')
  }
  if (missing(variable)) {
    stop('you need to specify the variable you want to estimate (variable)')
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
  if (confLevel <= 0 | confLevel >= 1) {
    stop('confLevel must be a numeric value between 0 and 1')
  }
  # Capture the unquoted column names
  col_exprs <- list(plotID = rlang::enexpr(plotID), variable = rlang::enexpr(variable))
  if (plotType == 'fixed') {
    col_exprs$plotSize <- rlang::enexpr(plotSize)
  }
  if (plotType == 'variable') {
    col_exprs$baColumn <- rlang::enexpr(baColumn)
  }
  for (arg_name in names(col_exprs)) {
    if (!rlang::is_symbol(col_exprs[[arg_name]])) {
      stop(paste0(arg_name, ' must be the unquoted name of a column in treeData'))
    }
    col_name <- rlang::as_string(col_exprs[[arg_name]])
    if (!(col_name %in% colnames(treeData))) {
      stop(paste0('column "', col_name, '" supplied to ', arg_name, ' is not in treeData'))
    }
  }
  # grpBy and standID can be one or more unquoted column names (e.g., grpBy = c(Species, DIA_Class))
  grpBy_names <- getColNames(rlang::enexpr(grpBy), 'grpBy', treeData, 'treeData')
  standID_names <- getColNames(rlang::enexpr(standID), 'standID', treeData, 'treeData')

  # Prep the data for summarizing -----------------------------------------
  plotIDSyms <- col_exprs$plotID
  variableSyms <- col_exprs$variable
  grpBySyms <- rlang::syms(grpBy_names)
  standIDSyms <- rlang::syms(standID_names)
  baColumnSyms <- col_exprs$baColumn
  plotSizeSyms <- col_exprs$plotSize
  # Shrink the size of treeData for ease
  treeData <- treeData %>%
    dplyr::select(!!plotIDSyms, !!variableSyms, !!!grpBySyms, !!baColumnSyms, !!plotSizeSyms, !!!standIDSyms)

  # Complete the tree-level data
  treeData <- treeData %>%
    tidyr::complete(tidyr::nesting(!!plotIDSyms, !!!standIDSyms), !!!grpBySyms) %>%
    dplyr::mutate(dplyr::across(c(!!variableSyms, !!baColumnSyms, !!plotSizeSyms), function(a) ifelse(is.na(a), 0, a)))

  # Calculate the expansion factors
  if (plotType == 'fixed') {
    treeData <- treeData %>%
      dplyr::mutate(TF = ifelse(!!plotSizeSyms > 0, 1 / !!plotSizeSyms, 0)) 
  } else {
    treeData <- treeData %>%
      dplyr::mutate(TF = ifelse(!!baColumnSyms > 0, BAF / !!baColumnSyms, 0))
  }

  # Calculate point/plot level data ---------------------------------------
  plotData <- treeData %>%
    dplyr::group_by(!!plotIDSyms, !!!standIDSyms, !!!grpBySyms) %>%
    dplyr::summarize(variable = sum(!!variableSyms * TF), .groups = 'drop')

  # Stand-level estimates -------------------------------------------------
  alpha <- 1 - confLevel
  ests <- plotData %>%
    dplyr::group_by(!!!standIDSyms, !!!grpBySyms) %>%
    dplyr::summarize(n = n(), 
                     t = qt(p = 1 - alpha / 2, df = n - 1), 
                     estimate = mean(variable),
                     standardError = sd(variable) / sqrt(n), 
                     ciLower = estimate - t * standardError, 
                     ciUpper = estimate + t * standardError, 
                     ciLevel = confLevel,
                     .groups = 'drop')

  out <- ests
  if (returnPlotSummaries) {
    out <- list(plotSummaries = plotData, ests = ests)
  }

  out

}

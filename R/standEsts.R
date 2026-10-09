standEsts <- function(plotData, variable, grpBy = NULL,  
                      confLevel = 0.95, design = 'sys', 
                      standID = NULL, ...) {

  # Some initial checks ---------------------------------------------------
  if (missing(plotData)) {
    stop('plotData must be provided')
  }
  if (!is.data.frame(plotData)) {
    stop('plotData must be a data frame where each row corresponds to an individual plot')
  }
  if (missing(variable)) {
    stop('you need to specify the variable you want to estimate (variable)')
  }
  if (!(tolower(design) %in% c('sys', 'srswor'))) {
    stop('calcEsts currently only supports systematic (sys) or simple random samples without replacement (srswor)')
  }
  if (confLevel <= 0 | confLevel >= 1) {
    stop('confLevel must be a numeric value between 0 and 1')
  }
  # Capture the unquoted column names
  col_exprs <- list(variable = rlang::enexpr(variable))
  for (arg_name in names(col_exprs)) {
    if (!rlang::is_symbol(col_exprs[[arg_name]])) {
      stop(paste0(arg_name, ' must be the unquoted name of a column in plotData'))
    }
    col_name <- rlang::as_string(col_exprs[[arg_name]])
    if (!(col_name %in% colnames(plotData))) {
      stop(paste0('column "', col_name, '" supplied to ', arg_name, ' is not in plotData'))
    }
  }
  # grpBy and standID can be one or more unquoted column names (e.g., grpBy = c(Species, DIA_Class))
  grpBy_names <- getColNames(rlang::enexpr(grpBy), 'grpBy', plotData, 'plotData')
  standID_names <- getColNames(rlang::enexpr(standID), 'standID', plotData, 'plotData')

  # Prep the data for summarizing -----------------------------------------
  variableSyms <- col_exprs$variable
  grpBySyms <- rlang::syms(grpBy_names)
  standIDSyms <- rlang::syms(standID_names)
  # Shrink the size of plotData for ease
  plotData <- plotData %>%
    dplyr::select(!!variableSyms, !!!grpBySyms, !!!standIDSyms)

  # Stand-level estimates -------------------------------------------------
  alpha <- 1 - confLevel
  ests <- plotData %>%
    dplyr::group_by(!!!standIDSyms, !!!grpBySyms) %>%
    dplyr::summarize(n = n(), 
                     t = qt(p = 1 - alpha / 2, df = n - 1), 
                     estimate = mean(!!variableSyms),
                     standardError = sd(!!variableSyms) / sqrt(n), 
                     ciLower = estimate - t * standardError, 
                     ciUpper = estimate + t * standardError, 
                     ciLevel = confLevel,
                     .groups = 'drop')

  ests
}

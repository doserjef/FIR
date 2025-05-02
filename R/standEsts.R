standEsts <- function(plotData, variable, grpBy = NULL,  
                      confLevel = 0.95, design = 'sys', 
                      standID = NULL, ...) {

  # Some initial checks ---------------------------------------------------
  if (missing(plotData)) {
    stop('plotData must be provided')
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

  # Prep the data for summarizing -----------------------------------------
  # Convert supplied characters to symbols
  variableSyms <- rlang::sym(variable)
  grpBySyms <- rlang::syms(grpBy)
  standIDSyms <- rlang::syms(standID)
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

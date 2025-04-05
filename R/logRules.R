logRules <- function(dia, logLength, rule, ...) {

  # Some initial checks ---------------------------------------------------
  if (missing(dia)) {
    stop('scaling diameter in inches (dia) must be specified')
  }
  if (missing(logLength)) {
    stop('log length in feet (logLength) must be specified') 
  }
  if (missing(rule)) {
    stop('log rule (rule) must be specified')
  }
  if (!is.numeric(dia)) {
    stop('dia must be a vector of numbers indicating the scaling diameter')
  }
  if (any(dia < 0)) {
    stop('at least one value in dia is negative. Scaling diameters can only be positive.') 
  }
  if (any(!is.numeric(logLength))) {
    stop('logLength must be a vector of numbers indicating the log lengths')
  }
  if (any(logLength < 0)) {
    stop('at least one value in logLength is negative. Log lengths can only be positive.')
  }
  if (!(tolower(rule) %in% c('international', 'doyle', 'scribner'))) {
    stop('rule must be one of the following: "international", "doyle", "scribner"') 
  }

  # Doyle -----------------------------------------------------------------
  if (tolower(rule) == 'doyle') {
    # TODO: give error when logLength > maximum scaling length
    out <- ((dia - 4) / 4)^2 * logLength
  }

  # Scribner --------------------------------------------------------------
  if (tolower(rule) == 'scribner') {
    # TODO: give error when logLength > maximum scaling length
    out <- (0.79 * dia^2 - 2 * dia - 4) * (logLength / 16)
  }

  # International 1/4 in. -------------------------------------------------
  if (tolower(rule) == 'international') {
    out.4 <- 0.22 * dia^2 - 0.71 * dia
    out.8 <- 0.44 * dia^2 - 1.20 * dia - 0.30 
    out.12 <- 0.66 * dia^2 - 1.47 * dia - 0.79
    out.16 <- 0.88 * dia^2 - 1.52 * dia - 1.36
    out.20 <- 1.10 * dia^2 - 1.35 * dia - 1.90
   
   if (logLength == 4) {
     out <- out.4
   } else if (logLength == 8) {
     out <- out.8
   } else if (logLength == 12) {
     out <- out.12
   } else if (logLength == 16) {
     out <- out.16
   } else if (logLength == 20) {
     out <- out.20
   } else if (logLength == 24) {
     out <- out.20 + out.4
   } else if (logLength == 28) {
     out <- out.20 + out.8
   } else if (logLength == 32) {
     out <- out.20 + out.12
   } else if (logLength == 36) {
     out <- out.20 + out.16
   } else if (logLength == 40) {
     out <- out.20 * 2
   } else {
     stop(paste0('logLength of ', logLength, ' is not supported for the international 1/4 rule function. Supported log lengths are 4, 8, 12, 16, 20, 24, 28, 32, 36, 40'))
   }
  }
  out
}

\name{logRules}
\alias{logRules}
\title{Compute board foot volumes from board foot log rules}

\usage{
logRules(dia, logLength, type, ...)
}

\description{
  Function for computing board foot volumes from commonly used board foot log rules in
  United States. Note this formula should not be used for calculating volume of standing 
  trees.
}

\arguments{

  \item{dia}{the scaling diameter of the log(s) in inches. The scaling diameter is the 
             diameter inside bark at the small end of the log.}

  \item{logLength}{the length of the log(s) in ft.}

  \item{type}{the name of the log rule to use for estimating lumber volume. Currently supported
              log rules are Doyle (\code{'doyle'}), Scribner (\code{'scribner'}), and 
              International 1/4 inch (\code{'international'}).}

  \item{...}{currently no additional arguments}

}

\author{
  Jeffrey W. Doser \email{jwdoser@ncsu.edu}, \cr
}

\value{
  A numeric vector containing the board foot volumes of the the logs given the scaling
  diameters, height, and log rule. 
}

\examples{
scaling.dias <- c(8, 20, 18, 34)
log.lengths <- c(16, 16, 12, 18)
logRules(dia = scaling.dias, logLength = log.lengths, type = 'doyle') 
}


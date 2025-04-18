\name{mesavageCubicFt}

\docType{data}

\alias{mesavageCubicFt}

\title{Cubic foot volume estimates of Mesavage (1947) for standing trees.}

\description{
A data frame containing the cubic foot volume estimates for standing trees 
presented by Mesavage (1947). This is used internally when calculating volumes
in \code{treeVolume}. 
}

\usage{
data(mesavageCubicFt)
}

\references{
  Mesavage, C. (1947). Tables for estimating cubic-foot volume of timber.
  USDA Forest Service, Washington, DC.
}


\format{
  \code{mesavageCubicFt} is a data frame with three columns: 

  \code{DBH}: the diameter at breast height in inches.

  \code{Height}: height measured as the number of 16-ft logs, rounded to the nearest half log.

  \code{Volume}: the corresponding volume for the given DBH and Height.
}

\keyword{datasets}

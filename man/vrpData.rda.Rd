\name{vrpData}

\docType{data}

\alias{vrpData}

\title{Hypothetical example variable radius plot data.}

\description{
A hypothetical data set of trees measured at three points using a BAF 10 wedge prism
in a variable radius plot framework. Each row is an individual tree. 
}

\usage{
data(vrpData)
}

\format{
  \code{vrpData} is a data frame with seven columns: 

  \code{Species}: species ID. 

  \code{PointID}: ID of each individual point that the given tree was measured at.

  \code{DBH_in}: diameter at breast height of each tree in inches.

  \code{Height_log}: merchantable height measured as the number of 16-ft logs (to the nearest half log).

  \code{DIA_Class}: 4-inch diameter class the given tree falls into. 
  
  \code{TreeNum}: the number of trees represented by each row. In this case, each row is a single tree, so this is just 1. 
}

\keyword{datasets}



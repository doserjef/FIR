\name{treeVolume}
\alias{treeVolume}
\title{Calculate board foot or cubic foot volumes of standing trees}

\usage{
treeVolume(dbh, mht, mht_units = 'log', gfc = 78, type, ...)
}

\description{
  Function for computing board foot or cubic foot volumes for standing trees based on the volume 
  tables published by Mesavage and Girard (1946). Note that the board foot volumes produced 
  from this function implement the equations published by Wiant Jr. (1986) and thus
  are approximations to the original tables published by Mesavage and Girard (1946). The cubic
  foot estimates are based on the original tables of Mesavage (1947).
}

\arguments{

  \item{dbh}{a vector of numeric values indicating the diameter at breast height 
             (4.5 in. above the ground) in inches for each standing tree.}

  \item{mht}{a vector of numeric values indicating the merchantable height of the 
             tree specified either in feet or in the number of 16 ft logs reported 
             to the nearest half log. Units are specified in \code{mht_units}.}

  \item{mht_units}{the units of the merchantable height measurements. Current options
                   include 16-ft logs (\code{'log'}) or feet (\code{'ft'}). Default is
                   16-ft logs.}

  \item{gfc}{a vector of numeric values indicating the Girard Form Class for each tree. 
             This can be a vector with a separate value for each tree, or a single value 
             if the same Girard form class is applied to all trees.}

  \item{type}{the name of the log rule or volume table to use for estimating volume. 
              Currently supported log rules are Doyle (\code{'doyle'}), 
              Scribner (\code{'scribner'}), and International 1/4 inch (\code{'international'}). 
              For cubic foot volumes, use \code{'mesavage_cubic_ft'} to provide estimates from 
              Mesavage (1947) GFC 78 volume table.

  \item{...}{currently no additional arguments}

}

\author{
  Jeffrey W. Doser \email{jwdoser@ncsu.edu}, \cr
}

\note{
  This code was adapted from David Larsen's forestry functions available at 
  \url{https://oakmissouri.org/forestfunction/}. 
}

\references{
  Mesavage, C., and Girard, J.W. (1946). Tables for estimating board-foot volume of timber.
  USDA Forest Service, Washington, DC. 

  Mesavage, C. (1947). Tables for estimating cubic-foot volume of timber.
  USDA Forest Service, Washington, DC.

  Wiant Jr., H.V. (1986). Formulas for Mesavage and Girard's volume tables. 
  Northern Journal of Applied Forestry. 3(3), 124
}

\value{
  A numeric vector containing the board foot volumes of the standing trees. 
}

\examples{
dbh <- c(10, 20, 18, 34)
mht_log <- c(1.5, 3, 2.5, 4) 
treeVolume(dbh = dbh, mht = mht_log, type = 'international') 
}


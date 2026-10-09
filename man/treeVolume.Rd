\name{treeVolume}
\alias{treeVolume}
\title{Calculate board foot or cubic foot volumes of standing trees}

\usage{
treeVolume(data, dbh, mht, type, mht_units = 'log', gfc = 78, ...)
}

\description{
  Function for computing board foot or cubic foot volumes for standing trees. Board foot volumes
  implement the equations published by Wiant Jr. (1986) and are approximations to the original
  tables published by Mesavage and Girard (1946). Cubic foot estimates are available either from
  the original tables of Mesavage (1947) or via Huber's Formula, which computes volume as the
  product of the tree's basal area (sq ft) and merchantable height (ft). Board foot and cubic
  foot volume types can be mixed within a single call, with the units of each tree indicated in
  the returned data frame.
}

\arguments{

  \item{data}{a data frame consisting of individual tree measurements. The rows correspond
              to individual trees and the columns correspond to different measurements and/or
              information on the tree. Column names supplied to \code{dbh}, \code{mht},
              \code{type}, and \code{gfc} must be given unquoted (e.g., \code{dbh = DBH}).}

  \item{dbh}{the unquoted name of the column in \code{data} that contains the diameter at
             breast height (4.5 in. above the ground) in inches for each standing tree.}

  \item{mht}{the unquoted name of the column in \code{data} that contains the merchantable
             height of each tree specified either in feet or in the number of 16 ft logs
             reported to the nearest half log. Units are specified in \code{mht_units}.}

  \item{type}{the log rule or volume formula to use for estimating volume. This can be
              either the unquoted name of a column in \code{data} that contains a separate
              value for each tree, or a single quoted value (e.g., \code{type = 'doyle'}) if
              the same volume type is applied to all trees. Currently supported board foot log rules are Doyle (\code{'doyle'}),
              Scribner (\code{'scribner'}), and International 1/4 inch (\code{'international'}).
              For cubic foot volumes, use \code{'mesavage_cubic_ft'} for estimates from the
              Mesavage (1947) GFC 78 volume table, or \code{'huber'} to apply Huber's Formula
              (basal area in sq ft multiplied by merchantable height in ft). Board foot and
              cubic foot types may be mixed freely across trees.}

  \item{mht_units}{the units of the merchantable height measurements. Current options
                   include 16-ft logs (\code{'log'}) or feet (\code{'ft'}). Default is
                   16-ft logs.}

  \item{gfc}{the Girard Form Class for each tree. This can be either the unquoted name of a
             column in \code{data} that contains a separate value for each tree, or a single
             numeric value if the same Girard form class is applied to all trees. Default is 78.}

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
  The input data frame \code{data} with two additional columns: \code{volume} (the estimated
  volume of each tree) and \code{units} (either \code{'board_ft'} or \code{'cubic_ft'}). If
  \code{type} is supplied as a single quoted value rather than a column in \code{data}, a
  third column \code{type} is also added containing the volume type used for each tree.
  If \code{data} already contains columns with any of these names, they are overwritten
  with a warning.
}

\examples{
trees <- data.frame(DBH = c(10, 20, 18, 34),
                    Height = c(1.5, 3, 2.5, 4))
# Board foot volume using the International 1/4 inch log rule for all trees
treeVolume(data = trees, dbh = DBH, mht = Height, type = 'international')
# Mixed: first two trees in board feet (Doyle), last two in cubic feet (Huber)
trees$VolType <- c('doyle', 'doyle', 'huber', 'huber')
treeVolume(data = trees, dbh = DBH, mht = Height, type = VolType)
# Girard form class supplied as a column
trees$GFC <- c(78, 80, 76, 78)
treeVolume(data = trees, dbh = DBH, mht = Height, type = VolType, gfc = GFC)
}


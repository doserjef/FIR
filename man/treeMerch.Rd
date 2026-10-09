\name{treeMerch}
\alias{treeMerch}
\title{Calculate monetary value of individual trees}

\usage{
treeMerch(data, pricing, mht_units = 'log', dbh_units = 'in', volume, ...)
}

\description{
  Function for computing monetary value for individual trees given a set 
  of DBH and merchantable height measurements, as well as timber price information 
  for the different products represented in the measurements.
}

\arguments{

  \item{data}{a data frame containing the individual tree measurements. Must be 
              a data frame with at least the following five columns: 
              \code{'Product_Type'} (the specific product that the given tree corresponds to), 
              \code{'Height'} (the merchantable height of the tree in 16-ft logs or ft), 
              \code{'Species'}, \code{'DBH'}, and \code{'GFC'} (Girard form class).
              If \code{volume} is supplied, only \code{'Product_Type'} and the
              column named in \code{volume} are required.}  

  \item{pricing}{a data frame containing pricing information for the different products 
                 represented in the \code{data$Product_Type}. The number of rows in the 
                 data frame should be equal to the total number of product types. The 
                 data frame must contain three columns: \code{"Product_Type"}, \code{"Price"},
                 and \code{"Vol_Type"} (the log rule or volume estimation approach). The 
                 price should be in the same units as the volume estimation approach. For
                 example, if \code{Vol_Type = 'Scribner'}, \code{Price} should indicate the
                 price in USD per Scribner board foot. Valid \code{Vol_Type} are
                 "scribner", "doyle", "international" (international 1/4 inch),
                 "mesavage_cubic_ft", and "huber". Different product types may use
                 different volume methods, including a mix of board foot and cubic foot
                 approaches. If \code{data} already contains a \code{"Price"} or
                 \code{"Vol_Type"} column, the values in \code{pricing} are used
                 instead, with a warning if the two disagree.}

  \item{mht_units}{the units of the merchantable height measurements. Current options
                   include 16-ft logs (\code{'log'}) or feet (\code{'ft'}). Default is
                   16-ft logs.}

  \item{dbh_units}{the units of the dbh measurements. Current options are inches (\code{'in'}) 
                   or centimeters (\code{'cm'}). Ignored if \code{volume} is supplied.}

  \item{volume}{optional unquoted name of a numeric column in \code{data} that
                contains pre-computed volume estimates for each tree (e.g., the
                \code{volume} column returned by \code{\link{treeVolume}}). If
                supplied, \code{treeVolume} is not called and these values are
                used directly to compute tree value, so they must be in the same
                units as the \code{Vol_Type} given for each product in
                \code{pricing}. \code{mht_units} and \code{dbh_units} are ignored.
                If not supplied (the default), volume is computed with
                \code{\link{treeVolume}}.}

  \item{...}{currently no additional arguments}

}

\author{
  Jeffrey W. Doser \email{jwdoser@ncsu.edu}, \cr
}

\value{
  A data frame of the same form as \code{data} with three additional columns:
  the volume type (\code{'Vol_Type'}), the volume units (\code{'Vol_Units'},
  either \code{'board_ft'} or \code{'cubic_ft'}), and the monetary value of
  the tree in whatever currency is specified in the \code{pricing} data frame
  (\code{'Value'}).
}

\examples{
data <- data.frame(Product_Type = 'PST', Height = 2, Species = 'Loblolly Pine', 
                   DBH = 20, GFC = 78)
price <- data.frame(Product_Type = 'PST', Price = 0.20, Vol_Type = 'Scribner')
treeMerch(data = data, pricing = price)

# Using volume already computed with treeVolume()
vol_data <- treeVolume(data = data, dbh = DBH, mht = Height,
                       type = 'scribner', gfc = GFC)
treeMerch(data = vol_data, pricing = price, volume = volume)
}


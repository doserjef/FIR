\name{treeMerch}
\alias{treeMerch}
\title{Calculate monetary value of individual trees}

\usage{
treeMerch(data, pricing, mht_units = 'log', dbh_units = 'in', ...)
}

\description{
  Function for computing monetary value (in USD) for individual trees given a set 
  of DBH and merchantable height measurements, as well as timber price information 
  for the different products represented in the measurements.
}

\arguments{

  \item{data}{a data frame containing the individual tree measurements. Must be 
              a data frame with at least the following five columns: 
              Product_Type (the specific product that the given tree corresponds to), 
              Height (the merchantable height of the tree in 16-ft logs or ft), 
              Species, DBH, and GFC (Girard form class).}  

  \item{pricing}{a data frame containing pricing information for the different products 
                 represented in the \code{data$Product_Type}. The number of rows in the 
                 data frame should be equal to the total number of product types. The 
                 data frame must contain three columns: \code{"Product_Type"}, \code{"Price"},
                 and \code{"Vol_Type"} (the log rule or volume estimation approach). The 
                 price should be in the same units as the volume estimation approach. For 
                 example, if \code{Vol_Type = 'Scribner'}, \code{Price} should indicate the 
                 price in USD per Scribner board foot. Valid \code{Vol_Type} are 
                 "scribner", "doyle", "international" (international 1/4 inch), and 
                 "mesavage_cubic_ft".}

  \item{mht_units}{the units of the merchantable height measurements. Current options
                   include 16-ft logs (\code{'log'}) or feet (\code{'ft'}). Default is
                   16-ft logs.}

  \item{dbh_units}{the units of the dbh measurements. Current options are inches (\code{'in'}) 
                   or centimeters (\code{'cm'}).}

  \item{...}{currently no additional arguments}

}

\author{
  Jeffrey W. Doser \email{jwdoser@ncsu.edu}, \cr
}

\value{
  A data frame of the same form as \code{data} with two additional columns 
  indicating the volume type (\code{'Vol_Type'}) and value of the tree in USD 
  (\code{'Value_USD'}). 
}

\examples{
data <- data.frame(Product_Type = 'PST', Height = 2, Species = 'Loblolly Pine', 
                   DBH = 20, GFC = 78)
price <- data.frame(Product_Type = 'PST', Price = 0.20, Vol_Type = 'Scribner')
treeMerch(data = data, pricing = price)
}


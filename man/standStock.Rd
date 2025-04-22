\name{standStock}
\alias{standStock}
\title{Calculate stand and stock tables given a set of tree measurements}

\usage{
standStock(treeData, plotID, variable, grpBy, plotType, plotSize, 
           BAF, baColumn, ...)
}

\description{
  Function for calculating stand and stock tables given a set of individual tree
  measurements. The function supports data collected using fixed radius plots
  or variable radius plots. Note that it is up to the user to make sure the units of all columns
  supplied to the arguments match up correctly. See the arguments below for specific
  details.
}

\arguments{

  \item{treeData}{a data frame consisting of individual tree measurements. The rows correspond
                  to individual trees and the columns correspond to different measurements and/or 
                  information on the tree. }

  \item{plotID}{a quoted name of the column in \code{treeData} that contains the ID
                of the plot on which each individual tree was measured.}

  \item{variable}{a quoted name of the variable that will be summarized into tabular format. 
                  This will determine whether the resulting table is a stand or stock table. 
                  The name of the variable must match a column in \code{treeData}.}

  \item{grpBy}{either one or two quoted names of the variable(s) that will be used to form 
               the groups of the stand or stock table. Users can summarize the \code{variable} by 
               one group (e.g., diameter class) or two groups (e.g., diameter class and species). 
               The names must correspond to columns in \code{treeData}.}

  \item{plotType}{a quoted word indicating whether data were collected with fixed radius plots
                  (\code{plotType = 'fixed'}) or variable radius plots (\code{plotType = 'variable'}).}

  \item{plotSize}{a quoted word indicating the column in \code{treeData} that contains the plot size
                  on which the individual tree was measured. This is only relevant when \code{plotType = 'fixed'}. 
                  NOTE: the plot size should be provided in the area units that you want to use 
                  for calculating per unit area values in the stand or stock table. For example, if you 
                  want estimates on a per acre basis, the plot size should be reported in acres. If the
                  plot size is reported in square feet, the estimates will
                  be on a square foot basis (i.e., no conversions to acres will be done).}

  \item{BAF}{a numeric value indicating the basal area factor used for variable radius plots. This 
             is only relevant when \code{plotType = 'variable'}.}

  \item{baColumn}{a quoted name of the column in \code{treeData} that contains the basal 
                  area for each individual tree. This is only required when \code{plotType = 'variable'}.}

  \item{...}{currently no additional arguments}

}

\author{
  Jeffrey W. Doser \email{jwdoser@ncsu.edu}, \cr
}

\value{
  If a single variable is supplied to \code{grpBy}, a single data frame will be returned 
  containing the stand and stock table. If there are two grouping variables (e.g., species 
  and diameter class), then two data frames will be returned in a list. The data frames consist
  of the same stand/stock table information, but presented in different ways. One data frame
  will be in long format which is most relevant for making visual representations of the 
  stand/stock table. The other is in wide format which is the format most relevant for 
  tabular presentation of the stand/stock table.
}

\examples{
# Load the example variable radius plot sampling data
data(vrpData)

# Look at vrpData 
vrpData

# Calculate a stand table showing trees per acre across the two species
# and four diameter classes.
standStock(treeData = vrpData, plotID = 'PointID', variable = 'TreeNum', 
           grpBy = c('Species', 'DIA_Class'), plotType = 'variable', 
           BAF = 10, baColumn = 'BA_sq_ft')
}


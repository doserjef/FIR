\name{calcEsts}
\alias{calcEsts}
\title{Calculate estimates and confidence intervals of forest variables per unit area}

\usage{
calcEsts(treeData, plotID, variable, grpBy = NULL, plotType, plotSize, 
           BAF, baColumn, confIntLevel = 0.95, design = 'sys', 
           returnPlotSummaries = FALSE, ...)
}

\description{
  Function for calculating estimates of forest variables on a per unit area basis 
  given a set of individual tree measurements. Tree measurements are assumed to be 
  collected using variable radius or fixed radius plots. Confidence intervals for the 
  estimate are also calculated. Estimates can be obtained for the entire data set or 
  can be estimated distinctly for individual subgroups (e.g., species, diameter class). 
}

\arguments{

  \item{treeData}{a data frame consisting of individual tree measurements. The rows correspond
                  to individual trees and the columns correspond to different measurements and/or 
                  information on the tree. }

  \item{plotID}{a quoted name of the column in \code{treeData} that contains the ID
                of the plot on which each individual tree was measured.}

  \item{variable}{a quoted name of the variable that will be estimated on a per unit area basis.  
                  The name of the variable must match a column in \code{treeData}.}

  \item{grpBy}{quoted names of the variable(s) that will be used to group estimates by. 
               Users can estimate the \code{variable} by multiple different groups if desired 
               (e.g., diameter class, species). The names must correspond to columns in \code{treeData}. 
               Note that if \code{grpBy} is not specified, then estimates will be calculated across
               the entire data set.}

  \item{plotType}{a quoted word indicating whether data were collected with fixed radius plots
                  (\code{plotType = 'fixed'}) or variable radius plots (\code{plotType = 'variable'}).}

  \item{plotSize}{a numeric value indicating the size of the plots used when sampling was 
                  done with fixed radius plots. This is only relevant when \code{plotType = 'fixed'}. 
                  NOTE: the plot size should be provided in the area units that you want to use 
                  for calculating per unit area values in the stand or stock table. For example, if you 
                  want estimates on a per acre basis, the plot size should be reported in acres. If the
                  plot size is reported in square feet, the estimates will
                  be on a square foot basis (i.e., no conversions to acres will be done).}

  \item{BAF}{a numeric value indicating the basal area factor used for variable radius plots. This 
             is only relevant when \code{plotType = 'variable'}.}

  \item{baColumn}{a quoted name of the column in \code{treeData} that contains the basal 
                  area for each individual tree. This is only required when \code{plotType = 'variable'}.}

  \item{confIntLevel}{the confidence level to use in confidence interval calculations. Default is 0.95 for a 95\% confidence interval.}

  \item{design}{the sampling design used. Currently only supports a systematic (\code{'sys'}) or simple random sampling without replacement (\code{srswor}) design.}

  \item{returnPlotSummaries}{a logical value taking value }

  \item{...}{currently no additional arguments}

}

\author{
  Jeffrey W. Doser \email{jwdoser@ncsu.edu}, \cr
}

\value{
  A data frame (tibble) where the rows correspond to the different groups for which estimates are obtained
  and the columns correspond to different components of the estimate. Columns included in the data frame are: 

  \item{n}{: the number of plots/points used to generate the estimate.}

  \item{t}{: the t-value used in the confidence interval calculation.}

  \item{estimate}{: the estimate of the variable of interest on a per unit area basis.}

  \item{standardError}{: the standard error of the estimate.}

  \item{ciLower}{: the lower bound of the confidence interval for the estimate.}

  \item{ciUpper}{: the upper bound of the confidence interval for the estimate.} 

  \item{ciLevel}{: the confidence level used to construct the confidence interval.}

  If \code{returnPlotSummaries = TRUE}, a list is returned instead of a data frame. The 
  list consists of two data frames, one containing point/plot level estimates and the 
  second containing the population estimates described above. 

}

\examples{
# Load the example variable radius plot sampling data
data(vrpData)

# Look at vrpData 
vrpData

# Calculate a stand table showing trees per acre across the two species
# and four diameter classes.
calcEsts(treeData = vrpData, plotID = 'PointID', variable = 'TreeNum', 
           grpBy = c('DIA_Class'), plotType = 'variable', 
           BAF = 10, baColumn = 'BA_sq_ft', confIntLevel = 0.95)
}


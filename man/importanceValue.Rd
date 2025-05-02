\name{importanceValue}
\alias{importanceValue}
\title{Calculate the importance value of a set of tree species}

\usage{
importanceValue(treeData, plotID, baColumn, species, standID, ...)
}

\description{
  Function for calculating the importance value of each individual species 
  based on the species frequency (number of plots at which the species
  was observed), abundance (proportion of the total number of individual trees 
  that belong to the given species), and dominance (proportion of the basal area of
  all species that belongs to the given species). In other words, the importance value
  is the sum of the relative frequency, relative abundance, and relative dominance, 
  multiplied by 100, such that the maximum possible value is 300. 
}

\arguments{

  \item{treeData}{a data frame consisting of individual tree measurements. The rows correspond
                  to individual trees and the columns correspond to different measurements and/or 
                  information on the tree. }

  \item{plotID}{a quoted name of the column in \code{treeData} that contains the ID
                of the plot on which each individual tree was measured.}

  \item{baColumn}{a quoted name of the column in \code{treeData} that contains the basal 
                  area for each individual tree.}

  \item{species}{a quoted name of the column in \code{treeData} that contains the species
                 ID for each individual tree.}

  \item{standID}{a quoted name of a column in \code{treeData} that contains the stand 
                 ID. If this is specified, importance values will be provided separately 
                 for each stand in the data set. Note this doesn't necessarily need to 
                 correspond to stands, but could indicate some other form of geographical
                 unit (e.g., management unit, county, state).}

  \item{...}{currently no additional arguments}

}

\author{
  Jeffrey W. Doser \email{jwdoser@ncsu.edu}, \cr
}

\value{
  A data frame (tibble) where the rows correspond to the different species. Columns included in the data frame are the species ID, the frequency of the species, the abundance of the species, the dominance of the species, and the overall importance value for the species.

}

\examples{
# Load the example variable radius plot sampling data
data(vrpData)

# Look at vrpData 
vrpData

# Calculate a stand table showing trees per acre across the two species
# and four diameter classes.
importanceValue(treeData = vrpData, plotID = 'PointID', baColumn = 'BA_sq_ft', 
                species = 'Species')
}


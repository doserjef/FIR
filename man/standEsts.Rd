\name{standEsts}
\alias{standEsts}
\title{Calculate estimates and confidence intervals from plot-level data}

\usage{
standEsts(plotData, variable, grpBy = NULL, confLevel = 0.95, 
          design = 'sys', standID = NULL, ...)
}

\description{
  Function for generating estimates of means, standard errors, and confidence
  intervals from a set of plot-level forest variables. This differs from the 
  \code{calcEsts} function in that \code{standEsts} takes plot-level data as 
  an input, while \code{calcEsts} takes tree-level data and expands it to a 
  per-acre basis. Confidence intervals for the estimate are also calculated. 
  Estimates can be obtained for the entire data set or can be estimated 
  distinctly for individual subgroups (e.g., species, diameter class). In 
  comparison to \code{calcEsts}, \code{standEsts} can be used to generate 
  estimates of means and confidence intervals for variables that are measured 
  at the plot-level and not for each individual tree (e.g., dominant height). 
  Note that \code{standEsts} does not do any sort of expansion, it assumes that 
  the plot-level data are already expanded to the per unit area basis or that 
  they do not need to be expanded.
}

\arguments{

  \item{plotData}{a data frame consisting of plot-level measurements. The rows correspond
                  to plots and the columns correspond to different variables. }

  \item{variable}{a quoted name of the variable that will be estimated.  
                  The name of the variable must match a column in \code{plotData}.}

  \item{grpBy}{quoted names of the variable(s) that will be used to group estimates by. 
               Users can estimate the \code{variable} by multiple different groups if desired 
               (e.g., diameter class, species). The names must correspond to columns in \code{plotData}. 
               Note that if \code{grpBy} is not specified, then estimates will be calculated across
               the entire data set.}

  \item{confLevel}{the confidence level to use in confidence interval calculations. Default is 0.95 for a 95\% confidence interval.}

  \item{design}{the sampling design used. Currently only supports a systematic (\code{'sys'}) or simple random sampling without replacement (\code{srswor}) design.}

  \item{standID}{a quoted name of a column in \code{plotData} that contains the stand 
                 ID. If this is specified, estimates will be provided separately 
                 for each stand in the data set. Note this doesn't necessarily need to 
                 correspond to stands, but could indicate some other form of geographical
                 unit (e.g., management unit, county, state).}

  \item{...}{currently no additional arguments}

}

\author{
  Jeffrey W. Doser \email{jwdoser@ncsu.edu}, \cr
}

\value{
  A data frame (tibble) where the rows correspond to the different groups for which estimates are obtained
  and the columns correspond to different components of the estimate. Columns included in the data frame are: 

  \item{n}{ the number of plots/points used to generate the estimate.}

  \item{t}{ the t-value used in the confidence interval calculation.}

  \item{estimate}{ the estimate of the variable of interest on a per unit area basis.}

  \item{standardError}{ the standard error of the estimate.}

  \item{ciLower}{ the lower bound of the confidence interval for the estimate.}

  \item{ciUpper}{ the upper bound of the confidence interval for the estimate.} 

  \item{ciLevel}{ the confidence level used to construct the confidence interval.}

}

\examples{
ex_plot_data <- data.frame(plotID = c(1, 2, 3, 4, 5, 6, 7, 8), 
                           domHeight = c(50, 57, 56, 45, 68, 78, 52, 66))

standEsts(plotData = ex_plot_data, variable = 'domHeight', confLevel = 0.90)


}


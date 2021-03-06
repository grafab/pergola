% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/map.R
\name{pullMap}
\alias{pullMap}
\title{Creates map object}
\usage{
pullMap(rf, split, fun = "haldane", corr = 1)
}
\arguments{
\item{rf}{Matrix of pairwise recombination frequencies.}

\item{split}{Split object.}

\item{fun}{Function to space the markers on the map. 
Default is "haldane". Alternatives are "kosambi", "carter" and "none.}

\item{corr}{Corrector, if recombinations are overestimated.
Allows to multiply all spaces by a fixed factor.}
}
\value{
Ordered vector of marker locations. Each marker has a name attribute.
}
\description{
Creates map object from matrix of pairwise recombination frequencies.
}
\examples{
data(simTetra)
simTetrageno <- basesToGenotypes(simTetra, 4)
rfMat <- calcRec(simTetrageno, 4)
split <- splitChr(rfMat, nchr = 7)
split <- sortLeafs(rfMat, split)
pullMap(rfMat, split = split)   
}


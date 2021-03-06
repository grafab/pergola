% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/map.R
\name{swapChrs}
\alias{swapChrs}
\title{Swap chromosomes}
\usage{
swapChrs(map, comp)
}
\arguments{
\item{map}{Map to switch.}

\item{comp}{Other map for comparison.}
}
\value{
map
}
\description{
Finds best matching chromosome for each chromosome and brings them into the same order.
}
\examples{
data(simTetra)
simTetrageno <- basesToGenotypes(simTetra, 4)
rfMat <- calcRec(simTetrageno, 4)
split <- splitChr(rfMat, nchr = 7)
split <- sortLeafs(rfMat, split)
map <- pullMap(rfMat, split = split)   
split <- sortLeafs(rfMat, split, method = "endlink")
map2 <- pullMap(rfMat, split = split)   
map <- swapChrs(map, map2)
}


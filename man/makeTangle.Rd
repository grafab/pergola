% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.R
\name{makeTangle}
\alias{makeTangle}
\title{Create a gray scale tanglegram}
\usage{
makeTangle(dend1, dend2, cutheight, k = NULL, ncol = k, ...)
}
\arguments{
\item{dend1}{First dendrogram. Required.}

\item{dend2}{Second dendrogram. Required.}

\item{cutheight}{The height, at which dend1 is cut. Influences number of colors.}

\item{k}{Number of desired linkage groups.}

\item{ncol}{Number of desired colors.}

\item{...}{Other parameters are forwarded to the tanglegram command.}
}
\value{
None. Plotting only.
}
\description{
Create tanglegram. Removes markers, that are not in both trees.
Calculates alternating light and dark shades of grey.
This function is a wrapper to functions of \pkg{dendextend}
}
\examples{
data(simTetra)
simTetraGen <- basesToGenotypes(simTetra, 4)
rfMat <- calcRec(simTetraGen, 4)
split <- splitChr(rfMat, nchr = 7)
split <- sortLeafs(rfMat, split)
map <- pullMap(rfMat, split = split)
dend <- mapToDend(map)
makeTangle(dend, dend, cutheight = 500, k = 7, ncol = 7)
}
\references{
Galili T. (2015), dendextend: an R package for visualizing, 
adjusting and comparing trees of hierarchical clustering, Bioinformatics
}


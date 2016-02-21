
#TODO required?
#' Wrapping function for hierarchical clustering
#' 
#' Clusters with fixed parameters
#'  
#' @param rfMat Matrix of pairwise recombination frequencies.
#' @return None.
#' @examples
#' data(simTetra)
#' simTetrageno<-bases2genotypes(simTetra,4)
#' rfMat<-calcRec(simTetrageno,4)
#' polyploid.hclust(rfMat)
#' @keywords internal
polyploid.hclust <- function(rfMat){
  return(hclust(as.dist(rfMat), method = "single"))
}





#' Reinsert duplicates
#' 
#' Adds previously filtered duplicates back into the map at the same position as their duplicate.
#'  
#' @param rf Matrix of pairwise recombination frequencies.
#' @param ord Vector with marker order.
#' @param split Vector of cluster numbers, created by splitChr(). Zeros indicated filtered markers and will be ignored.
#' @return Vector with unique marker order.
#' @return Matrix with equally good marker orders.
#' @examples
#' data(simTetra)
#' simTetrageno<-bases2genotypes(simTetra,4)
#' rfMat<-calcRec(simTetrageno,4)
#' split<-splitChr(rfMat,nchr=7)
#' order<-sortLeafs(rfMat,split)
#' oiu(rfMat,ord,split,max.n=6)   
#' @export
returnDup <- function(){
  
}






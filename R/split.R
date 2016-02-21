
#
#' Split markers into chromosomes
#' 
#' This function splits markers into linkage groups (LG), which ideally represent chromosomes.
#' The split is based on hierarchical clustering with a single linkage distance.
#'  
#' @param rf Matrix of pairwise recombination frequencies.
#' @param height Threshold value for grouping the markers.
#' @param nchr Expected number of chromosomes.
#' @param method Default is "single", which is used for the hierarchical clustering.
#' @param filter Logical, if the result should be filtered or not. Default is FALSE. Creates zeros for the markers below the threshold.
#' @param thresh Threshold for filtering. Default is 0.05, i.e. linkage groups with less than 5\% of markers, are filtered out.
#' @return Vector of cluster relationship. Same length and order as the matrix of recombination frequencies.
#' @examples
#' data(simTetra)
#' simTetrageno<-bases2genotypes(simTetra, 4)
#' rfMat<-calcRec(simTetrageno, 4)
#' splitChr(rfMat, nchr = 7)
#' @export
splitChr <- function(rf, height = 0.4, nchr = NULL, method = "single", filter = FALSE, 
                     thresh = 0.05, rm.dup = TRUE, ...){
  df <- data.frame(names = rownames(rf), split = 1, dup = 0)
  rownames(df) <- df$names
  if(rm.dup == TRUE){
    zeroes <- which(rf == 0, arr.ind = TRUE)
    offdiag <- which(zeroes[, 1] < zeroes[, 2]) #!= leads to both 
    for(j in offdiag){
      df$split[zeroes[j, 2]] <- 0
      df$dup[zeroes[j, 2]] <- zeroes[j, 1]
    }
  } 
  rfsub <- rf[df$split > 0, df$split > 0]
  tree <- hclust(as.dist(rfsub), method = method)
  minleaves <- thresh * ncol(rfsub)
  if(!is.null(nchr)){
    output <- cutree(tree = tree, k = nchr)
    while(filter){
      filtClus <- which(table(output) < minleaves)
      if(length(filtClus) > 0){
        tooFilt <- which(output %in% filtClus)
        if(length(tooFilt) < ncol(rfsub)){
          df$split[df$split > 0][tooFilt] <- 0
          rfsub <- rfsub[-tooFilt, -tooFilt]
          tree <- hclust(as.dist(rfsub), method = method)
          output <- cutree(tree = tree, k = nchr)
        }else{
          stop("Could not split data into ", nchr, " clusters." )
        }
      }else{
        filter <- FALSE
        #df$split[df$split > 0] <- output
      }
    }
  }else{
    output <- cutree(tree = tree, h = height)  
    if(filter){
      filtClus <- which(table(output) < minleaves)
      output[output %in% filtClust] <- 0
      #df$split[df$split>0] <- output
      
    }
  }
  df$split[df$split>0] <- output
  return(df)   
}

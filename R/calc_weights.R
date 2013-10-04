#' @title Calculate weights
#' @description Calculate weights for each of the nearest neighbours
#' @param x a matrix containing a shadow manifold of dimension (L x E)
#' @param dists a matrix of euclidian distances between all points on the manifold x
#' @param index atomic vector containing the index of the point in x from which to calculate nn weights.
#' @param nn a vector of indicies of the nearest neighbours
#' @return list
#' @keywords weights
#' @seealso \code{\link{get_nn}} 
#' @examples
#' #str_length(letters)
#' #str_length(c("i", "like", "programming", NA))
calc_weights <-
function(x,dists,index,nn)
{
    E <- ncol(x)
    u <- exp(-dists[index,nn[index,]]/dists[index,nn[index,1]])
    w <- u / sum(u)
    ## Return weights and time indicies ##
    return(list(w=w,nn=nn[index,]))
}

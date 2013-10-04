#' @title Get nearest neighbours
#' @description Finds the indicies of the nearest neighbours to a point on a manifold
#' @param x a shadow manifold 
#' @param dists a matrix of euclidian distances between all points on the manifold x
#' @return a vector of indicies of the E+2 nearest neighbours
#' @keywords neighbours
#' @seealso \code{\link{get_nn}} 
#' @examples
#' #str_length(letters)
#' #str_length(c("i", "like", "programming", NA))
get_nn <-
function(x,dists)
{
    E <- ncol(x)
    nn <- t(apply(dists,1,order))
    nn <- nn[,2:(E+2)]
    return(nn)
}

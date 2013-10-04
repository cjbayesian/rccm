#' @title Generate lagged vectors
#' @description Generates lagged vectors and combines them into a shadow manifold
#' @param X a chronological series of observations
#' @param tau the lag a which to generate each lagging vector
#' @param E the number of lags to include
#' @return list
#' @keywords weights
#' @seealso \code{\link{get_nn}} 
#' @examples
#' #str_length(letters)
#' #str_length(c("i", "like", "programming", NA))
gen_lagged_vec <-
function(X, tau=1, E=3)
{
    L <- length(X)
    x <- array(dim=c(L-(E-1)*tau,E*tau))
    tmap <- (1+(E-1)*tau):L
    for(i in tmap)
        x[i-((E-1)*tau),] <- X[seq(i,(i-(E-1)*tau),-tau)]

    return(list(x=x,tmap=tmap))
}

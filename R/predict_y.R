#' @title Predict Y
#' @description Predicts the value of an observation in series Y given the weights and nearest neighbours
#' @param y a shadow manifold
#' @param wt a vector of weights on which to calculate a spatial average
#' @return the predicted value of y
#' @keywords predict
#' @seealso \code{\link{calc_weights}} 
#' @examples
#' #str_length(letters)
#' #str_length(c("i", "like", "programming", NA))
predict_y <-
function(y,wt)
{
    sum(wt$w * y[wt$nn])
}

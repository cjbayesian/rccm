\name{calc_weights}
\alias{calc_weights}
\title{Calculate weights}
\usage{
  calc_weights(x, dists, index, nn)
}
\arguments{
  \item{x}{a matrix containing a shadow manifold of
  dimension (L x E)}

  \item{dists}{a matrix of euclidian distances between all
  points on the manifold x}

  \item{index}{atomic vector containing the index of the
  point in x from which to calculate nn weights.}

  \item{nn}{a vector of indicies of the nearest neighbours}
}
\value{
  list
}
\description{
  Calculate weights for each of the nearest neighbours
}
\examples{
#str_length(letters)
#str_length(c("i", "like", "programming", NA))
}
\seealso{
  \code{\link{get_nn}}
}
\keyword{weights}


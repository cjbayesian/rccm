\name{get_nn}
\alias{get_nn}
\title{Get nearest neighbours}
\usage{
  get_nn(x, dists)
}
\arguments{
  \item{x}{a shadow manifold}

  \item{dists}{a matrix of euclidian distances between all
  points on the manifold x}
}
\value{
  a vector of indicies of the E+2 nearest neighbours
}
\description{
  Finds the indicies of the nearest neighbours to a point
  on a manifold
}
\examples{
#str_length(letters)
#str_length(c("i", "like", "programming", NA))
}
\seealso{
  \code{\link{get_nn}}
}
\keyword{neighbours}


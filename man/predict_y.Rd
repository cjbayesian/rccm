\name{predict_y}
\alias{predict_y}
\title{Predict Y}
\usage{
  predict_y(y, wt)
}
\arguments{
  \item{y}{a shadow manifold}

  \item{wt}{a vector of weights on which to calculate a
  spatial average}
}
\value{
  the predicted value of y
}
\description{
  Predicts the value of an observation in series Y given
  the weights and nearest neighbours
}
\examples{
#str_length(letters)
#str_length(c("i", "like", "programming", NA))
}
\seealso{
  \code{\link{calc_weights}}
}
\keyword{predict}


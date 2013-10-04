\name{gen_lagged_vec}
\alias{gen_lagged_vec}
\title{Generate lagged vectors}
\usage{
  gen_lagged_vec(X, tau = 1, E = 3)
}
\arguments{
  \item{X}{a chronological series of observations}

  \item{tau}{the lag a which to generate each lagging
  vector}

  \item{E}{the number of lags to include}
}
\value{
  list
}
\description{
  Generates lagged vectors and combines them into a shadow
  manifold
}
\examples{
#str_length(letters)
#str_length(c("i", "like", "programming", NA))
}
\seealso{
  \code{\link{get_nn}}
}
\keyword{weights}


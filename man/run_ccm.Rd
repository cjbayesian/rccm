\name{run_ccm}
\alias{run_ccm}
\title{Run Convergent Cross Mapping}
\usage{
  run_ccm(X, Y, tau = 1, E = 2, randomWindow = FALSE,
    reps = 1)
}
\arguments{
  \item{X}{a vector containing series of observations}

  \item{Y}{a vector containing series of observations}

  \item{tau}{the lag a which to generate each lagging
  vector}

  \item{E}{the number of lags to include}

  \item{randomWindow}{logical indicating how to select the
  window on which to compute shadow manifolds. If set to
  FALSE, will use the window beginning at (E-1)tau.}

  \item{reps}{number of repeated random window samples on
  which to average the cross correlation}
}
\value{
  the predicted value of y
}
\description{
  Uses a leave one out cross validation to calculate the
  predictability of X|Y and Y|X.
}
\examples{
#str_length(letters)
#str_length(c("i", "like", "programming", NA))
}
\seealso{
  \code{\link{calc_weights}}
}
\keyword{predict}


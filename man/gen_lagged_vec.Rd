\name{gen_lagged_vec}
\alias{gen_lagged_vec}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
gen_lagged_vec(X, tau = 1, E = 3)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
%%     ~~Describe \code{X} here~~
}
  \item{tau}{
%%     ~~Describe \code{tau} here~~
}
  \item{E}{
%%     ~~Describe \code{E} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (X, tau = 1, E = 3) 
{
    L <- length(X)
    x <- array(dim = c(L - (E - 1) * tau, E * tau))
    tmap <- (1 + (E - 1) * tau):L
    for (i in tmap) x[i - ((E - 1) * tau), ] <- X[seq(i, (i - 
        (E - 1) * tau), -tau)]
    return(list(x = x, tmap = tmap))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

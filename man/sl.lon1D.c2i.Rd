\name{sl.lon1D.c2i}
\alias{sl.lon1D.c2i}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Convert Center Longitudes to Interface Longitudes
}
\description{
Convert a vector of center longitudes to a vector of interface longitudes, that is, longitudes between the center longitudes and 'extrapolated' boundary longitudes. It is ensured that the extrapolated values do not increase the range beyond 360 degree (see comment below in 'Details').
}
\usage{
sl.lon1D.c2i(lon.c)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lon.c}{
  a vector of length \code{N} with center longitudes in increasing or decreasing order.
}
}
\details{
THE CHECK OF EXTRAPOLATED VALUES NEEDS OPTIMIZATION (CURRENTLY ONLY GOOD FOR 'SMALL ADJUSTMENTS')!
}
\value{
A vector of length \code{N+1} with interface longitudes in increasing or decreasing order.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Helge Goessling
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
sl.lon1D.c2i(c(-100,-50,0,50))
## [1] -125  -75  -25   25   75
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

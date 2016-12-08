\name{sl.lat1D.c2i}
\alias{sl.lat1D.c2i}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Convert Center Latitudes to Interface Latitudes
}
\description{
Convert a vector of center latitudes to a vector of interface latitudes, that is, latitudes between the center latitudes and 'extrapolated' boundary latitudes. It is ensured that the extrapolated values are within the valid range [-90,90].
}
\usage{
sl.lat1D.c2i(lat.c)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lat.c}{
  a vector of length \code{N} with center latitudes in increasing or decreasing order.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A vector of length \code{N+1} with interface latitudes in increasing or decreasing order.
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
sl.lat1D.c2i(lat.c=c(70,76,82,88))
## Should return:
## [1] 67 73 79 85 90
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

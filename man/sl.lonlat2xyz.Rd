\name{sl.lonlat2xyz}
\alias{sl.lonlat2xyz}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Convert Lon-Lat to X-Y-Z
}
\description{
Covert a point from longitude-latitude to x-y-z coordinates on a unit sphere.
}
\usage{
sl.lonlat2xyz(lonlatvec)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lonlatvec}{
  a vector of length 2 with the longitude of the point at the first position and the latitude of the point at the second position.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A vector of length 3 with the x, y, and z positions of the point on the unit sphere.
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
\code{\link{sl.xyz2lonlat}}
}
\examples{
sl.lonlat2xyz(c(10,20))
## Should return:
## [1] 0.9254166 0.1631759 0.3420201
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

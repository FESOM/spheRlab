\name{sl.xyz2lonlat}
\alias{sl.xyz2lonlat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Convert X-Y-Z to Lon-Lat
}
\description{
Covert a point from x-y-z to longitude-latitude coordinates.
}
\usage{
sl.xyz2lonlat(xyzvec)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xyzvec}{
  a vector of length 3 with the x, y, and z coordinates of the point.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A vector of length 2 with the longitude of the point at the first position and the latitude of the point at the second position.
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
\code{\link{sl.lonlat2xyz}}
}
\examples{
sl.xyz2lonlat(c(0,1/sqrt(2),1/sqrt(2)))
## Should return:
## [1] 90 45
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

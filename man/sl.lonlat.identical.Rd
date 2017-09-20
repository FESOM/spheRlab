\name{sl.lonlat.identical}
\alias{sl.lonlat.identical}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Check if Lon-Lat Vectors Share Elements
}
\description{
Check if two pairs of longitude and latitude vectors have identical element pairs. Note that the comparison is done element-wise: if identical coordinates are contained but at different positions in the vector pairs, that's not identified.
}
\usage{
sl.lonlat.identical(lon1, lat1, lon2, lat2, recycle = FALSE, tolerance = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lon1}{
  a vector giving the longitudes of the first set of points.
}
  \item{lat1}{
  a vector giving the latitudes of the first set of points.
}
  \item{lon2}{
  a vector giving the longitudes of the second set of points.
}
  \item{lat2}{
  a vector giving the latitudes of the second set of points.
}
  \item{recycle}{
  a logical value indicating whether the coordinate vectors shall be recycled if they are shorter than the longest one of them.
}
  \item{tolerance}{
  a scalar giving the numerical tolerance, that is, by how much coordinates may differ to be counted as 'identical' (in degrees, applied separately to the longitudes and the latitudes).
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A vector with logical values indicating at which positions points are identical (or closer than the tolerance).
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Helge Goessling
}
\note{
This function is used in \code{\link{sl.plot.polygon}}.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
sl.lonlat.identical(lon1=1:5,lat1=rep(10,5),lon2=rep(4,5),lat2=13:9)
## Should return:
## [1] FALSE FALSE FALSE  TRUE FALSE
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

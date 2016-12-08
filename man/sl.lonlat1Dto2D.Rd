\name{sl.lonlat1Dto2D}
\alias{sl.lonlat1Dto2D}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Inflate Lat-Lon Vectors to Matrices
}
\description{
Inflate latitude and/or longitude vectors to corresponding matrices. By default, rows correspond to latitudes and columns correspond to longitudes; this can be changed by setting \code{swap=TRUE}. If only one vector (longitudes or latitudes) is provided, the number of latitudes or longitudes to which the vector shall be inflated must be provided.
}
\usage{
sl.lonlat1Dto2D(lon = NULL, lat = NULL, Nlon = NULL, Nlat = NULL, swap = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lon}{
  a vector with longitudes.
}
  \item{lat}{
  a vector with latitudes.
}
  \item{Nlon}{
  a scalar giving the number of longitudes to which the latitudes vector shall be inflated. Used only if \code{lon=NULL}.
}
  \item{Nlat}{
  a scalar giving the number of latitudes to which the longitudes vector shall be inflated. Used only if \code{lat=NULL}.
}
  \item{swap}{
  a logical value indicating whether the rows and columns of the output shall be swapped. By default (\code{swap=FALSE}), rows correspond to latitudes and columns correspond to longitudes.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
  \item{lon}{
  a matrix giving longitudes, inflated from the argument \code{lon} to be repeated at different latitudes.
}
  \item{lat}{
  	a matrix giving latitudes, inflated from the argument \code{lat} to be repeated at different longitudes.
}
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
sl.lonlat1Dto2D(lon=seq(-150,150,60),lat=c(-60,60,60))
## Should return:
## $lon
##      [,1] [,2] [,3] [,4] [,5] [,6]
## [1,] -150  -90  -30   30   90  150
## [2,] -150  -90  -30   30   90  150
## [3,] -150  -90  -30   30   90  150
## 
## $lat
##      [,1] [,2] [,3] [,4] [,5] [,6]
## [1,]  -60  -60  -60  -60  -60  -60
## [2,]   60   60   60   60   60   60
## [3,]   60   60   60   60   60   60
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

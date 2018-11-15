\name{sl.lonlat2D.c2i}
\alias{sl.lonlat2D.c2i}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Convert Curvilinear Lon-Lat Grid Center Locations to Interface Locations
}
\description{
Convert matrices of center longitudes and latitudes to corresponding matrices of interface coordinates, that is, the bounding longitudes and latitudes.
}
\usage{
sl.lonlat2D.c2i(lon.c, lat.c, extrapol.factor = 0.5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lon.c}{
  an \code{Nrow}x\code{Ncol} matrix with center longitudes of a curvilinear grid.
}
  \item{lat.c}{
  an \code{Nrow}x\code{Ncol} matrix with center latitudes of a curvilinear grid.
}
  \item{extrapol.factor}{
  a scalar specifiying how far the outer boundaries are extrapolated from the outermost centre locations. Default is \code{extrapol.factor=0.5}, meaning that the distance from the outermost centre locations to the outermost interface locations is \code{0.5} times the distance between adjacent centre locations.
}
}
\value{
  \item{lon.i}{
  an \code{Nrow+1}x\code{Ncol+1} matrix with interface longitudes of a curvilinear grid.
}
  \item{lat.i}{
  an \code{Nrow+1}x\code{Ncol+1} matrix with interface latitudes of a curvilinear grid.
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
\code{\link{sl.plot.fld.curvilin}}
}
\examples{
## To be provided ...
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

\name{sl.ll.rec}
\alias{sl.ll.rec}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Longitude/Latitude grid reconstruction
}
\description{
This function generates two \code{M x N} lon and lat grids, given lon and lat as vectors.
}
\usage{
sl.ll.rec(Longitude, Latitude)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Longitude}{
  A vector of of length \code{M}, containing the longitude coordinates of a general gridded variable.
}
  \item{Latitude}{
  A vector of of length \code{N}, containing the latitude coordinates of a general gridded variable.
}
}
\details{
This function provides the correct Longitude/Latitude input for the function \code{sl.mhd.edge}. This is particularly relevant when we are interested in a contour line (as example the sea ice edge) and longitude and latitude coordinates are on regular grids. If the lon/lat are already on a 2D grid, a warning message is displayed and the lon/lat grids are returned without further processing. 
}
\value{
A list containing lon and lat \code{M x N} arrays.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Lorenzo Zampieri
}
\note{
%%
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
new_coordinates <- sl.ll.rec(lon,lat)
## sl.ll.rec(lon,lat), with lon vector of length \code{M}, and lat vector of length \code{N}
## Should result in two corresponding \code{M x N} arrays, containing the lon/lat grids.
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

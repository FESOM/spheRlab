\name{sl.plot.landsea}
\alias{sl.plot.landsea}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot Land-Sea Mask of a Grid
}
\description{
Plot a land-sea mask based on an unstructured ocean grid.
}
\usage{
sl.plot.landsea(plot.init.res, lon, lat, elem, col.land = "grey", col.sea = "white", lwd = 1, lty = 1, polar.bg.res = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{plot.init.res}{
a spheRlab plot specifics list as returned by \code{\link{sl.plot.init}} (or a variant thereof).
}
  \item{lon}{
a vector of length \code{N} specifying the longitudes of the grid nodes.
}
  \item{lat}{
a vector of length \code{N} specifying the latitudes of the grid nodes.
}
  \item{elem}{
an \code{Ne}x\code{3} matrix providing the vertex indices of each triangular element spanned by the grid nodes. Note that \code{Ne} ~= 2x\code{N}.
}
  \item{col.land}{
the land colour. (Will be drawn at the beginning into the whole domain.)
}
  \item{col.sea}{
the ocean colour. (Each ocean element will be filled with this colour).
}
  \item{lwd}{
CURRENTLY NOT USED. Will be needed once coastlines can be drawn with this function.
}
  \item{lty}{
CURRENTLY NOT USED. Will be needed once coastlines can be drawn with this function.
}
  \item{polar.bg.res}{
a scalar specifying the resolution (in degrees) at which the background triangles are drawn.
}
}
\details{
First the whole plot domain is filled with \code{col.land}. For a polar projection, this is achieved with triangles that have one vertex at the projection North Pole and two points on the plot boundary, separated by \code{polar.bg.res} degrees. Second all visible ocean elements (triangles) are added.
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
Helge Goessling
}
\note{
1) This function could be amended slightly to enable drawing just the coastline of an ocean grid.

2) For more sophisticated plotting of more realistic coastlines and other geographical standard datasets, see \code{\link{sl.plot.naturalearth}}.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{sl.plot.elem}}, \code{\link{sl.plot.naturalearth}}
}
\examples{
## To be provided ...
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

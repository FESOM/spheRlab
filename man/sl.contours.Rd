\name{sl.contours}
\alias{sl.contours}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Compute Contours
}
\description{
Compute contour lines of a 2-dimensional field on an unstructured triangular mesh on a sphere.
}
\usage{
sl.contours(var = NULL, var.nc = NULL, varid = NULL, levels = 0, neighmat = NULL, lat, lon, elem, return.edge.info = FALSE, verbose = FALSE)
            
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{var}{
a numeric vector of length \code{N} resembling a 2-dimensional field on an unstructured mesh on a sphere.
}
  \item{var.nc}{
a character providing the system location of a NetCDF file containing the field to be analysed. Used only if \code{var} is not specified.
}
  \item{varid}{
a character providing the name of the variable in \code{var.nc} to be used. Used only if \code{var} is not specified.
}
  \item{levels}{
a vector providing the values for which contour lines shall be computed.
}
  \item{neighmat}{
an \code{N}x\code{M} matrix providing the neighbour nodes of each node (point) in \code{var} (or \code{var.nc}). If not provided, this matrix is computed using \code{\link{sl.findneighbours}} based on \code{elem}.
}
  \item{lat}{
a numeric vector of length \code{N} providing the latitudes of each node (point) in \code{var} (or \code{var.nc}).
}
  \item{lon}{
a numeric vector of length \code{N} providing the latitudes of each node (point) in \code{var} (or \code{var.nc}).
}
  \item{elem}{
an \code{Ne}x\code{3} matrix providing the vertex indices of each triangular element spanned by the nodes (points) in \code{var} (or \code{var.nc}). Note that \code{Ne} ~= 2x\code{N}.
}
  \item{return.edge.info}{
a logical value specifiying whether information on end points of, and relative positions on, the edges corresponding to each point of the contour(s) shall be returned. Default is \code{FALSE}.
}
  \item{verbose}{
a logical value specifiying whether additional statements shall be printed to document progress. Default is \code{FALSE}.
}
}
\details{
The algorithm implemented here does not work element-wise, but builds up contiguous contour segments step by step. Note that a linear evolution of the field on the elements (triangles) between the nodes is assumed.
}
\value{
  A list with one element for each level. Each of these is another list with the following elements:
  \item{level}{
  	a scalar giving the level of the contour.
  }
  \item{segments}{
  	a list of lists for each segment with elements \code{lat} (vector with latitudes of the \code{Ns} points forming the segment), \code{lon} (vector with longitudes of the \code{Ns} points forming the segment), and \code{length} (scalar giving the length of the segment). If \code{return.edge.info=TRUE}, the additional elements \code{edge.endpoints} (\code{Ns}x\code{2} integer matrix providing the end points of the edges corresponding to each point on the contour) and \code{edge.relpos} (vector giving the relative positions of the \code{Ns} contour points on the respective edge) are included.
  }
  \item{length}{
  	a scalar giving the length of the contour, summing all segments.
  }
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Helge Goessling
}
\note{
It is planned to extend this algorithm such that it works with any unstructured mesh based on polygons with any number of vertices. One simple way to achieve this would be to add a step at the beginning where any polygon with more than 3 vertices is decomposed into triangles, e.g. with a central reference point based on the polygon centroid.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
## Example to be provided ...
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

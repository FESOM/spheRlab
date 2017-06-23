\name{sl.plot.polygon}
\alias{sl.plot.polygon}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot Polygon
}
\description{
Plot a polygon into a spheRlab plot.
}
\usage{
sl.plot.polygon(plot.init.res, lon, lat, fill = TRUE, col.fill = "black", border = FALSE, col.border = col.fill, border.lwd = 0.01, border.lty = 1, ignore.visibility = FALSE, remove.identical.neighbours = TRUE, refine.boundary = TRUE, refine.boundary.precision = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{plot.init.res}{
a spheRlab plot specifics list as returned by \code{sl.plot.init} (or a variant thereof).
}
  \item{lon}{
a vector of length \code{N} specifying the longitudes of the polygon vertices.
}
  \item{lat}{
a vector of length \code{N} specifying the latitudes of the polygon vertices.
}
  \item{fill}{
a logical value indicating whether or not to fill the polygon.
}
  \item{col.fill}{
the fill colour. Used only if \code{fill=TRUE}.
}
  \item{border}{
a logical value indicating whether or not to draw the polygon edges.
}
  \item{col.border}{
the border (edge) line colour. Used only if \code{border=TRUE}.
}
  \item{border.lwd}{
a scalar specifying the border (edge) line width. Used only if \code{border=TRUE}.
}
  \item{border.lty}{
the border (edge) line type. Used only if \code{border=TRUE}.
}
  \item{ignore.visibility}{
a logical value indicating whether or not the polygon shall be attempted to be drawn completely even if it's classified partly or fully invisible, that is, outside the plot domain.
}
  \item{remove.identical.neighbours}{
a logical value indicating whether or not identical neighbours shall be removed from the polygon vertices before plotting.
}
  \item{refine.boundary}{
a logical value indicating whether or not to refine truncated polygons along curved plot boundaries (in polar projection).
}
  \item{refine.boundary.precision}{
a scalar specifying the precision (in degrees) for the refinement of truncated polygons along curved plot boundaries (in polar projection).
}
}
\details{
This is one of the lowest-level plotting functions of spheRlab. First the visibility of the polygon vertices is checked, which depends on the projection and plot domain boundaries. A polygon for which all vertices are invisible is not drawn. (This implies that a polygon is not drawn if all vertices are invisible, even if some other part of the polygon is visible.) Then for two partly visible edges an intersection point with the boundary is computed and the two intersection points are added to the polygon, replacing the (one or more) invisible vertices. The resulting polygon is then drawn, if applicable optionally after refinement of the truncation edge (see argument 'refine.boundary').
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
%%
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{sl.plot.lines}
}
\examples{
## To be provided ...
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

\name{sl.plot.polygon.qad}
\alias{sl.plot.polygon.qad}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot Polygon
}
\description{
Plot a polygon into a spheRlab plot.
}
\usage{
sl.plot.polygon.qad(plot.init.res, lon, lat, fill = TRUE, col.fill = "black", border = FALSE, col.border = col.fill, border.lwd = 0.01, border.lty = 1, ignore.visibility = FALSE, remove.identical.neighbours = TRUE, split.side = "none")
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
  \item{splitside}{
a character. If not \code{"none"}, it indicates a nested call with a polygon splitted by a circular boundary.
}
}
\details{
This is one of the lowest-level plotting functions of spheRlab. First the visibility of the polygon vertices is checked, which depends on the projection and plot domain boundaries. A polygon for which all vertices are invisible is not drawn. (This implies that a polygon is not drawn if all vertices are invisible, even if some other part of the polygon is visible.) Then for two partly visible edges an intersection point with the boundary is computed and the two intersection points are added to the polygon, replacing the (one or more) invisible vertices. The resulting polygon is then drawn.
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
The 'qad' in the function name stands for 'quick and dirty', referring to the fact that two more or less special cases are handled incorrectly: (i) If the polygon has more than one sequence of visible/invisible vertices, only one visible sequence is retained. (ii) If the two added points are located on distinct legs of the boundary, the resulting edge between them will cut through the plot domain instead of encompassing the whole relevant part of the boundary. In particular for correct plotting of rather complex polygons (e.g., with continents represented by polygons), a more comprehensive function needs to be implemented.
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

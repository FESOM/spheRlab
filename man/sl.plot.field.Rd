\name{sl.plot.field}
\alias{sl.plot.field}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot Unstructured Field on Polygons around Nodes
}
\description{
Plot a 2-dimensional numerical field on a triangular grid, where values are given at the nodes (vertices), by using the original node values and filling corresponding polygons defined by the surrounding 'stamp boundaries'.
}
\usage{
sl.plot.field(plot.init.res, num, lon.v, lat.v, fill = TRUE, col.fill = "colbar", border = FALSE, col.border = "colbar", colbar = sl.colbar.redgreyblue_256, colbar.breaks = NA, colbar.breaks.log = FALSE, border.lwd = 0.01, border.lty = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{plot.init.res}{
a spheRlab plot specifics list as returned by \code{\link{sl.plot.init}} (or a variant thereof).
}
  \item{num}{
a vector of length \code{N} specifying a 2-dimensional numerical (unstructured) field, with values valid at the nodes.
}
  \item{lon.v}{
an \code{N}x\code{M} matrix with the boundary polygon longitudes for each node in rows.
}
  \item{lat.v}{
an \code{N}x\code{M} matrix with the boundary polygon latitudes for each node in rows.
}
  \item{fill}{
a logical value indicating whether the polygons shall be filled.
}
  \item{col.fill}{
the fill colour. If \code{col.fill="colbar"} (default), the colourbar and breaks defined in \code{colbar} and \code{colbar.breaks} are used.
}
  \item{border}{
a logical value indicating whether or not to draw lines for the polygon borders.
}
  \item{col.border}{
the colour of the borders. If \code{col.border="colbar"} (default), then borders are coloured according to the field (node/polygon) values. This generates minute graphical overlap that prevents artefacts ("fractures") between polygons in the plot.
}
  \item{colbar}{
a spheRlab colourbar (as returned by \code{\link{sl.colbar}}) of length \code{Nc} to be used for the field.
}
  \item{colbar.breaks}{
a numeric vector of length \code{Nc-1} specifying the breaks between colours.
}
  \item{colbar.breaks.log}{
a logical value indicating whether colourbar breaks are to be placed logarithmically.
}
  \item{border.lwd}{
the line width of the borders.
}
  \item{border.lty}{
the line type of the borders.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
If a colourbar is used, it will be returned for further usage (e.g., for plotting of the colourbar).
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
\code{\link{sl.plot.field.elem}}, \code{\link{sl.plot.elem}}, \code{\link{sl.colbar}}, \code{\link{sl.num2colbar}}
}
\examples{
## To be provided ...
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

\name{sl.plot.elem}
\alias{sl.plot.elem}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot Grid Elements
}
\description{
Plot the elements of a triangular grid. The elements can be filled and/or their edges can be drawn. A special feature is colouring edges according to their length (resolution).
}
\usage{
sl.plot.elem(plot.init.res, lon, lat, elem, border = TRUE, col.border = "black", length.corr = "none", border.colbar = sl.colbar.redgreyblue_256, border.colbar.breaks = NA, border.colbar.breaks.log = FALSE, fill = TRUE, col.fill = "grey", cell_area = NA, lwd = 1, lty = 1)
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
an \code{Ne}x\code{3} matrix providing the vertex indices of each triangular element spanned by the nodes in \code{num}. Note that \code{Ne} ~= 2x\code{N}.
}
  \item{border}{
a logical value indicating whether or not to plot element edges.
}
  \item{col.border}{
the colour of the element edges.
}
  \item{length.corr}{
one of \code{"none"} and \code{"triag2quad"}, indicating whether or not to apply a correction to translate edge lengths to effective resolution of a regular grid (by multiplication with \code{sqrt(cos(pi/6))}; only used if \code{col.border="length"}). Default is \code{length.corr="none"}.
}
  \item{border.colbar}{
a spheRlab colourbar (as returned by \code{\link{sl.colbar}}) of length \code{Nc} to be used for element edges (only used if \code{col.border="length"}).
}
  \item{border.colbar.breaks}{
a numeric vector of length \code{Nc-1} specifying the breaks between colours (only used if \code{col.border="length"}).
}
  \item{border.colbar.breaks.log}{
a logical value indicating whether colourbar breaks are to be placed logarithmically (only used if \code{col.border="length"} and \code{col.border.breaks} is not specified).
}
  \item{fill}{
a logical value indicating whether or not to fill elements.
}
  \item{col.fill}{
the fill colour (only used if \code{fill=TRUE}). 
}
  \item{cell_area}{
NOT YET USED. Could be specified to fill with colours according to cell areas. However, this could also be done with \code{\link{sl.plot.field}}?
}
  \item{lwd}{
the line width of the borders (edges).
}
  \item{lty}{
the line type of the borders (edges).
}
}
\details{
For element filling (\code{fill=TRUE}), \code{\link{sl.plot.polygon}} is used. For edges (\code{border=TRUE}), \code{\link{sl.plot.lines}} is used; with \code{col.border="length"}, \code{\link{sl.gc.dist}} and \code{\link{sl.num2colbar}} are used.
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
Maybe functionality should be added to fill elements with colours according to resolution (not just edges as already implemented); however, this could also be done with \code{\link{sl.plot.field}}?
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{sl.plot.field}}, \code{\link{sl.plot.field.elem}}, \code{\link{sl.plot.landsea}}
}
\examples{
## To be provided ...
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

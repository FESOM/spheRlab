\name{sl.plot.field.elem}
\alias{sl.plot.field.elem}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot Field on Triangular Elements
}
\description{
Plot a 2-dimensional numerical field on a triangular grid, where values are given at the nodes (vertices), by averaging the vertex triplets and filling the triangular elements.
}
\usage{
sl.plot.field.elem(plot.init.res, num, lon, lat, elem, fill = TRUE, col.fill = "colbar", border = FALSE, col.border = "colbar", colbar = sl.colbar.redgreyblue_256, colbar.breaks = NA, colbar.breaks.log = FALSE, border.lwd = 0.01, border.lty = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{plot.init.res}{
a spheRlab plot specifics list as returned by \code{sl.plot.init} (or a variant thereof).
}
  \item{num}{
a vector of length \code{N} specifying a 2-dimensional numerical (unstructured) field, with values valid at the nodes (triangle vertices).
}
  \item{lon}{
a vector of length \code{N} specifying the node longitudes.
}
  \item{lat}{
a vector of length \code{N} specifying the node latitudes.
}
  \item{elem}{
an \code{Ne}x\code{3} matrix providing the vertex indices of each triangular element spanned by the nodes in \code{num}. Note that \code{Ne} ~= 2x\code{N}.
}
  \item{fill}{
a logical value indicating whether the elements (triangles) shall be filled.
}
  \item{col.fill}{
the fill colour. If \code{col.fill="colbar"} (default), the colourbar and breaks defined in \code{colbar} and \code{colbar.breaks} are used.
}
  \item{border}{
a logical value indicating whether or not to draw lines for the element borders (edges).
}
  \item{col.border}{
the colour of the borders. If \code{col.border="colbar"} (default), then borders are coloured according to the field (element) values. This generates minute graphical overlap that prevents artefacts ("fractures") between elements in the plot.
}
  \item{colbar}{
a spheRlab colourbar (as returned by \code{sl.colbar}) of length \code{Nc} to be used for the field.
}
  \item{colbar.breaks}{
a numeric vector of length \code{Nc-1} specifying the breaks between colours.
}
  \item{colbar.breaks.log}{
a logical value indicating whether colourbar breaks are to be placed logarithmically.
}
  \item{border.lwd}{
the line width of the borders (edges).
}
  \item{border.lty}{
the line type of the borders (edges).
}
}
\details{
The function uses \code{sl.plot.field} internally.
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
Unfortunately, \code{R} does not seem to have the capability to draw genuine colour gradients (as Matlab and Python do). If that capability is added at some point (or found to already exist), it should be added as an option to this function.

Note that, instead of using this function with \code{fill=FALSE}, one should use \code{sl.plot.elem} where edges are drawn only once.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{sl.plot.field}, \code{sl.plot.elem}
}
\examples{
## To be provided ...
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (plot.init.res, num, lon, lat, elem, fill = TRUE, col.fill = "colbar", 
    border = FALSE, col.border = "colbar", colbar = sl.colbar.redgreyblue_256, 
    colbar.breaks = NA, colbar.breaks.log = FALSE, border.lwd = 0.01, 
    border.lty = 1) 
{
    Ne = nrow(elem)
    num.elem = rep(NA, Ne)
    lon.elem = matrix(nrow = Ne, ncol = 3)
    lat.elem = matrix(nrow = Ne, ncol = 3)
    for (i in 1:Ne) {
        num.elem[i] = mean(num[elem[i, ]])
        lon.elem[i, ] = lon[elem[i, ]]
        lat.elem[i, ] = lat[elem[i, ]]
    }
    colbar.res = sl.plot.field(plot.init.res = plot.init.res, 
        num = num.elem, lon.v = lon.elem, lat.v = lat.elem, fill = fill, 
        col.fill = col.fill, border = border, col.border = col.border, 
        colbar = colbar, colbar.breaks = colbar.breaks, colbar.breaks.log = colbar.breaks.log, 
        border.lwd = border.lwd, border.lty = border.lty)
    return(get0("colbar.res"))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

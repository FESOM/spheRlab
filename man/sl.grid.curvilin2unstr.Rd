\name{sl.grid.curvilin2unstr}
\alias{sl.grid.curvilin2unstr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Convert Curvilinear to Unstructured Grid
}
\description{
Convert a curvilinear grid, where fields are provided in matrices and lat-lon coordinates are provided either in matrices (general) or vectors (only for lat-lon meshes), into an identical unstructured grid. The conversion allows to apply all the unstructured-grid algorithms also to curvilinear-grid data, without remapping.
}
\usage{
sl.grid.curvilin2unstr(lon = NULL, lat = NULL, Nx = NULL, Ny = NULL, vars = NULL, neighnodes = TRUE, neighelems = TRUE, quad2triag = TRUE, quad2triag.mode = "zigzag", close.top = FALSE, close.bottom = FALSE, close.sides = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lon}{
    an Nx X Ny matrix or an Nx vector providing the longitudes of the curvilinear grid, or \code{NULL} (default). In the latter case, \code{Nx} must be provided.
  }
  \item{lat}{
    an Nx X Ny matrix or an Ny vector providing the latitudes of the curvilinear grid, or \code{NULL} (default). In the latter case, \code{Ny} must be provided.
  }
  \item{Nx}{
    an integer specifying the number of longitudes. Default is \code{NULL}, in which case \code{lon} must be provided.
  }
  \item{Ny}{
    an integer specifying the number of latitudes. Default is \code{NULL}, in which case \code{lat} must be provided.
  }
  \item{vars}{
    a list of variables defined on the curvilinear grid. If the list elements are matrices, they are converted to vectors before they are returned.
  }
  \item{neighnodes}{
    a logical value specifying whether to return a matrix with the neighbour nodes for each node. Default is \code{TRUE}.
  }
  \item{neighelems}{
    a logical value specifying whether to return a matrix with the neighbour elements for each node. Default is \code{TRUE}.
  }
  \item{quad2triag}{
    a logical value specifying whether to cut rectangles into triangles. Default is \code{TRUE} (and currentls only that option is implemented).
}
  \item{quad2triag.mode}{
    a character specifying how to cut rectangles into triangles (if \code{quad2triag=TRUE}). Default is \code{'zigzag'} (and currently only that option is implemented).
}
  \item{close.top}{
    a logical value specifying whether to close the 'pole hole' at the top by connecting the first row of nodes across the pole. NOT YET IMPLEMENTED.
}
  \item{close.bottom}{
    a logical value specifying whether to close the 'pole hole' at the bottom by connecting the last row of nodes across the pole. NOT YET IMPLEMENTED.
}
  \item{close.sides}{
    a logical value specifying whether to close the gap at the sides by connecting the first and last columns of nodes across the 'zonal' gap. Default is \code{FALSE}.
}
}
\details{
This function is so far only partly implemented ,as revealed by the argument description. Functionality will be added when needed.

To remove land points from the grid, use \code{\link{sl.grid.reduce}}, see the example below.
}
\value{
a list with the elements \code{lon}, \code{lat}, \code{elem}, \code{coast}, \code{openbound}, \code{neighnodes}, \code{neighelems}, and \code{vars}. Some of these may be \code{NULL}, depending on the input arguments and on which options have already been implemented.
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
\code{\link{sl.grid.reduce}}
}
\examples{
# generate an example curvilinear grid
lon.0 = seq(-60,60,5)
lat.0 = seq(-30,30,5)
grd.0 = sl.grid.curvilin2unstr(lon = lon.0, lat = lat.0)
abg = sl.lonlatrot2abg(c(5,-20,0))
rot.lonlat = sl.rot(lon = grd.0$lon, lat = grd.0$lat, alpha = abg[1], beta = abg[2], gamma = abg[3])
grd = grd.0
grd$lon = rot.lonlat$lon
grd$lat = rot.lonlat$lat

# generate corresponding example data and insert NAs that define a land mask (arbitrary,
# not matching true land coordinates)
dat = exp(-((sl.gc.dist(lon = c(180,grd$lon), lat = c(85,grd$lat), sequential = FALSE)/0.4)^2))
dat.orig = dat
dat[c(7,140,250)] = NA
for (i in 1:2) {
  for (j in which(is.na(dat))) {
    dat[grd$neighnodes[j,]] = NA
  }
}

# remove land nodes from the grid
grd.reduced = sl.grid.reduce(grd = grd, remove.points = is.na(dat), return.info = TRUE)

# compute the 0.15-contour on the reduced grid
cont = sl.contours(var = dat[grd.reduced$reduce.kept$nodes], elem = grd.reduced$elem, levels = 0.15, neighmat = grd.reduced$neighnodes, lon = grd.reduced$lon, lat = grd.reduced$lat)

# plot
pir = sl.plot.init(projection = "polar", do.init.device = FALSE, col.background = "lightgrey")
cb1 = sl.plot.field.elem(pir, dat.orig, lon = grd$lon, lat=grd$lat, elem=grd$elem,
colbar = sl.colbar(cols=c("red","yellow")))
cb2 = sl.plot.field.elem(pir, dat[grd.reduced$reduce.kept$nodes], lon = grd.reduced$lon,
lat = grd.reduced$lat, elem = grd.reduced$elem, colbar = sl.colbar(cols=c("darkblue","white")))
cont = sl.contours(var = dat[grd.reduced$reduce.kept$nodes], elem = grd.reduced$elem,
levels = 0.15, neighmat = grd.reduced$neighnodes, lon = grd.reduced$lon, lat = grd.reduced$lat)
sl.plot.contours(pir, cont, col = "white", lwd = 2)
sl.plot.end(pir, do.close.device = FALSE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

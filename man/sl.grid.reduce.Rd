\name{sl.grid.reduce}
\alias{sl.grid.reduce}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Remove Nodes (and Elements) From an Unstructured Grid
}
\description{
Remove nodes and corresponding elements from an unstructured grid. Nodes that are left without any adjacent element are removed as well. Neighbourhood information is updated accordingly.
}
\usage{
sl.grid.reduce(grd, remove.points, set.coast = TRUE, set.openbound = FALSE, return.info = TRUE, auto.reduce = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{grd}{
a spheRlab grid list as returned by \code{\link{sl.grid.readFESOM}} or \code{\link{sl.grid.curvilin2unstr}}. Must contain at least the following list-elements: \code{elem} (Ne x Me matrix; Melem=3 in case of a triangular grid), \code{neighnodes} (Nnn x Mnn matrix), and \code{neighelems} (Nne x Mne matrix). These matrices can contain NAs in the respective right-most columns. If \code{auto.reduce=TRUE}, additional list-elements in \code{grd} that are vectors are reduced as well whereby their length is used to detect whether they hold information valid for nodes or for elements.
}
  \item{remove.points}{
  an integer or logical vector specifying the nodes to be removed. Note that additional nodes are removed if they end up without adjacent elements.
}
  \item{set.coast}{
  a logical value specifying whether to flag nodes adjacent to removed elements (due to the removal of nodes) as coastal in \code{grd$coast}.
}
  \item{set.openbound}{
  a logical value specifying whether to flag nodes adjacent to removed elements (due to the removal of nodes) as open boundary nodes in \code{grd$openbound}.
}
  \item{return.info}{
  a logical value specifying whether to return the indices of which nodes and elements of the original grid have been kept. Can be used e.g. to extract corresponding data values valid for the unreduced grid.
}
  \item{auto.reduce}{
  a logical value specifying whether to reduce additional list-elements in \code{grd} that are vectors. If so, the vector length is used to detect whether the list-elements hold information valid for nodes or for elements.
}
}
\details{
If \code{auto.reduce=TRUE}, additional list-elements in \code{grd} that are vectors are reduced as well whereby their length is used to detect whether they hold information valid for nodes or for elements.
}
\value{
  a grid list with the same (partly modified) elements as \code{grd}. If \code{return.info=TRUE}, an additional element \code{reduce.kept} with sub-elements \code{nodes} and \code{elems} providing the indices of nodes and elements of the original grid that have been kept.
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
\code{\link{sl.grid.curvilin2unstr}}, \code{\link{sl.grid.mergepoints}}
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
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
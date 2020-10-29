\name{sl.grid.mergepoints}
\alias{sl.grid.mergepoints}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Merge Nodes of an Unstructured Grid
}
\description{
Merge adjacent nodes of an unstructured grid. Neighbourhood information is updated accordingly and collapsing elements are removed.
}
\usage{
sl.grid.mergepoints(grd, remove.points, merge.points, return.info = TRUE, auto.reduce = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{grd}{
a spheRlab grid list as returned by \code{\link{sl.grid.readFESOM}} or \code{\link{sl.grid.curvilin2unstr}}. Must contain at least the following list-elements: \code{elem} (Ne x Me matrix; Melem=3 in case of a triangular grid), \code{neighnodes} (Nnn x Mnn matrix), and \code{neighelems} (Nne x Mne matrix). These matrices can contain NAs in the respective right-most columns. If \code{auto.reduce=TRUE}, additional list-elements in \code{grd} that are vectors are reduced as well whereby their length is used to detect whether they hold information valid for nodes or for elements.
}
  \item{remove.points}{
  an integer vector specifying the nodes to be removed (merged into an adjacent node). Must have the same length as \code{merge.points}.
}
  \item{merge.points}{
  an integer vector specifying the nodes into which the points in \code{remove.points} are to be merged. Must have the same length as \code{remove.points}.
}
  \item{return.info}{
  a logical value specifying whether to return the indices of which nodes and elements of the original grid have been kept. Can be used e.g. to extract corresponding data values valid for the unmerged grid.
}
  \item{auto.reduce}{
  a logical value specifying whether to reduce additional list-elements in \code{grd} that are vectors. If so, the vector length is used to detect whether the list-elements hold information valid for nodes or for elements.
}
}
\details{
If \code{auto.reduce=TRUE}, additional list-elements in \code{grd} that are vectors are reduced as well whereby their length is used to detect whether they hold information valid for nodes or for elements.
}
\value{
  a grid list with the same (partly modified) elements as \code{grd}. If \code{return.info=TRUE}, an additional element \code{merge.kept} with sub-elements \code{nodes} and \code{elems} providing the indices of nodes and elements of the original grid that have been kept.
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
\code{\link{sl.grid.curvilin2unstr}}, \code{\link{sl.grid.reduce}}
}
\examples{
# generate an example curvilinear grid
lon.0 = seq(-60,60,30)
lat.0 = seq(-30,30,20)
grd1 = sl.grid.curvilin2unstr(lon = lon.0, lat = lat.0)
grd1$coast = rep(FALSE,length(grd1$lon))
grd1$coast[c(1:5,10,15,20)] = TRUE

# merge four nodes into adjacent nodes
grd2 = sl.grid.mergepoints(grd1, remove.points = c(4,5,7,14), merge.points = c(9,10,6,15))

# initialize plot
pir = sl.plot.init(projection = "lonlat", lonlat.lonrange = c(-70,70), lonlat.latrange = c(-35,35), do.init.device = FALSE)

# plot original grid, including node and element numbers and coast symbols
sl.plot.elem(pir, lon = grd1$lon, lat = grd1$lat, elem = grd1$elem, col.border = "blue")
sl.plot.points(pir, pch = 1, lon = grd1$lon[grd1$coast], lat = grd1$lat[grd1$coast], col = "blue")
sl.plot.text(pir, labels = 1:length(grd1$lon), lon = grd1$lon-3, lat = grd1$lat, col = "blue")
for (i in 1:nrow(grd1$elem)) {
  sl.plot.text(pir, labels = i, lon = mean(grd1$lon[grd1$elem[i,]])-3, lat = mean(grd1$lat[grd1$elem[i,]]), col = "cyan")
}

# plot new grid, including node and element numbers and coast symbols
sl.plot.elem(pir, lon = grd2$lon, lat = grd2$lat, elem = grd2$elem, fill = FALSE, lty = 2, col.border = "red")
sl.plot.points(pir, pch = 4, lon = grd2$lon[grd2$coast], lat = grd2$lat[grd2$coast], col = "red")
sl.plot.text(pir, labels = 1:length(grd2$lon), lon = grd2$lon+3, lat = grd2$lat, col = "red")
for (i in 1:nrow(grd2$elem)) {
  sl.plot.text(pir, labels = i, lon = mean(grd2$lon[grd2$elem[i,]])+3, lat = mean(grd2$lat[grd2$elem[i,]]), col = "orange")
}

# finish plot
sl.plot.end(pir, do.close.device = FALSE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
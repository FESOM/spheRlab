\name{sl.grid.readFESOM}
\alias{sl.grid.readFESOM}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Read FESOM Grid
}
\description{
Read and analyse a FESOM (surface) grid based on the ascii files 'nod2d.out' and 'elem2d.out'.
}
\usage{
sl.grid.readFESOM(griddir, rot = FALSE, rot.invert = FALSE, rot.abg, reorder.ccw = TRUE, maxmaxneigh = 12, findneighbours.maxiter = 10, repeatlastpoint = TRUE, onlybaryc = FALSE, omitcoastnds = FALSE, calcpolyareas = TRUE, Rearth = 6371000, verbose = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{griddir}{
a character specifying the path to the FESOM grid directory.
}
  \item{rot}{
a logical value indicating whether or not to rotate the grid.
}
  \item{rot.invert}{
a logical value indicating whether or not to invert the rotation.
}
  \item{rot.abg}{
a vector of length 3 specifying the \code{alpha}, \code{beta}, and \code{gamma} parameters for the rotation. See \code{sl.lonlatrot2abg} for details.
}
  \item{reorder.ccw}{
a logical value indicating whether or not to reorder the triangular elements counterclockwise.
}
  \item{maxmaxneigh}{
an integer specifying an upper limit for the expected maximum number of neighbours for a node. For a typical (close to orthogonal) triangular mesh, there are 6 neighbours per node on average; the default value \code{maxmaxneigh=12} is relatively safe. A too small value results in an error. A larger value implies more memory usage.
}
  \item{findneighbours.maxiter}{
an integer specifying the maximum number of iterations used to order the neighbours of each node. The default \code{max.iter=10} should be on the safe side.
}
  \item{repeatlastpoint}{
a logical value indicating whether or not to pad the rows of the 'stamp polygon' matrices \code{stamppoly.lon} and \code{stamppoly.lat} (for nodes that have below-maximum polygon vertices) by repeating the last polygon vertex. If \code{repeatlastpoint=FALSE}, equidistant points along the line connecting the last and the first vertex will be inserted using \code{sl.fillequidist}.
}
  \item{onlybaryc}{
a logical value indicating whether or not to use only the barycenters (centroids) of the triangular elements for the stamp polygons, that is, to omit the edge medians inbetween. For a perfectly orthogonal grid the result will be identical as the lines connecting the centroids intersect the edge medians, but for non-orthogonal grids this introduces an inaccuracy.
}
  \item{omitcoastnds}{
a logical value indicating whether or not to omit coastal nodes from their corresponding stamp polygons. Usually the default \code{omitcoastnds=FALSE} is appropriate.
}
  \item{calcpolyareas}{
a logical value indicating whether or not to compute the areas of the triangular elements and of the stamp polygons.
}
  \item{Rearth}{
a scalar specifying the radius of the sphere - in case of FESOM the Earth. Used only if \code{calcpolyareas=TRUE}.
}
  \item{verbose}{
a logical value indicating whether or not print statements shall report on the function progess.
}
}
\details{
This is a special function which is typically the first step when converting FESOM grid information to a CDO-readable format, see the example below.
}
\value{
a spheRlab grid list with the following elements:
\item{lon}{
	a vector of length \code{N} with the node (grid point) longitudes.
}
\item{lat}{
	a vector of length \code{N} with the node (grid point) latitudes.
}
\item{elem}{
	an \code{Ne}x\code{3} matrix with the node indices of the triangular elements in rows.
}
\item{coast}{
	a vector of length \code{N} with logical values indicating whether or not nodes are coastal.
}
\item{neighnodes}{
	an \code{N}x\code{M} matrix with each row containing all neighbours of one node. The neighbours are arranged in clockwise or counterclockwise order, depending on the direction of rotation provided in \code{elem} (which is influenced by the argument \code{reorder.ccw}). \code{M} is the maximum number of neighbours actually found for a node (\code{M<=maxmaxneigh}).
}
\item{neighelems}{
	an \code{N}x\code{M} matrix with each row containing all triangular elements (identified by the row indices in \code{elem}) adjacent to one node. The elements are arranged in clockwise or counterclockwise order, depending on the direction of rotation provided in \code{elem} (which is influenced by the argument \code{reorder.ccw}). \code{M} is the maximum number of neighbours actually found for a node (\code{M<=maxmaxneigh}).
}
\item{stamppoly.lon}{
	an \code{N}x\code{P} matrix with each row containing the longitudes of all stamp polygon vertices of one node. If \code{onlybaryc=FALSE} (default) then the polygons (of internal nodes) consist of the edge medians (of which there are as many as neighbour nodes) and of the same number of triangle barycenters (centroids) inbetween, giving \code{P=2M}. Otherwise the edge medians are omitted and \code{P=M}. If \code{omitcoastnds=FALSE} (default) then coastal stamp polygons include the coastal node itself as vertex.
}
\item{stamppoly.lat}{
	as \code{stamppoly.lon}, but with latitudes.
}
\item{cellareas}{
	a vector of length \code{N} with the areas of the stamp polygons.
}
\item{elemareas}{
	a vector of length \code{Ne} with the areas of the triangular elements.
}
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Helge Goessling
}
\note{
It would probably make sense to split this function further as it consists of a number of well-defined blocks that might also be useful in other circumstances where the remainder of this function is not needed. The separate functions could be named consistenty \code{sl.grid.SOMETHING}.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{sl.grid.writeCDO}, \code{sl.grid.addinfo}
}
\examples{
\dontrun{
grid = sl.grid.readFESOM(griddir="PATH-TO-YOUR-FESOM-GRID",rot=TRUE,rot.invert=TRUE,rot.abg=c(50,15,-90))
ofile = "~/sl.grid.CDO"
sl.grid.writeCDO(grid,ofile=ofile)
system(paste0("cdo -f nc const,0,",ofile," ",ofile,".nc"))
sl.grid.addinfo(grid,ncdf.file.in=paste0(ofile,".nc"))
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
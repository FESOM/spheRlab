\name{sl.grid.readFESOM}
\alias{sl.grid.readFESOM}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Read FESOM Grid
}
\description{
Read and analyse a FESOM1 or FESOM2 grid from ascii '.out' files.
}
\usage{
sl.grid.readFESOM(griddir, rot = FALSE, rot.invert = FALSE, rot.abg, threeD = TRUE, remove.emptylev = TRUE, read.boundary = TRUE, reorder.ccw = TRUE, maxmaxneigh = 12, findneighbours.maxiter = 10, repeatlastpoint = TRUE, onlybaryc = FALSE, omitcoastnds = FALSE, calcpolyareas = TRUE, Rearth = 6371000, basicreadonly = FALSE, fesom2 = FALSE, verbose = TRUE)
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
a vector of length 3 specifying the \code{alpha}, \code{beta}, and \code{gamma} parameters for the rotation. See \code{\link{sl.lonlatrot2abg}} for details.
}
  \item{threeD}{
a logical value indicating whether or not to read 3D grid information. If \code{FALSE}, only surface information is read. Default is \code{TRUE}.
}
  \item{remove.emptylev}{
a logical value indicating whether or not to remove empty bottom levels from the 3D grid. Only used if \code{threeD=TRUE}.
}
  \item{read.boundary}{
a logical value indicating whether or not to read info on which 3D nodes are at horizontal or vertical boundaries. Only used if \code{threeD=TRUE}.
}
  \item{reorder.ccw}{
a logical value indicating whether or not to reorder the triangular surface elements counterclockwise.
}
  \item{maxmaxneigh}{
an integer specifying an upper limit for the expected maximum number of neighbours for a surface node. For a typical (close to orthogonal) triangular mesh, there are 6 neighbours per node on average; the default value \code{maxmaxneigh=12} is relatively safe. A too small value results in an error. A larger value implies more memory usage.
}
  \item{findneighbours.maxiter}{
an integer specifying the maximum number of iterations used to order the neighbours of each surface node. The default \code{max.iter=10} should be on the safe side.
}
  \item{repeatlastpoint}{
a logical value indicating whether or not to pad the rows of the 'stamp polygon' matrices \code{stamppoly.lon} and \code{stamppoly.lat} (for surface nodes that have below-maximum polygon vertices) by repeating the last polygon vertex. If \code{repeatlastpoint=FALSE}, equidistant points along the line connecting the last and the first vertex will be inserted using \code{\link{sl.fillequidist}}.
}
  \item{onlybaryc}{
a logical value indicating whether or not to use only the barycenters (centroids) of the triangular surface elements for the stamp polygons, that is, to omit the edge medians inbetween. For a perfectly orthogonal grid the result will be identical as the lines connecting the centroids intersect the edge medians, but for non-orthogonal grids this introduces an inaccuracy.
}
  \item{omitcoastnds}{
a logical value indicating whether or not to omit coastal surface nodes from their corresponding stamp polygons. Usually the default \code{omitcoastnds=FALSE} is appropriate.
}
  \item{calcpolyareas}{
a logical value indicating whether or not to compute the areas of the triangular surface elements and of the stamp polygons.
}
  \item{Rearth}{
a scalar specifying the radius of the sphere - in case of FESOM the Earth. Used only if \code{calcpolyareas=TRUE}.
}
  \item{basicreadonly}{
a logical value indicating whether or not only to read grid data without analysing neighborhood information etc. Reading is even faster with rot, reorder.ccw, and threeD all set to FALSE.
}
  \item{fesom2}{
a logical value indicating whether the grid to be read is a FESOM2 grid. If FALSE (default), the grid is assumed to be a FESOM1 grid.
}
  \item{verbose}{
a logical value indicating whether or not print statements shall report on the function progess.
}
}
\details{
This is a FESOM-specific function to read grid information from the standard FESOM1 and FESOM2 ascii '.out' files.
}
\value{
a spheRlab grid list with the following elements:
\item{N}{
	an integer specifying the number of surface nodes.
}
\item{Nlev}{
	an integer specifying the number of vertical levels.
}
\item{N3D}{
	an integer specifying the number of 3D nodes (only counting wet nodes).
}
\item{lon}{
	a vector of length \code{N} with the surface node (grid point) longitudes.
}
\item{lat}{
	a vector of length \code{N} with the surface node (grid point) latitudes.
}
\item{elem}{
	an \code{Ne}x\code{3} matrix with the surface node indices of the triangular surface elements in rows.
}
\item{coast}{
	a vector of length \code{N} with logical values indicating whether or not surface nodes are coastal.
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
\item{baryc.lon}{
	a vector of length \code{Ne} with the surface node (grid point) longitudes.
}
\item{baryc.lat}{
	a vector of length \code{Ne} with the surface node (grid point) latitudes.
}
\item{cellareas}{
	a vector of length \code{N} with the areas of the stamp polygons.
}
\item{elemareas}{
	a vector of length \code{Ne} with the areas of the triangular elements.
}
\item{depth}{
	a vector of length \code{Nlev} with the depth of the vertical levels (commonly in meters).
}
\item{depth.bounds}{
	a vector of length \code{Nlev+1} with the depth bounds of the vertical levels (commonly in meters).
}
\item{depth.lev}{
	an integer vector of length \code{N} with the depth underneath each surface node in terms of vertical levels.
}
\item{boundary}{
	an integer vector of length \code{N3D} specifying which 3D nodes are located at boundaries: the first digit encodes the vertical dimension (1=surface, 2=interior, 3=bottom), and the second digit encodes the horizontal dimension (0=interior, 1=coast). Note that this information is retrieved directly from 'nod3d.out' rather than derived from 3d element information. Not yet implemented for FESOM2; in this case \code{NULL} is returned.
}
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Helge Goessling
}
\note{
It would probably make sense to split this function further as it consists of a number of well-defined blocks that might also be useful in other circumstances where the remainder of this function is not needed. The separate functions could be named consistenty sl.grid.SOMETHING.

The 3D functionality is incomplete; e.g., no 3d-element information is considered. The functionality can be extended when needed.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{sl.grid.writeFESOM}}, \code{\link{sl.grid.writeCDO}}, \code{\link{sl.grid.addinfo}}
}
\examples{
\dontrun{
grid = sl.grid.readFESOM(griddir="PATH-TO-YOUR-FESOM-GRID",rot=TRUE,rot.invert=TRUE,rot.abg=c(50,15,-90))
sl.grid.writeCDO(grid,ofile="~/sl.grid.CDO.nc")
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
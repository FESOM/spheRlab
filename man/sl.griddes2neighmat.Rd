\name{sl.griddes2neighmat}
\alias{sl.griddes2neighmat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Read CDO Grid Description File and Derive Neighbourhood
}
\description{
Read longitude and latitude information (nodes and boundary vertices) from a CDO grid description file of an unstructured triangular mesh and derive neighbourhood and other information.
}
\usage{
sl.griddes2neighmat(CDOgriddes, critnum.neigh = 2, critnum.coast = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{CDOgriddes}{
a character providing the system location of the CDO grid description file (NetCDF format).
}
  \item{critnum.neigh}{
an integer needed to decide whether any two nodes are neighbours; nodes need to have at least \code{critnum.neigh} common boundary vertices to be classified as neighbours. The correct value may depend on the mesh structure, and/or the question at hand. The default value is \code{critnum.neigh=2}.
}
  \item{critnum.coast}{
an integer needed to decide whether a node is coastal: if any of the corresponding boundary vertices has less than \code{critnum.coast} matches with any boundary vertex of any other node, the node is assigned as coastal. The correct value depends on the mesh structure. The default is \code{critnum.coast=1}, which works for meshes where coastal nodes have at least one boundary vertex that is not shared with any neighbour node. Other meshes may need \code{critnum.coast=2}.
}
}
\details{
For each node given in the CDO mesh description the boundary vertices span a polygon that defines the 'stamp' of that node, that is, the patch over which the node value is assumed to be valid ('finite volume' interpretation). In this function the longitude and latitude information of the boundary vertices are used to derive the neighbourhood (and other) information. Note that the boundary polygon is not identical with what is usually referred to as an 'element': the latter is constructed by three nodes, whereas the boundary polygon consists of vertices surrounding each node, e.g., the edge medians and element centroids.
}
\value{
\item{lat}{
  a vector of length \code{N} giving the latitudes of all nodes.
}
\item{lon}{
  a vector of length \code{N} giving the longitudes of all nodes.
}
\item{neighmat}{
  a matrix of size \code{N}x\code{M} giving the (ordered) neighbours of all nodes, where \code{M} is the maximum number of neighbours for a node.
}
\item{Nneigh}{
  a vector of size \code{N}x\code{M} giving the (ordered) neighbours of all nodes.
}
\item{is.coast}{
  a vector of size \code{N} with logical values indicating which nodes are coastal.
}
\item{critnum.neigh}{
  an integer identical with the equally named function argument.
}
\item{critnum.coast}{
  an integer identical with the equally named function argument.
}
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
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
## To be provided ...
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

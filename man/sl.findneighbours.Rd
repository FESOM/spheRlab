\name{sl.findneighbours}
\alias{sl.findneighbours}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Find Node Neighbours Based on Triangular Elements
}
\description{
Given the triangular elements (node triplets) of a regular mesh, find all neighbour nodes for each node.
}
\usage{
sl.findneighbours(elem, maxmaxneigh = 12, reverse = TRUE, verbose = FALSE, max.iter = 10)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{elem}{
an \code{Ne}x\code{3} matrix with the node indices of the triangular elements in rows.
}
  \item{maxmaxneigh}{
an integer specifying an upper limit for the expected maximum number of neighbours for a node. For a typical (close to orthogonal) triangular mesh, there are 6 neighbours per node on average; the default value \code{maxmaxneigh=12} is relatively safe. A too small value results in an error. A larger value implies more memory usage.
}
  \item{reverse}{
a logical value indicating whether the order of the node neighbours shall be reversed (clockwise/counterclockwise).
}
  \item{verbose}{
a logical value indicating whether print statements shall report on the function progess.
}
  \item{max.iter}{
an integer specifying the maximum number of iterations used to order the neighbours of each node. The default \code{max.iter=10} should be on the safe side.
}
}
\details{
It may happen that, despite a sufficiently high value of \code{max.iter}, node neighbours can't be arranged in order and a corresponding warning is printed. In this case the affected nodes might be 'bad' nodes connecting two or more separate regions of the mesh domain, for example two ocean basins (as well as two pieces of land); such a connection should be represented either by multiple nodes, or closed completely. If 'bad' nodes exists, only one contiguous part of the mesh (wrt neighbour nodes and adjacent elements) will be returned for those nodes.
}
\value{
\item{neighbour.nodes}{
an \code{N}x\code{M} matrix with each row containing all neighbours of one node. The neighbours are arranged in clockwise or counterclockwise order, depending on the direction of rotation provided in \code{elem} (can be reversed by the argument \code{reverse}). \code{M} is the maximum number of neighbours actually found for a node (\code{M<=maxmaxneigh}).
}
\item{neighbour.elems}{
an \code{N}x\code{M} matrix with each row containing all triangular elements (identified by the row indices in \code{elem}) adjacent to one node. The elements are arranged in clockwise or counterclockwise order, depending on the direction of rotation provided in \code{elem} (can be reversed by the argument \code{reverse}). \code{M} is the maximum number of neighbours actually found for a node (\code{M<=maxmaxneigh}).
}
\item{internal.nodes}{
a vector of length \code{N} with logical values indicating which nodes are internal, that is, non-coastal.
}
\item{N.neighbour.nodes}{
a vector of length \code{N} with the number of neighbours for each node.
}
\item{all.elements.arranged}{
a logical value indicating whether all neighbours and elements have been ordered successfully.
}
\item{elems.completed}{
\code{NULL} if \code{all.elements.arranged=TRUE}, otherwise a vector of length \code{N} with logical values indicating which nodes were successfully arranged in order.
}
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Helge Goessling
}
\note{
This function is used by \code{\link{sl.grid.readFESOM}}.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{sl.grid.readFESOM}}
}
\examples{
## To be provided ...
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

\name{sl.elemfull2elemsub}
\alias{sl.elemfull2elemsub}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Generate Elements of an Unstructured-Mesh Node Subset
}
\description{
Generate the elements (triangles) of an unstructured-mesh node subset based on (i) the elements (triangles) of a corresponding larger (full) unstructured mesh and (ii) the latitude and longitude information for both the full mesh and the subset. Can be used to work with regionally truncated data (e.g. produced by '\code{cdo sellonlatbox ...}').
}
\usage{
sl.elemfull2elemsub(nodes.full.lon, nodes.full.lat, nodes.sub.lon, nodes.sub.lat, elem.full, tolerance = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{nodes.full.lon}{
a vector of size \code{N} with the longitudes of the nodes of the full mesh.
}
  \item{nodes.full.lat}{
a vector of size \code{N} with the latitudes of the nodes of the full mesh.
}
  \item{nodes.sub.lon}{
a vector of size \code{M<N} with the longitudes of the nodes of the reduced mesh; should be a subset of \code{nodes.full.lon}.
}
  \item{nodes.sub.lat}{
a vector of size \code{M<N} with the latitudes of the nodes of the reduced mesh; should be a subset of \code{nodes.full.lon}.
}
  \item{elem.full}{
an \code{Ne}x\code{3} matrix with the node indices of the triangular elements of the full mesh in rows.
}
  \item{tolerance}{
a scalar specifying the tolerance by which the matching longitudes and latitudes may be displaced. Default is \code{0}; if you allow a non-zero tolerance, check the results carefully!
}
}
\details{
By matching nodes based on their longitude and latitude information, the triangular elements of a full mesh that are contained in a subset of nodes are identified. This enables one to work with regionally truncated data (e.g. produced by '\code{cdo sellonlatbox ...}'). For a particular subset (truncation), this function needs to be run only once and the resulting element matrix can be used for any analyses of that subset.
}
\value{
\item{elem.sub}{
	an \code{Me}x\code{3} matrix with the node indices of the triangular elements of the node subset in rows.
}
\item{full2sub}{
	a vector of length \code{M} mapping each node of the subset to the nodes (indices) of the full mesh.
}
\item{sub2full}{
	a vector of length \code{N} mapping nodes of the full mesh to the nodes (indices) of the subset mesh. Contains \code{N-M} NAs.
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

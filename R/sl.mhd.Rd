\name{sl.mhd}
\alias{sl.mhd}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Modified Hausdorff Distance (MHD) Calculator
}
\description{
This function calculates the Modified Hausdorff Distance between two sea ice edges
}
\usage{
sl.mhd(Edge1,Edge2,LandEdge1,LandEdge2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Edge1}{
  A \code{M x 2} array, containing the (lon,lat) coordinates of the M points forming the (first) edge.
}
  \item{Edge1}{
  A \code{N x 2} array, containing the (lon,lat) coordinates of the N points forming the (second) edge.
}
  \item{LandEdge1}{
  A \code{P x 2} array, containing the (lon,lat) coordinates of the N points of the (first) land-ice interface between ice and land.
}
  \item{LandEdge2}{
  A \code{P x 2} array, containing the (lon,lat) coordinates of the N points of the (second) land-ice interface between ice and land.
}
}
\details{
To calculate the coordinates of the edge and of the land-ice interface please refer to the function \code{sl.mhd.edge}. \code{sl.mhd.edge} output is the natural imput for \code{sl.mhd}. The distance employed to compute the MHD is the "great circle distance", given by \code{sl.gc.dist} 
}
\value{
The MHD value in meters. 
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Lorenzo Zampieri
}
\note{
%%
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
MHD <- sl.mhd(Edge1,Edge2,LandEdge1,LandEdge2)
## sl.mhd(Edge1,Edge2,LandEdge1,LandEdge2) 
## Should result in double, which represent the MHD of the two edges in meters
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

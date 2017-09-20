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
sl.mhd(A,B,cA=NULL,cB=NULL,kA=NULL,kB=NULL,R=1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{A}{
  A \code{m x 2} array, containing the (lon,lat) coordinates of the \code{m} points forming the (first) edge.
}
  \item{B}{
  A \code{n x 2} array, containing the (lon,lat) coordinates of the \code{n} points forming the (second) edge.
}
  \item{cA}{
  A \code{p x 2} optional array, containing the (lon,lat) coordinates of the \code{p} points of the (first) land-ice interface between ice and land.
}
  \item{cB}{
  A \code{q x 2} optional array, containing the (lon,lat) coordinates of the \code{q} points of the (second) land-ice interface between ice and land.
}
  \item{kA}{
  An optional integer that can vary between 1 and the number of points of the segment A (\code{1<kA<m}). This represents the frequency with which the points in A are considered by the algorithm. As example, if \code{kA=2}, every second point is taken into account for the computation of the MHD, neglecting half of them. This parameter can be useful to speed up the computations, expecially when segment A has a shorter spatial step compared to B. However it can affect the accuracy of the calculated MHD, so be careful when using it. DEFAULT -> \code{kA = 1} 
}
  \item{kB}{
  An optional integer that can vary between 1 and the number of points of the segment B (\code{1<kA<n}). This represents the frequency with which the points in B are considered by the algorithm. As example, if \code{kB=2}, every second point is taken into account for the computation of the MHD, neglecting half of them. This parameter can be useful to speed up the computations, expecially when segment B has a shorter spatial step compared to A. However it can affect the accuracy of the calculated MHD, so be careful when using it. DEFAULT -> \code{kB = 1} 
}
  \item{R}{
 Radius of the sphere. DEFAULT -> \code{R = 1}. If \code{R = -1}, then \code{R = 6373044 [m]},  which is the mean quadratic radius.
}
}
\details{
To calculate the coordinates of the edge and of the land-ice interface please refer to the function \code{\link{sl.mhd.edge}}. \code{\link{sl.mhd.edge}} output is the natural imput for \code{\link{sl.mhd}}. The distance employed to compute the MHD is the "great circle distance", given by \code{\link{sl.gc.dist}}.
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
\code{\link{sl.mhd.edge}}
}
\examples{
MHD <- sl.mhd(Edge1,Edge2,LandEdge1,LandEdge2,R=-1)
## sl.mhd(Edge1,Edge2,LandEdge1,LandEdge2) 
## Should result in double, which represent the MHD of the two edges in meters
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
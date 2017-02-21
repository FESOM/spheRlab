\name{sl.mhd.edge}
\alias{sl.mhd.edge}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Edge Calculator for Modified Hausdorff Distance
}
\description{
This function provides the (lon,lat) coordinates of an edge (e.g. sea ice edge) and of its land-ice interface, given a threshold value.
}
\usage{
sl.mhd.edge(Var,LonGrid,LatGrid,Threshold)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Var}{
  The variable from which the edge has to be extracted. For sea ice this can be sea ice concentration or sea ice probability. Generally this is a 2D \code{M x N} array. In case of a model output with more than one ensemble member, this parameter can be passed as 3D \code{M x N x e} array, with \code{e} as the ensemble size. In this case the sea ice edge is calculated using the sea ice probability. IMPORTANT: Apply a data land mask as NaN directly to \code{Var} before passing the argument to the function. In this way the calculation of MHD will be meaningful.  
}
  \item{LonGrid}{
  The \code{M x N} longitude grid. See the function \code{sl.ll.rec} for more details.
}
  \item{LatGrid}{
  The \code{M x N} latitude grid. See the function \code{sl.ll.rec} for more details.
}
  \item{Threshold}{
  A threshold value (number). For sea ice concentration this is usually 15 \% 
}
}
\details{
Please refer to the function \code{sl.mhd.edge} to calculate the coordinates of the edge and of the land interface. \code{sl.mhd.edge} output is the natural imput for \code{sl.mhd}. The distance employed to compute the MHD is the "great circle distance", given by \code{sl.gc.dist} 
}
\value{
A list containing the (lon,lat) coordinates of the edge and of the land-ice interface.  
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
list <- sl.mhd.edge(SeaIceConcentration,LonGrid,LatGrid,Threshold)
## sl.mhd(Edge1,Edge2,LandEdge1,LandEdge2) 
## Should result in double, which represent the MHD of the two edges in meters
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

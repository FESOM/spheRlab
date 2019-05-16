\name{sl.trajectory.remaptime}
\alias{sl.trajectory.remaptime}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Remap Times of Trajectory on Sphere
}
\description{
Remap longitudes and latitudes of a trajectory on a sphere to a new time axis.
}
\usage{
sl.trajectory.remaptime(oldtime, oldlat, oldlon, newtime, method = "linear", extrapolate = FALSE, return.remapinfo = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{oldtime}{
a numeric vector of length \code{N} specifying the original time axis; must increase strict monotonously.
}
  \item{oldlat}{
a numeric vector of length \code{N} specifying the original latitudes of the trajectory.
}
  \item{oldlon}{
a numeric vector of length \code{N} specifying the original longitudes of the trajectory.
}
  \item{newtime}{
a numeric vector of length \code{M} specifying the new time axis; must increase strict monotonously.
}
  \item{method}{
a character specifying the interpolation method. Must be one of \code{"linear"} (default) and \code{"nearestneighbour"}.
}
  \item{extrapolate}{
a logical value specifying whether or not to extrapolate to times outside the original time range. Default is \code{FALSE} which implies that corresponding locations will be \code{NA}. If \code{TRUE}, the method specified in the argument \code{method} will be used also for the extrapolation.
}
  \item{return.remapinfo}{
a logical value specifying whether or not to return information on the remapping (left-neighbour indices and weights). Default is \code{FALSE}. 
}
}
%\details{
%The angle is computed at the second vertex.
%}
\value{
	\item{Lat }{a numeric vector of length \code{M} with the latitudes of the trajectory at the times specified in \code{newtime}.}
	\item{Lon }{a numeric vector of length \code{M} with the longitudes of the trajectory at the times specified in \code{newtime}.}
	
	If \code{return.remapinfo=TRUE}, an additional list element \code{remapinfo} is returned, with the following sub-elements:
	\item{weights.left.ind }{an integer vector of length \code{M} with the indices of the original points directly before the corresponding new point.}
	\item{weights.left }{an numeric vector of length \code{M} with the weights given to the points provided in \code{weights.left.ind}; the respective subsequent points were given the weights \code{(1-weights.left)}.}
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Helge Goessling
}
\note{
This function is used in the R-package \code{SIDFEx} to remap ice drift forecast trajectories.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%}

\examples{
sl.trajectory.remaptime(oldtime = 1:4, oldlat = 2:5, oldlon = seq(0,15,5), newtime = seq(2,5,pi/10), extrapolate = TRUE)
# Should return warnings related to the extrapolation and the following output:
# $Lat
#  [1] 3.000000 3.316975 3.631464 3.943230 4.259835 4.575002 4.886745 5.194824 5.499004 5.799052
#
# $Lon
#  [1]  5.000000  6.569664  8.140331  9.712088 11.281895 12.852290 14.424061 15.997287 17.572041 19.148394
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

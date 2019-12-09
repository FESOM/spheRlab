\name{sl.intersect}
\alias{sl.intersect}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Diagnose Intersections of Line Segments
}
\description{
Diagnose if and where line segments intersect, either within one set of line segments or across two sets, where the sets can be single or multiple concatenated line segments, including polygons.
}
\usage{
sl.intersect(lon1,lat1,lon2=NULL,lat2=NULL,close1=FALSE,close2=FALSE,use.dist=TRUE,dist1=NULL,dist2=NULL,distmat=NULL,return.on.firsthit=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lon1}{
  a numeric vector of length \code{N1 > 1} specifying the longitudes of the points constituting the first set of line segments.
}
  \item{lat1}{
  a numeric vector of length \code{N1 > 1} specifying the latitudes of the points constituting the first set of line segments.
}
  \item{lon2}{
  an optional numeric vector of length \code{N2 > 1} specifying the longitudes of the points constituting the second set of line segments. If \code{NULL} (default), line segment crossings within the first set are diagnosed.
}
  \item{lat2}{
  an optional numeric vector of length \code{N2 > 1} specifying the latitudes of the points constituting the second set of line segments. Only used if \code{lon2 != NULL}.
}
  \item{close1}{
  a logical value specifying whether or not to consider the first set as a polygon, that is, to connect the last and first points. Default is \code{FALSE}.
}
  \item{close2}{
  a logical value specifying whether or not to consider the second set as a polygon, that is, to connect the last and first points. Default is \code{FALSE}.
}
  \item{use.dist}{
  a logical value specifying whether or not to use segment lengths and point distances to exclude segments for which no intersection is possible a priori based on the criterion \code{(first segment length + second segment length) / 2 <= min(point distance))}. Default is \code{TRUE}.
}
  \item{dist1}{
  a numeric vector of length \code{N1-1} (or \code{N1} if \code{close1=TRUE}) specifying the sequential distances (in radians) between consecutive pairs of points in the first set. Used only if \code{use.dist=TRUE}. If \code{NULL}, the distances are computed using \code{\link{sl.gc.dist}}.
}
  \item{dist2}{
  a numeric vector of length \code{N2-1} (or \code{N2} if \code{close2=TRUE}) specifying the sequential distances (in radians) between consecutive pairs of points in the second set. Used only if \code{use.dist=TRUE}. If \code{NULL}, the distances are computed using \code{\link{sl.gc.dist}}.
}
  \item{distmat}{
  an optional numeric \code{N1xN2} (or \code{N1xN1} if \code{lon2=NULL}) matrix specifying the distances (in radians) between all pairs of points between the two sets (or within the single set). Used only if \code{use.dist=TRUE}. If \code{NULL}, the distances are computed using \code{\link{sl.finddist}}.
}
  \item{return.on.firsthit}{
  a logical value specifying whether or not to abort the function and return only the first intersection as soon as it occurs. Default is \code{FALSE}.
}
}
\details{
This function uses \code{\link{sl.line.line.intersect}} to diagnose intersection points between pairs of line segments. However, if \code{use.dist=TRUE} (default), before invoking \code{\link{sl.line.line.intersect}}, the segment lengths and point distances are used to exclude segments for which no intersection is possible a priori based on the criterion \code{(first segment length + second segment length) / 2 <= min(point distance))}. This may speed up the algorithm considerably, in particular when distances are already available and provided as input. In contrast, if \code{return.on.firsthit} and no distances are available beforehand, \code{use.dist=FALSE} can be faster.

This functions considers only those line segments that constitute the shortest great-circle connection between two consecutive points. To obtain great-circle intersection points located on the longer great-circle of either segment, and/or x-y-z coordinates of intersection points directly, consider using \code{\link{sl.line.line.intersect}} or \code{\link{sl.line.polygon.intersect}}.
}
\value{
A list with the following elements:
\item{anylines.intersect}{
  a logical value indicating whether any pair of line segments intersects.
}
\item{lines.intersect}{
  a numeric \code{Mx2} matrix providing for all \code{M} intersecting pairs of line segments the corresponding indices for the first and second (or first and first if \code{lon2=NULL}) set, where index \code{i} corresponds to the segment connecting points \code{i} and \code{i+1} (or \code{i} and \code{1} if \code{i=NX} and \code{closeX=TRUE}) within set \code{X}. \code{NULL} if \code{anylines.intersect=FALSE}.
}
\item{lon}{
  a numeric vector of length \code{M} providing the longitudes of the intersection points corresponding to each row of \code{lines.intersect}. \code{NULL} if \code{anylines.intersect=FALSE}.
}
\item{lat}{
  a numeric vector of length \code{M} providing the latitudes of the intersection points corresponding to each row of \code{lines.intersect}. \code{NULL} if \code{anylines.intersect=FALSE}.
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
\code{\link{sl.line.line.intersect}}, \code{\link{sl.line.polygon.intersect}}
}
\examples{
lon1 = c(0,0,10,10)
lat1 = c(0,10,0,10)
lon2 = seq(3,11,2)
lat2 = c(5,0,1,2,5)

plot(lon1,lat1,xlim=c(0,11),ylim=c(0,10),type="b",xlab="longitude",ylab="latitude")
lines(lon2,lat2,type="b",col="red")

res = sl.intersect(lon1,lat1,lon2,lat2)
str(res)
## Should return:
## List of 4
##  $ anylines.intersect: logi TRUE
##  $ lines.intersect   : int [1, 1:2] 2 3
##  $ lon               : num 8.35
##  $ lat               : num 1.68

points(res$lon,res$lat,col="blue")

res.close2 = sl.intersect(lon1,lat1,lon2,lat2,close2=TRUE)
str(res.close2)
## Should return:
## List of 4
##  $ anylines.intersect: logi TRUE
##  $ lines.intersect   : int [1:4, 1:2] 2 2 3 3 3 5 4 5
##  $ lon               : num [1:4] 8.35 5.05 10 10
##  $ lat               : num [1:4] 1.68 5.01 3.5 5.01

lines(lon2[c(1,length(lon2))],lat2[c(1,length(lon2))],col="red",lty=2)
points(res.close2$lon,res.close2$lat,col="blue",pch=4)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

\name{sl.finddist}
\alias{sl.finddist}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Find Point Pairs Based on Distance
}
\description{
Find pairs of points (and their distance) based on a distance function (e.g. nearest or furthest) from two sets of points on a sphere.
}
\usage{
sl.finddist(lon1, lat1, lon2, lat2, fun=min, return.vectors=TRUE, return.alldist=FALSE, exclude.zero=FALSE, return.degree=FALSE, Rsphere=1, reduce.memory=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lon1}{
a numeric vector of length \code{M} specifying the longitudes of the first set of points.
}
  \item{lat1}{
a numeric vector of length \code{M} specifying the latitudes of the first set of points.
}
  \item{lon2}{
a numeric vector of length \code{N} specifying the longitudes of the second set of points.
}
  \item{lat2}{
a numeric vector of length \code{N} specifying the latitudes of the second set of points.
}
  \item{fun}{
a function used to match points. Default is \code{min}, in which case the nearest neighbours are found. See details below.
}
  \item{return.vectors}{
a logical value specifying whether to return vectors providing the matching indices and distances for each of the sets of points. Default is \code{TRUE}.
}
  \item{return.alldist}{
a logical value specifying whether to return the matrix with all pairwise distances. Default is \code{FALSE}. Ignored if \code{reduce.memory=TRUE}, in which case no such matrix can be returned.
}
  \item{exclude.zero}{
a logical value specifying whether to exclude zero-distances from the matching. If \code{fun=min} (nearest-neighbour search) and the two sets of points are identical (\code{lon1=lon2} and \code{lat1=lat2}), \code{exclude.zero=TRUE} will prevent trivial identity-matching, allowing the search for nearest neighbours within one set of points. Default is \code{FALSE}.
}
  \item{return.degree}{
a logical value specifying whether to return distances in degrees. If \code{FALSE} (default), distances are in radians times \code{Rsphere}.
}
  \item{Rsphere}{
a scalar value giving the radius of the sphere. Default is a unit sphere, that is, \code{Rsphere=1}.
}
  \item{reduce.memory}{
a logical value specifying whether to compute distances redundantly for the two directions to reduce memory consumption, which can be required for very large sets of points. If \code{FALSE} (default), distances are stored in an \code{MxN} matrix and re-used, which is faster for not-too-large sets of points and allows to return the distance matrix.
}
}
\details{
With \code{fun=min} (default), this function performs a bi-directional nearest-neighbour search. With \code{fun=max}, furtherst points are searched. Whether or not it makes sense, other functions can be provided if they work on a numeric vector (of distances) and return a scalar that is contained in the input vector. For example, \code{fun=median} will usually NOT work if \code{M} or \code{N} are even because then the median is usually not contained in the set of numbers it relates to. Furthermore, the specified function must have an argument \code{na.rm}.

This function can also be used to search for nearest (or furtherst, or...) neighbours within a single set of points. To do so, just specify the same longitudes and latitudes for both sets of points (\code{lon1=lon2} and \code{lat1=lat2}). In this case, if \code{fun=min} (nearest-neighbour search), set \code{exclude.zero=TRUE} to prevent trivial identity-matching, but to find the respective nearest other point.

With \code{fun=min} (and \code{exclude.zero=FALSE}), distance metrics for two sets of points like the Hausdorff distance can easily be computed from the elements \code{dist.12} and \code{dist.21} (see examples).

Note that in case of multiple matches only the respective first match is provided.
}
\value{
A list with the following elements (all of which except the first two are optional, depending on corresponding arguments):
\item{ind}{
an integer vector of length \code{2} providing first the index of the point in set 1 that is closest (or furthest or ...) to any point in set 2, and second the corresponding point in set 2.
}
\item{dist}{
a numeric scalar providing the distance between the two points specified by \code{ind}. Units are according to the argument \code{return.degree}.
}
\item{ind.12}{
an integer vector of length \code{M} providing for each point in set 1 the respective index of the nearest (or furthest or ...) point in set 2. Units are according to the argument \code{return.degree}.
}
\item{dist.12}{
a numeric vector of length \code{M} providing the distances between the point pairs specified by \code{ind.12}.
}
\item{ind.21}{
an integer vector of length \code{N} providing for each point in set 2 the respective index of the nearest (or furthest or ...) point in set 1. Units are according to the argument \code{return.degree}.
}
\item{dist.21}{
a numeric vector of length \code{N} providing the distances between the point pairs specified by \code{ind.21}.
}
\item{dist.all}{
a numeric \code{MxN} matrix providing the distances between all point pairs.
}
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Helge Goessling
}
%\note{
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%}

\examples{
lon1 = c(0,10,10,0)
lat1 = c(0,0,10,10)
lon2 = seq(-2,14,2)
lat2 = seq(-4,0,0.5)

res.min = sl.finddist(lon1,lat1,lon2,lat2,return.degree=TRUE)
str(res.min)
## Should return:
## List of 7
##  $ ind     : num [1:2] 2 7
##  $ dist    : num 1
##  $ ind.12  : num [1:4] 2 7 8 4
##  $ dist.12 : num [1:4] 3.5 1 10.7 13.1
##  $ ind.21  : num [1:9] 1 1 1 1 2 2 2 2 2
##  $ dist.21 : num [1:9] 4.47 3.5 3.61 4.72 4.47 ...
##  $ dist.all: NULL

res.max = sl.finddist(lon1,lat1,lon2,lat2,return.degree=TRUE,fun=max)

# some plotting
plot(NA,xlim=c(-5,15),ylim=c(-5,15))
points(lon1,lat1,col="red")
points(lon2,lat2,col="blue")
for (i in 1:length(res.min$ind.12)) {
  lines(c(lon1[i],lon2[res.min$ind.12[i]]),c(lat1[i],lat2[res.min$ind.12[i]]),col=rgb(1,0,0,0.5))
}
for (i in 1:length(res.min$ind.21)) {
  lines(c(lon2[i],lon1[res.min$ind.21[i]]),c(lat2[i],lat1[res.min$ind.21[i]]),col=rgb(0,0,1,0.5))
}
lines(c(lon1[res.min$ind[1]],lon2[res.min$ind[2]]),c(lat1[res.min$ind[1]],lat2[res.min$ind[2]]))
lines(c(lon1[res.max$ind[1]],lon2[res.max$ind[2]]),c(lat1[res.max$ind[1]],lat2[res.max$ind[2]]),lty=2)

# Hausdorff distance:
max(max(res.min$dist.12), max(res.min$dist.21))
## Should return:
## [1] 13.11935

# Partial Hausdorff distance:
max(median(res.min$dist.12), median(res.min$dist.21))
## Should return:
## [1] 7.093482

# Modified Hausdorff distance:
max(mean(res.min$dist.12), mean(res.min$dist.21))
## Should return:
## [1] 7.07658

# Baddeley distance evaluated only over the union of the two sets:
p = 1
mean(c(res.min$dist.12,res.min$dist.21)^p)^(1/p)
## Should return:
## [1] 4.510111
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

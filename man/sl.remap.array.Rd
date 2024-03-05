\name{sl.remap.array}
\alias{sl.remap.array}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Remap Array
}
\description{
Remap a numeric array along one of its dimensions from an old to a new dimension variable vector.
}
\usage{
sl.remap(input.data, input.dimvals, new.dimvals, remap.dim = 1, method = "linear", extrapolate.method = "none", return.remapinfo = FALSE, verbose = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{input.data}{
a numeric array (can also be a vector or matrix) with \code{D <= 4} dimensions specifying the original data values. The dimension specified by \code{remap.dim} must have the same length as \code{input.dimvals}; the remapping is performed along this dimension while the other dimensions are kept.
}
  \item{input.dimvals}{
a numeric vector of length \code{N > 1} specifying the original dimension variable values; must increase strict monotonously.
}
  \item{new.dimvals}{
a numeric vector of length \code{M >= 1} specifying the new dimension variable values; must increase strict monotonously.
}
  \item{remap.dim}{
an integer for which \code{1 <= remap.dim <= D} specifying the dimension along which the data is remapped.
}
  \item{method}{
a character specifying the interpolation method. Must be one of \code{"linear"} (default) and \code{"nearestneighbour"}.
}
  \item{extrapolate.method}{
a character specifying the extrapolation method. Must be one of \code{"none"} (default), \code{"linear"}, and \code{"nearestneighbour"}. Default is \code{"none"} which implies that values outside the original dimension range will be \code{NA}.
}
  \item{return.remapinfo}{
a logical value specifying whether or not to return information on the remapping (nearest-neighbour-before indices and weights). Default is \code{FALSE}. 
}
  \item{verbose}{
a logical value specifying whether to produce warnings, in particular when the new dimension variable axis reaches outside the original dimension variable axis. Default is \code{TRUE}. 
}
}
%\details{
%}
\value{
	\item{data }{an array with \code{D} dimensions (like the input array \code{input.data}) where all expect the remapped dimension have the same length as in \code{input.data}, but the remapped dimension has the same length as \code{new.dimvals} instead of \code{input.dimvals}.}
	
	If \code{return.remapinfo=TRUE}, an additional list element \code{remapinfo} is returned, with the following sub-elements:
	\item{weights.left.ind }{an integer vector of length \code{M} with the indices of the original points directly before the corresponding new points.}
	\item{weights.left }{an numeric vector of length \code{M} with the weights given to the points provided in \code{weights.left.ind}; the respective subsequent points were given the weights \code{(1-weights.left)}.}
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Helge Goessling
}
\note{
This function is similar to and was inspired by \code{sl.trajectory.remaptime} but works on arrays (including vectors) instead of 1-dimensional spatial line objects on spheres (such as trajectories).
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{sl.trajectory.remaptime}}
}

\examples{
# Vector remapping
x = seq(-2, 3, by=0.5)
y = x^2
x.remap = seq(-1, 5, by=0.3)
y.remap.lin = as.vector(sl.remap.array(input.dat=y, input.dimvals=x, new.dimvals=x.remap,
                                       method="linear",extrapolate.method="linear")$data)
y.remap.nn = as.vector(sl.remap.array(input.dat=y, input.dimvals=x,new.dimvals=x.remap,
                                      method="nearestneighbour",extrapolate.method="nearestneighbour")$data)

plot(x,y,ylim=c(0,20),type="p",xlim=c(-2,5))
points(x.remap,y.remap.lin,col="red")
points(x.remap,y.remap.nn,col="blue")

# Matrix remapping
dat1 = matrix(seq(0.5,3,by=0.5),ncol=3)
dat2 = sl.remap.array(input.dat=dat1, input.dimvals=1:3, new.dimvals = seq(1.5,3.5,by=0.5),
                      remap.dim=2, method="linear",extrapolate.method="linear")

print(dat1)
print(dat2$data)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

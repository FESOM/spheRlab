\name{sl.rot}
\alias{sl.rot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Coordinate Rotation on Sphere
}
\description{
Rotate points on a sphere.
}
\usage{
sl.rot(lon, lat, alpha, beta, gamma, return.xyz = FALSE, invert = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lon}{
a scalar or vector specifying the longitude(s) of the point(s) to be rotated.
}
  \item{lat}{
a scalar or vector specifying the latitude(s) of the point(s) to be rotated.
}
  \item{alpha}{
a scalar specifying the \code{alpha} parameter for the rotation. See the code for details.
}
  \item{beta}{
a scalar specifying the \code{beta} parameter for the rotation. See the code for details.
}
  \item{gamma}{
a scalar specifying the \code{gamma} parameter for the rotation. See the code for details.
}
  \item{return.xyz}{
a logical value indicating whether or not the resulting points shall also be returned in x-y-z coordinates.
}
  \item{invert}{
a logical value indicating whether or not the rotation shall be inverted.
}
}
\details{
The parameters \code{alpha}, \code{beta}, and \code{gamma} can be derived from more intuitive parameters for the original coordinates of the new North Pole and the angle (in degrees) of rotation around the new North Pole using \code{sl.lonlatrot2abg}.
}

\value{
A list with the following elements:
  \item{lon}{a scalar or vector giving the longitude(s) or the rotated point(s)}
  \item{lat}{a scalar or vector giving the latitude(s) or the rotated point(s)}

If \code{return.xyz=TRUE}, also the following elements are added to the list:
  \item{x}{a scalar or vector giving the x-coordinate(s) or the rotated point(s)}
  \item{y}{a scalar or vector giving the y-coordinate(s) or the rotated point(s)}
  \item{z}{a scalar or vector giving the z-coordinate(s) or the rotated point(s)}
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
\code{sl.lonlatrot2abg}
}
\examples{
abg = sl.lonlatrot2abg(lonlatrot=c(20,30,-45))
sl.rot(lon=c(0,10,20),lat=c(0,0,0),alpha=abg[1],beta=abg[2],gamma=abg[3])
## Should return:
## $lon
## [1] -81.05239 -64.42540 -45.00000
## 
## $lat
## [1] 54.46865 58.52505 60.00000
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

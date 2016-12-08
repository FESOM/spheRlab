\name{sl.checkposition}
\alias{sl.checkposition}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Check Position of Point Relative to a Line on a Sphere
}
\description{
Check if a point is located to the left or to the right of a (directed) line, defined by two other points, on a sphere. This is equivalent to checking the direction of rotation of the resulting triangle.
}
\usage{
sl.checkposition(a, b, c, ccw.defined.as = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{a}{
Vector of length 2 with longitude and latitude of the start point of the line.
}
  \item{b}{
Vector of length 2 with longitude and latitude of the end point of the line.
}
  \item{c}{
Vector of length 2 with longitude and latitude of the point for which the position relative to the (great circle) line spanned from \code{a} to \code{b} is sought.
}
  \item{ccw.defined.as}{
By default \code{ccw.defined.as=1}, meaning that the return value is \code{1} if \code{c} is located to the left of the line (that is, if the resulting triangle rotates counterclockwise). If \code{ccw.defined.as=-1}, the return value is inverted.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
  Scalar return value: \code{1} if \code{c} is located to the left (if the triangle \code{a-b-c} rotates counterclockwise), \code{-1} if \code{c} is located to the right (if the triangle \code{a-b-c} rotates clockwise), or \code{0} if all points lie on one (great circle) line. (Values inverted if \code{ccw.defined.as=-1}.))
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
sl.checkposition(a=c(0,0),b=c(90,0),c=c(45,1))
## Should return:
## [1] 1
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

\name{sl.segment}
\alias{sl.segment}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Segment Logical Vector
}
\description{
Find contiguous segments of TRUE values in a logical vector. Returns either start and end indices of (one or more) segments, or one or more vectors of segment indices.
}
\usage{
sl.segment(logivec, extend = FALSE, first.only = FALSE, return.segments = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{logivec}{
a vector of logical values.
}
  \item{extend}{
a logical value indicating whether indices/segments shall be extended to contain the neighbouring \code{FALSE} values.
}
  \item{first.only}{
a logical value indicating whether only the first index pair or segment shall be returned.
}
  \item{return.segments}{
a logical value indicating whether, instead of start and end indices, complete segment index vectors shall be returned.
}
}
\details{
This function is used internally by \code{\link{sl.plot.polygon}} when polygons need to be truncated.
}
\value{
	If \code{return.segments=TRUE}: a vector of indices corresponding to a segment of contiguous \code{TRUE} values in \code{logivec} if only one segment is contained or if \code{first.only=TRUE}, or otherwise an (unnamed) list of such vectors.
	
	If \code{return.segments=FALSE}: a list with the elements \code{start} and \code{end}, each of which are scalars or vectors of the same length giving the start and end indices of segments of contiguous \code{TRUE} values in \code{logivec}.
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
%
}
\examples{
sl.segment(c(rep(TRUE,3),rep(FALSE,5),rep(TRUE,2),rep(FALSE,7),rep(TRUE,1)),extend=TRUE,first.only=FALSE,return.segments=TRUE)
## Should return:
## [[1]]
## [1]  8  9 10 11
## 
## [[2]]
## [1] 17 18  1  2  3  4

sl.segment(c(rep(TRUE,3),rep(FALSE,5),rep(TRUE,2),rep(FALSE,7),rep(TRUE,1)),extend=FALSE,first.only=FALSE,return.segments=FALSE)
## Should return:
## $start
## [1]  9 18
## 
## $end
## [1] 10  3
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

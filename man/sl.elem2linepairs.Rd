\name{sl.elem2linepairs}
\alias{sl.elem2linepairs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Extract Unique Edges from Triangular Mesh
}
\description{
Extract unique lines from elements (polygons) of an unstructured mesh. Useful for plotting to half the figure size  (vector format). By default, the edges are concatenated and returned as a vector composed of segments divided by NAs.
}
\usage{
sl.elem2linepairs(elem, concat = TRUE, verbose = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{elem}{
an \code{N}x\code{3} matrix with the node indices of the triangular elements of an unstructured mesh.
}
  \item{concat}{
a logical value indicating whether the resulting unique edges are concatenated and returned as a vector composed of segments divided by NAs. This can be computationally advantageous, for example for line plotting. Otherwise, a two-column matrix with each row giving one pair of nodes is returned.
}
  \item{verbose}{
a logical value indicating whether print statements shall report on the function progess.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
If \code{concat=TRUE} (default), the resulting unique edges are concatenated and returned as a vector composed of segments divided by NAs. Otherwise, a two-column matrix with each row giving one pair of nodes is returned.
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
elem = matrix(c(c(1,2,3),c(3,2,4),c(4,2,5),c(5,2,6)),ncol=3,byrow=TRUE)

sl.elem2linepairs(elem,concat=TRUE)
## Should return:
## [1]  1  2  3  4  5  6  2  4 NA  1  3 NA  2  5

sl.elem2linepairs(elem,concat=FALSE)
## Should return:
##       [,1] [,2]
##  [1,]    1    2
##  [2,]    1    3
##  [3,]    2    3
##  [4,]    2    4
##  [5,]    2    5
##  [6,]    2    6
##  [7,]    3    4
##  [8,]    4    5
##  [9,]    5    6
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

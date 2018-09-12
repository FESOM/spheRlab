\name{sl.plot.platon.end}
\alias{sl.plot.platon.end}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Finish Platon Plot
}
\description{
Finish a Platonian-type body plot.
}
\usage{
sl.plot.platon.end(plot.platon.init.res, corners = TRUE, corners.length = 0.05, adhes.surf = TRUE, outer.boundaries = FALSE, inner.boundaries = FALSE, col.adh = "grey", lty.adh = 1, lwd.adh = 0.5, col.bnd = "grey", lty.bnd = 1, lwd.bnd = 0.5, do.close.device = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{plot.platon.init.res}{
a spheRlab plot specifics list returned by \code{\link{sl.plot.platon.init}}.
}
  \item{corners}{
a logical value indicating whether or not to draw 'corners' for every face.
}
  \item{corners.length}{
a scalar specifying which fraction of the face edges shall be covered by the corners. Only used if \code{corners=TRUE}.
}
  \item{adhes.surf}{
a logical value indicating whether or not lines that delineate adhesive surfaces shall be drawn.
}
  \item{outer.boundaries}{
a logical value indicating whether or not the outer boundaries of the face net shall be drawn.
}
  \item{inner.boundaries}{
a logical value indicating whether or not the inner boundaries of the face net shall be drawn.
}
  \item{col.adh}{
the line colour of the adhesive surface boundaries. Only used if \code{adhes.surf=TRUE}.
}
  \item{lty.adh}{
the line type of the adhesive surface boundaries. Only used if \code{adhes.surf=TRUE}.
}
  \item{lwd.adh}{
the line width of the adhesive surface boundaries. Only used if \code{adhes.surf=TRUE}.
}
  \item{col.bnd}{
the line colour of the inner and/or outer face net boundaries. Only used if \code{outer.boundaries=TRUE} or \code{inner.boundaries=TRUE}.
}
  \item{lty.bnd}{
the line type of the inner and/or outer face net boundaries. Only used if \code{outer.boundaries=TRUE} or \code{inner.boundaries=TRUE}.
}
  \item{lwd.bnd}{
the line width of the inner and/or outer face net boundaries. Only used if \code{outer.boundaries=TRUE} or \code{inner.boundaries=TRUE}.
}
  \item{do.close.device}{
a logical value specifying whether to call \code{dev.off()} to close the corresponding device in the end. Default is \code{do.close.device=TRUE}.
}
}
\details{
Instead of using this function, a plot initialised by \code{\link{sl.plot.platon.init}} could also simply be closed using \code{dev.off}. However, \code{\link{sl.plot.platon.end}} allows one to draw inner and/or outer boundary lines of the face net, and/or lines delineating adhesive surfaces in an automated way before \code{dev.off} is called by \code{\link{sl.plot.platon.end}}.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
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
\code{\link{sl.plot.platon.init}}, \code{\link{sl.plot.end}}
}
\examples{
## To be provided ...
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

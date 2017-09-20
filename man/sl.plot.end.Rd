\name{sl.plot.end}
\alias{sl.plot.end}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Finish Plot
}
\description{
Finish a spheRlab plot.
}
\usage{
sl.plot.end(plot.init.res, line.border = TRUE, col.border = "black", precision = 1, lwd.border = 1, lty.border = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{plot.init.res}{
a spheRlab plot specifics list as returned by \code{\link{sl.plot.init}} (or a variant thereof).
}
  \item{line.border}{
a logical value indicating whether or not to draw lines along the plot boundaries.
}
  \item{col.border}{
the colour of the plot boundary lines.
}
  \item{precision}{
a scalar specifying for curved boundaries the precision (in degrees) at which these are broken into straight line segments.
}
  \item{lwd.border}{
a scalar specifying the line width of the boundaries.
}
  \item{lty.border}{
the line type of the boundaries.
}
}
\details{
Instead of using this function, a plot initialised by \code{\link{sl.plot.init}} or any variant thereof could also simply be closed using \code{\link{dev.off}}. However, \code{\link{sl.plot.end}} allows one to draw plot boundary lines in an automated way before \code{\link{dev.off}} is called by \code{\link{sl.plot.end}}.

For a plot initialised with \code{\link{sl.plot.platon.init}}, use \code{\link{sl.plot.platon.end}} instead of this function.
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
\code{\link{sl.plot.init}}, \code{\link{sl.plot.platon.end}}
}
\examples{
## To be provided ...
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

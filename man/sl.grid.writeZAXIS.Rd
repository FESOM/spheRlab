\name{sl.grid.writeZAXIS}
\alias{sl.grid.writeZAXIS}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Write z-axis for CDO
}
\description{
Write a z-axis description file that can be used to set the z-axis of a 3D data file with the Climate Data Operators (CDO).
}
\usage{
sl.grid.writeZAXIS(grid = NULL, levels = NULL, bounds = NULL, zaxistype = "depth_below_sea", ofile = "~/sl.grid.zaxis.txt", overwrite = FALSE, verbose = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{grid}{
a spheRlab grid list as returned by \code{\link{sl.grid.readFESOM}}; must contain an element 'depth', and optionally 'depth.bounds'. If \code{NULL} (default), \code{levels} must be specified explicitly.
}
  \item{levels}{
\code{NULL} (default) if levels are to be taken from \code{grid}, or a numeric vector speficying the depth levels explicitly.
}
  \item{bounds}{
\code{NULL} (default) if level bounds are to be taken from \code{grid}, or a numeric vector speficying the depth level bounds explicitly.
}
  \item{zaxistype}{
a character specifying the z-axis type. Default is \code{"depth_below_sea"}.
}
  \item{ofile}{
a character specifying the path and file name of the output file.
}
  \item{overwrite}{
a logical value indicating whether or not to overwrite an already existing file.
}
  \item{verbose}{
a logical value indicating whether or not print statements shall report on the function progess.
}
}
\details{
See the CDO operator 'setzaxis' to set the z-axis of a corresponding FESOM file.
}
\value{
%
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Helge Goessling
}
\note{
%%
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{sl.grid.readFESOM}}, \code{\link{sl.grid.writeCDO}}
}
\examples{
\dontrun{
# In this example a 'backward rotation' is performed, assuming that the original mesh coordinates are rotated;
# if that's not the case (which is now default), set 'rot=FALSE' or leave it unspecified (default is FALSE).
grid = sl.grid.readFESOM(griddir="PATH-TO-YOUR-FESOM-GRID",rot=TRUE,rot.invert=TRUE,rot.abg=c(50,15,-90))

# Write a corresponding z-axis description file that can be used with CDO to set the z-axis of a FESOM file ('cdo setzaxis,~/zaxis.txt ifile.nc ofile.nc')
res = sl.grid.writeZAXIS(grid,ofile="~/zaxis.txt")
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
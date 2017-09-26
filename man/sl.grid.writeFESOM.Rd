\name{sl.grid.writeFESOM}
\alias{sl.grid.writeFESOM}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Write FESOM Grid
}
\description{
Write a FESOM grid into FESOM-readable ascii files '.out'. SO FAR ONLY PARTIALLY IMPLEMENTED.
}
\usage{
sl.grid.writeFESOM(outpath, grd, griddir.name = "FESOM_grid", which.files = NULL, overwrite = FALSE, width = NULL, nsmall = NULL, verbose = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{outpath}{
a character specifying the root path where to write the FESOM grid.
  }
  \item{grd}{
a spheRlab grid as returned by \code{\link{sl.grid.readFESOM}}.
  }
  \item{griddir.name}{
a character specifying the name of the directory (within the root path specified by \code{outpath}) where to write the FESOM grid; typically, this corresponds to the grid name. If it does not yet exist, the directory is created.
  }
  \item{which.files}{
\code{NULL} or a character vector specifying which of the grid files are to be written; should be a subset of \code{c("nod2d","elem2d","nod3d","elem3d","aux3d")}. If \code{NULL}, all files are written. SO FAR ONLY PARTIALLY IMPLEMENTED, SEE NOTE BELOW.
  }
  \item{overwrite}{
a logical value indicating whether or not to overwrite already existing files. If \code{FALSE} (default), existing files are skipped.
  }
  \item{width}{
\code{NULL} or an integer specifying the minimum column width for the output text files. If \code{NULL}, various standard values are used for different files and columns.
  }
  \item{nsmall}{
\code{NULL} or an integer specifying the minimum number of digits to the right of the decimal point of real numbers for the output text files. If \code{NULL}, various standard values are used for different files and columns.
  }
  \item{verbose}{
a logical value indicating whether or not print statements shall report on the function progess.
  }
}
%\details{
%
%}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Helge Goessling
}
\note{
THIS FUNCTION IS ONLY PARTIALLY IMPLEMENTED: Currently only 'nod2d.out' and 'nod3d.out' can be written. The functionality will be extended when needed.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{sl.grid.readFESOM}}, \code{\link{sl.grid.writeCDO}}
}
\examples{
\dontrun{
grid = sl.grid.readFESOM(griddir="PATH-TO-YOUR-FESOM-GRID",rot=TRUE,rot.invert=TRUE,rot.abg=c(50,15,-90),threeD=TRUE)
sl.grid.writeFESOM(outpath="PATH-TO-NEW-ROTATED-GRID",grd=grid,griddir.name="newgrid",ofile=ofile)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
\name{sl.grid.FESOM3Ddata1Dto2D}
\alias{sl.grid.FESOM3Ddata1Dto2D}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Covert 3D FESOM data from 1D to 2D.
}
\description{
Covert 3D FESOM data from 1D to 2D where the second dimension is the depth, adding dummy nodes where necessary.
}
\usage{
sl.grid.FESOM3Ddata1Dto2D(ifile, ofile, meshdir, varnames=NULL, temp2sst=FALSE, salt2sss=FALSE, idepth=NULL, odepth=NULL, verbose=TRUE, return.env=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ifile}{
a character specifying the input file name.
}
  \item{ofile}{
a character specifying the output file name.
}
  \item{meshdir}{
a character specifying the path to the FESOM grid directory.
}
  \item{varnames}{
a character vector specifying the variables to be converted and/or kept. If NULL (default), all variables are converted and/or kept.
}
  \item{temp2sst}{
a logical value specifying whether an additional sea surface temperature field shall be generated from the 3D temperature field 'temp'. Default is FALSE.
}
  \item{salt2sss}{
a logical value specifying whether an additional sea surface salinity field shall be generated from the 3D salinity field 'salt'. Default is FALSE.
}
  \item{idepth}{
a character specifying the name of an ascii file with the ocean level depths, from surface to bottom with one row for each depth value. If NULL, this information is retrieved from the file 'nod3d.out' in 'meshdir'.
}
  \item{odepth}{
a character specifying the name of an output ascii file to store the ocean level depths retrieved from the file 'nod3d.out' in 'meshdir', from surface to bottom with one row for each depth value, for future use with the argument 'idepth'. If NULL, no file is written.
}
  \item{verbose}{
a logical value specifying whether or not print statements shall report on the function progess.
}
  \item{return.env}{
a logical value specifying whether all objects stored in the function environment before exit shall be returned as a list. If FALSE (default), NULL is returned.
}
}
\details{
This is a special function to covert FESOM1 output of 3D variables, which are by default stored as 1-dimensional vectors, to 2D variables where the second dimension is depth. This implies that dummy nodes with missing values are added in locations where the ocean is not as deep as the deepest level. The function reads from a NetCDF file and creates a new NetCDF file using the ncdf4 package. By default, all 3D variables contained in the input file are converted, and 2D variables are kept as they are. If variable names are specified, then only those variables are considered.

A global attribute 'history' is added to the output file to document the time and function arguments.
}
\value{
  If 'return.env=TRUE', all objects stored in the function environment before exit are returned. Otherwise, the return value is NULL.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Helge Goessling
}
\note{
Requires 'ncdf4'.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%%
}
\examples{
## sl.grid.FESOM3Ddata1Dto2D(ifile="~/fesom_file.nc",ofile="~/fesom_file_converted.nc",meshdir="~/fesom_meshdir")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

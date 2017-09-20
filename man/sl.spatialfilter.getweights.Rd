\name{sl.spatialfilter.getweights}
\alias{sl.spatialfilter.getweights}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Compute Spatial Filter Weights
}
\description{
Compute spatial filter weights for a field on an unstructured grid on a sphere.
}
\usage{
sl.spatialfilter.getweights(lon, lat, neighmat, areas, Rsphere = 1, method = "gauss", gauss.sigma = 2 * pi * Rsphere/360, cutoff = 3 * gauss.sigma)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lon}{
a vector of length \code{N} specifying the longitudes of the grid points.
}
  \item{lat}{
a vector of length \code{N} specifying the latitudes of the grid points.
}
  \item{neighmat}{
an \code{N}x\code{M} matrix with each row containing all neighbours of one node (grid point). See \code{\link{sl.findneighbours}} for details.
}
  \item{areas}{
a vector of length \code{N} specifying the areas which the grid point values represent.
}
  \item{Rsphere}{
the radius of the sphere.
}
  \item{method}{
a character specifying the filter type: one of \code{'gauss'} (Gaussian), \code{'lin'} (linear), and \code{'const'} (constant).
}
  \item{gauss.sigma}{
the \code{sigma} (width in terms of standard deviation in radians) of the Gaussian. Used only if \code{method='gauss'}.
}
  \item{cutoff}{
the distance at which the filter is truncated.
}
}
\details{
This function generates filter weights and the corresponding source and destination index information for an unstructured mesh on a sphere. Implemented filters are the Gaussian filter, a linear filter, and a constant (i.e., running mean) filter. In each case the filter weights take grid point areas (the areas for which the grid point values are assumed to hold) into account. This implies that, e.g., in case of the constant filter the weights are in fact not constant but proportional to the areas.
	
The argument \code{cutoff} is also used to define where the linear filter assumes zero if \code{method='lin'}, and determines the level of the constant (although area-weighted) weights if \code{method='const'}.
	
It is \code{M = n * N} where \code{n}, in the range [1,N], is the average number of source points for each destination point. For the identity filter, it is \code{n=1} and all weights equal \code{1}. For an untruncated filter it is \code{n=N}, but that's not recommended as the generation of the filter - and in particular its subsequent application - will be slowed down very much.

The generated filter information can be applied to a numerical field on the corresponding grid using \code{\link{sl.spatialfilter}}. However, the filter information has (not accidentally) the same format as used by the Climate Data Operators (\code{CDO}) and can be written to a NetCDF file that can be used with \code{CDO} to filter large amounts of data much faster than with \code{spheRlab}. A corresponding function to write the output into NetCDF still needs to be added.
}
\value{
A list with the following three elements:
  \item{src_address}{
  	an integer vector of length \code{M} giving the indices of the source points.
  }
  \item{dst_address}{
  	an integer vector of length \code{M} giving the indices of the destination points corresponding to the source points.
  }
  \item{filter_weights}{
  	an integer vector of length \code{M} giving the filter weights.
  }
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Helge Goessling
}
\note{
A function that facilitates writing the resulting filter information into a \code{CDO}-readable NetCDF file needs to be added to \code{spheRlab} asap.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{sl.spatialfilter}}
}
\examples{
## To be provided ...
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

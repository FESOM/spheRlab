\name{sl.load.naturalearth}
\alias{sl.load.naturalearth}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Load Natural Earth Data
}
\description{
Load (read) and/or download Natural Earth data.
}
\usage{
sl.load.naturalearth(what="all", resolution="medium", poly.split=TRUE, naturalearth.dir="~/naturalearthdata", force.download=FALSE, download.if.missing=TRUE, download.baseurl="https://www.naturalearthdata.com/http//www.naturalearthdata.com/download", read=TRUE, verbose=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{what}{
a character or character vector specifying which type of Natural Earth data
to load, e.g., 'coastline'. To list all possible values for the specified resolution, set what='list'. If what='all', all types available for the specified resolution are loaded.}
  \item{resolution}{
a character or character vector sepcifying which resolution(s) to use. Possible values are 'coarse' (1:110Mio), 'medium' (1:50Mio; default), and 'fine' (1:10Mio).
}
  \item{poly.split}{
a logical value indicating whether polygon objects (e.g. if \code{what='land'}) are to be split into small polygons before reading. Default is \code{TRUE}. If the splitting for a specific dataset is done for the first time, \code{sl.polygon.split()} is called (and optionally \code{sl.polygon.mergehole()} for polygons that include holes), and an R-object with the splitted polygons is saved in \code{naturalearth.dir}. This can take some time, highly depending on the polygon resolution. If such an object already exists (and \code{force.raw2split=FALSE}), the object is loaded instead of splitting the raw polygons again.
}
  \item{naturalearth.dir}{
a character specifying the local Natural Earth data directory. If data are downloaded from the internet, they will be save to this directory; if it does not yet exist, it will be created. Default is \code{NULL}, in which case such a value is sourced from \code{./spheRlab} if it exists, and if not it's assumed that \code{naturalearth.dir='~/naturalearthdata'}.
}
  \item{force.raw2split}{
a logical value specifying whether to redo the polygon splitting even if a corresponding splitted-polygons R-object already exists. Default is \code{FALSE}. Only relevant if a polygon object is specified (e.g. if \code{what='land'}) and \code{poly.split=TRUE}.
}
  \item{force.shape2raw}{
a logical value specifying whether to redo the conversion from the original Natural Earth shape file to an (raw, that is, not splitted) R-object if the latter already exists. Default is \code{FALSE}.
}
  \item{force.download}{
a logical value indicating whether or not to download the data from the internet, even if it already exists locally, to replace the local file. This can be useful when updates are available. By default, force.download=FALSE .
}
  \item{download.if.missing}{
a logical value indicating whether or not to download the data from the internet if it is missing locally. By default, download.if.missing=TRUE .
}
  \item{download.baseurl}{
a character specifiying the internet base URL from which to download the data.
}
  \item{read}{
a logical value indicating whether or not to load (read) tha data. If FALSE, data are only downloaded (if the corresponding arguments are set accordingly).
}
  \item{verbose}{
a logical value indicating whether or not to print more information.
}
}
\details{
This function, together with \code{\link{sl.plot.naturalearth}}, enables using the openly available and free-to-use Natural Earth data (https://www.naturalearthdata.com) - including coastlines, rivers, land and ocean polygons, and much more - in spheRlab. This function is also invoked from \code{\link{sl.plot.naturalearth}} if needed, so it does not need to be used explicitly to plot Natural Earth data.

If \code{read=TRUE}, the package rgdal is needed to read the shape file(s).
}
\value{
If \code{read=TRUE}, a named list with each element corresponding to one Natural Earth data set, with naming convention 'resolution_what'. List elements are return values of the rgdal function \code{readOGR()}.
}
\references{
https://www.naturalearthdata.com
}
\author{
Helge Goessling
}
\note{
%%
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{sl.plot.naturalearth}}
}
\examples{
# load coastlines at coarse resolution:
load.res = sl.load.naturalearth(what="coastline",resolution="coarse")

# list all possible 'what' values for fine resolution:
sl.load.naturalearth(what="list",resolution="fine")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

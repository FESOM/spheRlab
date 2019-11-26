\name{sl.plot.vectors}
\alias{sl.plot.vectors}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot Vectors
}
\description{
Plot vectors into a spheRlab plot.
}
\usage{
sl.plot.vectors(pir, lon, lat, u, v, ref=median(sqrt(u^2+v^2)), ref.length.degrees=1, length.by.length=TRUE, lwd.by.length=FALSE, col.by.length=FALSE, col="black", colbar=sl.colbar(cols=c("blue","red"),N=256), colbar.breaks=NA, colbar.breaks.log=FALSE, lwd.ref=1, lty=1, head.line=TRUE, head.fill=FALSE, head.fill.line=TRUE, head.size=1/3, head.width=1, ref.args=NULL, ignore.visibility=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{plot.init.res}{
a spheRlab plot specifics list as returned by \code{\link{sl.plot.init}} (or a variant thereof).
}
  \item{lon}{
a scalar, vector, matrix, or array specifying the longitudes of the points at which the vectors are centered.
}
  \item{lat}{
a scalar, vector, matrix, or array (same dimensions as \code{lon}) specifying the latitudes of the points at which the vectors are centered.
}
  \item{u}{
a scalar, vector, matrix, or array (same dimensions as \code{lon}) specifying the zonal (positive eastward) components of the vectors.
}
  \item{v}{
a scalar, vector, matrix, or array (same dimensions as \code{lon}) specifying the meridional (positive northward) components of the vectors.
}
  \item{ref}{
a scalar specifying the magnitude ('speed') corresponding to the reference vector; default is \code{median(sqrt(u^2+v^2))}.
}
  \item{ref.length.degrees}{
a scalar specifying the length of the reference vector in degrees.
}
  \item{length.by.length}{
a logical value specifying whether to scale vector length linearly with its magnitude; if \code{FALSE}, all vectors will be \code{ref.length.degrees} long. Default is \code{TRUE}.
}
  \item{lwd.by.length}{
  a logical value specifying whether to scale vector line width linearly with its magnitude; if \code{FALSE} (default), all vectors will be \code{lwd.ref} wide.
}
  \item{col.by.length}{
  a logical value specifying whether to vary vector colour with its magnitude; if \code{TRUE}, the arguments starting \code{colbar} are used to define the colourbar. If \code{FALSE} (default), all vectors will be coloured according to \code{col}.
}
  \item{col}{
  a colour for the vectors, used if \code{col.by.length=FALSE}. Default is \code{"black"}.
}
  \item{colbar}{
  a spheRlab colourbar (as returned by \code{\link{sl.colbar}}) with \code{Nc} colours for the vectors, used if \code{col.by.length=TRUE}. Default is \code{sl.colbar(cols=c("blue","red"),N=256)}.
}
  \item{colbar.breaks}{
a numeric vector of length \code{Nc-1} specifying the breaks between colours. Used only if \code{col.by.length=TRUE}. If \code{NA}, breaks are determined automatically.
}
  \item{colbar.breaks.log}{
a logical value indicating whether automatic colourbar breaks are to be placed logarithmically. Used only if \code{col.by.length=TRUE} and \code{colbar.breaks=NA}.
}
  \item{lwd.ref}{
  a scalar specifying the line width of the reference vector; default is \code{1}.
}
  \item{lty}{
  a scalar specifying the line type of the reference vector; default is \code{1} (solid).
}
  \item{head.line}{
  a logical value specifying whether or not to add the two typical lines bordering the arrow-point triangle; default is \code{TRUE}.
}
  \item{head.fill}{
  a logical value specifying whether or not to fill the arrow-point triangle; default is \code{FALSE}.
}
  \item{head.fill.line}{
  a logical value specifying whether or not to add all three lines bordering the arrow-point triangle; used only if \code{head.fill=TRUE}. Default is \code{TRUE}.
}
  \item{head.size}{
  a scalar specifying the size of the arrow-point triangle in terms of length ratio. Default is \code{1/3}.
}
  \item{head.width}{
  a scalar specifying the width of the arrow-point triangle relative to its length. Default is \code{1}.
}
  \item{ref.args}{
  a list to specify where and what type of a reference arrow is drawn. Possible list elements (and their defaults) are: \code{lon} (longitude; no default), \code{lat} (latitude; no default), \code{rot} (rotation in degrees counterclockwise, relative to eastward; default is \code{0}), \code{col} (arrow colour; default is \code{NULL}, in which case the colour is consistent with the remaining arrows), \code{circbox.bgcol} (background colour of the bounding box or circle; default is \code{"white"}; if \code{NULL}, no background is drawn), \code{circbox.col} (line colour of the bounding box or circle; default is \code{"black"}; if \code{NULL}, no box or circle line is drawn), \code{circbox.lwd} (line width of the bounding box or circle; default is \code{1}), \code{circ.ratio} (size of the circle relative to the arrow size; default is \code{1.5}), \code{box.ratio} (3-element vector specifying the along-direction and left-side and right-side size of the box relative to the arrow size; default is \code{c(1.5,0.5,0.5)}). If \code{ref.args=NULL}, no reference arrow is drawn.
}
  \item{ignore.visibility}{
  a logical value indicating whether objects shall be attempted to be drawn completely even if classified partly or fully invisible, that is, outside the plot domain.
}
}
\details{
The vectors are defined on the sphere, meaning that their length and orientation are distorted exactly in the way that any lines and polygons are distorted by the projection used. Accordingly, the vectors are drawn based on \code{sl.plot.lines()} and \code{sl.plot.polygon()}. The magnitude of the vectors (\code{sqrt(u^2+v^2)}) can be coded by any combination of arrow length (linear), line width (linear), and colour (arbitrary colours and breaks).

If \code{ref.args} is specified, a reference arrow with an optional circle around it is drawn, likewise on the sphere. This implies that the reference arrow is drawn in the map at a particular lon-lat location.
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
\code{\link{sl.plot.polygon}}, \code{\link{sl.plot.lines}}
}
\examples{
# define a cubed-sphere grid
N.xyz = 20
seq.xyz = seq(-1+1/N.xyz,1-1/N.xyz,2/N.xyz)
face = list(x=rep(-1,N.xyz^2),y=rep(seq.xyz,N.xyz),z=rep(seq.xyz,each=N.xyz))
x = c(face$x,-face$x,face$y,face$y,face$z,face$z)
y = c(face$y,face$y,face$z,face$z,face$x,-face$x)
z = c(face$z,face$z,face$x,-face$x,face$y,face$y)
lonlat = sl.xyz2lonlat(x=x, y=y, z=z)


# define a simple vector field with meridional component a function of longitude and zonal component constant
v = sin(3*lonlat$lon*2*pi/360) * 5
u = rep(2,length(v))

# define a colourbar
colbar = sl.colbar(c("blue","red"),N=8)

# plot vector field in lon-lat projection into the standard graphical device
pir = sl.plot.init(projection = "lonlat", do.init.device = FALSE)
sl.plot.naturalearth(pir, resolution = "coarse")
cb = sl.plot.vectors(pir = pir, lon = lonlat$lon, lat = lonlat$lat, u = u, v = v, ref = max(sqrt(u^2+v^2)), ref.length.degrees = 8, col.by.length = TRUE, colbar = colbar, ref.args = list(lon=-150,lat=0))
sl.plot.end(pir, do.close.device=FALSE)

# plot vector field in north polar projection into a pdf file
pir = sl.plot.init(projection = "polar", device = "pdf", file.name = "~/sl.plot.vectors.pdf")
sl.plot.naturalearth(pir, resolution = "coarse", what = "land", fill.col = "grey", polygon.borders=TRUE)
cb = sl.plot.vectors(pir = pir, lon = lonlat$lon, lat = lonlat$lat, u = u, v = v, ref.length.degrees = 5, col.by.length = TRUE, colbar = colbar, lwd.by.length=TRUE, head.fill = TRUE, ref.args = list(lon=-30,lat=50,circbox.type="circle",circbox.bgcol="grey"))
# if col.by.length=TRUE, you can also plot the colourbar to annotate the plot:
sl.plot.colbar(colbar, breaks = cb$breaks, labels.cex = 0.8, xshift = max(pir$xlim)-0.15, do.init=FALSE)
text("m/s", x = max(pir$xlim)-0.07, y = 0.3, cex = 0.8)
# (you can omit the reference vector by not specifying 'ref.args' in the call to sl.plot.vectors)
sl.plot.end(pir, do.close.device = TRUE)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

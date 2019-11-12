sl.plot.vectors <-
  function(pir,lon,lat,u,v,ref=median(sqrt(u^2+v^2)),ref.length.degrees=1,length.by.length=TRUE,lwd.by.length=FALSE,
           col.by.length=FALSE,col="black",colbar=sl.colbar(cols=c("blue","red"),N=256),colbar.breaks=NA,colbar.breaks.log=FALSE,
           lwd.ref=1,lty=1,head.line=TRUE,head.fill=FALSE,head.fill.line=TRUE,head.size=1/3,head.width=1,
           ref.args=NULL,ignore.visibility=FALSE) {
    
    not.na = !(is.na(lon) | is.na(lat) | is.na(u) | is.na(v))
    
    fac = 2*pi/360
    fac2 = ref.length.degrees*fac/ref
    lon = as.vector(lon[not.na])
    lon.rad = lon*fac
    lat = as.vector(lat[not.na])
    lat.rad = lat*fac
    u = as.vector(u[not.na])*fac2
    v = as.vector(v[not.na])*fac2
    
    len = sqrt(u^2+v^2)
    if (any(len >= pi/2)) {stop("sl.plot.vectors() works only for vectors shorter than 180 degree")}
    
    colbar.res = NULL
    if (col.by.length) {
      colbar.res = sl.num2colbar(len/fac2,colbar,colbar.breaks,colbar.breaks.log)
      col.ind = colbar.res$colour.index
    }
    
    for (i in 1:length(lon)) {
      
      if (col.by.length) {col = colbar[[col.ind[i]]]}
      
      dxyz.unitvec.u = c(-sin(lon.rad[i]), cos(lon.rad[i]), 0)
      dxyz.unitvec.v = c(-cos(lon.rad[i])*sin(lat.rad[i]), -sin(lon.rad[i])*sin(lat.rad[i]), cos(lat.rad[i]))
      
      dxyz = (u[i]*dxyz.unitvec.u + v[i]*dxyz.unitvec.v)/2 * tan(len[i]/2)/(len[i]/2)
      if (!length.by.length) {
        dxyz.length = sqrt(sum(dxyz^2))
        if (dxyz.length == 0) {next}
        dxyz = dxyz / sqrt(sum(dxyz^2)) * ref.length.degrees * fac / 2 * tan(ref.length.degrees*fac/2)/(ref.length.degrees*fac/2)
      }
      
      lonlat.start = sl.xyz2lonlat(sl.lonlat2xyz(c(lon[i],lat[i])) - dxyz)
      lonlat.end = sl.xyz2lonlat(sl.lonlat2xyz(c(lon[i],lat[i])) + dxyz)
      
      if (lwd.by.length) {
        lwd = len[i]/fac2/ref*lwd.ref
      } else {
        lwd = lwd.ref
      }
      
      sl.plot.lines(pir, c(lonlat.start[1],lonlat.end[1]), c(lonlat.start[2],lonlat.end[2]), col, lwd, lty, ignore.visibility)
      
      dxyz.head = (-v[i]*dxyz.unitvec.u + u[i]*dxyz.unitvec.v)/2 * tan(len[i]/2)/(len[i]/2)
      if (!length.by.length) {
        dxyz.head.length = sqrt(sum(dxyz.head^2))
        if (dxyz.head.length == 0) {next}
        dxyz.head = dxyz.head / sqrt(sum(dxyz.head^2)) * ref.length.degrees * fac * tan(ref.length.degrees*fac/2)/(ref.length.degrees*fac/2)
      }
      if (!is.null(ref.args) && !is.null(ref.args$only.get.dxyz) && ref.args$only.get.dxyz) {
        return(list(dxyz.front=dxyz,dxyz.left=dxyz.head))
      }
      dxyz.head = dxyz.head * head.size * head.width/2
      head.frac = 1 - 2 * head.size
      lonlat.head.left = sl.xyz2lonlat(sl.lonlat2xyz(c(lon[i],lat[i])) + head.frac*dxyz + dxyz.head)
      lonlat.head.point = sl.xyz2lonlat(sl.lonlat2xyz(c(lon[i],lat[i])) + dxyz)
      lonlat.head.right = sl.xyz2lonlat(sl.lonlat2xyz(c(lon[i],lat[i])) + head.frac*dxyz - dxyz.head)
      if (head.size > 0) {
        if (head.fill) {
          sl.plot.polygon(pir, c(lonlat.head.left[1],lonlat.head.point[1],lonlat.head.right[1]),
                          c(lonlat.head.left[2],lonlat.head.point[2],lonlat.head.right[2]),
                          fill=TRUE, col.fill=col, border=head.fill.line, border.lwd = lwd, ignore.visibility=ignore.visibility)
        }
        if (head.line) {
          sl.plot.lines(pir, c(lonlat.head.left[1],lonlat.head.point[1],lonlat.head.right[1]),
                          c(lonlat.head.left[2],lonlat.head.point[2],lonlat.head.right[2]),
                        col, lwd, lty, ignore.visibility)
        }
      }
      
    }
    
    if (is.list(ref.args) && "lon" %in% names(ref.args) && !is.null(ref.args$lon)) {
      
      if (is.null(ref.args$rot)) {ref.args$rot = 0}
      if (is.null(ref.args$circbox.type)) {ref.args$circbox.type = "box"}
      if (is.null(ref.args$circbox.col)) {ref.args$circbox.col = "black"}
      if (is.null(ref.args$bg.col)) {ref.args$circbox.bgcol = "white"}
      if (is.null(ref.args$circbox.lwd)) {ref.args$circbox.lwd = 1}
      if (is.null(ref.args$circ.ratio)) {ref.args$circ.ratio = 1.5}
      if (is.null(ref.args$box.ratio)) {ref.args$box.ratio = c(1.5,0.5,0.5)}
      
      ref.u = cos(ref.args$rot*fac) * ref
      ref.v = sin(ref.args$rot*fac) * ref
      if (col.by.length) {
        if (is.null(ref.args$col)) {
          ref.col = colbar[[sl.num2colbar(num=ref, colbar = colbar.res$colbar, breaks = colbar.res$breaks)$colour.index]]
        } else {
          ref.col = ref.args$col
        }
      } else {
        ref.col = col
      }
      
      if (!is.null(ref.args$bg.col) || !is.null(ref.args$circbox.col)) {
        abg = sl.lonlatrot2abg(lonlatrot = c(ref.args$lon,ref.args$lat,0))
        if (!(ref.args$circbox.type %in% c("circ","circle","box"))) {
          warning("'ref.args$circbox.type' must be 'box' or 'circle' (or 'circ'). Drawing none.")
        } else {
          if (ref.args$circbox.type == "box") {
            res = sl.plot.vectors(pir,lon=ref.args$lon,lat=ref.args$lat,u=ref.u,v=ref.v,ref=ref,ref.length.degrees=ref.length.degrees,
                                  length.by.length=FALSE,lwd.by.length=FALSE,
                                  col.by.length=FALSE,col=ref.col,lwd.ref=lwd.ref,lty=lty,head.line=head.line,head.fill=head.fill,
                                  head.fill.line=head.fill.line,head.size=head.size,head.width=head.width,
                                  ref.args=list(only.get.dxyz=TRUE),ignore.visibility=ignore.visibility)
            dxyz.front = res$dxyz.front * ref.args$box.ratio[1]
            dxyz.right = -res$dxyz.left * ref.args$box.ratio[2]
            dxyz.left = res$dxyz.left * ref.args$box.ratio[3]
            center.xyz = sl.lonlat2xyz(c(ref.args$lon,ref.args$lat))
            circbox.fl = sl.xyz2lonlat(center.xyz + dxyz.front + dxyz.left)
            circbox.fr = sl.xyz2lonlat(center.xyz + dxyz.front + dxyz.right)
            circbox.br = sl.xyz2lonlat(center.xyz - dxyz.front + dxyz.right)
            circbox.bl = sl.xyz2lonlat(center.xyz - dxyz.front + dxyz.left)
            circbox = list(lon = c(circbox.fl[1],circbox.fr[1],circbox.br[1],circbox.bl[1]),
                           lat = c(circbox.fl[2],circbox.fr[2],circbox.br[2],circbox.bl[2]))
          } else {
            circbox.lats.rot = rep(90-ref.length.degrees*ref.args$circ.ratio/2,36)
            circbox.lons.rot = seq(-175,175,10)
            circbox = sl.rot(circbox.lons.rot, circbox.lats.rot, alpha = abg[1], beta = abg[2], gamma = abg[3], invert=TRUE)
          }
          sl.plot.polygon(pir, circbox$lon, circbox$lat, fill = !is.null(ref.args$circbox.bgcol), col.fill = ref.args$circbox.bgcol,
                          border = !is.null(ref.args$circbox.col), col.border = ref.args$circbox.col, border.lwd = ref.args$circbox.lwd)
        }
      }
      
      sl.plot.vectors(pir,lon=ref.args$lon,lat=ref.args$lat,u=ref.u,v=ref.v,ref=ref,ref.length.degrees=ref.length.degrees,
                      length.by.length=FALSE,lwd.by.length=FALSE,
                      col.by.length=FALSE,col=ref.col,lwd.ref=lwd.ref,lty=lty,head.line=head.line,head.fill=head.fill,
                      head.fill.line=head.fill.line,head.size=head.size,head.width=head.width,
                      ref.args=NULL,ignore.visibility=ignore.visibility)
      
    }
    
    return(colbar.res)
    
  }

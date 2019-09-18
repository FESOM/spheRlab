sl.plot.field.contours <-
function (plot.init.res,num,lon,lat,elem,contours=TRUE,contours.levels=NA,contours.log=FALSE,col.contours="black",lwd.contours=1,lty.contours=1,contours.greater=TRUE,fill=TRUE,col.fill="colbar",colbar=sl.colbar(cols=c("blue","red"),N=10),border=FALSE,col.border="colbar",border.lwd=0.01,border.lty=1) {
	
  if (ncol(elem) != 3) {stop("sl.plot.field.contours not yet implemented for non-triangular elements")}
  if (!(contours || fill || border)) {
    warning("nothing selected for plotting; returning NULL"); return(NULL)
  }
  
  Ne = nrow(elem)
  
  if (anyNA(contours.levels)) {
    contours.levels = sl.num2colbarbreaks(num,colbar,breaks.log=contours.log)
  }
  contours.levels = unique(contours.levels)[order(unique(contours.levels))]
  Ncont = length(contours.levels)
  
  if (fill || border) {
    if ((Ncont+1) != length(colbar)) {
      #warning(paste("adjusting number of colours in 'colbar' to",Ncont+1))
      colbar = sl.colbar(colbar, Ncont+1)
    }
    cb.fill = col.fill
    cb.border = col.border
  }
  
  elem.min = apply(matrix(num[elem],ncol=3), 1, min)
  elem.max = apply(matrix(num[elem],ncol=3), 1, max)
  contours.in = matrix(rep(FALSE,Ne*Ncont),ncol=Ncont)
  for (i in 1:Ncont) {
    if (contours.greater) {
      contours.in[,i] = (contours.levels[i] >= elem.min & contours.levels[i] < elem.max)
    } else {
      contours.in[,i] = (contours.levels[i] > elem.min & contours.levels[i] <= elem.max)
    }
  }
  cont = (rowSums(contours.in) > 0)
  nocont = which(!cont)
  cont = which(cont)
  
  # plot elements that contain no contours
  if (length(nocont) > 0 && (fill || border)) {
    if (col.fill == "colbar" || col.border == "colbar") {
      colbar.res = sl.num2colbar(rowMeans(matrix(num[elem[nocont,]],ncol=3)),colbar,breaks=contours.levels)
      col.ind = colbar.res$colour.index
      colbar.res$colour.index = NULL
    }
    i = 0
    for (np in nocont) {
      i = i + 1
      if (col.fill == "colbar") {cb.fill = colbar[[col.ind[i]]]}
      if (col.border == "colbar") {cb.border = colbar[[col.ind[i]]]}
      sl.plot.polygon(plot.init.res,lon[elem[np,]],lat[elem[np,]],fill=fill,col.fill=cb.fill,
                      border=border,col.border=cb.border,border.lwd=border.lwd,border.lty=border.lty)
    }
  }
  
  # split and plot elements that contain one or more contours
  if (length(cont) > 0) {
    if (contours) {
      Nlns = sum(contours.in)
      lns.lon = rep(NA,Nlns*3)
      lns.lat = rep(NA,Nlns*3)
      ilns = 1
    }
    for (np in cont) {
      elem.ord = elem[np,]
      elem.ord = elem.ord[order(num[elem.ord])]
      num.ord = num[elem.ord]
      lon.ord = lon[elem.ord]
      lat.ord = lat[elem.ord]
      levs = contours.levels[which(contours.in[np,])]
      Nlev = length(levs)
      if ((fill && col.fill == "colbar") || (border && col.border == "colbar")) {
        col.inds.frst = min(which(contours.in[np,]))
        col.inds = col.inds.frst:(col.inds.frst+Nlev)
      }
      
      ### first sub-polygon
      lola1.3 = NULL
      lola1.2 = NULL
      lola2.3 = NULL
      if (fill || border) {
        if (col.fill == "colbar") {cb.fill = colbar[[col.inds[1]]]}
        if (col.border == "colbar") {cb.border = colbar[[col.inds[1]]]}
      }
      if (num.ord[1] == levs[1]) {
        # first sub-polygon is empty
        if (contours && contours.greater && num.ord[2] == levs[1]) {
          # first contour is aligned with the two min-nodes
          lns.lon[ilns:(ilns+1)] = lon.ord[1:2]
          lns.lat[ilns:(ilns+1)] = lat.ord[1:2]
          ilns = ilns + 3
        }
      } else {
        if (num.ord[3] == levs[1]) {
          # first sub-polygon is identical with the original triangle
          if (fill || border) {
            sl.plot.polygon(plot.init.res,lon.ord,lat.ord,fill=fill,col.fill=cb.fill,
                            border=border,col.border=cb.border,border.lwd=border.lwd,border.lty=border.lty)
          }
        } else {
          # first contour splits the original triangle and thus crosses the 1.3 (min.max) edge
          lola1.3 = sl.p2p(lon.ord[1],lat.ord[1],lon.ord[3],lat.ord[3],(levs[1]-num.ord[1])/(num.ord[3]-num.ord[1]))
          if (fill || border) {
            poly.lon = c(lon.ord[1],lola1.3$lon)
            poly.lat = c(lat.ord[1],lola1.3$lat)
          }
          if (num.ord[2] > levs[1]) {
            # first contour crosses the 1.2 (min.medium) edge
            lola1.2 = sl.p2p(lon.ord[1],lat.ord[1],lon.ord[2],lat.ord[2],(levs[1]-num.ord[1])/(num.ord[2]-num.ord[1]))
            if (fill || border) {
              poly.lon = c(poly.lon,lola1.2$lon)
              poly.lat = c(poly.lat,lola1.2$lat)
            }
            if (contours) {
              lns.lon[ilns:(ilns+1)] = c(lola1.3$lon,lola1.2$lon)
              lns.lat[ilns:(ilns+1)] = c(lola1.3$lat,lola1.2$lat)
              ilns = ilns + 3
            }
          } else {
            if (num.ord[2] == levs[1]) {
              # first contour goes through the medium node
              lola2.3 = list(lon=lon.ord[2],lat=lat.ord[2])
              if (fill || border) {
                poly.lon = c(poly.lon,lola2.3$lon)
                poly.lat = c(poly.lat,lola2.3$lat)
              }
            } else {
              # first contour crosses the 2.3 (medium.max) edge
              lola2.3 = sl.p2p(lon.ord[2],lat.ord[2],lon.ord[3],lat.ord[3],(levs[1]-num.ord[2])/(num.ord[3]-num.ord[2]))
              if (fill || border) {
                poly.lon = c(poly.lon,lola2.3$lon,lon.ord[2])
                poly.lat = c(poly.lat,lola2.3$lat,lat.ord[2])
              }
            }
            if (contours) {
              lns.lon[ilns:(ilns+1)] = c(lola1.3$lon,lola2.3$lon)
              lns.lat[ilns:(ilns+1)] = c(lola1.3$lat,lola2.3$lat)
              ilns = ilns + 3
            }
          }
          if (fill || border) {
            sl.plot.polygon(plot.init.res,poly.lon,poly.lat,fill=fill,col.fill=cb.fill,
                            border=border,col.border=cb.border,border.lwd=border.lwd,border.lty=border.lty)
          }
        }
      }
      
      if (Nlev > 1) {
        ### intermediate sub-polygons
        for (ilev in 2:Nlev) {
          if (fill || border) {
            if (col.fill == "colbar") {cb.fill = colbar[[col.inds[ilev]]]}
            if (col.border == "colbar") {cb.border = colbar[[col.inds[ilev]]]}
            if (is.null(lola2.3)) {
              # previous contour has not yet reached the medium node
              if (is.null(lola1.2)) {
                # previous (first) polygon is empty
                poly.lon = lon.ord[1]
                poly.lat = lat.ord[1]
              } else {
                # previous contour crosses the 1.2 (min.medium) edge
                poly.lon = c(lola1.2$lon,lola1.3$lon)
                poly.lat = c(lola1.2$lat,lola1.3$lat)
              }
            } else {
              # previous contour has reached the medium node
              poly.lon = c(lola2.3$lon,lola1.3$lon)
              poly.lat = c(lola2.3$lat,lola1.3$lat)
            }
          }
          lola1.3 = sl.p2p(lon.ord[1],lat.ord[1],lon.ord[3],lat.ord[3],(levs[ilev]-num.ord[1])/(num.ord[3]-num.ord[1]))
          if (fill || border) {
            poly.lon = c(poly.lon,lola1.3$lon)
            poly.lat = c(poly.lat,lola1.3$lat)
          }
          if (num.ord[2] > levs[ilev]) {
            # contour crosses the 1.2 (min.medium) edge
            lola1.2 = sl.p2p(lon.ord[1],lat.ord[1],lon.ord[2],lat.ord[2],(levs[ilev]-num.ord[1])/(num.ord[2]-num.ord[1]))
            if (fill || border) {
              poly.lon = c(poly.lon,lola1.2$lon)
              poly.lat = c(poly.lat,lola1.2$lat)
            }
            if (contours) {
              lns.lon[ilns:(ilns+1)] = c(lola1.3$lon,lola1.2$lon)
              lns.lat[ilns:(ilns+1)] = c(lola1.3$lat,lola1.2$lat)
              ilns = ilns + 3
            }
          } else {
            if (num.ord[2] == levs[ilev]) {
              # contour goes through the medium node
              lola2.3 = list(lon=lon.ord[2],lat=lat.ord[2])
              if (fill || border) {
                poly.lon = c(poly.lon,lola2.3$lon)
                poly.lat = c(poly.lat,lola2.3$lat)
              }
            } else {
              # contour crosses the 2.3 (medium.max) edge
              lola2.3 = sl.p2p(lon.ord[2],lat.ord[2],lon.ord[3],lat.ord[3],(levs[ilev]-num.ord[2])/(num.ord[3]-num.ord[2]))
              if (fill || border) {
                poly.lon = c(poly.lon,lola2.3$lon)
                poly.lat = c(poly.lat,lola2.3$lat)
                if (num.ord[2] > levs[ilev-1]) {
                  poly.lon = c(poly.lon,lon.ord[2])
                  poly.lat = c(poly.lat,lat.ord[2])
                }
              }
            }
            if (contours) {
              lns.lon[ilns:(ilns+1)] = c(lola1.3$lon,lola2.3$lon)
              lns.lat[ilns:(ilns+1)] = c(lola1.3$lat,lola2.3$lat)
              ilns = ilns + 3
            }
          }
          if (fill || border) {
            sl.plot.polygon(plot.init.res,poly.lon,poly.lat,fill=fill,col.fill=cb.fill,
                            border=border,col.border=cb.border,border.lwd=border.lwd,border.lty=border.lty)
          }
        }
      }
      
      ### last sub-polygon
      if (fill || border) {
        if (col.fill == "colbar") {cb.fill = colbar[[col.inds[Nlev+1]]]}
        if (col.border == "colbar") {cb.border = colbar[[col.inds[Nlev+1]]]}
      }
      if (num.ord[3] == levs[Nlev]) {
        # last sub-polygon is empty
        if (contours && !contours.greater && num.ord[2] == levs[Nlev]) {
          # last contour is aligned with the two max-nodes
          lns.lon[ilns:(ilns+1)] = lon.ord[2:3]
          lns.lat[ilns:(ilns+1)] = lat.ord[2:3]
          ilns = ilns + 3
        }
      } else if (fill || border) {
        if (num.ord[1] == levs[Nlev]) {
          # last sub-polygon is identical with the original triangle
          sl.plot.polygon(plot.init.res,lon.ord,lat.ord,fill=fill,col.fill=cb.fill,
                          border=border,col.border=cb.border,border.lwd=border.lwd,border.lty=border.lty)
        } else {
          # last contour splits the original triangle and thus crosses the 1.3 (min.max) edge
          poly.lon = c(lola1.3$lon,lon.ord[3])
          poly.lat = c(lola1.3$lat,lat.ord[3])
          if (!is.null(lola2.3)) {
            # last contour crosses the 2.3 (medium.max) edge or goes through the medium node
            poly.lon = c(poly.lon,lola2.3$lon)
            poly.lat = c(poly.lat,lola2.3$lat)
          } else {
            # last contour crosses the 1.2 (min.medium) edge
            poly.lon = c(poly.lon,lon.ord[2],lola1.2$lon)
            poly.lat = c(poly.lat,lat.ord[2],lola1.2$lat)
          }
          sl.plot.polygon(plot.init.res,poly.lon,poly.lat,fill=fill,col.fill=cb.fill,
                          border=border,col.border=cb.border,border.lwd=border.lwd,border.lty=border.lty)
        }
      }
      
    }
    
    # plot contour lines
    if (contours) {
      sl.plot.lines(plot.init.res,lon=lns.lon,lat=lns.lat,col=col.contours,lwd=lwd.contours,lty=lty.contours)
    }
    
  }

  if (exists("colbar.res")) {
    return(colbar.res)
  } else {
    return(NULL)
  }
	
}

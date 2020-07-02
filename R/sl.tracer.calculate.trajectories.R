sl.tracer.calculate.trajectories <- function(U, x.ini, t.ini = 0, grid, i.neighs=NULL, tri.cont.ini = NULL, abort.if = NULL, tgr = NULL, dt.def = 1, T.end = 10, 
                            maxiter = 4, qfrac = 2,  cart_geo= "geo", geo.gc_rad = "gc", patch.level = 10, method = "Petterssen", Rsphere = 6371){
  
    require(zeallot)
    if(!(method %in% c("Pansy", "Petterssen", "Euler"))){stop("method must be either 'Pansy', 'Petterssen' or 'Euler'.")}
    if(method=="Pansy" & cart_geo=="geo"){stop("Pansy is currently only implemented for cart_geo = 'cart'.")}    
    if(!(cart_geo %in% c("cart","geo"))){stop("cart_geo must be either 'cart' or 'geo'.")}
    if(!(geo.gc_rad %in% c("gc", "rad"))){stop("geo.gc_rad must be either 'gc' or 'rad'.")}
    if(qfrac <= 1){stop("qfrac <= 1 makes no sense, this increases the time step size - choose qfrac > 1")}
    if(is.null(i.neighs)){
      #print(".. creating neighborhood information")
      resy <- sl.findneighbours(grid$elem, verbose = F)
      i.neighs <- lapply(1:length(grid$elem[,1]), function(ele) {
        dd <- unique(as.vector(resy$neighbour.elems[grid$elem[ele,],]))
        if(any(is.na(dd))) dd = dd[-which(is.na(dd))]
        return(sort(dd))} )
      #print(".. done with that")
    }
    if(patch.level < 1){warning("patch.level < 1 makes no sense, will use default instead (10)"); patch.level = 10}
    if(maxiter < 1){warning("maxiter < 1 makes no sense, will use default instead (4)"); maxiter = 4}
    if(any(c(T.end, dt.def, t.ini) < 0)){stop("T.end, dt.def and t.ini must be >= 0")}
    if(T.end <= t.ini){stop("T.end must be larger than t.ini")}
  
    # determine maximal number of timesteps
    dt.min = dt.def/qfrac^maxiter
    Nt.max = length(seq(t.ini, T.end, dt.min))+1
    N.traj = length(x.ini[1,])
    Nt     = length(U[1,1,])
    
    points    = rbind(grid$lon,grid$lat)
    triangles = grid$elem
    if (is.null(tgr)){tgr = 0:length(U[1,1,])}
    if (is.null(tri.cont.ini)) tri.cont.ini = rep(NA, N.traj)
    
    if(method=="Petterssen"){
      # initialize trajectory arrays
      x.out.pet = array(dim=c(N.traj,3,Nt.max))
      t.out.pet = array(dim=c(N.traj,  Nt.max))   # make space for worst-case t-grid
      flags.pet = rep(NA, N.traj)
      
      ######################### PETTERSSEN SCHEME  ################################
  
      x.out.pet[,1:2,1] = t(x.ini)
      x.out.pet[,3,1]   = tri.cont.ini
      t.out.pet[,1]     = t.ini
      
      # initialize most variables
      xn = x1 = x2 = x3 = u_x1t1 = u_x0t0 = u_x2t1 = c(NA,NA)
      U_interp = array(dim = c(2,Nt))
      
      # start time integration
      for (itraj in 1:N.traj){
        n  = 1           # counter for current time step
        t  = t.ini       # initialize t new for each trajectory
        dt = dt.def      # set time step to default value
        
        for (n in 1:Nt.max){  # time integration for a target
          if(t>T.end){break}
          # break while loop if trajectory exited domain
          tri.cont = x.out.pet[itraj,3,n]
          if(is.na(tri.cont)) {
            if(n>1){
              flags.pet[itraj] = 1; break
            } else {
              res2 <- sl.tracer.get.triangle.of.target(triangles, points, x.out.pet[itraj,1:2,1], cart_geo, NULL)
              tri.cont = res2[[2]]; x.out.pet[itraj,3,1] = tri.cont
            }
          }

          # interpolate velocity to target location
          xn[1:2] = x.out.pet[itraj,1:2,n]
          c(U_interp[1:2,1:Nt], i.tri0, flag) %<-% sl.tracer.U.xinter(xn, U, triangles, points, i.neighs, tri.cont, cart_geo, patch.level)
          
          # interpolate velocity to current time
          u_x0t0[1:2] = sl.tracer.U.tinter(U_interp, tgr, t)
          
          # adjust time step if trajectory exits patch of triangle
          for (iter in 1:maxiter){
            if(cart_geo=="geo"){
              if(geo.gc_rad=="gc"){
                x1[1:2] = unlist(sl.shift(xn[1], xn[2], dt*u_x0t0[1], dt*u_x0t0[2], Rsphere))
              } else {x1[1:2] = xn + 180/(pi*Re)*dt*u_x0t0*c(1/cos(pi/180*xn[2]),1)}
            } else {
              x1[1:2] = xn + dt*u_x0t0
            }
            c(U_interp[1:2,1:Nt], i.tri, flag) %<-% sl.tracer.U.xinter(x1, U, triangles, points, i.neighs, tri.cont = i.tri0, cart_geo = cart_geo, patch.level = 1)
            if(is.na(i.tri)){
              dt = dt / qfrac
            } else {break}
          }
          if (is.na(i.tri)){flags.pet[itraj] = 2; break}
          
          u_x1t1[1:2] = sl.tracer.U.tinter(U_interp,tgr,t+dt)
          if(cart_geo=="geo"){
            if(geo.gc_rad=="gc"){x2[1:2] = unlist(sl.shift(xn[1],xn[2], 0.5*dt*(u_x0t0 + u_x1t1)[1], 0.5*dt*(u_x0t0 + u_x1t1)[2], Rsphere))} else {
              x2[1:2] = xn + 180/(pi*Re)*  0.5*dt*(u_x0t0 + u_x1t1) * c(1/cos(pi/180*xn[2]),1)
            }
          } else {
            x2[1:2] = xn + 0.5*dt*(u_x0t0 + u_x1t1)
          }
          
          c(U_interp[1:2,1:Nt], i.tri, flag) %<-% sl.tracer.U.xinter(x2, U, triangles, points, i.neighs, tri.cont = i.tri0, cart_geo = cart_geo, patch.level = patch.level)
          
          if(is.na(i.tri)) {
            flags.pet[itraj] = 1; break
          }
          
          
          u_x2t1[1:2] = sl.tracer.U.tinter(U_interp,tgr,t+dt)
          if(cart_geo=="geo"){
            if(geo.gc_rad=="gc"){
              x3[1:2] = unlist(sl.shift(xn[1], xn[2], 0.5*dt*(u_x0t0 + u_x2t1)[1], 0.5*dt*(u_x0t0 + u_x2t1)[2], Rsphere))} else {
              x3[1:2] = xn + 180/(pi*Re)* 0.5*dt*(u_x0t0 + u_x2t1)*c(1/cos(pi/180*xn[2]),1)
            }
          } else {
            x3[1:2] = xn + 0.5*dt*(u_x0t0 + u_x2t1)
          }
          
          # FINAL DOMAIN CHECK
          patch <- i.neighs[[i.tri0]]
          patch = unique(unlist(i.neighs[patch]))
          res <- sl.tracer.get.triangle.of.target(triangles[patch,], points, x3, cart_geo, patch)
          i.tri = res[[2]]
          if(is.na(i.tri)){
            patch <- i.neighs[[i.tri0]]
            for (p in 1:patch.level){
              if (p > 1){patch = unique(unlist(i.neighs[patch]))}
              c(mu, i.tri, i.pat) %<-% sl.tracer.get.triangle.of.target(
                triangles = triangles[patch,], 
                points = points, 
                target = x3, cart_geo = cart_geo,
                patch.indices = patch)
              if (!is.na(i.tri)){break}
            } 
            if (is.na(i.tri)){
              print("got here")
              print(paste0("time step ", n))
              print(paste0("i.tri = ", i.tri))
              print(paste0("traj = ", itraj))
              # absolutely the worst case: check whole grid again...
              c(mu, i.tri, i.pat) %<-% sl.tracer.get.triangle.of.target(triangles, points, x3, cart_geo, NULL)
            }
            if(is.na(i.tri)){flags.pet[itraj] = 1; break}
          }
          
          # abort criterion
          if (!is.null(abort.if)){
            if (length(dim(abort.if))==2){
              # find corresponding last time step of abort array
              m = max(which(tgr <= t))
              if(abort.if[i.tri, m]){flags.pet[itraj] = 3; break}
            } else {if(abort.if[i.tri]){flags.pet[itraj] = 3; break}}
          }  
          
          # update position, element containing position and output time grid
          x.out.pet[itraj,1:2,n+1] = x3
          x.out.pet[itraj,  3,n+1] = i.tri
          t.out.pet[itraj,    n+1] = t+dt
          
          # update time counts
          n  = n+1
          t  = t+dt
          dt = dt.def
        } # end time loop
      } # end for trajectories
      return(list(pos = x.out.pet, time = t.out.pet, flag = flags.pet))
    } # end if mode == Petterssen
    if(method=="Euler"){

      # initialize trajectory arrays (and more)
      x.out.adt = array(dim=c(N.traj,3,Nt.max+1))
      t.out.adt = array(dim=c(N.traj,  Nt.max+1))   # save possible irregular t-grid
      flags.adt = rep(NA,N.traj)       
      xn = u = x_pseudo = c(NA,NA); U_interp = array(dim = c(2,Nt))
      ######################### adaptv. explicit euler ################################
    
      x.out.adt[,1:2,1] = t(x.ini)
      x.out.adt[,3,1]   = tri.cont.ini
      t.out.adt[,1]     = 0
      
      for (itraj in 1:N.traj){
        t  = t.ini
        dt = dt.def
        n=1
        while(t < T.end){
          # find containing triangle
          if(!is.na(flags.adt[itraj])){break()} # skip flagged targets
          if(n==1){
            tri.cont = x.out.adt[itraj,3,1]
          } else {
            tri.cont = x.out.adt[itraj,3,n]
          }
          if(is.na(tri.cont)) {
            if(n>1){
              flags.adt[itraj] = 1; break
            } else {
              res2 <- sl.tracer.get.triangle.of.target(triangles, points, x.out.adt[itraj,1:2,1], cart_geo, NULL)
              tri.cont = res2[[2]]; x.out.adt[itraj,3,1] = tri.cont
            }
          }
          
          # interpolate velocity to target location and current time
          xn = x.out.adt[itraj,1:2,n]
          c(U_interp[1:2,1:Nt], i.tri0, flag) %<-% sl.tracer.U.xinter(x = xn, U = U, triangles = triangles, points = points, linds = i.neighs, tri.cont =
                                                                        tri.cont, cart_geo = cart_geo, patch.level = patch.level)
          u[1:2] = sl.tracer.U.tinter(U_interp, tgr, t)
          
          # take half a time step if trajectory exits patch of triangle
          for (iter in 1:maxiter){
            if(cart_geo == "geo"){
              if(geo.gc_rad=="gc"){
                x_pseudo[1:2] = unlist(sl.shift(xn[1], xn[2], dt*u[1], dt*u[2], Rsphere))
              } else {
                x_pseudo[1:2] = xn + 180/(pi*Re)*dt*u*c(1/cos(pi/180*xn[2]),1)
              }
            } else {
              x_pseudo[1:2] = xn + u*dt
            }
            c(mu, i.tri, i.pat) %<-% sl.tracer.get.triangle.of.target(triangles[i.neighs[[i.tri0]],], points, x_pseudo, cart_geo, i.neighs[[i.tri0]])
            if(is.na(i.tri)){
              dt = dt / qfrac
            } else {break}
          }
          if (is.na(i.tri)){flags.adt[itraj] = 2; break}
          
          x.out.adt[itraj,1:2,n+1] = x_pseudo
          x.out.adt[itraj,  3,n+1] = i.tri
          t.out.adt[itraj,    n+1] = t + dt
          n = n+1
          t  = t + dt
          dt = dt.def
          
        }
      }
      return(list(pos = x.out.adt[,,], time = t.out.adt[,], flag = flags.adt))
    } # end if mode == Euler
    if(method=="Pansy"){
      
      
      prelim.anaSys <- function(u_e, v_e, Rivt){
        A = matrix(nrow = 2, ncol = 2)
        b = matrix(nrow = 2, ncol = 1)
        
        Lu     = Rivt %*% u_e
        Lv     = Rivt %*% v_e
        A[1,]  = Lu[1:2]
        A[2,]  = Lv[1:2]
        b[1:2] = c(Lu[3],Lv[3])
        
        es = eigen(A)
        l  = es$values
        S  = es$vectors
        S_ = solve(S)
        V  = diag(l)
        
        f  = S_ %*% b
        return(list(f, l, S, S_, V))
      }
      
      z <- function(t, l, f, z0) {
        if (!any(l==0))  {(z0+f/l)*exp(l*t)-f/l}
        else {
          i = which(l==0)
          out = c(NA,NA)
          out[i]  = f[i]*t + z0[i]
          if (length(i)==1){
            out[3-i] = (z0[3-i]+f[3-i]/l[3-i])*exp(l[3-i]*t)-f[3-i]/l[3-i]
          }
          return(out)
        }
      }
      
      # initialize trajectory arrays (and more)
      x.out.mat = array(dim=c(N.traj,3,Nt.max+1))
      t.out.mat = array(dim=c(N.traj,  Nt.max+1))   # save possible irregular t-grid
      flags.mat = rep(NA,N.traj)                    # save time when grid is left
      xn = u = dx = c(NA,NA); U_interp = array(dim = c(2,Nt))
      
      S = S_ = V =  matrix(nrow=2,ncol=2)
      b = matrix(nrow=2,ncol=1)
      
      #########################  PANSY SCHEME  ################################
      x.out.mat[,1:2,1] = t(x.ini)
      x.out.mat[,3,1]   = tri.cont.ini
      t.out.mat[,1]     = 0
      
      # start time integration
      for (itraj in 1:N.traj){
        n=1
        t = t.ini
        dt = dt.def
        while (t < T.end){
          tri.cont = x.out.mat[itraj,3,n]
          if ((n>1) & is.na(tri.cont)) break
          
          xn[1:2] = x.out.mat[itraj,1:2,n]
          
          # find containing triangle first time
          if(is.na(tri.cont) & n==1){
            res2 <- sl.tracer.get.triangle.of.target(triangles, points, xn, cart_geo, NULL)
            tri.cont = res2[[2]]; x.out.mat[itraj,3,n] = tri.cont
          }
          
          res <- sl.tracer.get.nodecoords.of.triangle(tri = triangles[tri.cont,], points = points)
          
          R2d <- rbind(matrix(unlist(res), nrow = 2),rep(1,3))
          
          Rivt = t(solve(R2d))
          Ux = sl.tracer.U.tinter(U, tgr, t)
          
          u_e  = Ux[1,triangles[tri.cont,]] #c(u1[1],u2[1],u3[1])
          v_e  = Ux[2,triangles[tri.cont,]] #c(u1[2],u2[2],u3[2])
          
          q = prelim.anaSys(u_e, v_e, Rivt)
          
          z0 = q[[4]] %*% xn
          
          # begin new
          for (iter in 1:maxiter){
            xnew = Re(q[[3]] %*% z(dt, q[[2]], q[[1]], z0))
            c(mu, i.tri, i.pat) %<-% sl.tracer.get.triangle.of.target(
              triangles = triangles[i.neighs[[tri.cont]],], 
              points = points, 
              target = xnew,
              patch.indices = i.neighs[[tri.cont]], cart_geo = "cart")
            if(is.na(i.tri)){
              dt = dt / qfrac
            } else {break}
          }
          if (is.na(i.tri)){flags.mat[itraj] = 2; break}
          # end new
          
          # do euler fwd and store time grid
          x.out.mat[itraj,1:2,n+1] = xnew
          x.out.mat[itraj,  3,n+1] = i.tri
          t.out.mat[itraj,    n+1] = t+dt
          
          t = t + dt
          dt = dt.def
          n = n+1
        }
        
      }
      return(list(pos = x.out.mat[,,], time = t.out.mat[,], flag = flags.mat))
    }
    
  }
    

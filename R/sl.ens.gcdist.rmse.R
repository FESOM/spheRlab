sl.ens.gcdist.rmse <- function(ensm.in, t.ind = 1:length(ensm.in[1,1,]), obs = NULL, mode = "pm", Rsphere = 1){
  Nt = length(t.ind); Nm = length(ensm.in[,1,1])
  msepos.ens = rep(0, Nt)
  
  if (mode=="pm"){
    i.tout = 1
    for(i.time in t.ind){
      if(any(is.na(ensm.in[,,t.ind]))){warning(paste0("ensemble contains NAs at t.ind = ", i.time))}
      for (i in 1:Nm){
        verif = ensm.in[ i,,]
        forcs = ensm.in[-i,,]
        dd = sl.gc.dist(c(verif[1,i.time], forcs[,1,i.time]), 
                        c(verif[2,i.time], forcs[,2,i.time]), Rsphere = Rsphere, sequential = F)
        msepos.ens[i.tout] = msepos.ens[i.tout] + sum(dd^2)
      }
      i.tout = i.tout + 1
    }
  msepos.ens = msepos.ens / (Nm*(Nm-1)-1)
  } else if (mode=="obs"){
    if(is.null(obs)){warning("if mode=='obs', input variable 'obs' must be given as well"); return(NULL)}
    i.tout = 1
    for(i.time in t.ind){
      if(any(is.na(ensm.in[,,t.ind]))){warning(paste0("ensemble contains NAs at t.ind = ", i.time))}
      
      verif = obs
      forcs = ensm.in
      dd = sl.gc.dist(c(verif[1,i.time], forcs[,1,i.time]), 
                      c(verif[2,i.time], forcs[,2,i.time]), Rsphere = Rsphere, sequential = F)
      msepos.ens[i.tout] = msepos.ens[i.tout] + sum(dd^2)
      
      i.tout = i.tout + 1
    }
    msepos.ens = msepos.ens / Nm
  } else {
    warning("mode must either be 'pm' or 'obs'."); return(NULL)
  }
  return(sqrt(msepos.ens))
}
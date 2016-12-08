sl.findneighbours <-
function (elem,maxmaxneigh=12,reverse=TRUE,verbose=FALSE,max.iter=10) {
        
# elem: an Nx3 matrix containing triangular elements, i.e. the
#       corresponding nodes in clockwise order, as rows
# maxneighbours: a priori estimate of an upper bound for the
#       maximum number of neighbour nodes (if too low, function crashes)
# reverse: logical flag whether to reverse order of neighbours
# verbose: obvious

# find neighbouring nodes and elements in correct order
N = max(elem)
Ne = nrow(elem)
neighmat = matrix(nrow=N,ncol=maxmaxneigh)
Nneigh = rep(0,N)
barmat = matrix(nrow=N,ncol=maxmaxneigh)
iscomplete = rep(FALSE,N)
iekdone = matrix(rep(FALSE,Ne*3),ncol=3)
niter = 0
completed = TRUE
while (sum(iekdone) < Ne*3) {
        niter = niter + 1
        if (niter > max.iter) {
                warning("some elements could not be arranged in order!")
                completed = FALSE
                break
        }
        print(paste("starting iteration ",niter,sep=""))
        for (ie in 1:Ne) {
                if (sum(iekdone[ie,]) == 3) { next }
                for (k in 1:3) {
                        if (iekdone[ie,k]) { next }
                        i = elem[ie,k]
                        if (iscomplete[i]) {stop("ups! trying to add neighbors to a node labelled complete!")}
                        neigh1 = elem[ie,k%%3+1]
                        neigh2 = elem[ie,(k+1)%%3+1]
                        if (is.na(neighmat[i,1])) {
                                barmat[i,1] = ie
                                neighmat[i,1] = neigh1
                                neighmat[i,2] = neigh2
                                Nneigh[i] = 2
                                iekdone[ie,k] = TRUE
                        } else {
                                found1 = FALSE
                                found2 = FALSE
                                if (neighmat[i,Nneigh[i]] == neigh1) {found1 = TRUE}
                                if (neighmat[i,1] == neigh2) {found2 = TRUE}
                                if (found1 && found2) {
                                        if (verbose) {print("found both, node complete")}
                                        barmat[i,Nneigh[i]] = ie
                                        iscomplete[i] = TRUE
                                        iekdone[ie,k] = TRUE
                                } else {
                                        if (Nneigh[i] == maxmaxneigh) {stop("ups! maxmaxneigh is insufficient!")}
                                        if (found1) {
                                        if (verbose) {print("found 1")}
                                        neighmat[i,(Nneigh[i]+1)] = neigh2
                                        barmat[i,Nneigh[i]] = ie
                                        Nneigh[i] = Nneigh[i] + 1
                                        iekdone[ie,k] = TRUE
                                } else {if (found2) {
                                        if (verbose) {print("found 2")}
                                        neighmat[i,2:maxmaxneigh] = neighmat[i,1:(maxmaxneigh-1)]
                                        neighmat[i,1] = neigh1
                                        barmat[i,2:maxmaxneigh] = barmat[i,1:(maxmaxneigh-1)]
                                        barmat[i,1] = ie
                                        Nneigh[i] = Nneigh[i] + 1
                                        iekdone[ie,k] = TRUE
                                } else {
                                        if (verbose) {print("found none, retry element in next iteration")}
                                }}}
                        }
                }
        }
}
             
maxneigh = max(Nneigh)
neighmat = neighmat[,1:maxneigh]
barmat = barmat[,1:maxneigh]
if (reverse) {
        print("reversing order of neighbours")
        for (i in 1:N) {
        		if (Nneigh[i] > 1) {
                	neighmat[i,1:Nneigh[i]] = neighmat[i,Nneigh[i]:1]
                	barmat[i,1:(Nneigh[i]-1)] = barmat[i,(Nneigh[i]-1):1]
                }
        }
}

if (completed) {
	elems.completed=NULL
} else {
	elems.completed=iekdone
}

return(list(neighbour.nodes=neighmat,neighbour.elems=barmat,internal.nodes=iscomplete,N.neighbour.nodes=Nneigh,all.elements.arranged=completed,elems.completed=elems.completed))

}

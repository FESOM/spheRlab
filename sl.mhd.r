sl.mhd <- function(A,B,cA,cB){

# Earth's radius in m 
R <- 6371000 

# Checking the dimension of A and B
A_dim <- dim(A)
B_dim <- dim(B)
cA_dim <- dim(cA)
cB_dim <- dim(cB)

if(length(A_dim) != length(A_dim)){
	print("ERROR: The dimensions of points in the two sets are not equal, please for each point express its lon/lat coordinates as Nx2 vector")
	return()
} else{
	
#	Calculating the forward HD
	fhd <- 0
	for(a in 1:A_dim[1]){
		tempdist <- numeric(cA_dim[1]+B_dim[1]) 
		for (b in 1:B_dim[1]){
			tempdist[b] <- sl.gc.dist(c(A[a,1],B[b,1]),c(A[a,2],B[b,2]),R)
			}
		for (b in 1:cA_dim[1]){
			tempdist[b+B_dim[1]] <- sl.gc.dist(c(A[a,1],cA[b,1]),c(A[a,2],cA[b,2]),R)
			}
		fhd <- fhd + min(tempdist)
		}
		fhd <- fhd / A_dim[1]
		
#	Calculating the reverse HD
	rhd <- 0
	for(a in 1:B_dim[1]){
		tempdist <- numeric(cB_dim[1]+A_dim[1]) 
		for (b in 1:A_dim[1]){
			tempdist[a] <- sl.gc.dist(c(A[b,1],B[a,1]),c(A[b,2],B[a,2]),R)
			}
		for (b in 1:cB_dim[1]){
			tempdist[a+A_dim[1]] <- sl.gc.dist(c(B[a,1],cB[b,1]),c(B[a,2],cB[b,2]),R)
		    }
		rhd <- rhd + min(tempdist)
		}
		rhd <- rhd / B_dim[1]

	
	mhd <- max(fhd,rhd);
	return(mhd)
}
} 
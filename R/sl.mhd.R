sl.mhd <- function(A,B,cA=NULL,cB=NULL,kA=NULL,kB=NULL,R=NULL)
{

	options(show.error.locations = TRUE)

	# Checking the correct size of A and B
	# Errors and Warnings

	A_dim <- dim(A)
	B_dim <- dim(B)

	if(length(A_dim) != length(B_dim))
	{
		stop("ERROR: The dimensions of points in the two sets are not equal, please make sure that lon-lat coordinates are stored in a (N X 2) array")
		return()
	} 
	if (A_dim[1]==0 | B_dim[1]==0)
	{
		warning("WARNING: One of the two segments is empty. Mod. Hausdorff Distance can not be computed.")
		return(NA)
	}

	# Defining Missing Parameters

	if (missing(kA)){kA<-1}      # No points are skipped in the first segment (A)
	if (missing(kB)){kB<-1}      # No points are skipped in the second segment (B)
	if (missing(R)){R<-1}
	if (R==-1){R<-6373044}
	if (missing(cA)==0)
		{
			A <- rbind(A,cA)
		}
	if (missing(cB)==0)
		{
			B <- rbind(B,cB)
		}

	A_dim_c <- dim(A)
	B_dim_c <- dim(B)
		
	# Calculating the forward HD

	fhd <- 0
	lons <- c(0,B[,1])
	lats <- c(0,B[,2])

	k <- seq.int(1,A_dim[1],kA)

	for(a in seq.int(1,A_dim[1],kA))
	{
		tempdist <- numeric(B_dim_c[1])
		lons[1] <- A[a,1]
		lats[1] <- A[a,2]
		tempdist <- sl.gc.dist(lons,lats,Rsphere=R,sequential=FALSE)
		fhd <- fhd + min(tempdist)
	}

	fhd <- fhd / length(k)
		
	# Calculating the reverse HD

	rhd <- 0
	lons <- c(0,A[,1])
	lats <- c(0,A[,2])		

	k <- seq.int(1,B_dim[1],kB)

	for(b in seq.int(1,B_dim[1],kB))
	{
		tempdist <- numeric(A_dim_c[1])
		lons[1] <- B[b,1]
		lats[1] <- B[b,2]
		tempdist <- sl.gc.dist(lons,lats,Rsphere=R,sequential=FALSE)
		rhd <- rhd + min(tempdist)
	}

	rhd <- rhd / length(k)

	mhd <- max(fhd,rhd);
	return(mhd)

} 

###############################################################
#####             Parallelized Version                    #####
###############################################################
# This is just the part that it is different
# Requires in addition 
#library("foreach")
#library("doParallel")
	
	# Calculating the forward HD

	# fhd <- 0
	# lons <- c(0,B[,1])
	# lats <- c(0,B[,2])

	# k <- seq.int(1,A_dim[1],kA)

	# cores=detectCores()
	# cl <- makeCluster(cores[1]-1) # not to overload your computer
	# registerDoParallel(cl)

	# fhd_vec <- foreach(a=seq.int(1,A_dim[1],kA), .combine=cbind, .packages = "spheRlab") %dopar% 
	# {	
		# lons[1] <- A[a,1]
		# lats[1] <- A[a,2]
		# tempdist_vec <- sl.gc.dist(lons,lats,Rsphere=R,sequential=FALSE)
		# tempdist_min <- min(tempdist_vec)
		# tempdist_min
	# }
		
	# fhd <- sum(fhd_vec) / length(k)
	
	# stopCluster(cl)
		
# #	Calculating the reverse HD

	# rhd <- 0
	# lons <- c(0,A[,1])
	# lats <- c(0,A[,2])

	# k <- seq.int(1,B_dim[1],kB)

	# cores=detectCores()
	# cl <- makeCluster(cores[1]-1) # not to overload your computer
	# registerDoParallel(cl)

	# rhd_vec <- foreach(b=seq.int(1,B_dim[1],kB), .combine=cbind, .packages = "spheRlab") %dopar% 
	# {	
		# lons[1] <- B[b,1]
		# lats[1] <- B[b,2]
		# tempdist_vec <- sl.gc.dist(lons,lats,Rsphere=R,sequential=FALSE)
		# tempdist_min <- min(tempdist_vec)
		# tempdist_min
	# }
		
	# rhd <- sum(rhd_vec)/length(k)
	
	# stopCluster(cl)
	# mhd <- max(fhd,rhd);
	# return(mhd)

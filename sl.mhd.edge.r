sl.mhd.edge <- function(sic,lon,lat,thr){

dim_sic <- dim(sic)
sic_contr <- sic
# Sea ice concentration binarization
if (length(dim_sic)==2){
	sic[sic>=thr] <- 1
	sic[sic< thr] <- 0
	sic[is.na(sic)] <- -1
	}


# More than one ensemble member
if (length(dim_sic)==3){
	sic[is.na(sic)] <-0
	sic[sic>=thr] <- 1
	sic[sic< thr] <- 0
	sic_new <- matrix(0,nrow=dim_sic[1],ncol=dim_sic[2])
	for (i in 1:dim_sic[3]){
		sic_new <- (sic_new + sic[,,i])
		}
	# Sea ice probability
	sip <- sic_new/dim_sic[3] 
	
	# Second Binarization
	sip[sip>=0.5]=1
	sip[sip< 0.5]=0
	sic <- sip
	sic[is.na(sic_contr[,,1])] <- -1
	}
	
	print(dim(sic))
	

# Defining the edge matrix 
edge_m <- matrix(0,nrow=dim_sic[1],ncol=dim_sic[2])
		
# Classification of the edge 

count <- 0 # Counter for the edge size

for (i in 1:dim_sic[1]){
	for (j in 1:dim_sic[2]){
		
		if (sic[i,j]==1){
			
			# Inside the matrix
			if (i>1 && i<dim_sic[1] && j>1 && j<dim_sic[2]){
				if ((sic[i,j+1]==0) || (sic[i+1,j]==0) || (sic[i-1,j]==0) || (sic[i,j-1]==0)){edge_m[i,j] <- 1; count <- count + 1}
			} else if (i==1 && j>1 && j<dim_sic[2]){ # Some different cases - 4 BORDERS
				if (sic[i+1,j]==0 || sic[i,j+1]==0 || sic[i,j-1]==0){edge_m[i,j] <- 1; count <- count + 1}
			} else if (i==dim_sic[1] && j>1 && j<dim_sic[2]){
				if (sic[i,j+1]==0 || sic[i-1,j]==0 || sic[i,j-1]==0){edge_m[i,j] <- 1; count <- count + 1}
			} else if (j==1&& i>1 && i<dim_sic[1]){
				if (sic[i+1,j]==0 || sic[i-1,j]==0 || sic[i,j+1]==0){edge_m[i,j] <- 1; count <- count + 1}
			} else if (j==dim_sic[2] && i>1 && i<dim_sic[1]){
				if (sic[i-1,j]==0 || sic[i+1,j]==0 || sic[i,j-1]==0){edge_m[i,j] <- 1; count <- count + 1}
			} else if (i==1&& j==1){ # Some different cases - 4 CORNERS
				if (sic[i,j+1]==0 || sic[i+1,j]==0){edge_m[i,j] <- 1; count <- count + 1}
			} else if (i==dim_sic[1] && j==1){
				if (sic[i,j+1]==0 || sic[i-1,j]==0){edge_m[i,j] <- 1; count <- count + 1}
			} else if (i==1&& j==dim_sic[2]){
				if (sic[i,j-1]==0 || sic[i+1,j]==0){edge_m[i,j] <- 1; count <- count + 1}
			} else if (i==dim_sic[1] && j==dim_sic[2]){
				if (sic[i,j-1]==0 || sic[i-1,j]==0){edge_m[i,j] <- 1; count <- count + 1}
			}
			}
		}
	}

edge <- matrix(0, nrow=count, ncol=2)
count2  <- 0

for (i in 1:dim_sic[1]){
	for (j in 1:dim_sic[2]){
		
		if (edge_m[i,j]==1){
			count2 <- count2 + 1
			edge[count2,1] <- lon[i,j]
			edge[count2,2] <- lat[i,j]
		}
		}
	}
# Landmask extraction
land_m <- matrix(0,nrow=dim_sic[1],ncol=dim_sic[2])

count_3 <- 0 # Counter for the edge size

for (i in 1:dim_sic[1]){
	for (j in 1:dim_sic[2]){
		
		if (sic[i,j]==-1){
			
			# Inside the matrix
			if (i>1&& i<dim_sic[1]&& j>1&& j<dim_sic[2]){
				if ((sic[i,j+1]==1) || (sic[i+1,j]==1) || (sic[i-1,j]==1) || (sic[i,j-1]==1)){land_m[i,j] <- 1; count_3 <- count_3 + 1}
			} else if (i==1&& j>1&& j<dim_sic[2]){ # Some different cases - 4 BORDERS
				if (sic[i+1,j]==1 || sic[i,j+1]==1 || sic[i,j-1]==1){land_m[i,j] <- 1; count_3 <- count_3 + 1}
			} else if (i==dim_sic[1]&& j>1&& j<dim_sic[2]){
				if (sic[i,j+1]==1 || sic[i-1,j]==1 || sic[i,j-1]==1){land_m[i,j] <- 1; count_3 <- count_3 + 1}
			} else if (j==1&& i>1&& i<dim_sic[1]){
				if (sic[i+1,j]==1 || sic[i-1,j]==1 || sic[i,j+1]==1){land_m[i,j] <- 1; count_3 <- count_3 + 1}
			} else if (j==dim_sic[2]&& i>1&& i<dim_sic[1]){
				if (sic[i-1,j]==1 || sic[i+1,j]==1 || sic[i,j-1]==1){land_m[i,j] <- 1; count_3 <- count_3 + 1}
			} else if (i==1&& j==1){ # Some different cases - 4 CORNER
				if (sic[i,j+1]==1 || sic[i+1,j]==1){land_m[i,j] <- 1; count_3 <- count_3 + 1}
			} else if (i==dim_sic[1]&& j==1){
				if (sic[i,j+1]==1 || sic[i-1,j]==1){land_m[i,j] <- 1; count_3 <- count_3 + 1}
			} else if (i==1&& j==dim_sic[2]){
				if (sic[i,j-1]==1 || sic[i+1,j]==1){land_m[i,j] <- 1; count_3 <- count_3 + 1}
			} else if (i==dim_sic[1]&& j==dim_sic[2]){
				if (sic[i,j-1]==1 || sic[i-1,j]==1){land_m[i,j] <- 1; count_3 <- count_3 + 1}
			}
			}
		}
	}

land <- matrix(0, nrow=count_3, ncol=2)
count2  <- 0

for (i in 1:dim_sic[1]){
	for (j in 1:dim_sic[2]){
		
		if (land_m[i,j]==1){
			count2 <- count2 + 1
			land[count2,1] <- lon[i,j]
			land[count2,2] <- lat[i,j]
		}
		}
	}

return(list(edge=edge, coast=land))

}
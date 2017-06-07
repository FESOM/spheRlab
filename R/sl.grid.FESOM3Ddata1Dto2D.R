sl.grid.FESOM3Ddata1Dto2D <- function (ifile,ofile,meshdir,varnames=NULL,temp2sst=FALSE,salt2sss=FALSE,idepth=NULL,odepth=NULL,compression=NA,shuffle=FALSE,verbose=TRUE,return.env=FALSE) {

require("ncdf4")

Nlev = scan(paste(meshdir,"/aux3d.out",sep=""),n=1,what=integer())
N = scan(paste(meshdir,"/nod2d.out",sep=""),n=1,what=integer())
aux3d.mat = matrix(scan(paste(meshdir,"/aux3d.out",sep=""),na.strings="-999",skip=1,n=Nlev*N,what=integer()),ncol=Nlev,byrow=TRUE)
while (sum(is.na(aux3d.mat[,Nlev])) == N) {
	if (verbose) {print(paste("removing empty level",Nlev,"from data"))}
	Nlev = Nlev - 1
	aux3d.mat = aux3d.mat[,1:Nlev]
}
N1D = sum(!is.na(aux3d.mat))

# retrieve depth information
if (is.null(idepth)) {
	if (verbose) {print("retrieving depth information from nod3d.out")}
	depth = unique(scan(paste(meshdir,"/nod3d.out",sep=""))[seq(5,N1D*5+1,5)]) * -1
	if (!is.null(odepth)) {
		if (verbose) {print("writing depth information to odepth")}
		write(depth,odepth,ncolumns=1)
	}
} else {
	if (verbose) {print("reading depth information from idepth")}
	depth = scan(idepth)
}
if (length(depth) != Nlev) { stop("data in aux3d.out is inconsistent with the number of depth levels") }

in.f = nc_open(ifile)
ncells.dim = ncdim_def(name="ncells",units="",vals=1:N,create_dimvar=FALSE)
depth.dim = ncdim_def(name="depth",units="m",vals=depth)
time.dim.in = in.f$dim$time
if (is.null(time.dim.in)) {
	time.dim.in = in.f$dim$T
	if (is.null(time.dim.in)) {
		stop("found no proper time dimension, must be named 'time' or 'T'")
	}
}
Nt = time.dim.in$len
time.dim = ncdim_def(name="time",units=time.dim.in$units,vals=time.dim.in$vals,unlim=time.dim.in$unlim)
if (time.dim.in$name == "T") {
	time.dim$vals = ncvar_get(in.f,"time",collapse_degen=FALSE)
}
if (is.null(varnames)) {
	varnames = list()
	for (i in 1:in.f$nvars) {
		varnames[[i]] = in.f$var[[i]]$name
	}
}
vars = list()
vals = list()
convert.vars = c()
ilist = 0
for (i in 1:in.f$nvars) {
	ilist = ilist + 1
	vari = in.f$var[[i]]
	if (vari$name %in% varnames) {
		if (verbose) {print(paste("processing variable",vari$name))}
		if (vari$prec == "int") { vari$prec = "integer" }
		if (vari$ndims == 2 && vari$size[1] == N1D && vari$size[2] == Nt) {
			vals[[ilist]] = ncvar_get(in.f,vari,collapse_degen=FALSE)
			vars[[ilist]] = ncvar_def(vari$name,vari$units,list(ncells.dim,depth.dim,time.dim),vari$missval,vari$longname,vari$prec,compression=compression,shuffle=shuffle)
			convert.vars = c(convert.vars,TRUE)
			if (vari$name == "temp" && !is.null(temp2sst) && as.logical(temp2sst)) {
				if (verbose) {print("extracting sea surface temperatures from 3D potential temperature field")}
				ilist = ilist + 1
				vals[[ilist]] = ncvar_get(in.f,vari,count=c(N,-1),collapse_degen=FALSE)
				vars[[ilist]] = ncvar_def("sst",vari$units,list(ncells.dim,time.dim),vari$missval,"Sea Surface Temperature",vari$prec,compression=compression,shuffle=shuffle)
				convert.vars = c(convert.vars,FALSE)
			}
			if (vari$name == "salt" && !is.null(salt2sss) && as.logical(salt2sss)) {
				if (verbose) {print("extracting sea surface salinities from 3D salinity field")}
				ilist = ilist + 1
				vals[[ilist]] = ncvar_get(in.f,vari,count=c(N,-1),collapse_degen=FALSE)
				vars[[ilist]] = ncvar_def("sss",vari$units,list(ncells.dim,time.dim),vari$missval,"Sea Surface Salinity",vari$prec,compression=compression,shuffle=shuffle)
				convert.vars = c(convert.vars,FALSE)
			}
			next
		}
		if (vari$ndims == 2 && vari$size[1] == N && vari$size[2] == Nt) {
			vals[[ilist]] = ncvar_get(in.f,vari,collapse_degen=FALSE)
			vars[[ilist]] = ncvar_def(vari$name,vari$units,list(ncells.dim,time.dim),vari$missval,vari$longname,vari$prec,compression=compression,shuffle=shuffle)
			convert.vars = c(convert.vars,FALSE)
			next
		}
		if (vari$ndims == 1 && vari$size[1] == Nt) {
			vals[[ilist]] = ncvar_get(in.f,vari,collapse_degen=FALSE)
			vars[[ilist]] = ncvar_def(vari$name,vari$units,list(time.dim),vari$missval,vari$longname,vari$prec,compression=compression,shuffle=shuffle)
			convert.vars = c(convert.vars,FALSE)
			next
		}
		if (verbose) {warning(paste("variable",vari$name,"not recognised and thus omitted"))}
	} else {
		if (verbose) {print(paste("skipping variable",vari$name))}
	}
	ilist = ilist - 1
}
if (verbose) {print(paste(sum(convert.vars),"3D variables are converted from 1D to 2D, adding dummy data where necessary"))}
out.f = nc_create(ofile,vars)
if (sum(convert.vars) > 0) {
	ncatt_put(out.f,"depth","positive","down")
	ncatt_put(out.f,"depth","axis","Z")
}
ncatt_put(out.f,"time","calendar","proleptic_gregorian")
for (i in 1:ilist) {
	if (convert.vars[i]) {
		valsi = array(dim=c(N,Nlev,Nt))
		for (ti in 1:Nt) {
			valsi[,,ti] = vals[[i]][,ti][aux3d.mat]
		}
	} else {
		valsi = vals[[i]]
	}
	ncvar_put(out.f,vars[[i]],valsi)
	if (N %in% dim(valsi)) {
		ncatt_put(out.f,vars[[i]],"grid_type","unstructured")
	}
}
ncatt_put(out.f,0,"history",paste0(date(),": sl.grid.FESOM3Ddata1Dto2D( ifile=",ifile,", ofile=",ofile,", meshdir=",meshdir,", varnames=",varnames,", temp2sst=",temp2sst,", salt2sss=",salt2sss,", idepth=",idepth,", odepth=",odepth,", verbose=",verbose,", return.env=",return.env," )"))
nc_close(out.f)

if (return.env) {
	return(as.list(environment()))
} else {
	return(NULL)
}

}
sl.read.args = function (sep = ",", missval = NULL) {
    args.in = commandArgs(TRUE)
    if (length(args.in) == 0) {
    	warning("No arguments found. Returning NULL.")
    	return(NULL)
    }
    args.in.strsplit = strsplit(args.in, sep)
    Nargs = length(args.in)
    args = list()
    for (i in 1:Nargs) {
        arg.i = args.in.strsplit[[i]]
        arg.i.l = length(arg.i)
        if (arg.i.l > 1) {
            args[[arg.i[1]]] = arg.i[2:arg.i.l]
        }
        else {
            args[[arg.i[1]]] = missval
        }
    }
    return(args)
}

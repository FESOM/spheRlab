sl.view.colbar <-
function (colbar,labels=TRUE,labels.cex=1,labels.col="black") {
	
	#par(mar=rep(0,4))
	plot(NA,xlim=c(0,1),ylim=c(0,1),main=deparse(substitute(colbar)),xaxt="n",yaxt="n",xlab="",ylab="",bty="n")
	
	L = length(colbar)
	MN = ceiling(sqrt(L))
	dxy = 1/MN
	
	i = 1
	for (m in 1:MN) {
		if (i > L) {break}
		ymi = 1 - m/MN
		yma = 1 - (m-1)/MN
		for (n in 1:MN) {
			if (i > L) {break}
			xmi = (n-1)/MN
			xma = n/MN
			rect(xmi,ymi,xma,yma,col=colbar[[i]],border=NA)
			if (labels) {
				text(x=(xmi+xma)/2,y=(ymi+yma)/2,labels=i,cex=labels.cex*10/MN,col=labels.col)				
			}
			i = i + 1
		}
	}
	
	#for (n in 1:(MN-1)) {lines(x=c(n/MN,n/MN),y=c(0,1),col="grey")}
	#for (m in 1:(MN-1)) {lines(x=c(0,1),y=c(m/MN,m/MN),col="black")}
	rect(0,0,1,1,lwd=2)
	
	#dev.off()
	
}

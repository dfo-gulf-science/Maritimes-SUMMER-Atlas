## length frequency plot, separately for 4X and 4VW

figure15.fct <- function(list.in, cex.in, pos.ylabel=c(0,0)) {

# list.in <- list(df.1=read.csv("C:/Users/RicardD/Documents/GitHub/FishInverAtlas_Ricard/Figures-Data/SS12_lf4x.csv", header=TRUE), df.2=read.csv("C:/Users/RicardD/Documents/GitHub/FishInverAtlas_Ricard/Figures-Data/SS12_lf4vw.csv", header=TRUE))

xx <- rbind(list.in[[1]][,1],list.in[[2]][,1])
yy <- rbind(list.in[[1]][,2:7],list.in[[2]][,2:7])

	x.range <- c(0,range(xx)[2])
	y.range <- c(0,range(yy)[2])


## NAFO 4X
dat.in <- list.in[[1]]
## smooth the density first, it makes the plot look better
  #add 2 lines at the end for the smoother to work
  ncollf <- ncol(dat.in)
  maxlf=max(dat.in[,1])
  incrLong=dat.in[2,1]-dat.in[1,1]
  lfmax1=c(maxlf+incrLong,rep(0,ncollf-1))
  lfmax2=c(maxlf+2*incrLong,rep(0,ncollf-1))
  lfmax3=c(maxlf+3*incrLong,rep(0,ncollf-1))
  dat.in<-rbind(dat.in,lfmax1,lfmax2,lfmax3)

  ## rollmean
  	moy.mob=3
		for(i in 2:ncol(dat.in)){
			dat.in[,i]=rollmean(dat.in[,i],moy.mob,na.pad=TRUE)	
		}
		dat.in=dat.in[-c(1,nrow(dat.in)),]
	
	##
	# y.range <- c(0,max(dat.in[,2:7]))

	xlabel = "Length (cm) / Longueur (cm)"
	ylabel2 = "Frequ\u{E9}nce par trait"	
	ylabel1 = "Frequency per tow"
	#leg.label = c("1970 - 1974", "1975 - 1979", "1980 - 1984", "1985 - 1989", "1990 - 1994", "1995 - 1999", "2000 - 2004", "2005 - 2009", "2010 - 2014")
	leg.label = c("1970 - 1977", "1978 - 1985", "1986 - 1993", "1994 - 2001", "2002 - 2009", "2010 - 2020")
	
	#my.cols <- c("#000000","#FFEDA0","#FED976","#FEB24C","#FD8D3C","#FC4E2A","#E31A1C","#BD0026","#800026") # obtained from color brewer
	my.cols <- c("#000000","#FED976","#FEB24C","#FD8D3C","#F03B20","#BD0026","#5c0012") # obtained from color brewer
#FFFFCC
	
	plot(dat.in[,1], dat.in[,2], col=my.cols[1], axes=FALSE, xlim=x.range, ylim=y.range, type='l', lwd=2, ann=FALSE)
	lines(dat.in[,1], dat.in[,3], col=my.cols[2], lwd=2)
	lines(dat.in[,1], dat.in[,4], col=my.cols[3], lwd=2)
	lines(dat.in[,1], dat.in[,5], col=my.cols[4], lwd=2)
	lines(dat.in[,1], dat.in[,6], col=my.cols[5], lwd=2)
	lines(dat.in[,1], dat.in[,7], col=my.cols[6], lwd=2)
#	lines(dat.in[,1], dat.in[,8], col=my.cols[7], lwd=2)
#	lines(dat.in[,1], dat.in[,9], col=my.cols[8], lwd=2)	
#	lines(dat.in[,1], dat.in[,10], col=my.cols[9], lwd=2)	
	
	# labels and such
	pretty.x <- pretty(x.range)
	pretty.y  <- pretty(y.range)
	
	axis(side=1, at = pretty.x, cex.axis=cex.in$axis, labels=TRUE, tcl=-0.2, las=0, mgp=c(0,0.45,0))
	axis(side=1, seq(min(pretty.x), max(pretty.x), by=((pretty.x[2]-pretty.x[1])/2)), labels=F, tck = -0.015)
	axis(side=2, at = pretty.y, cex.axis=cex.in$axis, labels=TRUE, tcl=-0.15, las=1, mgp=c(0,0.3,0))
	axis(side=2, seq(min(pretty.y), max(pretty.y), by=((pretty.y[2]-pretty.y[1])/2)), labels=F, tck = -0.01)
	
	# Affichage du titre et des axes	
	mtext(xlabel, side = 1, line = 1.5, cex=cex.in$labels)
	mtext(ylabel1, side = 2, line = 3.5+pos.ylabel[2], cex=cex.in$labels)
	mtext(ylabel2, side = 2, line = 2.3+pos.ylabel[2], cex=cex.in$labels)
	

	# legend
	legend("top", bty="n", "4X")	

## NAFO 4VW
dat.in <- list.in[[2]]
## smooth the density first, it makes the plot look better
  #add 2 lines at the end for the smoother to work
  ncollf <- ncol(dat.in)
  maxlf=max(dat.in[,1])
  incrLong=dat.in[2,1]-dat.in[1,1]
  lfmax1=c(maxlf+incrLong,rep(0,ncollf-1))
  lfmax2=c(maxlf+2*incrLong,rep(0,ncollf-1))
  lfmax3=c(maxlf+3*incrLong,rep(0,ncollf-1))
  dat.in<-rbind(dat.in,lfmax1,lfmax2,lfmax3)

  ## rollmean
  	moy.mob=3
		for(i in 2:ncol(dat.in)){
			dat.in[,i]=rollmean(dat.in[,i],moy.mob,na.pad=TRUE)	
		}
		dat.in=dat.in[-c(1,nrow(dat.in)),]

	xlabel = "Length (cm) / Longueur (cm)"
	ylabel2 = "Frequ\u{E9}nce par trait"	
	ylabel1 = "Frequency per tow"
	#leg.label = c("1970 - 1974", "1975 - 1979", "1980 - 1984", "1985 - 1989", "1990 - 1994", "1995 - 1999", "2000 - 2004", "2005 - 2009", "2010 - 2014")
	
	plot(dat.in[,1], dat.in[,2], col=my.cols[1], axes=FALSE, xlim=x.range, ylim=y.range, type='l', lwd=2, ann=FALSE)
	lines(dat.in[,1], dat.in[,3], col=my.cols[2], lwd=2)
	lines(dat.in[,1], dat.in[,4], col=my.cols[3], lwd=2)
	lines(dat.in[,1], dat.in[,5], col=my.cols[4], lwd=2)
	lines(dat.in[,1], dat.in[,6], col=my.cols[5], lwd=2)
	lines(dat.in[,1], dat.in[,7], col=my.cols[6], lwd=2)
	#lines(dat.in[,1], dat.in[,8], col=my.cols[7], lwd=2)
	#lines(dat.in[,1], dat.in[,9], col=my.cols[8], lwd=2)	
	#lines(dat.in[,1], dat.in[,10], col=my.cols[9], lwd=2)	
	
	# labels and such
	pretty.x <- pretty(x.range)
	pretty.y  <- pretty(y.range)
	
	axis(side=1, at = pretty.x, cex.axis=cex.in$axis, labels=TRUE, tcl=-0.2, las=0, mgp=c(0,0.45,0))
	axis(side=1, seq(min(pretty.x), max(pretty.x), by=((pretty.x[2]-pretty.x[1])/2)), labels=F, tck = -0.015)
	axis(side=2, at = pretty.y, cex.axis=cex.in$axis, labels=TRUE, tcl=-0.15, las=1, mgp=c(0,0.3,0))
	axis(side=2, seq(min(pretty.y), max(pretty.y), by=((pretty.y[2]-pretty.y[1])/2)), labels=F, tck = -0.01)
	
	# Affichage du titre et des axes	
	mtext(xlabel, side = 1, line = 1.5, cex=cex.in$labels)
#	mtext(ylabel1, side = 2, line = 1.75+pos.ylabel[2], cex=cex.in$labels)
#	mtext(ylabel2, side = 2, line = 1+pos.ylabel[2], cex=cex.in$labels)
	

	# legend
	legend("topright", bty="n", leg.label, col = my.cols, lwd=2, text.col = "Black", lty=1)	
	legend("top", bty="n", "4VW")
	
} # end function

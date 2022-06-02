## length frequency plot, separately for 4X and 4VW

lengthfreqNAFO.fct <- function(spec.code) {
  fn <- file.path(figdata.path, paste0("SS",spec.code,"_lf4x.csv"))
  lf.4x <- read.csv(fn, header=TRUE)
  fn <- file.path(figdata.path, paste0("SS",spec.code,"_lf4vw.csv"))
  lf.4vw <- read.csv(fn, header=TRUE)
  list.in <- list(df.1=lf.4x, df.2=lf.4vw)
  
xx <- rbind(list.in[[1]][,1],list.in[[2]][,1])
yy <- rbind(list.in[[1]][,2:7],list.in[[2]][,2:7])

	x.range <- c(0,range(xx)[2])
	y.range <- c(0,range(yy)[2])


## NAFO 4X
dat.in.4x <- list.in[[1]]
## smooth the density first, it makes the plot look better
  #add 2 lines at the end for the smoother to work
  ncollf <- ncol(dat.in.4x)
  maxlf=max(dat.in.4x[,1])
  incrLong=dat.in.4x[2,1]-dat.in.4x[1,1]
  lfmax1=c(maxlf+incrLong,rep(0,ncollf-1))
  lfmax2=c(maxlf+2*incrLong,rep(0,ncollf-1))
  lfmax3=c(maxlf+3*incrLong,rep(0,ncollf-1))
  dat.in.4x<-rbind(dat.in.4x,lfmax1,lfmax2,lfmax3)

  ## rollmean
  	moy.mob=3
		for(i in 2:ncol(dat.in.4x)){
			dat.in.4x[,i]=rollmean(dat.in.4x[,i],moy.mob,na.pad=TRUE)	
		}
		dat.in.4x=dat.in.4x[-c(1,nrow(dat.in.4x)),]
	
	##

## NAFO 4VW
dat.in.4vw <- list.in[[2]]
## smooth the density first, it makes the plot look better
  #add 2 lines at the end for the smoother to work
  ncollf <- ncol(dat.in.4vw)
  maxlf=max(dat.in.4vw[,1])
  incrLong=dat.in.4vw[2,1]-dat.in.4vw[1,1]
  lfmax1=c(maxlf+incrLong,rep(0,ncollf-1))
  lfmax2=c(maxlf+2*incrLong,rep(0,ncollf-1))
  lfmax3=c(maxlf+3*incrLong,rep(0,ncollf-1))
  dat.in.4vw<-rbind(dat.in.4vw,lfmax1,lfmax2,lfmax3)

  ## rollmean
  	moy.mob=3
		for(i in 2:ncol(dat.in.4vw)){
			dat.in.4vw[,i]=rollmean(dat.in.4vw[,i],moy.mob,na.pad=TRUE)	
		}
		dat.in.4vw=dat.in.4vw[-c(1,nrow(dat.in.4vw)),]

		y.range <- c(0,max(dat.in.4x[,2:7], dat.in.4vw[,2:7])*1.05)
		
		xlabel = "Length (cm)" # / Longueur (cm)
		ylabel1 = "Frequency per tow"
		leg.label = c("1970 - 1977", "1978 - 1985", "1986 - 1993", "1994 - 2001", "2002 - 2009", "2010 - 2020")
		
		#my.cols <- c("#000000","#FFEDA0","#FED976","#FEB24C","#FD8D3C","#FC4E2A","#E31A1C","#BD0026","#800026") # obtained from color brewer
		my.cols <- c("#000000","#FED976","#FEB24C","#FD8D3C","#F03B20","#BD0026","#5c0012") # obtained from color brewer
		#FFFFCC
		par(mai=c(0.5, 0.75, 0.23, 0.1))
		
		plot(dat.in.4x[,1], dat.in.4x[,2], col=my.cols[1], axes=FALSE, xlim=x.range, ylim=y.range, type='l', lwd=2, ann=FALSE)
		lines(dat.in.4x[,1], dat.in.4x[,3], col=my.cols[2], lwd=2)
		lines(dat.in.4x[,1], dat.in.4x[,4], col=my.cols[3], lwd=2)
		lines(dat.in.4x[,1], dat.in.4x[,5], col=my.cols[4], lwd=2)
		lines(dat.in.4x[,1], dat.in.4x[,6], col=my.cols[5], lwd=2)
		lines(dat.in.4x[,1], dat.in.4x[,7], col=my.cols[6], lwd=2)
		#	lines(dat.in.4x[,1], dat.in.4x[,8], col=my.cols[7], lwd=2)
		#	lines(dat.in.4x[,1], dat.in.4x[,9], col=my.cols[8], lwd=2)	
		#	lines(dat.in.4x[,1], dat.in.4x[,10], col=my.cols[9], lwd=2)	
		
		# labels and such
		pretty.x <- pretty(x.range)
		pretty.y  <- pretty(y.range)
		
		axis(side=1, at = pretty.x, cex.axis=1, labels=TRUE, tcl=-0.2, las=0, mgp=c(0,0.45,0))
		axis(side=1, seq(min(pretty.x), max(pretty.x), by=((pretty.x[2]-pretty.x[1])/2)), labels=F, tck = -0.015)
		axis(side=2, at = pretty.y, cex.axis=1, labels=TRUE, tcl=-0.15, las=1, mgp=c(0,0.3,0))
		axis(side=2, seq(min(pretty.y), max(pretty.y), by=((pretty.y[2]-pretty.y[1])/2)), labels=F, tck = -0.01)
		
		# Affichage du titre et des axes	
		mtext(xlabel, side = 1, line = 1.5, cex=1)
		mtext(ylabel1, side = 2, line = 2.5, cex=1)
		# legend
		legend("top", bty="n", "4X", cex=1.2)	
		
		box()
		
		
		xlabel = "Length (cm)" # / Longueur (cm)
	#ylabel2 = "Frequ\u{E9}nce par trait"	
	ylabel1 = "Frequency per tow"
	#leg.label = c("1970 - 1974", "1975 - 1979", "1980 - 1984", "1985 - 1989", "1990 - 1994", "1995 - 1999", "2000 - 2004", "2005 - 2009", "2010 - 2014")
	par(mai=c(0.5, 0.5, 0.23, 0.1))
	
	plot(dat.in.4vw[,1], dat.in.4vw[,2], col=my.cols[1], axes=FALSE, xlim=x.range, ylim=y.range, type='l', lwd=2, ann=FALSE)
	lines(dat.in.4vw[,1], dat.in.4vw[,3], col=my.cols[2], lwd=2)
	lines(dat.in.4vw[,1], dat.in.4vw[,4], col=my.cols[3], lwd=2)
	lines(dat.in.4vw[,1], dat.in.4vw[,5], col=my.cols[4], lwd=2)
	lines(dat.in.4vw[,1], dat.in.4vw[,6], col=my.cols[5], lwd=2)
	lines(dat.in.4vw[,1], dat.in.4vw[,7], col=my.cols[6], lwd=2)
	#lines(dat.in.4vw[,1], dat.in.4vw[,8], col=my.cols[7], lwd=2)
	#lines(dat.in.4vw[,1], dat.in.4vw[,9], col=my.cols[8], lwd=2)	
	#lines(dat.in.4vw[,1], dat.in.4vw[,10], col=my.cols[9], lwd=2)	
	
	# labels and such
	pretty.x <- pretty(x.range)
	pretty.y  <- pretty(y.range)
	
	axis(side=1, at = pretty.x, cex.axis=1, labels=TRUE, tcl=-0.2, las=0, mgp=c(0,0.45,0))
	axis(side=1, seq(min(pretty.x), max(pretty.x), by=((pretty.x[2]-pretty.x[1])/2)), labels=F, tck = -0.015)
	axis(side=2, at = pretty.y, cex.axis=1, labels=TRUE, tcl=-0.15, las=1, mgp=c(0,0.3,0))
	axis(side=2, seq(min(pretty.y), max(pretty.y), by=((pretty.y[2]-pretty.y[1])/2)), labels=F, tck = -0.01)
	
	# Affichage du titre et des axes	
	mtext(xlabel, side = 1, line = 1.5, cex=1)
	
	# legend
	legend("topright", bty="n", leg.label, col = my.cols, lwd=2, text.col = "Black", lty=1)	
	legend("top", bty="n", "4VW", cex=1.2)
	
	box()
	
} # end function

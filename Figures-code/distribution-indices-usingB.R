## distribution indices timeseries plots, using biomass

figure18.fct <- function(dat.in, cex.in, pos.ylabel=c(0,0), which.measure=c('areaocc','D','Gini')) {
# dat.in <- read.csv(file.path(figdata.path,"SS10_distribution-usingbiomass.csv"))

	xlabel = "Year / Ann\u{E9}e"

	my.cols <- colorRampPalette(c('blue','red'))(dim(dat.in)[1])
	yr.cols <- my.cols[dat.in$year-min(dat.in$year)+1]

	if('areaocc' %in% which.measure){

	## area occupied
a.occ <- dat.in[,c(1,2)]

# axes and labels
	ylabel2 = expression(paste("Aire d'occupation (", 10^3, " ", km^2,")", sep=""))	
	ylabel1 = expression(paste("Area of occupancy (", 10^3, " ", km^2,")", sep=""))
	
	# labels and such
	x.range <- range(a.occ$year)
	pretty.x <- pretty(x.range)
	y.range  <- c(range(a.occ$area.occupied)[1]*0.5, range(a.occ$area.occupied)[2]*1.5)
	pretty.y  <- pretty(y.range)
	
	loess.area <- loess(area.occupied~year, data=a.occ)
	area.loess.df <- data.frame(year=a.occ$year, pred=predict(loess.area))
	

	plot(area.occupied~year, data=a.occ, type='b', axes=FALSE, ann=FALSE, pch=19, xlim=x.range, ylim=y.range)
	
	axis(side=1, at = pretty.x, cex.axis=cex.in$axis, labels=TRUE, tcl=-0.2, las=0, mgp=c(0,0.2,0))
	axis(side=1, seq(min(pretty.x), max(pretty.x), by=((pretty.x[2]-pretty.x[1])/2)), labels=F, tck = -0.015)
	axis(side=2, at = pretty.y, cex.axis=cex.in$axis, labels=TRUE, tcl=-0.15, las=1, mgp=c(0,0.2,0))
	axis(side=2, seq(min(pretty.y), max(pretty.y), by=((pretty.y[2]-pretty.y[1])/2)), labels=F, tck = -0.01)
	
	lines(area.loess.df, col='red',lwd=2.5)
	
	# Affichage du titre et des axes	
	mtext(xlabel, side = 1, line = 1.5, cex=cex.in$labels)
	mtext(ylabel1, side = 2, line = 2.4+pos.ylabel[2], cex=cex.in$labels)
	mtext(ylabel2, side = 2, line = 1.2+pos.ylabel[2], cex=cex.in$labels)
}	


if('D' %in% which.measure){
## D50, D75 and D95
	ylabel2 = expression(paste("\u{C9}tendue g\u{E9}ographique  (", 10^3, " ", km^2,")", sep=""))	
	ylabel1 = expression(paste("Geographic range (", 10^3, " ", km^2,")", sep=""))

d.perc <- dat.in[,c(1,3,4)]

	#if(min(d.perc$year)<1985){	x.range <- c(range(d.perc$year)[1],range(d.perc$year)[2]+4)}
	#if(min(d.perc$year)>=1985){	x.range <- c(range(d.perc$year)[1],range(d.perc$year)[2]+2)}
	x.range <- range(d.perc$year)
	pretty.x <- pretty(x.range)
	y.range <- c(0,range(d.perc$D95)[2]*1.25)
	pretty.y  <- pretty(y.range)

	loess.D75 <- loess(D75~year, data=d.perc)
	D75.loess.df <- data.frame(year=d.perc$year, pred=predict(loess.D75))
	loess.D95 <- loess(D95~year, data=d.perc)
	D95.loess.df <- data.frame(year=d.perc$year, pred=predict(loess.D95))
	
	plot(D75~year, data=d.perc, type='n', axes=FALSE, ann=FALSE, pch=1, xlim=x.range, ylim=y.range)
	ll <- dim(d.perc)[1]
	sapply(1:(ll-1), function(i){segments(d.perc$year[i], d.perc$D75[i], d.perc$year[i+1], d.perc$D75[i+1], col=yr.cols[i], lty=1, lwd=0.15)})
	sapply(1:(ll-1), function(i){segments(d.perc$year[i], d.perc$D95[i], d.perc$year[i+1], d.perc$D95[i+1], col=yr.cols[i], lty=1, lwd=0.15)})
	
	points(D75~year, data=d.perc, type='p', pch=20, col=yr.cols)
	points(D95~year, data=d.perc, type='p', pch=20, col=yr.cols)

	lines(D75.loess.df, col='red',lwd=2.5)
	lines(D95.loess.df, col='red',lwd=2.5)

	axis(side=1, at = pretty.x, cex.axis=cex.in$axis, labels=TRUE, tcl=-0.2, las=0, mgp=c(0,0.4,0))
	axis(side=1, seq(min(pretty.x), max(pretty.x), by=((pretty.x[2]-pretty.x[1])/2)), labels=F, tck = -0.015)
	axis(side=2, at = pretty.y, cex.axis=cex.in$axis, labels=TRUE, tcl=-0.15, las=1, mgp=c(0,0.4,0))
	axis(side=2, seq(min(pretty.y), max(pretty.y), by=((pretty.y[2]-pretty.y[1])/2)), labels=F, tck = -0.01)

	mtext(xlabel, side = 1, line = 1.5, cex=cex.in$labels)
	mtext(ylabel1, side = 2, line = 3.0+pos.ylabel[2], cex=cex.in$labels)
	mtext(ylabel2, side = 2, line = 1.8+pos.ylabel[2], cex=cex.in$labels)
	
	ii <- length(d.perc$D75)
	if(min(d.perc$year)<1985){ 
		text(max(d.perc$year)+4, D75.loess.df$pred[ii], "D75%", cex=cex.in$labels, col='red')
		text(max(d.perc$year)+4, D95.loess.df$pred[ii], "D95%", cex=cex.in$labels, col='red')
		}
	if(min(d.perc$year)>=1985){ 
		text(max(d.perc$year)+2, D75.loess.df$pred[ii], "D75%", cex=cex.in$labels, col='red')
		text(max(d.perc$year)+2, D95.loess.df$pred[ii], "D95%", cex=cex.in$labels, col='red')
		}
		

}
if('Gini' %in% which.measure){

	## Gini index
	ylabel2 = "Index Gini"
	ylabel1 = "Gini index"

gini <- dat.in[,c(1,5)]
	x.range <- range(gini$year)
	pretty.x <- pretty(x.range)
	y.range <- c(range(gini$Gini)[1]*0.5, 1)
	pretty.y  <- pretty(y.range)

	loess.Gini <- loess(Gini~year, data=gini)
	Gini.loess.df <- data.frame(year=gini$year, pred=predict(loess.Gini))

	plot(Gini~year, data=gini, type='b', axes=FALSE, ann=FALSE, pch=19, xlim=x.range, ylim=y.range)
	lines(Gini.loess.df, col='red',lwd=2.5)
	
	axis(side=1, at = pretty.x, cex.axis=cex.in$axis, labels=TRUE, tcl=-0.2, las=0, mgp=c(0,0.2,0))
	axis(side=1, seq(min(pretty.x), max(pretty.x), by=((pretty.x[2]-pretty.x[1])/2)), labels=F, tck = -0.015)
	axis(side=2, at = pretty.y, cex.axis=cex.in$axis, labels=TRUE, tcl=-0.15, las=1, mgp=c(0,0.2,0))
	axis(side=2, seq(min(pretty.y), max(pretty.y), by=((pretty.y[2]-pretty.y[1])/2)), labels=F, tck = -0.01)

	mtext(xlabel, side = 1, line = 1.5, cex=cex.in$labels)
	mtext(ylabel1, side = 2, line = 2.4+pos.ylabel[2], cex=cex.in$labels)
	mtext(ylabel2, side = 2, line = 1.2+pos.ylabel[2], cex=cex.in$labels)

}
} # end function

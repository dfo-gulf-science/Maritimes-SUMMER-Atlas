## stratified random estimates plots, biomass only

figure14.fct <- function(dat.in, cex.in, pos.ylabel=c(0,0)) {
# dat.in <- read.csv("C:/ATLAS_poissons_SS/Data/SS10_stratified.csv")


#################
## biomass
	x.range <- range(dat.in$year)
	pretty.x <- pretty(x.range)
	y.range  <- c(0, range(dat.in$b+sqrt(dat.in$b.var))[2]*1.05)
	pretty.y  <- pretty(y.range)


	# colour code the years to identify the beginning and end of the time-series
	my.cols <- colorRampPalette(c('blue','red'))(dim(dat.in)[1])
	yr.cols <- my.cols[dat.in$year-min(dat.in$year)+1]

plot(dat.in[,1], dat.in[,4], type='n', axes=FALSE, ann=FALSE, pch=15, lty=1, xlim=x.range, ylim=y.range)

ll <- dim(dat.in)[1]
sapply(1:(ll-1), function(i){segments(dat.in[i,1], dat.in[i,4], dat.in[i+1,1], dat.in[i+1,4], col=yr.cols[i], lty=1, lwd=0.15)})

points(dat.in[,1], dat.in[,4], pch=20, col=yr.cols)

mm <- mean(dat.in[,4])
nn <- length(dat.in[,1])
lines(dat.in[,1], rep(mm,nn), lty=1, col=grey(0.7), lwd=1.5)

#abline(h=mm, lty=1, col=grey(0.7), lwd=1.5)

stdev <- sqrt(var(dat.in[,4]))
lb <- mm - (0.5*stdev)
ub <- mm + (0.5*stdev)
lines(dat.in[,1], rep(lb,nn), lty=2, col=grey(0.7), lwd=1.5)
lines(dat.in[,1], rep(ub,nn), lty=2, col=grey(0.7), lwd=1.5)
#abline(h=lb, lty=2, col=grey(0.7), lwd=1.5)
#abline(h=ub, lty=2, col=grey(0.7), lwd=1.5)

## variance
lines(dat.in[,1], dat.in[,4] + sqrt(dat.in[,5]), lty=1, col=grey(0.7), lwd=1.5)
yy <- dat.in[,4] - sqrt(dat.in[,5])
yy[yy<=0] <- 0 # if the mean minus the standard error is negative, set to zero
lines(dat.in[,1], yy, lty=1, col=grey(0.7), lwd=1.5)

# axes and labels
	xlabel = "Year / Ann\u{E9}e"
	ylabel2 = "Poids moyen (kg) par trait"	
	ylabel1 = "Mean weight (kg) per tow"

	# labels and such
	
	axis(side=1, at = pretty.x, cex.axis=cex.in$axis, labels=TRUE, tcl=-0.2, las=0, mgp=c(0,0.4,0))
	axis(side=1, seq(min(pretty.x), max(pretty.x), by=((pretty.x[2]-pretty.x[1])/2)), labels=F, tck = -0.015)
	axis(side=2, at = pretty.y, cex.axis=cex.in$axis, labels=TRUE, tcl=-0.15, las=1, mgp=c(0,0.4,0))
	axis(side=2, seq(min(pretty.y), max(pretty.y), by=((pretty.y[2]-pretty.y[1])/2)), labels=F, tck = -0.01)
	
	# Affichage du titre et des axes	
	mtext(xlabel, side = 1, line = 1.5, cex=cex.in$labels)
	mtext(ylabel1, side = 2, line = 3.2+pos.ylabel[2], cex=cex.in$labels)
	mtext(ylabel2, side = 2, line = 2.0+pos.ylabel[2], cex=cex.in$labels)

} # end function

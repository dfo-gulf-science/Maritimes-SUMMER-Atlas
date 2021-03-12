## bottom salinity habitat preference
#SalinityDist
figure4.fct <- function(dat.in, cex.in, pos.ylabel=c(0,0)) {

# trim the incoming data frame to remove 0% and 100%
zero.catch <- which(dat.in$cum.catch == 0)
zero.all <- which(dat.in$cum == 0)
hundred.catch <- which(dat.in$cum.catch == 100)
hundred.all <- which(dat.in$cum == 100)
lb <- min(max(zero.catch), max(zero.all))-1
ub <- max(min(hundred.catch), min(hundred.all))+2
dat.in <- dat.in[lb:ub,]

plot(dat.in[,1], dat.in[,2], axes=FALSE, ann=FALSE, lty=2, lwd=1.5, type='l')
lines(dat.in[,1], dat.in[,3], lty=1, lwd=2, type='l')

# axes and labels
	xlabel = "Salinity (PSU) / Salinit\u{E9} (UPS)"
	ylabel2 = "Fr\u{E9}quence cumul\u{E9}e (%)"	
	ylabel1 = "Cumulative frequency (%)"
	leg.label = c("Catch / Capture", "Salinity / Salinit\u{E9}")
	
	# labels and such
	x.range <- range(dat.in$salinity)
	pretty.x <- pretty(x.range)
	y.range  <- range(dat.in$cum)
	pretty.y  <- pretty(y.range)
	
	axis(side=1, at = pretty.x, cex.axis=cex.in$axis, labels=TRUE, tcl=-0.2, las=0, mgp=c(0,0.2,0))
	axis(side=1, seq(min(pretty.x), max(pretty.x), by=((pretty.x[2]-pretty.x[1])/2)), labels=F, tck = -0.015)
	axis(side=2, at = pretty.y, cex.axis=cex.in$axis, labels=TRUE, tcl=-0.15, las=1, mgp=c(0,0.2,0))
	axis(side=2, seq(min(pretty.y), max(pretty.y), by=((pretty.y[2]-pretty.y[1])/2)), labels=F, tck = -0.01)
	
	# Affichage du titre et des axes	
	mtext(xlabel, side = 1, line = 1.5, cex=cex.in$labels)
	mtext(ylabel1, side = 2, line = 2.4+pos.ylabel[2], cex=cex.in$labels)
	mtext(ylabel2, side = 2, line = 1.2+pos.ylabel[2], cex=cex.in$labels)

	legend("bottomright", bty="n", leg.label, col = c("Black",grey(0.5)), lwd=2, 
	      		text.col = "Black", lty = c(1, 2), cex=cex.in$legend)


} # end function

## depth habitat preference

depthpref.fct <- function(spec.code) {

  fn <- file.path(figdata.path, paste0("SS",spec.code,"_depthdist.csv"))
  dat.in <- read.csv(fn, header=TRUE)
  
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
	xlabel = "Depth (m)"
	ylabel1 = "Cumulative frequency (%)"
	leg.label = c("Catch", "Depth")
	
	# labels and such
	x.range <- range(dat.in$depth)
	pretty.x <- pretty(x.range)
	y.range  <- range(dat.in$cum)
	pretty.y  <- pretty(y.range)
	
	axis(side=1, at = pretty.x, cex.axis=1, labels=TRUE, tcl=-0.2, las=0, mgp=c(0,0.2,0))
	axis(side=1, seq(min(pretty.x), max(pretty.x), by=((pretty.x[2]-pretty.x[1])/2)), labels=F, tck = -0.015)
	axis(side=2, at = pretty.y, cex.axis=1, labels=TRUE, tcl=-0.15, las=1, mgp=c(0,0.2,0))
	axis(side=2, seq(min(pretty.y), max(pretty.y), by=((pretty.y[2]-pretty.y[1])/2)), labels=F, tck = -0.01)
	
	# Affichage du titre et des axes	
	mtext(xlabel, side = 1, line = 1.5, cex=1)
	mtext(ylabel1, side = 2, line = 2.5, cex=1)

	legend("bottomright", bty="n", leg.label, col = c("Black",grey(0.5)), lwd=2, 
	      		text.col = "Black", lty = c(1, 2), cex=1)
	
	box()
} # end function

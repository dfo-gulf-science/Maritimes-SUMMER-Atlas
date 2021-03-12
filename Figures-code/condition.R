## average fish condition plot
#Condition
figure9.fct <- function(dat.in, cex.in, pos.ylabel=c(0,0)) {
#dat.in <- read.csv("C:\\Documents and Settings\\RicardD\\My Documents\\Dropbox\\ATLAS_poissons_SS\\Data\\SS10_lw.csv", header=TRUE)
#dat.in <- read.csv("D:\\Dropbox\\ATLAS_poissons_SS\\Data\\SS10_lw.csv", header=TRUE)

# remove entries where weight is null
dat.in <- subset(dat.in, !is.na(FWT) & condition <= 2.0)

# median length
l.50 <- quantile(dat.in$FLEN)[3]

yearly.cond <- tapply(dat.in$condition, dat.in$YEAR, mean)
yearly.cond.lower50 <- tapply(subset(dat.in, FLEN<l.50)$condition, subset(dat.in, FLEN<l.50)$YEAR, mean)
yearly.cond.upper50 <- tapply(subset(dat.in, FLEN>=l.50)$condition, subset(dat.in, FLEN>=l.50)$YEAR, mean)

# axes and labels
	xlabel = "Year / Ann\u{E9}e"
	ylabel2 = "Condition moyenne"	
	ylabel1 = "Mean condition"
	
	# labels and such
	x.range <- range(as.numeric(names(yearly.cond)))
	pretty.x <- pretty(x.range)
	y.range  <- range(c(yearly.cond,yearly.cond.lower50,yearly.cond.upper50)) # c(0.9,1.1) 
	#y.range  <- range(dat.in$condition)
	pretty.y  <- pretty(y.range)


	# we either have a species with data for the whole time or with missing data for the Gavaris years
	data.1990 <- length(dat.in[dat.in$YEAR==1990,]$FSHNO)
	
	if(data.1990){
	plot(as.numeric(names(yearly.cond)), yearly.cond, type='b', axes=FALSE, ann=FALSE, pch=19, xlim=x.range, ylim=y.range)
	nn <- length(names(yearly.cond))
	lines(as.numeric(names(yearly.cond)), rep(1,nn), lty=2, col=grey(0.7), lwd=1.5)
	
	lines(as.numeric(names(yearly.cond.lower50)), yearly.cond.lower50, lty=1, col=grey(0.7), lwd=1.0)
	lines(as.numeric(names(yearly.cond.upper50)), yearly.cond.upper50, lty=1, col=grey(0.7), lwd=2)
	
	}
	if(!data.1990) {
	ii.early <- as.numeric(names(yearly.cond))<1990
	ii.late <- as.numeric(names(yearly.cond))>1990
	nn <- length(names(yearly.cond))
	plot(as.numeric(names(yearly.cond[ii.early])), yearly.cond[ii.early], type='b', axes=FALSE, ann=FALSE, pch=19, xlim=x.range, ylim=y.range)
	lines(as.numeric(names(yearly.cond)), rep(1,nn), lty=2, col=grey(0.7), lwd=1.5)
	lines(as.numeric(names(yearly.cond[ii.late])), yearly.cond[ii.late], type='b', pch=19)
	
	ii.early <- as.numeric(names(yearly.cond.lower50))<1990
	ii.late <- as.numeric(names(yearly.cond.lower50))>1990

	lines(as.numeric(names(yearly.cond.lower50[ii.early])), yearly.cond.lower50[ii.early], lty=1, col=grey(0.7), lwd=1.0)
	lines(as.numeric(names(yearly.cond.lower50[ii.late])), yearly.cond.lower50[ii.late], lty=1, col=grey(0.7), lwd=1.0)

	ii.early <- as.numeric(names(yearly.cond.upper50))<1990
	ii.late <- as.numeric(names(yearly.cond.upper50))>1990
	
	lines(as.numeric(names(yearly.cond.upper50[ii.early])), yearly.cond.upper50[ii.early], lty=1, col=grey(0.7), lwd=2)
	lines(as.numeric(names(yearly.cond.upper50[ii.late])), yearly.cond.upper50[ii.late], lty=1, col=grey(0.7), lwd=2)
	
	}

	axis(side=1, at = pretty.x, cex.axis=cex.in$axis, labels=TRUE, tcl=-0.2, las=0, mgp=c(0,0.2,0))
	axis(side=1, seq(min(pretty.x), max(pretty.x), by=((pretty.x[2]-pretty.x[1])/2)), labels=F, tck = -0.015)
	axis(side=2, at = pretty.y, cex.axis=cex.in$axis, labels=TRUE, tcl=-0.15, las=1, mgp=c(0,0.2,0), line=1)
	axis(side=2, seq(min(pretty.y), max(pretty.y), by=((pretty.y[2]-pretty.y[1])/2)), labels=F, tck = -0.01, line=1)
	
	# Affichage du titre et des axes	
	mtext(xlabel, side = 1, line = 1, cex=cex.in$labels)
	mtext(ylabel1, side = 2, line = 3.25+pos.ylabel[2], cex=cex.in$labels)
	mtext(ylabel2, side = 2, line = 2.5+pos.ylabel[2], cex=cex.in$labels)

} # end function


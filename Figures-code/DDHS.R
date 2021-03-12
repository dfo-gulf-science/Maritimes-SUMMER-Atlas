## density-dependent habitat selection plots, beta values vs. local density

figure12.fct <- function(dat.in, cex.in, pos.ylabel=c(0,0), stratum.measure="mean") {
#dat.in <- read.csv("C:/ATLAS_poissons_SS/Data/SS10_DDHSslopes.csv")

	cut.50 <- quantile(dat.in$mean.n)[3]
	dat.in$abundance.high <- ifelse(dat.in$mean.n>=cut.50,1,0)

	cut.50 <- quantile(dat.in$strat.median.top25)[3]
	dat.in$abundance.high.mediantop25 <- ifelse(dat.in$strat.median.top25>=cut.50,1,0)
	
	## identify the strata that are within 2* mean standard error of the slope estimates
	avg.stderr <- mean(dat.in$slope.glm.poisson.stderr)
	dat.in$slope.zero <- ifelse(abs(dat.in$slope.glm.poisson) <= (2*avg.stderr),1,0)

	dat.in$high.suitability <- ifelse(dat.in$slope.zero==1 & dat.in$abundance.high==1,1,0)
	dat.in$high.suitability.mediantop25 <- ifelse(dat.in$slope.zero==1 & dat.in$abundance.high.mediantop25==1,1,0)

my.lm <- lm(slope.glm.poisson~mean.n, data=dat.in)
my.loess <- loess(slope.glm.poisson~mean.n, data=dat.in, degree=1)

my.lm.mediantop25 <- lm(slope.glm.poisson~strat.median.top25, data=dat.in)
my.loess.mediantop25 <- loess(slope.glm.poisson~strat.median.top25, data=dat.in, degree=1)

my.df <- data.frame(x=dat.in$mean.n, y=predict(my.loess), yy=predict(my.lm))
oo<-order(my.df$x)

my.df.mediantop25 <- data.frame(x=dat.in$strat.median.top25, y=predict(my.loess.mediantop25), yy=predict(my.lm.mediantop25))
oo.mediantop25<-order(my.df.mediantop25$x)

# axes and labels
	ylabel2 = "Pente SDDH Poisson"
	ylabel1 = "DDHS Poisson slope"
	
	
#plot(slope.glm.nb~mean.n, data=dat.in, type='n', axes=FALSE, ann=FALSE, xlim=x.range, ylim=y.range)
#text(x=dat.in$mean.n, y=dat.in$slope.glm.nb, labels=dat.in$stratum)
if(stratum.measure=="mean") {
	xlabel = "Mean stratum abundance / Abondance moyenne par strate"
	# labels and such
	x.range <- c(0,range(dat.in$mean.n)[2])
	pretty.x <- pretty(x.range)
	y.range  <- range(dat.in$slope.glm.nb)
	pretty.y  <- pretty(y.range)

plot(slope.glm.poisson~mean.n, data=dat.in, type='n', axes=FALSE, ann=FALSE, xlim=x.range, ylim=y.range)

lines(y~x, data=my.df[oo,], col='red', lwd=2)
lines(yy~x, data=my.df[oo,], lwd=1.5, lty=1, col=grey(0.5))

text(x=dat.in$mean.n, y=dat.in$slope.glm.poisson, labels=(dat.in$stratum-400), col=ifelse(dat.in$high.suitability==1,"red","black"), cex=ifelse(dat.in$high.suitability==1,1.25,1))

#text(x=dat.in$strat.quan95, y=dat.in$slope.glm.nb, labels=dat.in$stratum, cex=0.5)
#text(x=dat.in$strat.quan75, y=dat.in$slope.glm.nb, labels=dat.in$stratum, cex=0.75)


xx <- seq(0,range(dat.in$mean.n)[2],0.1)
lines(xx, rep(0,length(xx)), lty=2, lwd=0.75)

## draw the box that defines the high suitability strata
xx <- seq(cut.50,range(dat.in$mean.n)[2],0.1)
lines(xx, rep(2*avg.stderr,length(xx)), lty=2, lwd=0.75, col='red')
lines(xx, rep(-2*avg.stderr,length(xx)), lty=2, lwd=0.75, col='red')
lines(c(cut.50,cut.50), c(2*avg.stderr,-2*avg.stderr), lty=2, lwd=0.75, col='red')

	axis(side=1, at = pretty.x, cex.axis=cex.in$axis, labels=TRUE, tcl=-0.2, las=0, mgp=c(0,0.2,0))
	axis(side=1, seq(min(pretty.x), max(pretty.x), by=((pretty.x[2]-pretty.x[1])/2)), labels=F, tck = -0.015)
	axis(side=2, at = pretty.y, cex.axis=cex.in$axis, labels=TRUE, tcl=-0.15, las=1, mgp=c(0,0.2,0))
	axis(side=2, seq(min(pretty.y), max(pretty.y), by=((pretty.y[2]-pretty.y[1])/2)), labels=F, tck = -0.01)

		# Affichage du titre et des axes	
	mtext(xlabel, side = 1, line = 1.5, cex=cex.in$labels)
	mtext(ylabel1, side = 2, line = 3.5+pos.ylabel[2], cex=cex.in$labels)
	mtext(ylabel2, side = 2, line = 2.3+pos.ylabel[2], cex=cex.in$labels)
	}

if(stratum.measure=="mediantop25") {
	xlabel = "Median stratum abundance / Abondance m\u{E9}diane par strate"
	# labels and such
	x.range <- c(0,range(dat.in$strat.median.top25)[2])
	pretty.x <- pretty(x.range)
	y.range  <- range(dat.in$slope.glm.nb)
	pretty.y  <- pretty(y.range)

	plot(slope.glm.poisson~strat.median.top25, data=dat.in, type='n', axes=FALSE, ann=FALSE, xlim=x.range, ylim=y.range)

	lines(y~x, data=my.df.mediantop25[oo.mediantop25,], col='red', lwd=2)
	lines(yy~x, data=my.df.mediantop25[oo.mediantop25,], lwd=1.5, lty=1, col=grey(0.5))

	text(x=dat.in$strat.median.top25, y=dat.in$slope.glm.poisson, labels=(dat.in$stratum-400), col=ifelse(dat.in$high.suitability.mediantop25==1,"red","black"), cex=ifelse(dat.in$high.suitability.mediantop25==1,1.25,0.75))


xx <- seq(0,range(dat.in$strat.median.top25)[2],0.1)
lines(xx, rep(0,length(xx)), lty=2, lwd=0.75)

## draw the box that defines the high suitability strata
xx <- seq(cut.50,range(dat.in$strat.median.top25)[2],0.1)
lines(xx, rep(2*avg.stderr,length(xx)), lty=2, lwd=0.75, col='red')
lines(xx, rep(-2*avg.stderr,length(xx)), lty=2, lwd=0.75, col='red')
lines(c(cut.50,cut.50), c(2*avg.stderr,-2*avg.stderr), lty=2, lwd=0.75, col='red')

	axis(side=1, at = pretty.x, cex.axis=cex.in$axis, labels=TRUE, tcl=-0.2, las=0, mgp=c(0,0.2,0))
	axis(side=1, seq(min(pretty.x), max(pretty.x), by=((pretty.x[2]-pretty.x[1])/2)), labels=F, tck = -0.015)
	axis(side=2, at = pretty.y, cex.axis=cex.in$axis, labels=TRUE, tcl=-0.15, las=1, mgp=c(0,0.2,0))
	axis(side=2, seq(min(pretty.y), max(pretty.y), by=((pretty.y[2]-pretty.y[1])/2)), labels=F, tck = -0.01)

		# Affichage du titre et des axes	
	mtext(xlabel, side = 1, line = 1.5, cex=cex.in$labels)
	mtext(ylabel1, side = 2, line = 3.5+pos.ylabel[2], cex=cex.in$labels)
	mtext(ylabel2, side = 2, line = 2.3+pos.ylabel[2], cex=cex.in$labels)
	}
	

} # end function

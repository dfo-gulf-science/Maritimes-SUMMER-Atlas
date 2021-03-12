## plot distribution vs. abundance for the 3 distribution indices

figure19.fct <- function(dat.in, cex.in, pos.ylabel=c(0,0), which.measure=c('areaocc','D','Gini')) {
#abundance <- read.csv("C:\\ATLAS_poissons_SS\\Data\\SS10_stratified.csv")
#distribution <- read.csv("C:\\ATLAS_poissons_SS\\Data\\SS10_distribution.csv")

abundance <- dat.in[[1]]
distribution <- dat.in[[2]]

merged.df <- merge(abundance, distribution, "year")

	xlabel = "Mean biomass / Biomasse moyenne"
	xlabel1 = "Mean weight (kg) per tow"
	xlabel2 = "Poids moyen (kg) par trait"
	
if('areaocc' %in% which.measure){
###########################################
## AREA OCCUPIED

	ylabel2 = expression(paste("Aire d'occupation (", 10^3, " ", km^2,")", sep=""))	
	ylabel1 = expression(paste("Area of occupancy (", 10^3, " ", km^2,")", sep=""))

	x.range <- range(merged.df$b)
	pretty.x <- pretty(x.range)
	y.range  <- c(range(merged.df$area.occupied)[1]*0.5, range(merged.df$area.occupied)[2]*1.5)
	pretty.y  <- pretty(y.range)

	# colour code the years to identify the beginning and end of the time-series
	my.cols <- colorRampPalette(c('blue','red'))(dim(merged.df)[1])
	yr.cols <- my.cols[merged.df$year-min(merged.df$year)+1]
	
plot(area.occupied~b, data=merged.df, pch=20, axes=FALSE, ann=FALSE, xlim=x.range, ylim=y.range, col=yr.cols)

ll <- dim(merged.df)[1]
#lines(area.occupied~n, data=merged.df, type='l',lty=1,lwd=0.25,col=grey(0.7))
#segments(merged.df$n[1], merged.df$area.occupied[1], merged.df$n[2], merged.df$area.occupied[2], col=yr.cols[1], lty=1, lwd=0.25)
sapply(1:(ll-1), function(i){segments(merged.df$b[i], merged.df$area.occupied[i], merged.df$b[i+1], merged.df$area.occupied[i+1], col=yr.cols[i], lty=1, lwd=0.15)})


pearson.corr <- round(cor(merged.df$area.occupied, merged.df$b),3)
xx<-pretty.x[2]
yy<-pretty.y[6]
#text(xx,yy,paste("Pearson = ",pearson.corr,sep=""),cex=1.0)
legend('topright',paste("Pearson = ",pearson.corr,sep=""),cex=1.0,bty='n')

	axis(side=1, at = pretty.x, cex.axis=cex.in$axis, labels=TRUE, tcl=-0.2, las=0, mgp=c(0,-0.2,0))
	axis(side=1, seq(min(pretty.x), max(pretty.x), by=((pretty.x[2]-pretty.x[1])/2)), labels=F, tck = -0.015)
	axis(side=2, at = pretty.y, cex.axis=cex.in$axis, labels=TRUE, tcl=-0.15, las=1, mgp=c(0,0.2,0))
	axis(side=2, seq(min(pretty.y), max(pretty.y), by=((pretty.y[2]-pretty.y[1])/2)), labels=F, tck = -0.01)

	# Affichage du titre et des axes	
	mtext(xlabel, side = 1, line = 0.5, cex=cex.in$labels)
	mtext(ylabel1, side = 2, line = 1.75+pos.ylabel[2], cex=cex.in$labels)
	mtext(ylabel2, side = 2, line = 1+pos.ylabel[2], cex=cex.in$labels)
###########################################
}

if('D' %in% which.measure){
###########################################
## D75
	ylabel2 = expression(paste("\u{C9}tendue g\u{E9}ographique  (", 10^3, " ", km^2,")", sep=""))	
	ylabel1 = expression(paste("Geographic range (", 10^3, " ", km^2,")", sep=""))

	x.range <- range(merged.df$b)
	pretty.x <- pretty(x.range,n=4)
	y.range  <- c(range(merged.df$D75)[1]*0.5, range(merged.df$D75)[2]*1.1)
	pretty.y  <- pretty(y.range,n=4)

	# colour code the years to identify the beginning and end of the time-series
	yy <- seq(min(merged.df$year),max(merged.df$year))
	ll <- length(yy)

	my.cols <- colorRampPalette(c('blue','red'))(ll+1)
	yr.cols <- my.cols[merged.df$year-min(merged.df$year)+1]

	plot(D75~b, data=merged.df, pch=20, axes=FALSE, ann=FALSE, xlim=x.range, ylim=y.range, col=yr.cols)
	ll <- dim(merged.df)[1]
	sapply(1:(ll-1), function(i){segments(merged.df$b[i], merged.df$D75[i], merged.df$b[i+1], merged.df$D75[i+1], col=yr.cols[i], lty=1, lwd=0.15)})
	
	my.cor <- cor.test(merged.df$D75, merged.df$b)
	pearson.corr <- round(my.cor$estimate,3)
	pearson.pvalue <- round(my.cor$p.value,3)
	pearson.confint.low <- round(my.cor$conf.int[1],3)
	pearson.confint.high <- round(my.cor$conf.int[2],3)
	pearson.pvalue <- ifelse(pearson.pvalue<=0.05, paste(pearson.pvalue,"**"), ifelse(pearson.pvalue<=0.1, paste(pearson.pvalue,"*"), pearson.pvalue))
	
	xx<-pretty.x[3]
	yy<-pretty.y[4]
	#text(xx,yy,paste("Pearson = ",pearson.corr," (p=", pearson.pvalue, ")",sep=""),cex=1.5)
	legend('topright',paste("D75%, Pearson = ",pearson.corr," (p=", pearson.pvalue, ")",sep=""),cex=1.0,bty='n')
	## add some year labels
#	tt <- subset(merged.df, year %in% c(1970,1985,1991,1992,2000,2012))
#	xx <- tt$n
#	yy <- tt$D75
#	text(xx,yy,tt$year,cex=1.0)


	axis(side=1, at = pretty.x, cex.axis=cex.in$axis, labels=TRUE, tcl=-0.2, las=0, mgp=c(0,0.4,0))
	axis(side=1, seq(min(pretty.x), max(pretty.x), by=((pretty.x[2]-pretty.x[1])/2)), labels=F, tck = -0.015)
	axis(side=2, at = pretty.y, cex.axis=cex.in$axis, labels=TRUE, tcl=-0.15, las=1, mgp=c(0,0.4,0))
	axis(side=2, seq(min(pretty.y), max(pretty.y), by=((pretty.y[2]-pretty.y[1])/2)), labels=F, tck = -0.01)

	# Affichage du titre et des axes	
	#mtext(xlabel, side = 1, line = 1.5, cex=cex.in$labels)
	mtext(xlabel1, side = 1, line = 1.5, cex=cex.in$labels)
	mtext(xlabel2, side = 1, line = 2.5, cex=cex.in$labels)
	mtext(ylabel1, side = 2, line = 2.8+pos.ylabel[2], cex=cex.in$labels)
	mtext(ylabel2, side = 2, line = 1.6+pos.ylabel[2], cex=cex.in$labels)
###########################################
}

if('Gini' %in% which.measure){
###########################################
## Gini
	ylabel2 = "Index Gini"
	ylabel1 = "Gini index"

	x.range <- range(merged.df$b)
	pretty.x <- pretty(x.range)
	y.range  <- c(range(merged.df$Gini)[1]*0.5, range(merged.df$Gini)[2]*1.5)
	pretty.y  <- pretty(y.range)

		# colour code the years to identify the beginning and end of the time-series
	my.cols <- colorRampPalette(c('blue','red'))(dim(merged.df)[1])
	yr.cols <- my.cols[merged.df$year-min(merged.df$year)+1]

plot(Gini~b, data=merged.df, pch=20, axes=FALSE, ann=FALSE, xlim=x.range, ylim=y.range, col=yr.cols)
ll <- dim(merged.df)[1]
sapply(1:(ll-1), function(i){segments(merged.df$b[i], merged.df$Gini[i], merged.df$b[i+1], merged.df$Gini[i+1], col=yr.cols[i], lty=1, lwd=0.15)})

pearson.corr <- round(cor(merged.df$Gini, merged.df$b),3)
xx<-pretty.x[2]
yy<-pretty.y[6]
text(xx,yy,paste("Pearson = ",pearson.corr,sep=""),cex=1.0)

	axis(side=1, at = pretty.x, cex.axis=cex.in$axis, labels=TRUE, tcl=-0.2, las=0, mgp=c(0,-0.2,0))
	axis(side=1, seq(min(pretty.x), max(pretty.x), by=((pretty.x[2]-pretty.x[1])/2)), labels=F, tck = -0.015)
	axis(side=2, at = pretty.y, cex.axis=cex.in$axis, labels=TRUE, tcl=-0.15, las=1, mgp=c(0,0.2,0))
	axis(side=2, seq(min(pretty.y), max(pretty.y), by=((pretty.y[2]-pretty.y[1])/2)), labels=F, tck = -0.01)

	# Affichage du titre et des axes	
	mtext(xlabel, side = 1, line = 0.5, cex=cex.in$labels)
	mtext(ylabel1, side = 2, line = 1.75+pos.ylabel[2], cex=cex.in$labels)
	mtext(ylabel2, side = 2, line = 1+pos.ylabel[2], cex=cex.in$labels)
###########################################

}


## 


}
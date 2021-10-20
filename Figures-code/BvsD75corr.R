## plot distribution vs. abundance for the 3 distribution indices

BvsD75corr.fct <- function(spec.code) {
  fn <- file.path(figdata.path, paste0("SS",spec.code,"_stratified.csv"))
  abundance <- read.csv(fn, header=TRUE)
  fn <- file.path(figdata.path, paste0("SS",spec.code,"_distribution-usingbiomass.csv"))
  distribution <- read.csv(fn, header=TRUE)
  
  ## for SF species, remove the early years
  if(is.na(distribution[distribution$year==1970,c("area.surveyed")])) distribution <- distribution[distribution$year>=1999,]
  
  ## remove 2018
  distribution[distribution$year==2018,c("DWAO","D75","D95")] <- NA
  
merged.df <- merge(abundance, distribution, "year")

	xlabel = "Mean biomass"
	xlabel1 = "Mean weight (kg) per tow"
	
###########################################
## D75
	ylabel1 = expression(paste("Geographic range (", 10^3, " ", km^2,")", sep=""))

	x.range <- range(merged.df$b)
	pretty.x <- pretty(x.range,n=4)
	y.range  <- c(range(na.omit(merged.df$D75))[1]*0.5, range(na.omit(merged.df$D75))[2]*1.1)
	pretty.y  <- pretty(y.range,n=4)

	b.fn <- file.path(figdata.path, paste0("SS",spec.code,"_stratified.csv"))
	b.dat.in <- read.csv(b.fn, header=TRUE)
	
	if(range(b.dat.in$b)[2]<10){par(mar=c(3,5,1,1))}else{par(mar=c(3,4,1,1))}
	
	# colour code the years to identify the beginning and end of the time-series
	yy <- seq(min(merged.df$year),max(merged.df$year))
	ll <- length(yy)

	ny <- nrow(merged.df)
	my.cols <- bluetored.fct(ny)
	
	yr.cols <- my.cols[merged.df$year-min(merged.df$year)+1]
	# my.cols <- colorRampPalette(c('blue','red'))(ll+1)
	# yr.cols <- my.cols[merged.df$year-min(merged.df$year)+1]

	plot(D75~b, data=merged.df, pch=20, axes=FALSE, ann=FALSE, xlim=x.range, ylim=y.range, col=yr.cols)
	ll <- dim(merged.df)[1]
	sapply(1:(ll-1), function(i){segments(merged.df$b[i], merged.df$D75[i], merged.df$b[i+1], merged.df$D75[i+1], col=yr.cols[i], lty=1, lwd=1)})
	
	my.cor <- cor.test(merged.df$D75, merged.df$b)
	pearson.corr <- round(my.cor$estimate,3)
	pearson.pvalue <- round(my.cor$p.value,3)
	pearson.confint.low <- round(my.cor$conf.int[1],3)
	pearson.confint.high <- round(my.cor$conf.int[2],3)
	pearson.pvalue <- ifelse(pearson.pvalue<=0.05, paste(pearson.pvalue,"**"), ifelse(pearson.pvalue<=0.1, paste(pearson.pvalue,"*"), pearson.pvalue))
	
	xx<-pretty.x[3]
	yy<-pretty.y[4]
	#text(xx,yy,paste("Pearson = ",pearson.corr," (p=", pearson.pvalue, ")",sep=""),cex=1.5)
	legend('topright',paste("D75%, Pearson = ",pearson.corr," (p=", pearson.pvalue, ")",sep=""),cex=1.5,bty='n')
	## add some year labels
#	tt <- subset(merged.df, year %in% c(1970,1985,1991,1992,2000,2012))
#	xx <- tt$n
#	yy <- tt$D75
#	text(xx,yy,tt$year,cex=1.0)


	axis(side=1, at = pretty.x, cex.axis=1.3, labels=TRUE, tcl=-0.2, las=0, mgp=c(0,0.4,0))
	axis(side=1, seq(min(pretty.x), max(pretty.x), by=((pretty.x[2]-pretty.x[1])/2)), labels=F, tck = -0.015)
	axis(side=2, at = pretty.y, cex.axis=1.3, labels=TRUE, tcl=-0.15, las=1, mgp=c(0,0.4,0))
	axis(side=2, seq(min(pretty.y), max(pretty.y), by=((pretty.y[2]-pretty.y[1])/2)), labels=F, tck = -0.01)

	# Affichage du titre et des axes	
	mtext(xlabel1, side = 1, line = 1.5, cex=1.5)
	mtext(ylabel1, side = 2, line = 1.5, cex=1.5)
	
	box()
###########################################

}

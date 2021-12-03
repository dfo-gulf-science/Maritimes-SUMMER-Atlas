## figure of environmental preference

envpref.fct <- function(spec.code,env.var) {

	switch(env.var,
	  depth={
	    fn <- file.path(figdata.path, paste0("SS",spec.code,"_depthdist.csv"))
	    dat.in <- read.csv(fn, header=TRUE)
	    
	  },
	  bottomtemperature={
	    fn <- file.path(figdata.path, paste0("SS",spec.code,"_temperaturedist.csv"))
	    dat.in <- read.csv(fn, header=TRUE)
	    
	  },
	  bottomsalinity={
	    fn <- file.path(figdata.path, paste0("SS",spec.code,"_salinitydist.csv"))
	    dat.in <- read.csv(fn, header=TRUE)
	    
	  },
	  NULL = {
	    stop("This environmental variable is not recognised!\n")
	    
	  }
	)# end switch
	
	
	# trim the incoming data frame to remove 0% and 100%
	zero.catch <- which(dat.in$cum.catch == 0)
	zero.all <- which(dat.in$cum == 0)
	hundred.catch <- which(dat.in$cum.catch == 100)
	hundred.all <- which(dat.in$cum == 100)
	lb <- min(max(zero.catch), max(zero.all))-1
	ub <- max(min(hundred.catch), min(hundred.all))+2
	dat.in <- dat.in[lb:ub,]
	
	plot(dat.in[,1], dat.in[,2], axes=FALSE, ann=FALSE, lty=1, lwd=2, type='l', xaxs="i", yaxs="i") #
	lines(dat.in[,1], dat.in[,3], lty=2, lwd=1.5, type='l')
	
	##  segments showing the 5, 25, 50, 75 and 95 percentiles
	my.ii <- data.frame(quan=100*c(0.05,0.25,0.5,0.75,0.95), index.1=NA, var.value=NA)
	var.step <- diff(dat.in[,1])[1]
	for(i in 1:nrow(my.ii)){
	  ## there are a few instances (e.g. bottom salinity for Atlantic halibut )
	  ## where the first CDF value exceeds 0.05 and the first step in the loop
	  ## generates an error, so fix this
	  if(length(which(dat.in[,2]<my.ii$quan[i]))==0){
	    my.ii$index.1[i] <- 1
	  }
	  else {
	    my.ii$index.1[i] <- tail(which(dat.in[,2]<my.ii$quan[i]), n=1)
	  }
	  ## linear interpolation to determine the exact value where the catch CDF is equal to the quantile sought
	  cdf.step <- dat.in[my.ii$index.1[i]+1,2] - dat.in[my.ii$index.1[i],2] 
	  interpolation.fac <- ((my.ii$quan[i] - dat.in[my.ii$index.1[i],2]) / cdf.step)
	  my.ii$var.value[i] <- dat.in[my.ii$index.1[i],1] + (interpolation.fac*var.step)
	}
	
	
	segments(dat.in[1,1], my.ii$quan, my.ii$var.value, my.ii$quan, lty=1, lwd=0.5, col=grey(0.2))
	segments(my.ii$var.value, 0, my.ii$var.value, my.ii$quan, lty=1, lwd=0.5, col=grey(0.2))
	
	switch(env.var,
	       depth={
	         # axes and labels
	         xlabel = "Depth (m)"
	         ylabel1 = "Cumulative frequency (%)"
	         leg.label = c("Catch", "Depth")
	         
	       },
	       bottomtemperature={
	         # axes and labels
	         xlabel = "Bottom temperature (\u{B0}C)"
	         ylabel1 = "Cumulative frequency (%)"
	         leg.label = c("Catch", "Bottom temperature")
	         
	       },
	       bottomsalinity={
	         # axes and labels
	         xlabel = "Bottom salinity (PSU)"
	         ylabel1 = "Cumulative frequency (%)"
	         leg.label = c("Catch", "Salinity")
	         
	       },
	       NULL = {
	         stop("This environmental variable is not recognised!\n")
	         
	       }
	)# end switch
	
	# labels and such
	x.range <- range(dat.in[,1])
	pretty.x <- pretty(x.range)
	#y.range  <- range(dat.in$cum)
	#pretty.y  <- pretty(y.range)
	
	axis(side=1, at = pretty.x, cex.axis=1, labels=TRUE, tcl=-0.2, las=0, mgp=c(0,0.2,0))
	axis(side=1, seq(min(pretty.x), max(pretty.x), by=((pretty.x[2]-pretty.x[1])/2)), labels=F, tck = -0.015)
	#axis(side=2, at = pretty.y, cex.axis=1, labels=TRUE, tcl=-0.15, las=1, mgp=c(0,0.2,0))
	#axis(side=2, seq(min(pretty.y), max(pretty.y), by=((pretty.y[2]-pretty.y[1])/2)), labels=F, tck = -0.01)
	axis(side=2, at = c(5,25,50,75,95), cex.axis=1, labels=TRUE, tcl=-0.15, las=1, mgp=c(0,0.2,0))
	
	# Affichage du titre et des axes	
	mtext(xlabel, side = 1, line = 1.5, cex=1)
	mtext(ylabel1, side = 2, line = 2.5, cex=1)
	
	legend("bottomright", bty="o", leg.label, col = c("Black","Black"), lwd=2, 
	       text.col = "Black", lty = c(1, 2), cex=1, bg=rgb(1,1,1,alpha=0.7))
	
	box()
	
	
} # end function

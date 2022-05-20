## figure of environmental preference

##
##
env.pref.fig.fct <- function(spec.code, env.var){
  in.fn <- file.path(figdata.path, paste(paste("SS", spec.code, "_", paste0(env.var, "dist"), ".csv", sep = "")))
  in.df <- read.table(in.fn, sep = ",", header = TRUE)

  ## nn <- names(in.df)
  
  ## quantiles
  my.ii <- data.frame(quan=c(0.05,0.25,0.5,0.75,0.95), index.1=NA, var.value=NA)
  var.step <- diff(in.df[,1])[1]
  for(i in 1:nrow(my.ii)){
    ## there are a few instances (e.g. bottom salinity for Atlantic halibut )
    ## where the first CDF value exceeds 0.05 and the first step in the loop
    ## generates an error, so fix this
    if(length(which(in.df[,3]<my.ii$quan[i]))==0){
      my.ii$index.1[i] <- 1
    }
    else {
      my.ii$index.1[i] <- tail(which(in.df[,3]<my.ii$quan[i]), n=1)
    }
    
    
    ## linear interpolation to determine the exact value where the catch CDF is equal to the quantile sought
    cdf.step <- in.df[my.ii$index.1[i]+1,3] - in.df[my.ii$index.1[i],3] 
    interpolation.fac <- ((my.ii$quan[i] - in.df[my.ii$index.1[i],3]) / cdf.step)
    my.ii$var.value[i] <- in.df[my.ii$index.1[i],1] + (interpolation.fac*var.step)
  }
  
  
  my.xlab <- ifelse(env.var=="depth", "Depth (m)", ifelse(env.var=="bottomtemp","Temperature (C)",ifelse(env.var=="bottomsalinity","Salinity (PSU)","blah")))
  my.legend <- ifelse(env.var=="depth", "Depth", ifelse(env.var=="bottomtemp","Temperature",ifelse(env.var=="bottomsalinity","Salinity","blah")))
  
  
  par(mar = c(2.5, 3, 1, 1), xaxs="i", yaxs="i")
  
  plot(in.df[,1], in.df[,2], type='l', lty=2, lwd=1, axes=FALSE, xlab="", ylab="")
  lines(in.df[,1], in.df[,3], lty=1, lwd=1)
  axis(side=1, padj=-1)
  axis(side=2, at=c(0.05,0.25,0.5,0.75,0.95), labels=c(5,25,50,75,95), padj=1)
  mtext(side=1, my.xlab, line=1.2)
  mtext(side=2, "Cumulative frequency (%)", line=1.5)
  box()
  legend("bottomright", c("Catch",my.legend), lty=c(1,2), lwd=c(1,1), bty="n")
  
  segments(in.df[1,1], my.ii$quan, my.ii$var.value, my.ii$quan, lty=1, lwd=0.5, col=grey(0.2))
  segments(my.ii$var.value, 0, my.ii$var.value, my.ii$quan, lty=1, lwd=0.5, col=grey(0.2))
  
} # end function

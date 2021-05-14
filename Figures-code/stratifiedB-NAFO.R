## stratified random estimates plots, biomass only, separately for 4X and 4VW

stratifiedB-NAFO.fct <- function(spec.code) {
  
  
  col1 <- "#7570B340"
  col2 <- "#D95F0240"
  col3 <- "#1B9E7740"

  plot(c(-1,0,1),c(0,0,0.5), pch=19, cex=4, col=c(col1,col2,col3), xlim=c(-2,2), ylim=c(-2,2))

  polygon(c(-2,2,2,-2), c(-0.5,-0.5,0.5,0.5), col=col1)
  polygon(c(-2,2,2,-2), c(-2,1,2,-1), col=col2)
  polygon(c(-2,2,2,-2), c(1,-2,-1,2), col=col3)
  
  # color for 4X
  col.4x <- col1
  # color for 4VW
  col.4vw <- col2
  
  yrs <- 1970:2020
  ts1 <- data.frame(year=yrs, y=(-2.5 + (yrs-1969)*0.1), y.err=0.5)
  ts2 <- data.frame(year=yrs, y=(2.5 + (yrs-1969)*-0.1), y.err=0.5)
  plot(y~year,data=ts1, type="l", col=col1, lwd=3)
  polygon(c(ts1$year,rev(ts1$year)), c(ts1$y+ts1$y.err,rev(ts1$y)-ts1$y.err), col=col1, border=col1)
  
  lines(y~year,data=ts2, type="l", col=col2, lwd=3)
  polygon(c(ts2$year,rev(ts2$year)), c(ts2$y+ts2$y.err,rev(ts2$y)-ts2$y.err), col=col2, border=col2)
  
  
  } # end function

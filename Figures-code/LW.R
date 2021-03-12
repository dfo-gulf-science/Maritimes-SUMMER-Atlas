## LengthWeight plot

figure8.fct <- function(dat.in, cex.in, pos.ylabel=c(0,0)) {
# remove entries where weight is null
dat.in <- subset(dat.in, !is.na(FWT))

## fit a linear model to obtain the parameter estimates
lw.fit <- lm(log(FWT)~log(FLEN),data=dat.in)

x.range <- c(0,range(dat.in$FLEN)[2])
y.range <- c(0,range(dat.in$FWT)[2])

pretty.x=pretty(c(x.range[1],x.range[2]))
pretty.y=pretty(c(y.range[1],y.range[2]))
  
## subsample the data if there is too many points to appear
  n = length(dat.in[,1])
  id = seq(1,n)
  if (n < 1000) {
       tmp=which(id %% 1 == 0)
  } else if (n < 10000) {
       tmp=which(id %% 3 == 0)
  } else tmp=which(id %% 5 == 0)

  
## generate the plot
  plot(0, type="n", axes=FALSE, ann=FALSE, xlim=x.range, ylim=y.range)
  points(dat.in[tmp,]$FLEN,dat.in[tmp,]$FWT, pch=21, col="Black", bg="white", cex=0.5);
  xx <- seq(x.range[1],x.range[2])
  lines(xx, exp(coef(lw.fit)[1]) * (xx^coef(lw.fit)[2] ), col='red', type='l', lwd=3)

  box(bty="l")
  
aa = signif(exp(coef(lw.fit)[1]),4)
bb = signif(coef(lw.fit)[2],5)

text(x.range[1],y.range[2], bty="n", (bquote(y == .(aa)*x^.(bb))),  col = "Black", cex=1.25*cex.in$title, adj=c(0,2))

## axes and labels
	# Text
  xlabel = "Length (cm) / Longueur (cm)"
  ylabel2 = "Poids total (g)"
  ylabel1 = "Total weight (g)"

  axis(1, at = pretty.x, cex.axis=cex.in$axis, labels=TRUE, tcl=-0.2, las=0, mgp=c(0,-0.2,0), tck = -0.01)
  axis(side=1, seq(min(pretty.x), max(pretty.x), by=((pretty.x[2]-pretty.x[1])/2)), labels=F, tck = -0.01)
  axis(2, at = pretty.y, cex.axis=cex.in$axis, labels=TRUE, tcl=-0.15, las=1, mgp=c(0,0.2,0))
  axis(side=2, seq(min(pretty.y), max(pretty.y), by=((pretty.y[2]-pretty.y[1])/2)), labels=F, tck = -0.01)

  
  mtext(xlabel, side = 1, line = 0.5, cex=cex.in$labels)
  mtext(ylabel1, side = 2, line = 2.75+pos.ylabel[2], cex=cex.in$labels)
  mtext(ylabel2, side = 2, line = 2+pos.ylabel[2], cex=cex.in$labels)


# yearly.cond <- tapply(dat.in$condition, dat.in$YEAR, mean)
# plot(names(yearly.cond), yearly.cond, type='b')

} # end function


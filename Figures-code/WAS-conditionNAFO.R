## average fish condition plot, separately for NAFO 4X and 4VW

conditionNAFO.fct <- function(spec.code) {
  fn <- file.path(figdata.path, paste0("SS",spec.code,"_lw4x.csv"))
lw.4x <- read.csv(fn, header=TRUE)
fn <- file.path(figdata.path, paste0("SS",spec.code,"_lw4vw.csv"))
lw.4vw <- read.csv(fn, header=TRUE)
dat.in <- list(df.1=lw.4x, df.2=lw.4vw)

# remove entries where weight is null
## also, remove crazy conditions that exceed 2.0
nafo4x <- subset(dat.in[[1]], !is.na(FWT) & condition<=2.0)
nafo4vw <- subset(dat.in[[2]], !is.na(FWT) & condition<=2.0)



lw.fit4x <- lm(log(FWT)~log(FLEN),data=nafo4x)
lw.fit4vw <- lm(log(FWT)~log(FLEN),data=nafo4vw) 

#aa4x = signif(exp(coef(lw.fit4x)[1]),4)
aa4x = round(exp(coef(lw.fit4x)[1]),4)
#bb4x = signif(coef(lw.fit4x)[2],5)
bb4x = round(coef(lw.fit4x)[2],5)

#aa4vw = signif(exp(coef(lw.fit4vw)[1]),4)
#bb4vw = signif(coef(lw.fit4vw)[2],5)
aa4vw = round(exp(coef(lw.fit4vw)[1]),4)
bb4vw = round(coef(lw.fit4vw)[2],5)

# for consistency, make sure that the y range is shared by both 4X and 4VW plots
tt.dat <- subset(rbind(nafo4x,nafo4vw), !is.na(condition))
yearly.cond.all <- tapply(tt.dat$condition, tt.dat$YEAR, mean)
#y.range  <- range(yearly.cond.all) # c(0.9,1.1) 
y.range  <- c(0.93,1.1) 
## NAFO 4X
dat.in <- nafo4x
l.50 <- quantile(dat.in$FLEN)[3]

tt.dat <- subset(dat.in, !is.na(condition))

yearly.cond <- tapply(tt.dat$condition, tt.dat$YEAR, mean)

yearly.cond.lower50 <- tapply(subset(tt.dat, FLEN<l.50)$condition, subset(tt.dat, FLEN<l.50)$YEAR, mean)
yearly.cond.upper50 <- tapply(subset(tt.dat, FLEN>=l.50)$condition, subset(tt.dat, FLEN>=l.50)$YEAR, mean)

# axes and labels
xlabel = "Year / Ann\u{E9}e"
ylabel2 = "Condition moyenne"	
ylabel1 = "Mean condition"


# labels and such
x.range <- range(as.numeric(names(yearly.cond)))
pretty.x <- pretty(x.range)
pretty.y  <- pretty(y.range)


# we either have a species with data for the whole time or with missing data for the Gavaris years
data.1990 <- length(dat.in[dat.in$YEAR==1990,]$FSHNO)

if(data.1990){
  plot(as.numeric(names(yearly.cond)), yearly.cond, type='b', axes=FALSE, ann=FALSE, pch=19, xlim=x.range, ylim=y.range)
  nn <- length(names(yearly.cond))
  lines(as.numeric(names(yearly.cond)), rep(1,nn), lty=2, col=grey(0.7), lwd=2)
  
  lines(as.numeric(names(yearly.cond.lower50)), yearly.cond.lower50, lty=1, col=grey(0.7), lwd=1.0)
  lines(as.numeric(names(yearly.cond.upper50)), yearly.cond.upper50, lty=1, col=grey(0.7), lwd=3)
  legend("top", bty="n", legend=c("4X"))	
  legend("topright", bty="o", legend=(bquote(y == .(aa4x)*x^.(bb4x))), cex=0.9)	
}
if(!data.1990) {
  ii.early <- as.numeric(names(yearly.cond))<1990
  ii.late <- as.numeric(names(yearly.cond))>1990
  nn <- length(names(yearly.cond))
  plot(as.numeric(names(yearly.cond[ii.early])), yearly.cond[ii.early], type='b', axes=FALSE, ann=FALSE, pch=19, xlim=x.range, ylim=y.range)
  lines(as.numeric(names(yearly.cond)), rep(1,nn), lty=2, col=grey(0.7), lwd=2)
  lines(as.numeric(names(yearly.cond[ii.late])), yearly.cond[ii.late], type='b', pch=19)
  
  ii.early <- as.numeric(names(yearly.cond.lower50))<1990
  ii.late <- as.numeric(names(yearly.cond.lower50))>1990
  
  lines(as.numeric(names(yearly.cond.lower50[ii.early])), yearly.cond.lower50[ii.early], lty=1, col=grey(0.7), lwd=1.0)
  lines(as.numeric(names(yearly.cond.lower50[ii.late])), yearly.cond.lower50[ii.late], lty=1, col=grey(0.7), lwd=1.0)
  
  ii.early <- as.numeric(names(yearly.cond.upper50))<1990
  ii.late <- as.numeric(names(yearly.cond.upper50))>1990
  
  lines(as.numeric(names(yearly.cond.upper50[ii.early])), yearly.cond.upper50[ii.early], lty=1, col=grey(0.7), lwd=3)
  lines(as.numeric(names(yearly.cond.upper50[ii.late])), yearly.cond.upper50[ii.late], lty=1, col=grey(0.7), lwd=3)
  legend("top", bty="n", c("4X"))
  legend("topright", bty="o", legend=(bquote(y == .(aa4x)*x^.(bb4x))), cex=0.9)	
}

axis(side=1, at = pretty.x, cex.axis=1, labels=TRUE, tcl=-0.2, las=0, mgp=c(0,0.4,0))
axis(side=1, seq(min(pretty.x), max(pretty.x), by=((pretty.x[2]-pretty.x[1])/2)), labels=F, tck = -0.015)
axis(side=2, at = pretty.y, cex.axis=1, labels=TRUE, tcl=-0.15, las=1, mgp=c(0,0.4,0), line=1)
axis(side=2, seq(min(pretty.y), max(pretty.y), by=((pretty.y[2]-pretty.y[1])/2)), labels=F, tck = -0.01, line=1)

# Affichage du titre et des axes	
mtext(xlabel, side = 1, line = 1.5, cex=1)
mtext(ylabel1, side = 2, line = 3.5, cex=1)

## NAFO 4VW
dat.in <- nafo4vw
l.50 <- quantile(dat.in$FLEN)[3]

yearly.cond <- tapply(dat.in$condition, dat.in$YEAR, mean)
yearly.cond.lower50 <- tapply(subset(dat.in, FLEN<l.50)$condition, subset(dat.in, FLEN<l.50)$YEAR, mean)
yearly.cond.upper50 <- tapply(subset(dat.in, FLEN>=l.50)$condition, subset(dat.in, FLEN>=l.50)$YEAR, mean)

# axes and labels
xlabel = "Year"
ylabel1 = "Mean condition"

# labels and such
x.range <- range(as.numeric(names(yearly.cond)))
pretty.x <- pretty(x.range)
pretty.y  <- pretty(y.range)


# we either have a species with data for the whole time or with missing data for the Gavaris years
data.1990 <- length(dat.in[dat.in$YEAR==1990,]$FSHNO)

if(data.1990){
  plot(as.numeric(names(yearly.cond)), yearly.cond, type='b', axes=FALSE, ann=FALSE, pch=19, xlim=x.range, ylim=y.range)
  nn <- length(names(yearly.cond))
  lines(as.numeric(names(yearly.cond)), rep(1,nn), lty=2, col=grey(0.7), lwd=1.5)
  
  lines(as.numeric(names(yearly.cond.lower50)), yearly.cond.lower50, lty=1, col=grey(0.7), lwd=1.0)
  lines(as.numeric(names(yearly.cond.upper50)), yearly.cond.upper50, lty=1, col=grey(0.7), lwd=3)
  legend("top", bty="n", c("4VW"))
  legend("topright", bty="o", legend=(bquote(y == .(aa4vw)*x^.(bb4vw))), cex=0.9)	
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
  
  lines(as.numeric(names(yearly.cond.upper50[ii.early])), yearly.cond.upper50[ii.early], lty=1, col=grey(0.7), lwd=3)
  lines(as.numeric(names(yearly.cond.upper50[ii.late])), yearly.cond.upper50[ii.late], lty=1, col=grey(0.7), lwd=3)
  legend("top", bty="n", c("4VW"))
  legend("topright", bty="o", legend=(bquote(y == .(aa4vw)*x^.(bb4vw))), cex=0.9)	
  
}

axis(side=1, at = pretty.x, cex.axis=1, labels=TRUE, tcl=-0.2, las=0, mgp=c(0,0.4,0))
axis(side=1, seq(min(pretty.x), max(pretty.x), by=((pretty.x[2]-pretty.x[1])/2)), labels=F, tck = -0.015)
axis(side=2, at = pretty.y, cex.axis=1, labels=TRUE, tcl=-0.15, las=1, mgp=c(0,0.4,0), line=1)
axis(side=2, seq(min(pretty.y), max(pretty.y), by=((pretty.y[2]-pretty.y[1])/2)), labels=F, tck = -0.01, line=1)

# Affichage du titre et des axes	
mtext(xlabel, side = 1, line = 1.5, cex=1)

} # end function


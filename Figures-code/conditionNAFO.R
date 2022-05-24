## average fish condition plot, separately for NAFO 4X and 4VW

conditionNAFO.fct <- function(spec.code) { #, file.name
  
  fn <- file.path(figdata.path, paste0("SS",spec.code,"_lw.csv"))
  lw <- read.csv(fn, header=TRUE)
  
  all.df <- lw

  
	all.df$nafo <- factor(all.df$nafo, levels=c("4X","4VW"), ordered=TRUE)
	all.df$year <- factor(all.df$YEAR, levels=1969:2021, ordered=TRUE)
	
	## remove rows where condition is NA, because there is no weight
	## remove conditions above 2.0
	## remove conditions below 0.5
	ii <- which(!is.na(all.df$condition) & (all.df$condition<2.0 & all.df$condition>0.5))
	all.df <- all.df[ii,]
	
	
	
	lw.fit4x <- lm(log(FWT)~log(FLEN),data=all.df[all.df$nafo=="4X",])
	lw.fit4vw <- lm(log(FWT)~log(FLEN),data=all.df[all.df$nafo=="4VW",])
	
	aa4x = round(exp(coef(lw.fit4x)[1]),4)
	bb4x = round(coef(lw.fit4x)[2],5)
	
	aa4vw = round(exp(coef(lw.fit4vw)[1]),4)
	bb4vw = round(coef(lw.fit4vw)[2],5)
	
	y.range  <- c(0.70,1.35) 
	
	## yearly mean
	tt.dat <- all.df[all.df$nafo=="4X",]
	yearly.cond.4x <- tapply(tt.dat$condition, tt.dat$YEAR, mean)
	yearly.cond.q25.4x <- tapply(tt.dat$condition, tt.dat$YEAR, quantile, probs=c(0.25))
	yearly.cond.q75.4x <- tapply(tt.dat$condition, tt.dat$YEAR, quantile, probs=c(0.75))
	
	tt.dat <- all.df[all.df$nafo=="4VW",]
	yearly.cond.4vw <- tapply(tt.dat$condition, tt.dat$YEAR, mean)
	yearly.cond.q25.4vw <- tapply(tt.dat$condition, tt.dat$YEAR, quantile, probs=c(0.25))
	yearly.cond.q75.4vw <- tapply(tt.dat$condition, tt.dat$YEAR, quantile, probs=c(0.75))
	
	xlabel <- "Year" #
	ylabel <- "Mean condition"
	
	# labels and such
	x.range <- c(1970,2020)
	pretty.x <- seq(1970,2020,10)
	pretty.y  <- seq(0.8,1.3,0.1)
	
	# we either have a species with data for the whole time or with missing data for the Gavaris years
	data.1990 <- length(all.df[all.df$YEAR==1990,]$FSHNO)
	
	## 4X
	par(mai=c(0.5, 0.75, 0.1, 0.1))
	if(data.1990){
	  plot(as.numeric(names(yearly.cond.4x)), yearly.cond.4x, type='n', axes=FALSE, ann=FALSE, pch=19, xlim=x.range, ylim=y.range)
	  nn <- length(names(yearly.cond.4x))
	  
	  xx <- c(as.numeric(names(yearly.cond.q25.4x)), rev(as.numeric(names(yearly.cond.q25.4x))))
	  yy1 <- c(yearly.cond.q25.4x, rev(yearly.cond.q75.4x))

	  polygon(xx, yy1, col=grey(0.8), border=grey(0.8))
	  lines(as.numeric(names(yearly.cond.4x)), yearly.cond.4x, lty=1)
	  points(as.numeric(names(yearly.cond.4x)), yearly.cond.4x, pch=19)
	  
	  lines(as.numeric(names(yearly.cond.4x)), rep(1,nn), lty=2, col=grey(0.3), lwd=2)
	  
	  legend("top", cex=1.2, bty="n", legend=c("4X"))	
	  #legend("topright", bty="o", legend=(bquote(y == .(aa4x)*x^.(bb4x))), cex=0.9)	
	}
	if(!data.1990) {
	  ii.early <- as.numeric(names(yearly.cond.4x))<1990
	  ii.late <- as.numeric(names(yearly.cond.4x))>1990
	  nn <- length(names(yearly.cond.4x))
	  plot(as.numeric(names(yearly.cond.4x)), yearly.cond.4x, type='n', axes=FALSE, ann=FALSE, pch=19, xlim=x.range, ylim=y.range)
	  
	  ## early part
	  xx <- c(as.numeric(names(yearly.cond.q25.4x[ii.early])), rev(as.numeric(names(yearly.cond.q25.4x[ii.early]))))
	  yy1 <- c(yearly.cond.q25.4x[ii.early], rev(yearly.cond.q75.4x[ii.early]))
	  polygon(xx, yy1, col=grey(0.8), border=grey(0.8))
	  lines(as.numeric(names(yearly.cond.4x[ii.early])), yearly.cond.4x[ii.early], lty=1)
	  points(as.numeric(names(yearly.cond.4x[ii.early])), yearly.cond.4x[ii.early], pch=19)
	  
	  ## late part
	  xx <- c(as.numeric(names(yearly.cond.q25.4x[ii.late])), rev(as.numeric(names(yearly.cond.q25.4x[ii.late]))))
	  yy1 <- c(yearly.cond.q25.4x[ii.late], rev(yearly.cond.q75.4x[ii.late]))
	  polygon(xx, yy1, col=grey(0.8), border=grey(0.8))
	  lines(as.numeric(names(yearly.cond.4x[ii.late])), yearly.cond.4x[ii.late], lty=1)
	  points(as.numeric(names(yearly.cond.4x[ii.late])), yearly.cond.4x[ii.late], pch=19)
	  
	  lines(as.numeric(names(yearly.cond.4x)), rep(1,nn), lty=2, col=grey(0.3), lwd=2)
	  
	  legend("top", cex=1.2, bty="n", c("4X"))
	  #legend("topright", bty="o", legend=(bquote(y == .(aa4x)*x^.(bb4x))), cex=0.9)	
	}
	
	axis(side=1, at = pretty.x, cex.axis=1, labels=TRUE, tcl=-0.2, las=0, mgp=c(0,0.4,0))
	axis(side=1, seq(min(pretty.x), max(pretty.x), by=((pretty.x[2]-pretty.x[1])/2)), labels=F, tck = -0.015)
	axis(side=2, at = pretty.y, cex.axis=1, labels=TRUE, tcl=-0.15, las=1, mgp=c(0,0.4,0))
	axis(side=2, seq(min(pretty.y), max(pretty.y), by=((pretty.y[2]-pretty.y[1])/2)), labels=F, tck = -0.01)
	box()
	# Affichage du titre et des axes	
	mtext(xlabel, side = 1, line = 1.5, cex=1)
	mtext(ylabel, side = 2, line = 2, cex=1)
	
	
	## 4VW
	par(mai=c(0.5, 0.5, 0.1, 0.1))
	if(data.1990){
	  plot(as.numeric(names(yearly.cond.4vw)), yearly.cond.4vw, type='n', axes=FALSE, ann=FALSE, pch=19, xlim=x.range, ylim=y.range)
	  nn <- length(names(yearly.cond.4vw))
	  
	  xx <- c(as.numeric(names(yearly.cond.q25.4vw)), rev(as.numeric(names(yearly.cond.q25.4vw))))
	  yy1 <- c(yearly.cond.q25.4vw, rev(yearly.cond.q75.4vw))
	  
	  polygon(xx, yy1, col=grey(0.8), border=grey(0.8))
	  lines(as.numeric(names(yearly.cond.4vw)), yearly.cond.4vw, lty=1)
	  points(as.numeric(names(yearly.cond.4vw)), yearly.cond.4vw, pch=19)
	  
	  lines(as.numeric(names(yearly.cond.4vw)), rep(1,nn), lty=2, col=grey(0.3), lwd=2)
	  
	  legend("top", cex=1.2, bty="n", legend=c("4vw"))	
	  #legend("topright", bty="o", legend=(bquote(y == .(aa4vw)*x^.(bb4vw))), cex=0.9)	
	}
	if(!data.1990) {
	  ii.early <- as.numeric(names(yearly.cond.4vw))<1990
	  ii.late <- as.numeric(names(yearly.cond.4vw))>1990
	  nn <- length(names(yearly.cond.4vw))
	  plot(as.numeric(names(yearly.cond.4vw)), yearly.cond.4vw, type='n', axes=FALSE, ann=FALSE, pch=19, xlim=x.range, ylim=y.range)
	  
	  ## early part
	  xx <- c(as.numeric(names(yearly.cond.q25.4vw[ii.early])), rev(as.numeric(names(yearly.cond.q25.4vw[ii.early]))))
	  yy1 <- c(yearly.cond.q25.4vw[ii.early], rev(yearly.cond.q75.4vw[ii.early]))
	  polygon(xx, yy1, col=grey(0.8), border=grey(0.8))
	  lines(as.numeric(names(yearly.cond.4vw[ii.early])), yearly.cond.4vw[ii.early], lty=1)
	  points(as.numeric(names(yearly.cond.4vw[ii.early])), yearly.cond.4vw[ii.early], pch=19)
	  
	  ## late part
	  xx <- c(as.numeric(names(yearly.cond.q25.4vw[ii.late])), rev(as.numeric(names(yearly.cond.q25.4vw[ii.late]))))
	  yy1 <- c(yearly.cond.q25.4vw[ii.late], rev(yearly.cond.q75.4vw[ii.late]))
	  polygon(xx, yy1, col=grey(0.8), border=grey(0.8))
	  lines(as.numeric(names(yearly.cond.4vw[ii.late])), yearly.cond.4vw[ii.late], lty=1)
	  points(as.numeric(names(yearly.cond.4vw[ii.late])), yearly.cond.4vw[ii.late], pch=19)
	  
	  lines(as.numeric(names(yearly.cond.4vw)), rep(1,nn), lty=2, col=grey(0.3), lwd=2)
	  
	  legend("top", cex=1.2, bty="n", c("4VW"))
	  #legend("topright", bty="o", legend=(bquote(y == .(aa4vw)*x^.(bb4vw))), cex=0.9)	
	}
	
	axis(side=1, at = pretty.x, cex.axis=1, labels=TRUE, tcl=-0.2, las=0, mgp=c(0,0.4,0))
	axis(side=1, seq(min(pretty.x), max(pretty.x), by=((pretty.x[2]-pretty.x[1])/2)), labels=F, tck = -0.015)
	axis(side=2, at = pretty.y, cex.axis=1, labels=TRUE, tcl=-0.15, las=1, mgp=c(0,0.4,0))
	axis(side=2, seq(min(pretty.y), max(pretty.y), by=((pretty.y[2]-pretty.y[1])/2)), labels=F, tck = -0.01)
	box()
	# Affichage du titre et des axes	
	mtext(xlabel, side = 1, line = 1.5, cex=1)
	
	} # end function




# p <- ggplot(data=all.df, aes(x=year, y=condition, group=nafo)) + 
#   # geom_violin(fill=grey(0.8)) + facet_grid(.~nafo) +
#   geom_jitter(position=position_jitter(width=0.3, height=0.2)) +
#   #geom_boxplot(fill=grey(0.8)) + 
#   facet_grid(.~nafo) + 
#   geom_hline(yintercept=1, col=grey(0.7), lty=2) +
#   stat_summary(fun=mean, geom="line", color="black") +
#   stat_summary(fun=mean, geom="point", color="black", pch=19) +
#   stat_summary(fun=function(z){quantile(z,0.75)}, geom="line", color=grey(0.5)) +
#   stat_summary(fun=function(z){quantile(z,0.25)}, geom="line", color=grey(0.5)) +
#   #stat_summary(fun.min= function(z){quantile(z,0.25)}, fun.max= function(z){quantile(z,0.75)}, fun=median) + 
#   #stat_summary(fun=mean, geom="point", shape=20, size=2, color="red", fill="red") +
#   theme_bw() +
#   theme(panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
#   scale_y_log10() + 
#   scale_x_discrete(breaks=seq(1970,2020,10), limits=factor(1969:2021, levels=1969:2021, ordered=TRUE)) +
#   xlab("Year") + ylab("Fish condition")
# 
# p
# 
# ggsave(filename=file.path(fig.path, file.name), p, width=12, height=6, units="in")
# 

############################################################################
cruise.track.fct <- function(cruise.track, add=FALSE){
  mm <- matrix(c(0,0,0,0,1,0,0,2,0), nr=3)
  ll <- layout(mm, widths=c(0.05,0.8,0.15), heights=c(0.01,0.92,0.07))
  
  x <- cruise.track
  
  ## range of dates for this cruise.number
  rr <- range(x$datetime)
  
  ## assign time interval to each tow, 200 equal time windows between the first and last tow
  t.i <- seq(rr[1], rr[2], length.out=200)
  x$t.i <- findInterval(x$datetime, t.i)
  
  ## identify time window between survey legs
  ## which difference of time between tows is greater than 24 hours?
  #ii <- nrow(x)
  #x$minutes.until.next <- NA
  #x$minutes.until.next[1:(ii-1)] <- as.numeric(diff(x$datetime))
  #oo <- which(x$minutes.until.next > (24*60))
  
  # colour code based on the time interval
  my.cols <- bluetored.fct(200)
  
  x$col <- my.cols[x$t.i]
  
  ## basemap
  my.xlim <- range(x$longitude) * c(1.01,0.99)
  my.ylim <- range(x$latitude) * c(0.99,1.01)
  
  par(mar=c(0,0,0,0))
  plot(x$longitude, x$latitude, type="n", xlim=my.xlim, ylim=my.ylim, axes=FALSE, xlab="", ylab="")
  
  #bathymetry(dem = FALSE, levels = list(minor = -500, major = seq(0, -5000, by = -1000)), contour.cex=c(0,0), contour.col = my.blues[c(9,9)])
  bathymetry(dem = FALSE, levels = list(minor = -100, major = seq(0, -5000, by = -1000)), contour.cex=c(0,0), contour.col = my.blues[c(2,9)])
  
  ## fix for bug with default coastline resolution
  
  
  coastline(col=grey(0.8), border=grey(0.8))
  map.strata("rv", stratum=c(440:495), labels=TRUE)
  
  
  ## tows as points
  points(x$longitude, x$latitude, col=x$col, pch=19, cex=1)
  
  ## segments between stations
  #for(l in 1:length(oo)){
  #
  #	
  #} ## loop over legs
  if (nrow(x)>1){
    for(i in 1:(nrow(x)-1)){
      segments(x[i,"longitude"], x[i,"latitude"], x[i+1,"longitude"], x[i+1,"latitude"], col=x[i,"col"], lwd=0.75)
    }
  }
  axis(side=1, cex.axis=0.8, padj=-1)
  axis(side=2, cex.axis=0.8, padj=1)
  
  ## label first and last dates
  nr <- nrow(x)
  points(x[c(1,nr),"longitude"], x[c(1,nr),"latitude"], col="black", pch=1, cex=1)
  
  fd <- format(x[1,"datetime"], "%b %d %Y")
  ld <- format(x[nr,"datetime"], "%b %d %Y")
  
  text(x[1,"longitude"]+0.17, x[1,"latitude"]-0.055, fd, cex=0.75)
  text(x[nr,"longitude"]+0.17, x[nr,"latitude"]-0.055, ld, cex=0.75)
  
  
  
  #ti <- paste("Mission ", unique(x$mission), unique(format(x$datetime, "%b %Y")))
  #mtext(side=3, ti, cex=1.1)
  
  box()
  
  ll <- c(paste("Mission", unique(x$mission)), paste(fd, "to", ld), paste(nr, " sets", sep=""), paste(nrow(x[x$experiment %in% c(1,2,5,8),]), " representative sets"), paste(nrow(x[x$experiment==3,]), " null sets"))
  legend("topright", ll, bty="n")
  
  ## matrix for legend
  par(mar=c(1,3,1,0.5))
  mm <- matrix(seq(1,200), nr=200)
  image(t(mm), col=my.cols, axes=F)
  
  ##points(-0.75, 0, pch=19)
  ## what experiment types do we have?
  ee <- unique(x$experiment)
  n.e <- length(ee)
  
  ##oo.e <- order(ee, decreasing=TRUE)
  ##ee.o <- ee[oo.e]
  
  ## are we dealing with a mission with only experiment types 1, 3 and 9?
  #if(all(ee==c(1,3,9))){ x.l <- data.frame(x=ifelse(x$experiment==1,-0.8,ifelse(x$experiment==3, -0.2, 0.5)), t.i=x$t.i) ##,pch=ifelse(x$experiment==1,19,1)
  #}
  #else {
  #if(all(ee!=c(1,3,9))) 
  x.l <- data.frame(
    x=ifelse(x$experiment==1,-0.8,ifelse(x$experiment==2,-0.5,ifelse(x$experiment==3, -0.2, ifelse(x$experiment==5, 0.1, ifelse(x$experiment==8,0.4, 0.7))))), 
    t.i=x$t.i) ##,pch=ifelse(x$experiment==1,19,1)
  #}
  points(x.l$x, ((x.l$t.i)+0.5)/200, pch=19, cex=0.2)
  #text(0,1,"Date1", cex=0.7)
  #text(0,0,"Date2", cex=0.7)
  
  dd.s <- seq(x[1,"datetime"], x[nr,"datetime"], length.out=5)
  dd <- format(dd.s, "%b %d")
  
  axis(side=2, at=seq(0,1,0.25), c(1,0), labels=dd, las=1, cex.axis=0.6)
  
  #if(all(ee==c(1,3,9))){
  #  axis(side=1, at=c(-0.8,-0.2,0.5), labels=FALSE, cex.axis=0.4, tck=-0.05)
  ##  axis(side=3, at=c(-0.8,-0.2,0.5), labels=FALSE, cex.axis=0.4, tck=-0.05)
  #  axis(side=1, at=c(-0.8,-0.2,0.5), labels=c(1,3,9), cex.axis=0.4, padj=-4.3, tck=-0.05)
  ##  axis(side=3, at=c(-0.8,-0.2,0.5), labels=c(1,3,9), cex.axis=0.4, padj=3.5, tck=-0.05)
  #}
  #else {
  axis(side=1, at=c(-0.8,-0.5,-0.2,0.1,0.4,0.7), labels=FALSE, cex.axis=0.4, tck=-0.05)
  axis(side=3, at=c(-0.8,-0.5,-0.2,0.1,0.4,0.7), labels=FALSE, cex.axis=0.4, tck=-0.05)
  axis(side=1, at=c(-0.8,-0.2,0.4), labels=c(1,3,8), cex.axis=0.4, padj=-4.3, tck=-0.05)
  axis(side=3, at=c(-0.5,0.1,0.7), labels=c(2,5,9), cex.axis=0.4, padj=3.5, tck=-0.05)
  #}
  
  
  #mtext(side=1, "experiment", cex=0.3, line=0.8)
  #if(all(ee==c(1,3,9))) {
  #  mtext(side=1, paste("1", "stratified random"), cex=0.5, line=0.3, at=-0.8)
  #  mtext(side=1, paste("3", "unrepresentative (null)"), cex=0.5, line=1.0, at=-0.8)
  #  mtext(side=1, paste("9", "hydrography"), cex=0.5, line=1.7, at=-0.8)
  #}
  #else {
  mtext(side=1, paste("1", "stratified random"), cex=0.3, line=0.3, at=-0.8)
  mtext(side=1, paste("2", "fixed station"), cex=0.3, line=0.65, at=-0.8)
  mtext(side=1, paste("3", "unrepresentative (null)"), cex=0.3, line=1.0, at=-0.8)
  mtext(side=1, paste("5", "comparative"), cex=0.3, line=1.35, at=-0.8)
  mtext(side=1, paste("8", "exploratory"), cex=0.3, line=1.7, at=-0.8)
  mtext(side=1, paste("9", "hydrography"), cex=0.3, line=2.05, at=-0.8)
  #}
  
  
  
} ## end function definition
############################################################################

## colour palettes
my.blues <- c(
  rgb(247/255,251/255,255/255),
  rgb(222/255,235/255,247/255),
  rgb(198/255,219/255,239/255),
  rgb(158/255,202/255,225/255),
  rgb(107/255,174/255,214/255),
  rgb(066/255,146/255,198/255),
  rgb(033/255,113/255,181/255),
  rgb(008/255,081/255,156/255),
  rgb(008/256,048/256,107/256)
)


pal.fire=c(	rgb(255/255, 246/255, 143/255, 1),
            rgb(255/255, 215/255, 0/255, 1),
            rgb(255/255, 100/255, 2/255, 1),
            rgb(221/255, 8/255, 6/255, 1),
            rgb(139/255, 0/255, 0/255, 1))

## from colorbrewer2, diverging, 6 classes
bluetored.fct <- function(n){colorRampPalette(c("#4575b4", "#91bfdb", "#fee090", "#fc8d59", "#d73027"))(n)}


library(Mar.datawrangling)
library(gulf)

get_survey(db='rv',survey="4X",keepBadSets = TRUE,data.dir = data.dir)
#want set, mission, datetime, lat, long
x <- GSINF
names(x) <- tolower(names(x))
colnames(x)[colnames(x)=="sdate"] <- "datetime"
colnames(x)[colnames(x)=="type"] <- "experiment"
x$year <- lubridate::year(x$datetime)
x<- x[,c("year","mission","datetime","experiment","longitude","latitude")]
x <- x[with(x,order(datetime, decreasing = T)),]

missions <- unique(x$mission)
for (m.n in 1:length(missions)){
  x.t <- x[x$mission==missions[m.n],]
  oo <- order(x.t$datetime)
  yr <- unique(x.t$year)
  
  fig.name <- paste(paste("RV-4X", yr, missions[m.n], "cruise-track", sep="-"), ".tiff", sep="")
  
  tiff(file.path(fig.path, fig.name), width=1600, height=1200, compression="lzw", res=300)
  cruise.track.fct(x.t[oo,]) ## x <- x.t[oo,]
  dev.off()
  
}
message("End of Mike fun")
stop()
#x <- read.card(card.type="set")
# x <- rv$set
# 
# x$mission = paste(x$vessel.code, x$cruise.number, sep="")
# ## length(table(x$mission)) ## 56 missions
# ## there are 2 instances where start.hour is NA, remove those
# x <- x[!is.na(x$start.hour),]

# x$datetime <- as.POSIXlt(paste(paste(x$year, x$month, x$day, sep="-"), paste(x$start.hour, x$start.minute, sep=":"), sep=" "))
# 
# x$longitude <- longitude(x)
# x$latitude <- latitude(x)

# uu <- unique(x[,c("year","mission")])
# oo <- order(uu$year, decreasing = TRUE)
# write.csv(uu[oo, c("year","mission")], file=file.path(report.path, "missions.csv"), row.names = F)

##all.x <- x
##x<-all.x
## cruise.number is not a unique identifier of missions (e.g. 204 and 192)
# vars <- c("year","mission","datetime","experiment","longitude","latitude")
for(m.n in uu$mission){ ## m.n <- unique(x$mission)[53]
  x.t <- x[x$mission==m.n,c(vars)]
  oo <- order(x.t$datetime)
  yr <- unique(x.t$year)
  
  fig.name <- paste(paste("RV-4T", yr, m.n, "cruise-track", sep="-"), ".tiff", sep="")
  
  tiff(file.path(fig.path, fig.name), width=1600, height=1200, compression="lzw", res=300)
  cruise.track.fct(x.t[oo,]) ## x <- x.t[oo,]
  dev.off()
  
  fig.name <- paste(paste("RV-4T", yr, m.n, "cruise-track", sep="-"), ".pdf", sep="")
  
  pdf(file.path(fig.path, fig.name), width=8, height=6)
  cruise.track.fct(x.t[oo,]) ## x <- x.t[oo,]
  dev.off()
  
  
}

## LaTex file showing all the individual cruise tracks
tex.fn <- file.path(report.path,"cruisetracks.tex")

l1 <- "% LaTeX file to include all cruise tracks to be included in the Gulf RV long history\n"
cat(l1, file=tex.fn)
oo<-order(uu$year, decreasing=TRUE)
for(m.n in uu$mission[oo]){
  tt <- paste("RV-4T-", m.n, sep="")
  ll <- paste("\\include{", tt, "}\n", sep="")
  cat(ll, file=tex.fn, append=TRUE)
}

## now create the cruise-level LaTeX file from the template
template.tex.fn <- file.path(report.path, "RV-4T-cruisetrack-template.tex")

for(i in 1:nrow(uu)){
  this.fn <- file.path(report.path, paste("RV-4T-", unique(x$mission)[i], ".tex", sep=""))
  
  a <- readLines(template.tex.fn)
  b <- gsub("MISSIONCODE", uu$mission[i], a)
  cc <- gsub("MISSIONYEAR", uu$year[i], b)
  z <- cc
  cat(z, file=this.fn, sep="\n")
}



################################
## now deal with mission with sets that show up as "others" (i.e. not 1 or 3)
table(experiment.str(x$experiment), x$mission)
## 1970, P079 regular survey set (fixed)
## 1971, P091 regular survey set (fixed)
## 1972, P106 regular survey set (fixed)
## 1974, P143 regular survey set (fixed)
## 1975, P157 regular survey set (fixed)
## 1976, P172 regular survey set (fixed)
## 1977, P188 regular survey set (fixed)
## 1978, P204 regular survey set (fixed)
## 1979, P229 regular survey set (fixed)
## 1980, P244 regular survey set (fixed)
## 1981, P260 regular survey set (fixed)
## 1982, P278 regular survey set (fixed)
## 1983, P296 regular survey set (fixed)
## 1984, P312 regular survey set (fixed)
## 1985, P327 regular survey set (fixed)
## 1985, H141 exploratory fishing + regular survey set (fixed)
## 1986, H159 exploratory fishing + regular survey set (fixed)
## 1987, H179 exploratory fishing + regular survey set (fixed) + hydrography
## 1988, H192
## 1992, H245 comparative fishing
## 1992, N176 comparative fishing
## 1994, N210
## 1998, N846 comparative fishing
## 1999, N941 comparative fishing
## 2000, N045 comparative fishing
## 2004, N446 comparative fishing
## 2004, T434 comparative fishing
## 2005, N542 comparative fishing
## 2005, T507 comparative fishing


## 2003, T352 has 2 tows on the Scotian Shelf
## 1973, P122 set number 1 occurred in August? SHOULD BE SEPTEMBER


############################################################################
cruise.track.comp.fct <- function(cruise.track1, cruise.track2, add=FALSE){
  
  mm <- matrix(c(0,0,0,0,1,0,0,2,0), nr=3)
  ll <- layout(mm, widths=c(0.05,0.8,0.15), heights=c(0.01,0.92,0.07))
  
  x1 <- cruise.track1
  x2 <- cruise.track2
  
  
  ## range of dates for these cruise numbers
  rr <- range(c(x1$datetime, x2$datetime))
  
  ## assign time interval to each tow, 200 equal time windows between the first and last tow
  t.i <- seq(rr[1], rr[2], length.out=200)
  x1$t.i <- findInterval(x1$datetime, t.i)
  x2$t.i <- findInterval(x2$datetime, t.i)
  
  # colour code based on the time interval
  my.cols <- bluetored.fct(200)
  
  x1$col <- my.cols[x1$t.i]
  x2$col <- my.cols[x2$t.i]
  
  ## basemap
  my.xlim <- range(c(x1$longitude, x2$longitude)) * c(1.01,0.99)
  my.ylim <- range(c(x1$latitude, x2$latitude)) * c(0.99,1.01)
  
  par(mar=c(0,0,0,0))
  plot(x1$longitude, x1$latitude, type="n", xlim=my.xlim, ylim=my.ylim, axes=FALSE, xlab="", ylab="")
  
  bathymetry(dem = FALSE, levels = list(minor = -100, major = seq(0, -5000, by = -1000)), contour.cex=c(0,0), contour.col = my.blues[c(2,9)])
  
  coastline(col=grey(0.8), border=grey(0.8))
  
  ## tows as points
  points(x1$longitude, x1$latitude, col=x1$col, pch=19, cex=1)
  points(x2$longitude, x2$latitude, col=x2$col, pch=18, cex=1)
  
  ## segments between stations
  if (nrow(x1)>1){
    for(i in 1:(nrow(x1)-1)){
      segments(x1[i,"longitude"], x1[i,"latitude"], x1[i+1,"longitude"], x1[i+1,"latitude"], col=x1[i,"col"], lwd=0.75, lty=1)
    }
  }
  if (nrow(x2)>1){
    for(i in 1:(nrow(x2)-1)){
      segments(x2[i,"longitude"], x2[i,"latitude"], x2[i+1,"longitude"], x2[i+1,"latitude"], col=x2[i,"col"], lwd=0.75, lty=2)
    }
  }
  axis(side=1, cex.axis=0.8, padj=-1)
  axis(side=2, cex.axis=0.8, padj=1)
  
  box()
  
  nr1 <- nrow(x1)
  fd1 <- format(x1[1,"datetime"], "%b %d %Y")
  ld1 <- format(x1[nr1,"datetime"], "%b %d %Y")
  
  nr2 <- nrow(x2)
  fd2 <- format(x2[1,"datetime"], "%b %d %Y")
  ld2 <- format(x2[nr2,"datetime"], "%b %d %Y")
  
  
  #ll1 <- c(paste("Mission", unique(x1$mission)), paste(fd1, "to", ld1), paste(nr1, " sets", sep=""))
  #ll2 <- c(paste("Mission", unique(x2$mission)), paste(fd2, "to", ld2), paste(nr2, " sets", sep=""))
  ll1 <- c(paste("Mission", unique(x1$mission)))
  ll2 <- c(paste("Mission", unique(x2$mission)))
  
  legend("topright", c(ll1,ll2), bty="n", pch=c(19,18), lty=c(1,2))
  
  
  
  ## matrix for legend
  par(mar=c(1,3,1,0.5))
  mm <- matrix(seq(1,200), nr=200)
  image(t(mm), col=rev(my.cols), axes=F)
  
  abline(v=0, lty=1, lwd=0.2)
  
  x1.l <- data.frame(x=ifelse(x1$experiment==1,-0.8,ifelse(x1$experiment==3,-0.55,-0.3)), t.i=x1$t.i) 
  points(x1.l$x, ((x1.l$t.i)+0.5)/200, pch=19, cex=0.2)
  
  x2.l <- data.frame(x=ifelse(x2$experiment==1,0.15,ifelse(x2$experiment==3,0.4,0.65)), t.i=x2$t.i) 
  points(x2.l$x, ((x2.l$t.i)+0.5)/200, pch=19, cex=0.2)
  
  dd.s <- seq(rr[1], rr[2], length.out=5)
  dd <- format(dd.s, "%b %d")
  axis(side=2, at=seq(0,1,0.25), c(1,0), labels=dd, las=1, cex.axis=0.6)
  
  axis(side=1, at=c(-0.5,0.5), labels=c(unique(x1$mission), unique(x2$mission)), cex.axis=0.4, padj=-3, las=1)
  
} ## end function definition
############################################################################


## make a similar figure but with two cruise tracks to show the comparative fishing
## 1985, P327
## 1985, H141
x1.t <- x[x$mission=="P327",c(vars)]
oo1 <- order(x1.t$datetime)
x2.t <- x[x$mission=="H141",c(vars)]
oo2 <- order(x2.t$datetime)

fig.name <- paste(paste("RV-4T-1985-P327-H141", "cruise-track", sep="-"), ".tiff", sep="")
tiff(file.path(fig.path, fig.name), width=1500, height=1200, compression="lzw", res=300)
cruise.track.comp.fct(x1.t[oo1,], x2.t[oo2,])
dev.off()

fig.name <- paste(paste("RV-4T-1985-P327-H141", "cruise-track", sep="-"), ".pdf", sep="")
pdf(file.path(fig.path, fig.name), width=15, height=12)
cruise.track.comp.fct(x1.t[oo1,], x2.t[oo2,])
dev.off()

## 1992, H245 comparative fishing
## 1992, N176 comparative fishing
x1.t <- x[x$mission=="H245",c(vars)]
oo1 <- order(x1.t$datetime)
x2.t <- x[x$mission=="N176",c(vars)]
oo2 <- order(x2.t$datetime)

fig.name <- paste(paste("RV-4T-1992-H245-N176", "cruise-track", sep="-"), ".tiff", sep="")
tiff(file.path(fig.path, fig.name), width=1500, height=1200, compression="lzw", res=300)
cruise.track.comp.fct(x1.t[oo1,], x2.t[oo2,])
dev.off()

fig.name <- paste(paste("RV-4T-1992-H245-N176", "cruise-track", sep="-"), ".pdf", sep="")
pdf(file.path(fig.path, fig.name), width=15, height=12)
cruise.track.comp.fct(x1.t[oo1,], x2.t[oo2,])
dev.off()


## 2004, N446 comparative fishing
## 2004, T434 comparative fishing
x1.t <- x[x$mission=="N446",c(vars)]
oo1 <- order(x1.t$datetime)
x2.t <- x[x$mission=="T434",c(vars)]
oo2 <- order(x2.t$datetime)

fig.name <- paste(paste("RV-4T-2004-N446-T434", "cruise-track", sep="-"), ".tiff", sep="")
tiff(file.path(fig.path, fig.name), width=1500, height=1200, compression="lzw", res=300)
cruise.track.comp.fct(x1.t[oo1,], x2.t[oo2,])
dev.off()

fig.name <- paste(paste("RV-4T-2004-N446-T434", "cruise-track", sep="-"), ".pdf", sep="")
pdf(file.path(fig.path, fig.name), width=15, height=12)
cruise.track.comp.fct(x1.t[oo1,], x2.t[oo2,])
dev.off()


## 2005, N542 comparative fishing
## 2005, T507 comparative fishing
x1.t <- x[x$mission=="N542",c(vars)]
oo1 <- order(x1.t$datetime)
x2.t <- x[x$mission=="T507",c(vars)]
oo2 <- order(x2.t$datetime)

fig.name <- paste(paste("RV-4T-2005-N542-T507", "cruise-track", sep="-"), ".tiff", sep="")
tiff(file.path(fig.path, fig.name), width=1500, height=1200, compression="lzw", res=300)
cruise.track.comp.fct(x1.t[oo1,], x2.t[oo2,])
dev.off()

fig.name <- paste(paste("RV-4T-2005-N542-T507", "cruise-track", sep="-"), ".pdf", sep="")
pdf(file.path(fig.path, fig.name), width=15, height=12)
cruise.track.comp.fct(x1.t[oo1,], x2.t[oo2,])
dev.off()
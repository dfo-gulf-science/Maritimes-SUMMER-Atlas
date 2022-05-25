## single map with tow locations

fn<-file.path(mapping.path, "ScotianShelfStrataUTM83F_dissolve.shp")
SS.strata.mask <- importShapefile(fn, readDBF=TRUE, projection="UTM", zone=19)
SS.strata.mask.LL <- convUL(SS.strata.mask, km=FALSE)
SS.strata.mask.LL$X <- SS.strata.mask.LL$X + 360



presence.fig.fct <- function(spec.code) {
  ## spec.code <- 240
fn <- file.path(figdata.path, paste0("SS",spec.code,"_catch.csv"))
catch.dat <- read.csv(fn, header=TRUE)

my.xlim <- c(291.25,303.75)
my.ylim <- c(41,47.5)

xx.lon<-pretty(my.xlim,n=5)
yy.lat<-pretty(my.ylim, n=4)

rr <- range(catch.dat$YEAR)
yrs.labels <- paste(rr[1],rr[2],sep="-")

my.cols.palette <- c('white','#FEF0D9', '#FDCC8A', '#FC8D59', '#E34A33', '#B30000')

tt <- catch.dat
tt$occ <- ifelse(tt$totno.corr==0,0,1)
pr.occ <- round(mean(tt$occ), digits=3)

data(worldLLhigh)
plotMap(worldLLhigh, my.xlim, my.ylim, col=grey(0.9),plt=c(0.1,1.0,0.1,1.0),border=grey(0.7),axes=FALSE,tckLab=FALSE,xlab="",ylab="")
text(293.2,46.6,yrs.labels,cex=1.5)
text(293.2,46.1,paste("P(occ) = ",pr.occ,sep=""),cex=1.25)
text(293.2,45.8,paste("n = ",sum(tt$occ), " catches" ,sep=""),cex=1.25)

addPolys(SS.strata.mask.LL)
ttt <- subset(tt, occ==1)
points(360+ttt$lon, ttt$lat, type='p', pch=3, col='red')

par(las=1)
axis(side=1, at=xx.lon[2:7], labels=paste(360-xx.lon[2:7],"\u{B0}W",sep=""))
par(las=2)
axis(side=2, at=yy.lat, labels=paste(yy.lat,"\u{B0}N",sep=""))



box()

} # end function

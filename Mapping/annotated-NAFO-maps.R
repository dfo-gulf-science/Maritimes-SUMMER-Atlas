##

## NAFO divisions extracted from the gulf package and saved locally as a CSV
# library(gulf)
# nafo.polys <- nafo.polygons(c("4V","4W","4X"))
# index <- which(nafo.polys[[li]]$coastline==FALSE)
# 
# 
# polys.df <- 
# do.call(
#         rbind,
# lapply(1:4, 
#        function(li){
#                ix<-which(nafo.polys[[li]]$coastline==FALSE)
#                df <- data.frame(name=nafo.polys[[li]]$label, longitude=nafo.polys[[li]]$x[ix], latitude=nafo.polys[[li]]$y[ix])
#                return(df)
#        }
# )
# )
# 
# write.csv(polys.df, file=file.path(mapping.path, "nafos.csv"))
# 
##  script to make the different maps that appear in the Tech Report
before <- ls()

polys.df <- read.csv(file.path(mapping.path, "nafos.csv"))
## fix order for 4X so as not to get a line across NS and the Bay of Fundy
polys.df[polys.df$name=="4X",] <- polys.df[polys.df$name=="4X",][c(9:14,1:8),]
polys.df[polys.df$name=="4W",] <- polys.df[polys.df$name=="4W",][c(5:9,1:4),]

data(worldLLhigh)
gebco <- raster(file.path(mapping.path, "GEBCO-Scotia-Fundy.nc"))

## keep only the region to plot
y <- extent(291.5,303.5,41.5,48)
gebco <- crop(gebco, y)

# Colour scheme
blue.col <- colorRampPalette(c("darkblue", "lightblue"))
yellow.col <- colorRampPalette(c("lightyellow", "orange"))
# Break points
br <- c(-15000, -10000, -5000, seq(from=-1000, to=1000, by=50))

my.cols <- c(blue.col(23), yellow.col(20))

## annotated map of the region showing NAFO divisions
f1.n <- file.path(mapping.path,"annotated-map-NAFO.png")

png(file=f1.n, width=900, height=668)
par(mar=c(4.5,4.5,1,0.1))
# Plot
plot(gebco, 
     col=my.cols, 
     breaks=br, 
     legend=FALSE,
     xlab="Longitude (\u{B0}W)",
     ylab="Latitude (\u{B0}N)",
     cex.lab=2,
     cex.axis=1.5
     )
#contour(gebco, levels=c(-100,-500,-1000),add=TRUE)
legend(301.2,43.45, 
       title="Elevation / Depth (m)", 
       legend=c("more than 500","50-250","0-50","0-50","50-100","100-150","150-200","250-500","500-1000"," more than 1000"),
       fill=my.cols[c(40,34,25,23,21,19,17,15,10,4)]
         )

## NAFO Divisions
lines(360+polys.df[polys.df$name=="4X",c("longitude")], polys.df[polys.df$name=="4X",c("latitude")], col="white", lty=1, lwd=2)
lines(360+polys.df[polys.df$name=="4W",c("longitude")], polys.df[polys.df$name=="4W",c("latitude")], col="white", lty=1, lwd=2)
lines(360+polys.df[polys.df$name=="4Vn",c("longitude")], polys.df[polys.df$name=="4Vn",c("latitude")], col="white", lty=1, lwd=2)
lines(360+polys.df[polys.df$name=="4Vs",c("longitude")], polys.df[polys.df$name=="4Vs",c("latitude")], col="white", lty=1, lwd=2)
text(295.5,42,"4X",cex=2,col="white")
text(299,42.75,"4W",cex=2,col="white")
text(302.5,43.8,"4Vs",cex=2,col="white")
text(300.8,46.3,"4Vn",cex=2,col="white")


#plotMap(worldLLhigh, xlim=c(291.5,303.5), ylim=c(41.5,48), col=grey(0.8), plt=c(0.1,0.9,0.1,0.9),border='black',axes=TRUE,tckLab=FALSE,xlab="",ylab="", add=T)
text(296.5,45, "Nova Scotia", font=2, cex=1.5)
text(293,46.5, "New Brunswick", font=2, cex=1.5)
text(297,46.3, "PEI", font=2, cex=1.5)
text(302,46.4, "Laurentian Channel", srt=310, cex=1.3)
text(300.25,44.75, "Eastern Scotian Shelf", cex=1.3)
text(296.05,43.4, "Western Scotian Shelf", cex=1.3)
text(293.8,44.75, "Bay of Fundy", srt=45, cex=1.3)
text(293.0,41.8, "Georges Bank", cex=1.3)
text(293.55,42.6, "Fundian Channel", cex=1.3)
dev.off()

## try using ggplot
library(ggplot2)
g <- ggplot(data=as.data.frame(gebco,xy = TRUE)) + 
        geom_raster(aes(x=x,y=y,fill = Elevation.relative.to.sea.level)) +
        scale_fill_viridis_c()

## copy the files to the Technical Report folder
file.copy(c(f1.n), file.path(actualreport.path, "figures"), overwrite=TRUE)

rm(list= ls()[!(ls() %in% before)]) ## clean up after ourselves

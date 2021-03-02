##
##  script to make the different maps that appear in the Tech Report
before <- ls()

data(worldLLhigh)
gebco <- raster(file.path(mapping.path, "GEBCO-Scotia-Fundy.nc"))

# Colour scheme
blue.col <- colorRampPalette(c("darkblue", "lightblue"))
yellow.col <- colorRampPalette(c("lightyellow", "orange"))
# Break points
br <- c(-15000, -10000, -5000, seq(from=-1000, to=1000, by=50))


## annotated map of the region showing NAFO divisions
f1.n <- file.path(mapping.path,"annotated-map-NAFO.png")

png(file=f1.n, width=900, height=900)
# Plot
plot(gebco, col=c(blue.col(23), yellow.col(20)), breaks=br, xlim=c(291.5,303.5), ylim=c(41.5,48))
#plotMap(worldLLhigh, xlim=c(291.5,303.5), ylim=c(41.5,48), col=grey(0.8), plt=c(0.1,0.9,0.1,0.9),border='black',axes=TRUE,tckLab=FALSE,xlab="",ylab="", add=T)
text(296.5,45, "Nova Scotia", font=2)
text(293,46.5, "New Brunswick", font=2)
text(297,46.3, "PEI", font=2)
text(302,46.4, "Laurentian Channel", srt=315)
text(300,45, "Eastern Scotian Shelf")
text(296,43.5, "Western Scotian Shelf")
text(293.8,44.75, "Bay of Fundy", srt=45)
text(293.0,41.8, "Georges Bank")
text(293.5,42.5, "Fundian Channel")
dev.off()



## copy the files to the Technical Report folder
file.copy(c(f1.n), file.path(actualreport.path, "figures"), overwrite=TRUE)

rm(list= ls()[!(ls() %in% before)]) ## clean up after ourselves

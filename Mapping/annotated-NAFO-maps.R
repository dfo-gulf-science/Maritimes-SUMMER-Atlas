##
##  script to make the different maps that appear in the Tech Report
before <- ls()

data(worldLLhigh)

## annotated map of the region showing NAFO divisions
f1.n <- file.path(mapping.path,"annotated-map-NAFO.png")

png(file=f1.n, width=900, height=900)
plotMap(worldLLhigh, xlim=c(291.5,303.5), ylim=c(41.5,48), col=grey(0.8), plt=c(0.1,0.9,0.1,0.9),border='black',axes=TRUE,tckLab=FALSE,xlab="",ylab="")

text(296.5,45, "Nova Scotia")
text(293,46.5, "New Brunswick")
text(297,46.25, "PEI")
text(302,46.5, "Laurentian Channel")
text(300,45, "Eastern Scotian Shelf")
text(296,43.5, "Western Scotian Shelf")
text(293.5,44.75, "Bay of Fundy")
text(292.5,42, "Georges Bank")
text(293.5,42.5, "Fundian Channel")
dev.off()


## map of the SUMMER strata
f2.n <- file.path(mapping.path,"SUMMER-strata-map.png")

png(file=f2.n, width=900, height=900)
plotMap(worldLLhigh, xlim=c(291.5,303.5), ylim=c(41.5,48), col=grey(0.8), plt=c(0.1,0.9,0.1,0.9),border='black',axes=TRUE,tckLab=FALSE,xlab="",ylab="")

dev.off()


## map of the tow locations
f3.n <- file.path(mapping.path,"SUMMER-tows-map.png")

png(file=f3.n, width=900, height=900)
plotMap(worldLLhigh, xlim=c(291.5,303.5), ylim=c(41.5,48), col=grey(0.8), plt=c(0.1,0.9,0.1,0.9),border='black',axes=TRUE,tckLab=FALSE,xlab="",ylab="")

dev.off()


## copy the files to the Technical Report folder
file.copy(c(f1.n,f2.n,f3.n), file.path(actualreport.path, "figures"), overwrite=TRUE)

rm(list= ls()[!(ls() %in% before)]) ## clean up after ourselves

##  script to make the different maps that appear in the Tech Report
before <- ls()

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
file.copy(c(f2.n,f3.n), file.path(actualreport.path, "figures"), overwrite=TRUE)

rm(list= ls()[!(ls() %in% before)]) ## clean up after ourselves

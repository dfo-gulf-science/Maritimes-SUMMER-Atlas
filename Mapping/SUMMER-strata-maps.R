##  script to make the different maps that appear in the Tech Report
before <- ls()

## map of the SUMMER strata
f2.n <- file.path(mapping.path,"SUMMER-strata-map.png")

fn<-file.path(mapping.path, "ScotianShelfStrataUTM83F.shp")
SS.strata <- importShapefile(fn, readDBF=TRUE, projection="UTM", zone=19)

SS.strata.LL <- convUL(SS.strata, km=FALSE)
SS.strata.LL$X <- SS.strata.LL$X 

SS.strata.LL2 <- SS.strata.LL
SS.strata.LL2$X <- SS.strata.LL2$X + 360

my.pids <- unique(SS.strata.LL$PID)
strata.list <- lapply(my.pids, function(i){subset(SS.strata.LL, PID == i)})


png(file=f2.n, width=900, height=900)
data(worldLLhigh)
plotMap(worldLLhigh, xlim=c(291.5,303.5), ylim=c(41.5,48), col=grey(0.8), plt=c(0.1,0.9,0.1,0.9),border='black',axes=TRUE,tckLab=FALSE,xlab="",ylab="")
addPolys(SS.strata.LL2)
dev.off()


## map of the tow locations
f3.n <- file.path(mapping.path,"SUMMER-tows-map.png")

qu <- paste("
SELECT 
mission,
setno,
strat,
sdate,
TO_CHAR(sdate,'yyyy') YEAR,
TO_CHAR(sdate,'mm') MONTH,
TO_CHAR(sdate,'dd') DAY,
-1*ROUND(TRUNC(SLONG/100)+MOD(SLONG,100)/60,5) SLO,
ROUND(TRUNC(SLAT/100)+MOD(SLAT,100)/60,5) SLA
FROM groundfish.gsinf
where
type=1
order by YEAR, mission, setno
", sep="")

tows.df <- sqlQuery(chan, qu)

summer.tows.df <- subset(
  tows.df, 
  (STRAT == '440' | STRAT == '441' | STRAT == '442' | STRAT == '443' | STRAT == '444' | STRAT == '445' | STRAT == '446' | STRAT == '447' | STRAT == '448' | STRAT == '449' | 
     STRAT == '450' | STRAT == '451' | STRAT == '452' | STRAT == '453' | STRAT == '454' | STRAT == '455' | STRAT == '456' | STRAT == '457' | STRAT == '458' | STRAT == '459' | 
     STRAT == '460' | STRAT == '461' | STRAT == '462' | STRAT == '463' | STRAT == '464' | STRAT == '465' | STRAT == '466' | 
     STRAT == '470' | STRAT == '471' | STRAT == '472' | STRAT == '473' | STRAT == '474' | STRAT == '475' | STRAT == '476' | STRAT == '477' | STRAT == '478' | 
     STRAT == '480' | STRAT == '481' | STRAT == '482' | STRAT == '483' | STRAT == '484' | STRAT == '485' | 
     STRAT == '490' | STRAT == '491' | STRAT == '492' | STRAT == '493' | STRAT == '494' | STRAT == '495' ) & (MONTH == 6 | MONTH == 7 | MONTH == 8)
)


png(file=f3.n, width=900, height=900)
plotMap(worldLLhigh, xlim=c(291.5,303.5), ylim=c(41.5,48), col=grey(0.8), plt=c(0.1,0.9,0.1,0.9),border='black',axes=TRUE,tckLab=FALSE,xlab="",ylab="")
points(360+summer.tows.df$SLO, summer.tows.df$SLA, pch=19, cex=0.5)
dev.off()


## copy the files to the Technical Report folder
file.copy(c(f2.n,f3.n), file.path(actualreport.path, "figures"), overwrite=TRUE)

rm(list= ls()[!(ls() %in% before)]) ## clean up after ourselves

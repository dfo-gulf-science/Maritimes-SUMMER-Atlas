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


png(file=f2.n, width=900, height=668)
data(worldLLhigh)
plotMap(worldLLhigh, xlim=c(291.5,303.5), ylim=c(41.5,48), col=grey(0.8), plt=c(0.1,0.98,0.1,0.98),border='black',axes=TRUE,tckLab=FALSE,xlab="",ylab="")
addPolys(SS.strata.LL2)
## annotate with strata numbers


dev.off()


## use Mike McMahon's Maritimes.data package and ggplot instead
library(Mar.data)
library(sf)
library(ggplot2)
library(tidyverse)

data(Strata_Mar_sf)

boundaries <- read_sf(file.path(main.path, "AC/AC_1M_BoundaryPolygons_shp/AC_1M_BoundaryPolygons.shp"))

boundaries_simple <- boundaries %>%
  filter(
    POL_DIV %in% c(
      "Quebec", "Newfoundland and Labrador" ,
      #"New York", "New Hampshire", "Vermont",
      "Maine",
      "New Brunswick", "Nova Scotia",
      "Prince Edward Island"
    ),
    SELECTION == "sparse" #"dense"
  ) %>%
  st_transform(4326)

#strata.labels <- rbind(
#  data.frame(x=c(-58.6),y=c(46.5),text=c("440"))
#)



g <- ggplot(data = Strata_Mar_sf[Strata_Mar_sf$StrataID %in% c(440:495),]) + 
  geom_sf(data=boundaries_simple, fill="cornsilk", color=grey(0.8)) +
  geom_sf(fill="salmon") +  
  # geom_text(data=strata.labels, aes(x=x, y=y, label=text)) + 
  #geom_sf_label(aes(label = StrataID), size=2, alpha=0.5) +
  geom_sf_label(aes(label = StrataID), size=2.5, col="black", fontface = "bold", alpha=1) + 
  theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.5), panel.background = element_rect(fill = "powderblue")) + #, panel.border=element_rect(linetype="solid")
  xlim(-68,-57) + ylim(41.9,47) +
  xlab("Longitude (\u{B0}W)") + ylab("Latitude (\u{B0}N)")

f4.n <- file.path(mapping.path, "SUMMER-strata-map-sf.png")
ggsave(f4.n, g)

## try a map with depth
gebco <- raster(file.path(mapping.path, "GEBCO-Scotia-Fundy.nc"))
## keep only the region to plot
y <- extent(291.5,303.5,41.5,48)
gebco <- crop(gebco, y)

my.df <- as.data.frame(gebco,xy = TRUE)
my.df$z <- (my.df$Elevation.relative.to.sea.level)

# Colour scheme
blue.col <- colorRampPalette(c("darkblue", "lightblue"))
yellow.col <- colorRampPalette(c("lightyellow", "orange"))

g <- ggplot(data=my.df) + 
  geom_raster(aes(x=-1*(360-x),y=y, fill=z)) + 
  scale_fill_gradient2(low="darkblue", mid="lightblue", high="orange",midpoint=0) 

# 
# +
#   geom_sf(data = Strata_Mar_sf[Strata_Mar_sf$StrataID %in% c(440:495),], fill=grey(0.9)) +
#   geom_sf(data=boundaries_simple, fill=grey(0.8), color=grey(0.3)) + 
#   xlab("Longitude (\u{B0}W)") + ylab("Latitude (\u{B0}N)") + xlim(-68,-57) + ylim(41.9,47)
# 
# g +  coord_sf(expand = FALSE)

## how do the strata boundaries match the corresponding isobaths from the GEBCO grid?


  
## map of the tow locations
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

f3.n <- file.path(mapping.path,"SUMMER-tows-map.png")


png(file=f3.n, width=900, height=900)
plotMap(worldLLhigh, xlim=c(291.5,303.5), ylim=c(41.5,48), col=grey(0.8), plt=c(0.1,0.9,0.1,0.9),border='black',axes=TRUE,tckLab=FALSE,xlab="",ylab="")
points(360+summer.tows.df$SLO, summer.tows.df$SLA, pch=19, cex=0.5)
dev.off()

## tow locations using ggplot
g <- ggplot(boundaries_simple) +
  geom_sf(fill="cornsilk", color=grey(0.8)) +
  geom_sf(data = Strata_Mar_sf[Strata_Mar_sf$StrataID %in% c(440:495),], fill="salmon",alpha=0.1) +
  geom_point(data=summer.tows.df, aes(SLO,SLA), pch=19, cex=0.2) +
  theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.5), panel.background = element_rect(fill = "powderblue")) +
  xlim(-68,-57) + ylim(41.9,47) +
  xlab("Longitude (\u{B0}W)") + ylab("Latitude (\u{B0}N)")

f5.n <- file.path(mapping.path, "SUMMER-tows-map-sf.png")
ggsave(f5.n, g)


## copy the files to the Technical Report folder
file.copy(c(f2.n,f3.n,f4.n,f5.n), file.path(actualreport.path, "figures"), overwrite=TRUE)

rm(list= ls()[!(ls() %in% before)]) ## clean up after ourselves

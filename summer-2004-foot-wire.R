## checking the effect of having that 19' foot-wire on the trawl catches
## - following on conversation with Bill MacEachern

## details about the 2004 summer survey

qu <- paste0(
  "
  SELECT 
  *
  FROM
  GROUNDFISH.GSINF
  WHERE
  MISSION IN ('TEL2004529','TEL2004530')
  "
)

sets <- sqlQuery(chan,qu)

plot(sets$SDATE, as.factor(sets$TYPE), ylim=c(0,3), pch=19)
abline(v=as.Date("2004-07-18 00:00:00"))

library(gulf)
sets$longitude <- -1*dmm2deg(sets$SLONG)
sets$latitude <- dmm2deg(sets$SLAT)

sets$leg <- ifelse(sets$SDATE<as.Date("2004-07-18 00:00:00"),1,2)


## one leg had the wire and the other didn't, so let's compare the catches of things that
## the net would catch by scrapping on the bottom

## map showing the location of the tows in the 2 legs
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

Strata_Mar_sf$centroid <- st_centroid(Strata_Mar_sf)


g <- ggplot(data = Strata_Mar_sf[Strata_Mar_sf$StrataID %in% c(440:495),]) + 
  geom_sf(data=boundaries_simple, fill="cornsilk", color=grey(0.8)) +
#  geom_sf(fill="salmon") +  
  theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.5), panel.background = element_rect(fill = "powderblue")) + #, panel.border=element_rect(linetype="solid")
  geom_point(data=sets, aes(x=longitude, y=latitude, colour=as.factor(leg))) +
  xlim(-68,-57) + ylim(41.9,47) +
  xlab("Longitude (\u{B0}W)") + ylab("Latitude (\u{B0}N)")

f1.n <- file.path(mapping.path, "Foot-wire-2004-locations-map.png")
ggsave(f1.n, g)


## now bring the catch
qu <- paste0(
  "
  SELECT
  I.*,
  C.*
  FROM
  GROUNDFISH.GSINF I, 
  GROUNDFISH.GSCAT C
  WHERE
  I.MISSION=C.MISSION AND 
  I.SETNO=C.SETNO AND
  I.MISSION IN ('TEL2004529','TEL2004530')
  "
)

catches <- sqlQuery(chan,qu)


catches$leg <- ifelse(catches$SDATE<as.Date("2004-07-18 00:00:00"),1,2)


tt <- as.data.frame.matrix(table(catches$SPEC,catches$leg))
tt$prop.leg2 <- tt$`2` / (tt$`1` + tt$`2`)

species <- row.names(tt)[tt$prop.leg2>0.6]

qu <- paste0(
  "
  SELECT 
  * 
  FROM 
  GROUNDFISH.GSSPECIES
  WHERE CODE IN
  (
  ",
  paste0(species,collapse=","),
  ")"
)
spec.df <- sqlQuery(chan,qu)

## what would we expect to catch with the belly dragging on the bottom?


catches$longitude <- -1*dmm2deg(catches$SLONG)
catches$latitude <- dmm2deg(catches$SLAT)

catches$leg <- ifelse(catches$SDATE<as.Date("2004-07-18 00:00:00"),1,2)


## sand dollar 6500


g <- ggplot(data = Strata_Mar_sf[Strata_Mar_sf$StrataID %in% c(440:495),]) + 
  geom_sf(data=boundaries_simple, fill="cornsilk", color=grey(0.8)) +
  #  geom_sf(fill="salmon") +  
  theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.5), panel.background = element_rect(fill = "powderblue")) + #, panel.border=element_rect(linetype="solid")
  geom_point(data=catches[catches$SPEC==6500,], aes(x=longitude, y=latitude, colour=as.factor(leg))) +
  xlim(-68,-57) + ylim(41.9,47) +
  xlab("Longitude (\u{B0}W)") + ylab("Latitude (\u{B0}N)")

f2.n <- file.path(mapping.path, "Foot-wire-2004-sand-dollar-catches-map.png")
ggsave(f2.n, g)


## sea stars 6100

## alligatorfish
## daubed shanny


## do sand dollar for a number of years now, to see where sand dollars were caught


## what sand dollars are caught in years after 2004?



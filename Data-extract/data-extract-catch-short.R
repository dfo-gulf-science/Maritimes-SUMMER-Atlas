## extract tow-level catch data including catches of zero
##
#spec.num <- 610
extract.catch.short.fct <- function(spec.num) {

# survey data
## bring back a data frame with tows and their associated environmental covariates
qu <- paste("
SELECT 
mission,
setno,
strat,
sdate,
TO_CHAR(sdate,'yyyy') YEAR,
TO_CHAR(sdate,'mm') MONTH,
TO_CHAR(sdate,'dd') DAY,
dmin,
dmax,
bottom_temperature,
bottom_salinity,
dist,
gear,
-1*ROUND(TRUNC(SLONG/100)+MOD(SLONG,100)/60,5) SLO,
ROUND(TRUNC(SLAT/100)+MOD(SLAT,100)/60,5) SLA
FROM groundfish.gsinf
where
type=1 and
TO_CHAR(sdate,'yyyy') >= 1999
order by YEAR, mission, setno
", sep="")

tows.df <- sqlQuery(chan, qu)
tows.df$depth <- (tows.df$DMIN + tows.df$DMAX) /2
tows.df$decade <- floor(tows.df$YEAR/10)

## SUMMER survey, strata 440 to 495
summer.tows.df <- subset(
tows.df, 
(STRAT == '440' | STRAT == '441' | STRAT == '442' | STRAT == '443' | STRAT == '444' | STRAT == '445' | STRAT == '446' | STRAT == '447' | STRAT == '448' | STRAT == '449' | 
STRAT == '450' | STRAT == '451' | STRAT == '452' | STRAT == '453' | STRAT == '454' | STRAT == '455' | STRAT == '456' | STRAT == '457' | STRAT == '458' | STRAT == '459' | 
STRAT == '460' | STRAT == '461' | STRAT == '462' | STRAT == '463' | STRAT == '464' | STRAT == '465' | STRAT == '466' | 
STRAT == '470' | STRAT == '471' | STRAT == '472' | STRAT == '473' | STRAT == '474' | STRAT == '475' | STRAT == '476' | STRAT == '477' | STRAT == '478' | 
STRAT == '480' | STRAT == '481' | STRAT == '482' | STRAT == '483' | STRAT == '484' | STRAT == '485' | 
STRAT == '490' | STRAT == '491' | STRAT == '492' | STRAT == '493' | STRAT == '494' | STRAT == '495' ) & (MONTH == 6 | MONTH == 7 | MONTH == 8)
)

# species
qu <- paste("
SELECT 
i.mission,
i.setno,
i.strat,
i.sdate,
TO_CHAR(i.sdate,'yyyy') YEAR,
TO_CHAR(i.sdate,'mm') MONTH,
TO_CHAR(i.sdate,'dd') DAY,
c.spec,
s.SPEC SCIEN,
s.comm,
c.totno,
c.totwgt,
i.dmin,
i.dmax,
i.bottom_temperature,
i.bottom_salinity,
i.dist,
i.gear,
c.totno * (1.75/i.dist) as totnocorr,
c.totwgt * (1.75/i.dist) as totwgtcorr
FROM 
groundfish.gsinf i,
groundfish.gscat c,
groundfish.GSSPECIES s
where
i.type=1 and
TO_CHAR(i.sdate,'yyyy') >= 1999 AND
i.mission = c.mission AND
i.setno = c.setno AND
s.CODE=c.spec and
S.CODE='",spec.num,"'
order by YEAR, i.mission, i.setno
", sep="")

spec.df <- sqlQuery(chan, qu)

merged.df <- merge(spec.df, summer.tows.df, 
                   by=c("MISSION", "SETNO"), 
                   all.y=TRUE)[,c(1,2,8,9,10,11,12,19,
                                  20,21,23,24,25,26,27,28,29,32,33,34,35,30)]

merged.df[is.na(merged.df$SPEC),]$SPEC <- spec.num
merged.df[is.na(merged.df$COMM),]$COMM <- unique(merged.df[!is.na(merged.df$COMM),]$COMM)
merged.df[is.na(merged.df$SCIEN),]$SCIEN <- unique(merged.df[!is.na(merged.df$SCIEN),]$SCIEN)

merged.df[is.na(merged.df$TOTNO),]$TOTNO <- 0
merged.df[is.na(merged.df$TOTWGT),]$TOTWGT <- 0
merged.df[is.na(merged.df$TOTNOCORR),]$TOTNOCORR <- 0
merged.df[is.na(merged.df$TOTWGTCORR),]$TOTWGTCORR <- 0

names(merged.df) <- c("mission","setno","spec","scien","comm","totno","totwgt","totno.corr","totwgt.corr","Strata","YEAR","month","day","dmin","dmax","temperature","salinity","lon","lat","DEPTH","decade","dist")
##
##
tt <- droplevels(merged.df)
merged.df <- tt

return(merged.df)

} # end function


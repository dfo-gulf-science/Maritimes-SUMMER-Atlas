## cumulative frequency distribution of environmental parameters
##
## make a figure that shows the cumulative frequency distribution of 3 environmetal parameters for the summer survey
## 1 - depth
## 2 - bottom temperature
## 3 - bottom salinity
##
## following methods from Perry and Smith

## get set data 
qu <- paste("
SELECT 
mission,
setno,
strat,
sdate,
TO_CHAR(sdate,'yyyy') YEAR,
TO_CHAR(sdate,'mm') MONTH,
TO_CHAR(sdate,'dd') DAY,
DMIN,
DMAX,
BOTTOM_TEMPERATURE,
BOTTOM_SALINITY
FROM groundfish.gsinf
where
type=1
order by YEAR, mission, setno
", sep="")

tows.df <- sqlQuery(chan, qu)

## keep only representative tows from the summer survey
summer.tows.df <- subset(
  tows.df, 
  (STRAT == '440' | STRAT == '441' | STRAT == '442' | STRAT == '443' | STRAT == '444' | STRAT == '445' | STRAT == '446' | STRAT == '447' | STRAT == '448' | STRAT == '449' | 
     STRAT == '450' | STRAT == '451' | STRAT == '452' | STRAT == '453' | STRAT == '454' | STRAT == '455' | STRAT == '456' | STRAT == '457' | STRAT == '458' | STRAT == '459' | 
     STRAT == '460' | STRAT == '461' | STRAT == '462' | STRAT == '463' | STRAT == '464' | STRAT == '465' | STRAT == '466' | 
     STRAT == '470' | STRAT == '471' | STRAT == '472' | STRAT == '473' | STRAT == '474' | STRAT == '475' | STRAT == '476' | STRAT == '477' | STRAT == '478' | 
     STRAT == '480' | STRAT == '481' | STRAT == '482' | STRAT == '483' | STRAT == '484' | STRAT == '485' | 
     STRAT == '490' | STRAT == '491' | STRAT == '492' | STRAT == '493' | STRAT == '494' | STRAT == '495' ) & (MONTH == 6 | MONTH == 7 | MONTH == 8)
)

## depth
table(is.na(summer.tows.df$DMIN))
table(is.na(summer.tows.df$DMAX))

depth.df <- summer.tows.df[which(!is.na(summer.tows.df$DMIN)),]
depth.df$DEPTH <- (summer.tows.df$DMIN+summer.tows.df$DMAX) / 2

order(depth.df$DEPTH)

## bottom temperature
table(is.na(summer.tows.df$BOTTOM_TEMPERATURE))

## bottom salinty
table(is.na(summer.tows.df$BOTTOM_SALINITY))


## figure with overall cumulative distribution of depth
## figure with overall cumulative distribution of bottom temperature
## figure with overall cumulative distribution of bottom salinity


## overlay decadal


## 


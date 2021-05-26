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
depth.df$DEPTH <- (depth.df$DMIN+depth.df$DMAX) / 2

# order(depth.df$DEPTH)


st <- read.csv(file.path(actualreport.path, "strata-statistics.csv"))
st <- st[which(st$STRAT %in% c(440:466, 470:478, 480:485, 490:495)),]

x <- depth.df
x$unique.id <- paste0(x$MISSION, "-", x$SETNO)

##
## following Table 1 in Perry and Smith (1994)
# nh = number of hauls or sets in stratum h (h = 1, . . ., L)
nh <- aggregate(unique.id~STRAT, data=x, length)
st <- merge(st, nh, by="STRAT")
st$nh <- st$unique.id


## n = total number of hauls
n <- nrow(x)

# Nh, = total number of possible sets in stratum h
# a.k.a. number of trawlable units
# swept area of a 30-minute WIIa set, 
swept.area <- 4E-02 ## NOT THE REAL VALUE, check this
st$Nh <- st$AREA / swept.area

# N = total number of possible sets overall
N <- sum(st$Nh)

# Wh = proportion of the survey area in stratum h
st$Wh <- st$AREA / sum(st$AREA)
## in Perry and Smith, the proportion is computed as Nh/N 
## (st$Wh - st$Nh / N) < 1E-12 ## the numbers numerically differ in R, but they are the same

depths <- seq(min(x$DEPTH), max(x$DEPTH), length.out=200)

cdf.df <- expand.grid(depth=depths, cdf=NA) ## CDF for sets

for(t in depths){
  this.depth.cdf <- 0
  
  ## loop over strata
  for(h in unique(x$STRAT)){
    ## number of unique tows in this stratum
    t.df <- x[x$STRAT==h,]
    ## loop over sets in each stratum
    for(i in 1:nrow(t.df)){
      this.set.cdf <- ifelse(t.df[i,"DEPTH"]<=t,1,0) * (st[st$STRAT==h,"Wh"] / st[st$STRAT==h,"nh"])
      this.depth.cdf <- this.depth.cdf + this.set.cdf
    }## end loop over sets
  }## end loop over strata
  
  
  cdf.df[cdf.df$depth==t,"cdf"] <- this.depth.cdf
  
}## end loop over depths

out.df <- cdf.df
names(out.df) <- c("depth","cdf.depth")

library(ggplot2)
g <- ggplot(out.df, aes(x=depth, y=cdf.depth)) +
  geom_line() +
  theme_bw()
g

ggsave(g, file=file.path(actualreport.path, "figures/summer-depth-CDF.png"))


############################################
############################################
## now do by decade
depth.df <- summer.tows.df[which(!is.na(summer.tows.df$DMIN)),]
depth.df$DEPTH <- (depth.df$DMIN+depth.df$DMAX) / 2

# order(depth.df$DEPTH)


st <- read.csv(file.path(actualreport.path, "strata-statistics.csv"))
st <- st[which(st$STRAT %in% c(440:466, 470:478, 480:485, 490:495)),]

# Wh = proportion of the survey area in stratum h
st$Wh <- st$AREA / sum(st$AREA)
## in Perry and Smith, the proportion is computed as Nh/N 
## (st$Wh - st$Nh / N) < 1E-12 ## the numbers numerically differ in R, but they are the same


x <- depth.df
x$unique.id <- paste0(x$MISSION, "-", x$SETNO)

x$decade <- ceiling((x$YEAR - 1969)/10)
table(x$decade, x$YEAR)

## by decade
decades <- 1:6

## following Table 1 in Perry and Smith (1994)
# nh = number of hauls or sets in stratum h (h = 1, . . ., L)
nh <- aggregate(unique.id~STRAT+decade, data=x, length)
st <- merge(st, nh, by=c("STRAT"))
st$nh <- st$unique.id


## n = total number of hauls by decade
n <- aggregate(unique.id~decade, data=x, length)

# Nh, = total number of possible sets in stratum h
# a.k.a. number of trawlable units
# swept area of a 30-minute WIIa set, 
swept.area <- 4E-02 ## NOT THE REAL VALUE, check this
st$Nh <- st$AREA / swept.area

# N = total number of possible sets overall by decade
N <- aggregate(Nh~decade, data=st, sum)[1,2]


depths <- seq(min(x$DEPTH), max(x$DEPTH), length.out=200)

cdf.df <- expand.grid(decade=decades,depth=depths, cdf=NA) ## CDF for sets

for(d in decades){

  for(t in depths){
    this.depth.cdf <- 0
    
    ## loop over strata
    for(h in unique(x$STRAT)){
      ## number of unique tows in this stratum
      t.df <- x[x$STRAT==h & x$decade==d,]
      ## loop over sets in each stratum
      for(i in 1:nrow(t.df)){
        this.set.cdf <- ifelse(t.df[i,"DEPTH"]<=t,1,0) * (st[st$STRAT==h & st$decade==d,"Wh"] / st[st$STRAT==h & st$decade==d,"nh"])
        this.depth.cdf <- this.depth.cdf + this.set.cdf
      }## end loop over sets
    }## end loop over strata
    
    cdf.df[cdf.df$decade==d & cdf.df$depth==t,"cdf"] <- this.depth.cdf
    
  }## end loop over depths
  
}## end the loop over decades

out.df <- cdf.df
names(out.df) <- c("decade","depth","cdf.depth")


g <- ggplot(out.df, aes(x=depth, y=cdf.depth)) +
  geom_line(aes(lty=as.factor(decade)))

g
## this is pointless for depth, there are no differences between decades


## bottom temperature
table(is.na(summer.tows.df$BOTTOM_TEMPERATURE))


## bottom salinty
table(is.na(summer.tows.df$BOTTOM_SALINITY))


## figure with overall cumulative distribution of depth, bottom temperature and bottom salinity


## 


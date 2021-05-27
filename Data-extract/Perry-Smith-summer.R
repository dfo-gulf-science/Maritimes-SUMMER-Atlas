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
SURFACE_TEMPERATURE,
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
     STRAT == '490' | STRAT == '491' | STRAT == '492' | STRAT == '493' | STRAT == '494' | STRAT == '495' ) 
  & 
    (MONTH == 6 | MONTH == 7 | MONTH == 8)
)


###########################################
## DEPTH
###########################################
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

depth.cdf <- cdf.df
depth.cdf$variable="Depth (m)"
names(depth.cdf)[1] <- "variable.value"

out.df <- cdf.df
names(out.df) <- c("depth","cdf.depth")

library(ggplot2)
g <- ggplot(out.df, aes(x=depth, y=cdf.depth)) +
  geom_line() +
  theme_bw()
g

ggsave(g, file=file.path(actualreport.path, "figures/summer-depth-CDF.png"))



###########################################
## BOTTOM TEMPERATURE
###########################################
temperature.df <- summer.tows.df[which(!is.na(summer.tows.df$BOTTOM_TEMPERATURE)),]

st <- read.csv(file.path(actualreport.path, "strata-statistics.csv"))
st <- st[which(st$STRAT %in% c(440:466, 470:478, 480:485, 490:495)),]

x <- temperature.df
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

temperatures <- seq(min(x$BOTTOM_TEMPERATURE), max(x$BOTTOM_TEMPERATURE), length.out=200)

cdf.df <- expand.grid(temperature=temperatures, cdf=NA) ## CDF for sets

for(t in temperatures){
  this.temperature.cdf <- 0
  
  ## loop over strata
  for(h in unique(x$STRAT)){
    ## number of unique tows in this stratum
    t.df <- x[x$STRAT==h,]
    ## loop over sets in each stratum
    for(i in 1:nrow(t.df)){
      this.set.cdf <- ifelse(t.df[i,"BOTTOM_TEMPERATURE"]<=t,1,0) * (st[st$STRAT==h,"Wh"] / st[st$STRAT==h,"nh"])
      this.temperature.cdf <- this.temperature.cdf + this.set.cdf
    }## end loop over sets
  }## end loop over strata
  
  
  cdf.df[cdf.df$temperature==t,"cdf"] <- this.temperature.cdf
  
}## end loop over temperatures

temp.cdf <- cdf.df
temp.cdf$variable="Bottom temperature (\u{B0}C)"
names(temp.cdf)[1] <- "variable.value"
out.df <- cdf.df
names(out.df) <- c("temperature","cdf.temperature")

g <- ggplot(out.df, aes(x=temperature, y=cdf.temperature)) +
  geom_line() +
  theme_bw()
g

ggsave(g, file=file.path(actualreport.path, "figures/summer-bottom-temperature-CDF.png"))


###########################################
## SURFACE TEMPERATURE
###########################################
idx1 <- which(!is.na(summer.tows.df$SURFACE_TEMPERATURE))
temperature.df <- summer.tows.df[idx1,]

table(temperature.df$SURFACE_TEMPERATURE==temperature.df$BOTTOM_TEMPERATURE)
## this is pretty dubious, to have the same surface and bottom temperature
quantile(temperature.df$SURFACE_TEMPERATURE)
## and it also contains the lowest value of surface temperature, so remove for now
idx2 <- which(!is.na(summer.tows.df$SURFACE_TEMPERATURE) & !(summer.tows.df$SURFACE_TEMPERATURE==summer.tows.df$BOTTOM_TEMPERATURE))
temperature.df <- summer.tows.df[idx2,]
quantile(temperature.df$SURFACE_TEMPERATURE)


st <- read.csv(file.path(actualreport.path, "strata-statistics.csv"))
st <- st[which(st$STRAT %in% c(440:466, 470:478, 480:485, 490:495)),]

x <- temperature.df
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

temperatures <- seq(min(x$SURFACE_TEMPERATURE), max(x$SURFACE_TEMPERATURE), length.out=200)

cdf.df <- expand.grid(temperature=temperatures, cdf=NA) ## CDF for sets

for(t in temperatures){
  this.temperature.cdf <- 0
  
  ## loop over strata
  for(h in unique(x$STRAT)){
    ## number of unique tows in this stratum
    t.df <- x[x$STRAT==h,]
    ## loop over sets in each stratum
    for(i in 1:nrow(t.df)){
      this.set.cdf <- ifelse(t.df[i,"SURFACE_TEMPERATURE"]<=t,1,0) * (st[st$STRAT==h,"Wh"] / st[st$STRAT==h,"nh"])
      this.temperature.cdf <- this.temperature.cdf + this.set.cdf
    }## end loop over sets
  }## end loop over strata
  
  
  cdf.df[cdf.df$temperature==t,"cdf"] <- this.temperature.cdf
  
}## end loop over temperatures

stemp.cdf <- cdf.df
stemp.cdf$variable="Surface temperature (\u{B0}C)"
names(stemp.cdf)[1] <- "variable.value"
out.df <- cdf.df
names(out.df) <- c("temperature","cdf.temperature")

g <- ggplot(out.df, aes(x=temperature, y=cdf.temperature)) +
  geom_line() +
  theme_bw()
g

ggsave(g, file=file.path(actualreport.path, "figures/summer-surface-temperature-CDF.png"))



###########################################
## BOTTOM SALINITY
###########################################
salinity.df <- summer.tows.df[which(!is.na(summer.tows.df$BOTTOM_SALINITY) & summer.tows.df$BOTTOM_SALINITY>0),]


st <- read.csv(file.path(actualreport.path, "strata-statistics.csv"))
st <- st[which(st$STRAT %in% c(440:466, 470:478, 480:485, 490:495)),]

x <- salinity.df
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

salinities <- seq(min(x$BOTTOM_SALINITY), max(x$BOTTOM_SALINITY), length.out=200)

cdf.df <- expand.grid(salinity=salinities, cdf=NA) ## CDF for sets

for(t in salinities){
  this.salinity.cdf <- 0
  
  ## loop over strata
  for(h in unique(x$STRAT)){
    ## number of unique tows in this stratum
    t.df <- x[x$STRAT==h,]
    ## loop over sets in each stratum
    for(i in 1:nrow(t.df)){
      this.set.cdf <- ifelse(t.df[i,"BOTTOM_SALINITY"]<=t,1,0) * (st[st$STRAT==h,"Wh"] / st[st$STRAT==h,"nh"])
      this.salinity.cdf <- this.salinity.cdf + this.set.cdf
    }## end loop over sets
  }## end loop over strata
  
  
  cdf.df[cdf.df$salinity==t,"cdf"] <- this.salinity.cdf
  
}## end loop over salinities

sal.cdf <- cdf.df
sal.cdf$variable="Bottom salinity (psu)"
names(sal.cdf)[1] <- "variable.value"

out.df <- cdf.df
names(out.df) <- c("salinity","cdf.salinity")

g <- ggplot(out.df, aes(x=salinity, y=cdf.salinity)) +
  geom_line() +
  theme_bw()
g

ggsave(g, file=file.path(actualreport.path, "figures/summer-salinity-CDF.png"))


############################################
############################################
## figure with overall cumulative distribution of depth, bottom temperature and bottom salinity
all.df <- rbind(depth.cdf,temp.cdf,sal.cdf)

g <- ggplot(all.df, aes(x=variable.value, y=cdf)) +
  geom_line(aes(x=variable.value, y=cdf)) + 
  facet_wrap(~variable, scales="free",ncol=1) +
  theme_bw() +
  xlab("Variable value") + 
  ylab("Cumulative frequency distribution")

g

ggsave(g, file=file.path(actualreport.path, "figures/summer-depth-temperature-salinity-CDF.png"), height = 8, width = 6)
############################################
############################################



############################################
############################################
## now do by decade for depth
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




############################################
## now do by decade for bottom and surface temperature
############################################
decades <- 1:6

btemp.df <- summer.tows.df[which(!is.na(summer.tows.df$BOTTOM_TEMPERATURE)),]

idx2 <- which(!is.na(summer.tows.df$SURFACE_TEMPERATURE) & !(summer.tows.df$SURFACE_TEMPERATURE==summer.tows.df$BOTTOM_TEMPERATURE))
stemp.df <- summer.tows.df[idx2,]


st <- read.csv(file.path(actualreport.path, "strata-statistics.csv"))
st <- st[which(st$STRAT %in% c(440:466, 470:478, 480:485, 490:495)),]

# Wh = proportion of the survey area in stratum h
st$Wh <- st$AREA / sum(st$AREA)
## in Perry and Smith, the proportion is computed as Nh/N 
## (st$Wh - st$Nh / N) < 1E-12 ## the numbers numerically differ in R, but they are the same

## SURFACE TEMPERATURE
x <- stemp.df
x$unique.id <- paste0(x$MISSION, "-", x$SETNO)

x$decade <- ceiling((x$YEAR - 1969)/10)
table(x$decade, x$YEAR)

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


temperatures <- seq(min(x$SURFACE_TEMPERATURE), max(x$SURFACE_TEMPERATURE), length.out=200)

cdf.df <- expand.grid(decade=decades, temperature=temperatures, cdf=NA) ## CDF for sets

for(d in decades){
  
  for(t in temperatures){
    this.temp.cdf <- 0
    
    ## loop over strata
    for(h in unique(x$STRAT)){
      ## number of unique tows in this stratum
      t.df <- x[x$STRAT==h & x$decade==d,]
      ## loop over sets in each stratum
      for(i in 1:nrow(t.df)){
        this.set.cdf <- ifelse(t.df[i,"SURFACE_TEMPERATURE"]<=t,1,0) * (st[st$STRAT==h & st$decade==d,"Wh"] / st[st$STRAT==h & st$decade==d,"nh"])
        this.temp.cdf <- this.temp.cdf + this.set.cdf
      }## end loop over sets
    }## end loop over strata
    
    cdf.df[cdf.df$decade==d & cdf.df$temperature==t,"cdf"] <- this.temp.cdf
    
  }## end loop over depths
  
}## end the loop over decades



out.df <- cdf.df
names(out.df) <- c("decade","surface.temperature","cdf.surface.temperature")

decades.df <- data.frame(decade=decades, decade.text=c("1970-1979","1980-1989","1990-1999","2000-2009","2010-2019","2020"))

out.df <- merge(out.df, decades.df, by="decade")
surface.temp.df <- out.df

surface.temp.df$variable="Surface temperature (\u{B0}C)"
names(surface.temp.df)[2] <- "variable.value"
names(surface.temp.df)[3] <- "cdf.temperature"



g <- ggplot(out.df, aes(x=surface.temperature, y=cdf.surface.temperature)) +
  geom_line(aes(group=decade.text, linetype=decade.text, colour = decade.text))

g
ggsave(g, file=file.path(actualreport.path, "figures/summer-surface-temperature-decadal-CDF.png"), height = 6, width = 8)



st <- read.csv(file.path(actualreport.path, "strata-statistics.csv"))
st <- st[which(st$STRAT %in% c(440:466, 470:478, 480:485, 490:495)),]

# Wh = proportion of the survey area in stratum h
st$Wh <- st$AREA / sum(st$AREA)
## in Perry and Smith, the proportion is computed as Nh/N 
## (st$Wh - st$Nh / N) < 1E-12 ## the numbers numerically differ in R, but they are the same

### BOTTOM TEMPERATURE
x <- btemp.df
x$unique.id <- paste0(x$MISSION, "-", x$SETNO)

x$decade <- ceiling((x$YEAR - 1969)/10)
table(x$decade, x$YEAR)

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


temperatures <- seq(min(x$BOTTOM_TEMPERATURE), max(x$BOTTOM_TEMPERATURE), length.out=200)

cdf.df <- expand.grid(decade=decades, temperature=temperatures, cdf=NA) ## CDF for sets

for(d in decades){
  #print(paste0("Decade: ", d))
  
  for(t in temperatures){
    #print(paste0("Temperature: ", t))
    this.temp.cdf <- 0
    
    ## loop over strata
    for(h in unique(x$STRAT)){
      ## number of unique tows in this stratum
      t.df <- x[x$STRAT==h & x$decade==d,]
      ## loop over sets in each stratum
      for(i in 1:nrow(t.df)){
        this.set.cdf <- ifelse(t.df[i,"BOTTOM_TEMPERATURE"]<=t,1,0) * (st[st$STRAT==h & st$decade==d,"Wh"] / st[st$STRAT==h & st$decade==d,"nh"])
        this.temp.cdf <- this.temp.cdf + this.set.cdf
      }## end loop over sets
    }## end loop over strata
    
    cdf.df[cdf.df$decade==d & cdf.df$temperature==t,"cdf"] <- this.temp.cdf
    
  }## end loop over depths
  
}## end the loop over decades



out.df <- cdf.df
names(out.df) <- c("decade","bottom.temperature","cdf.bottom.temperature")

decades.df <- data.frame(decade=decades, decade.text=c("1970-1979","1980-1989","1990-1999","2000-2009","2010-2019","2020"))

out.df <- merge(out.df, decades.df, by="decade")
bottom.temp.df <- out.df

bottom.temp.df$variable="Bottom temperature (\u{B0}C)"
names(bottom.temp.df)[2] <- "variable.value"
names(bottom.temp.df)[3] <- "cdf.temperature"


g <- ggplot(out.df, aes(x=bottom.temperature, y=cdf.bottom.temperature)) +
  geom_line(aes(group=decade.text, linetype=decade.text, colour = decade.text))

g
ggsave(g, file=file.path(actualreport.path, "figures/summer-bottom-temperature-decadal-CDF.png"), height = 6, width = 8)

############################################
############################################
## figure with decadal cumulative distribution of surface and bottom temperature
all.df <- rbind(surface.temp.df,bottom.temp.df)
all.df$variable <- factor(all.df$variable, ordered=TRUE, levels=c("Surface temperature (\u{B0}C)","Bottom temperature (\u{B0}C)"))

g <- ggplot(all.df, aes(x=variable.value, y=cdf.temperature)) +
  geom_line(aes(x=variable.value, y=cdf.temperature, group=decade.text, colour=decade.text)) + 
  facet_wrap(~variable, scales="fixed",ncol=1) + 
  scale_color_discrete("Decade") +
  theme_bw() + 
  xlab("Variable value") + 
  ylab("Cumulative frequency distribution")

g

ggsave(g, file=file.path(actualreport.path, "figures/summer-surface-temperatures-decadal-CDF.png"), height = 8, width = 6)
############################################
############################################

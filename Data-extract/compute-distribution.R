##
##
distribution.fct <- function(catch.df) {
# catch.df <- read.csv("C:/ATLAS_poissons_SS/Data/SS13_catch.csv")
# catch.df <- read.csv("C:/ATLAS_poissons_SS/Data/SS10_catch.csv")

#yrs<- seq(min(catch.df$YEAR),max(catch.df$YEAR))

## five-year period
catch.df$pentad <- floor(catch.df$YEAR/5)

# area occupied, as sum of (proportion of tows with catch multiplied by stratum surface area)
non.zero.catch <- subset(catch.df, totno !=0)
table.catch.t <- table(non.zero.catch$YEAR, non.zero.catch$Strata)

## find the stratum-year combinations that ensure that we are only keeping strata where the species was caught at least 5 times over the whole time series
num.rec.stratum <- table(non.zero.catch$Strata)
#strat.keep <- num.rec.stratum[num.rec.stratum>=5]
strat.keep <- num.rec.stratum
#strat.keep <- table(catch.df$Strata)
nzc.tt <- subset(non.zero.catch, Strata %in% names(strat.keep))

yrs<- sort(unique(nzc.tt$YEAR))

table.catch <- table(nzc.tt$YEAR, nzc.tt$Strata)

## keep only years and strata where there is catch
#my.ss <- sort(unique(non.zero.catch$Strata))
#my.yy <- sort(unique(non.zero.catch$YEAR))
#catch.t.df <- subset(catch.df, Strata %in% my.ss & YEAR %in% my.yy)

catch.t.df <- subset(catch.df, Strata %in% names(strat.keep))
my.yy <- sort(unique(nzc.tt$YEAR))

catch.t.df2 <- subset(catch.t.df, YEAR %in% my.yy)

table.all <- table(catch.t.df2$YEAR, catch.t.df2$Strata)


## make sure that the two tables are the same dimensions


my.prop <- table.catch / table.all
my.prop.df <- as.data.frame(my.prop)
names(my.prop.df) <- c("year", "stratum", "prop")

## strata statistics
qu <- paste("
select 
*
from
groundfish.gsstratum
", sep="")

strata.stats.df <- sqlQuery(chan, qu)

merged.df <- merge(my.prop.df, strata.stats.df, by.x="stratum", by.y="STRAT", all.x=TRUE, all.y=FALSE)

merged.df$area.occupied <- merged.df$prop * merged.df$AREA

area.occ <- tapply(na.omit(merged.df)$area.occupied, na.omit(merged.df)$year, sum)

# Gini index
summer.strata.stats.df <- subset(
strata.stats.df, 
STRAT %in% names(strat.keep)
)
# (STRAT == '440' | STRAT == '441' | STRAT == '442' | STRAT == '443' | STRAT == '444' | STRAT == '445' | STRAT == '446' | STRAT == '447' | STRAT == '448' | STRAT == '449' | 
# STRAT == '450' | STRAT == '451' | STRAT == '452' | STRAT == '453' | STRAT == '454' | STRAT == '455' | STRAT == '456' | STRAT == '457' | STRAT == '458' | STRAT == '459' | 
# STRAT == '460' | STRAT == '461' | STRAT == '462' | STRAT == '463' | STRAT == '464' | STRAT == '465' | STRAT == '466' | 
# STRAT == '470' | STRAT == '471' | STRAT == '472' | STRAT == '473' | STRAT == '474' | STRAT == '475' | STRAT == '476' | STRAT == '477' | STRAT == '478' | 
# STRAT == '480' | STRAT == '481' | STRAT == '482' | STRAT == '483' | STRAT == '484' | STRAT == '485' | 
# STRAT == '490' | STRAT == '491' | STRAT == '492' | STRAT == '493' | STRAT == '494' | STRAT == '495' ) 
# )
summer.strata.tt <- summer.strata.stats.df[order(summer.strata.stats.df$STRAT),]
summer.strat <- data.frame(Strata=summer.strata.tt$STRAT, area=summer.strata.tt$AREA, NH=summer.strata.tt$AREA/0.011800615)

merged.catch <- merge(catch.df, summer.strat, by="Strata")

# average catch per stratum per year
stratified.mat <- lapply(yrs, function(i){tapply(subset(merged.catch, YEAR==i)$totno.corr, subset(merged.catch, YEAR==i)$Strata, mean)})

#########################################################################################################
## PROBLEM HERE WHEN THERE IS A YEAR WHERE THERE IS NO TOWS IN A GIVEN STRATUM, (e.g. i==15 (1984), stratum 474)
#########################################################################################################

ll <- seq(1,length(stratified.mat))

stratified.weighted.mat <- sapply(ll, function(i){
	# identify the strata for year i
	ss.tt <- subset(summer.strat, Strata %in% names(stratified.mat[[i]]))
	## now compute the yearly totals
	stratified.mat[[i]] * (ss.tt$area/sum(ss.tt$area))
	})

stratified.yearly <- sapply(ll, function(i){
	# identify the strata for year i
	ss.tt <- subset(summer.strat, Strata %in% names(stratified.mat[[i]]))
	## now compute the yearly totals
	sum(stratified.mat[[i]] * (ss.tt$area/sum(ss.tt$area)), na.rm=TRUE)
	})

yearly.stratified <- data.frame(year=yrs, n.strat=stratified.yearly)


##
nn <- length(yrs)
df.to.fill <- data.frame(year=rep(-99,nn), D75=rep(-99,nn), D95=rep(-99,nn), Gini=rep(-99,nn))

# D50, D75 and D95
for(i in 1:length(yrs)){ # loop over years
#for(i in 1:(yrs-1969)){ # loop over years
#print(i)
#stratified.df <- as.data.frame(stratified.weighted.mat[,i])
stratified.df <- as.data.frame(stratified.weighted.mat[[i]])

oo.desc <- rev(order(stratified.df)) # descending order, for Dx%
oo.asc <- order(stratified.df) # ascending order, for Gini

oo.desc.strat.est <- stratified.df[oo.desc,]
oo.desc.strat.names <- row.names(stratified.df)[oo.desc]

strat.est.desc.df <- na.omit(data.frame(strat=oo.desc.strat.names, strat.est=oo.desc.strat.est))

strat.est.desc.df$cumsum <- cumsum(strat.est.desc.df$strat.est)
strat.est.desc.df$percent <- (strat.est.desc.df$cumsum / sum(strat.est.desc.df$strat.est)) * 100

tt.df <- merge(strat.est.desc.df, summer.strata.stats.df, by.x="strat", by.y="STRAT")
oo.desc <- rev(order(tt.df$strat.est))
tt.df <- tt.df[oo.desc,]
tt.df$cumsum.area <- cumsum(tt.df$AREA)

#ifelse(length(which(tt.df$percent < 75)) == 0, 1, max(which(tt.df$percent < 75)))

d.75.i <- ifelse(length(which(tt.df$percent < 75)) == 0, 1, max(which(tt.df$percent < 75))) # max(which(tt.df$percent < 75))
d.95.i <- ifelse(length(which(tt.df$percent < 95)) == 0, 1, max(which(tt.df$percent < 95))) # max(which(tt.df$percent < 95))

d.75 <- tt.df[d.75.i,]$cumsum.area
d.95 <- tt.df[d.95.i,]$cumsum.area

df.to.fill[i,1] <- yrs[i]
df.to.fill[i,2] <- d.75/1000
df.to.fill[i,3] <- d.95/1000

## Gini
oo.asc.strat.est <- stratified.df[oo.asc,]
oo.asc.strat.names <- row.names(stratified.df)[oo.asc]

strat.est.asc.df <- na.omit(data.frame(strat=oo.asc.strat.names, strat.est=oo.asc.strat.est))
strat.est.asc.df$cumsum <- cumsum(strat.est.asc.df$strat.est)
strat.est.asc.df$percent <- (strat.est.asc.df$cumsum/sum(strat.est.asc.df$strat.est)) * 100

tt.df <- merge(strat.est.asc.df, summer.strata.stats.df, by.x="strat", by.y="STRAT")
oo.asc <- order(tt.df$strat.est)
tt.df <- tt.df[oo.asc,]
tt.df$cumsum.area <- cumsum(tt.df$AREA)
tt.df$perc.area <- tt.df$cumsum.area / sum(tt.df$AREA)
tt.df$prop.area <- tt.df$AREA / sum(tt.df$AREA)

gini <- 2*(0.5 - sum(tt.df$prop.area*(tt.df$percent/100)))
df.to.fill[i,4] <- gini

}


## final data frame to send back

final.df <- data.frame(year=names(area.occ), area.occupied=area.occ/1000, D75=df.to.fill[,2], D95=df.to.fill[,3], Gini=df.to.fill[,4])

return(final.df)

} # end function

##
##
stratified.fct <- function(catch.df, DDHS=FALSE) {
# catch.df <- read.csv("C:/ATLAS_poissons_SS/Data/SS10_catch.csv", header=TRUE)

#yrs<- seq(min(catch.df$YEAR),max(catch.df$YEAR))
yrs<- sort(unique(catch.df$YEAR))
n.yrs <- length(yrs)

## strata statistics
qu <- paste("
select 
*
from
groundfish.gsstratum
", sep="")

strata.stats.df <- sqlQuery(chan, qu)

summer.strata.stats.df <- subset(
strata.stats.df, 
(STRAT == '440' | STRAT == '441' | STRAT == '442' | STRAT == '443' | STRAT == '444' | STRAT == '445' | STRAT == '446' | STRAT == '447' | STRAT == '448' | STRAT == '449' | 
STRAT == '450' | STRAT == '451' | STRAT == '452' | STRAT == '453' | STRAT == '454' | STRAT == '455' | STRAT == '456' | STRAT == '457' | STRAT == '458' | STRAT == '459' | 
STRAT == '460' | STRAT == '461' | STRAT == '462' | STRAT == '463' | STRAT == '464' | STRAT == '465' | STRAT == '466' | 
STRAT == '470' | STRAT == '471' | STRAT == '472' | STRAT == '473' | STRAT == '474' | STRAT == '475' | STRAT == '476' | STRAT == '477' | STRAT == '478' | 
STRAT == '480' | STRAT == '481' | STRAT == '482' | STRAT == '483' | STRAT == '484' | STRAT == '485' | 
STRAT == '490' | STRAT == '491' | STRAT == '492' | STRAT == '493' | STRAT == '494' | STRAT == '495' ) 
)

summer.strata.tt <- summer.strata.stats.df[order(summer.strata.stats.df$STRAT),]
summer.strat <- data.frame(Strata=summer.strata.tt$STRAT, area=summer.strata.tt$AREA, NH=summer.strata.tt$AREA/0.01)

my.out.n.mean <- lapply(yrs, function(i){tapply(subset(catch.df,YEAR==i)$totno.corr, subset(catch.df,YEAR==i)$Strata, mean)})

#my.out.n.mean.strat <- lapply(summer.strat$Strata, function(i){tapply(subset(catch.df,Strata==i)$totno.corr, subset(catch.df,Strata==i)$YEAR, mean)})


# variance calculations
my.out.n.var <- lapply(yrs, function(i){tapply(subset(catch.df,YEAR==i)$totno.corr, subset(catch.df,YEAR==i)$Strata, var)})
n.tows <- lapply(yrs, function(i){table(subset(catch.df,YEAR==i)$Strata)})
first.term <- lapply(1:n.yrs, function(i){(1 - (n.tows[[i]] / subset(summer.strat, Strata %in% names(n.tows[[i]]))$NH))})
second.term <- lapply(1:n.yrs, function(i){((subset(summer.strat, Strata %in% names(n.tows[[i]]))$NH / sum(subset(summer.strat, Strata %in% names(n.tows[[i]]))$NH))^2)})
third.term <- lapply(1:n.yrs, function(i){(my.out.n.var[[i]] / n.tows[[i]])})
var.n.final <- lapply(1:n.yrs, function(i){sum(first.term[[i]] * second.term[[i]] * third.term[[i]], na.rm=TRUE)})


my.out.b.mean <- lapply(yrs, function(i){tapply(subset(catch.df,YEAR==i)$totwgt.corr, subset(catch.df,YEAR==i)$Strata, mean)})
my.out.b.var <- lapply(yrs, function(i){tapply(subset(catch.df,YEAR==i)$totwgt.corr, subset(catch.df,YEAR==i)$Strata, var)})
third.term <- lapply(1:n.yrs, function(i){(my.out.b.var[[i]] / n.tows[[i]])})
var.b.final <- lapply(1:n.yrs, function(i){sum(first.term[[i]] * second.term[[i]] * third.term[[i]], na.rm=TRUE)})

summer.strat$strat.weight <- summer.strat$area / sum(summer.strat$area)

#years <- seq(range(catch.df$YEAR)[1], range(catch.df$YEAR)[2])


my.df <- data.frame(
year = yrs,
n = unlist(
lapply(1:n.yrs, function(i){
							ss.tt <- names(my.out.n.mean[[i]])
							sum(my.out.n.mean[[i]] * subset(summer.strat, Strata %in% ss.tt)$strat.weight, na.rm=T)})
),
n.var = unlist(
lapply(1:n.yrs, function(i){
							var.n.final[[i]]
							#ss.tt <- names(my.out.n.var[[i]])
							#sum(my.out.n.var[[i]] * subset(summer.strat, Strata %in% ss.tt)$strat.weight, na.rm=T)}
							})
),
b = unlist(
lapply(1:n.yrs, function(i){
							ss.tt <- names(my.out.b.mean[[i]])
							sum(my.out.b.mean[[i]] * subset(summer.strat, Strata %in% ss.tt)$strat.weight, na.rm=T)
							})
),
b.var = unlist(
lapply(1:n.yrs, function(i){
							var.b.final[[i]]
							#ss.tt <- names(my.out.b.var[[i]])
							#sum(my.out.b.var[[i]] * subset(summer.strat, Strata %in% ss.tt)$strat.weight, na.rm=T)}
							})
)
)


if(DDHS){
## now fit density-dependent habitat selection models
tt.df <- merge(catch.df, my.df, by.x="YEAR", by.y="year")

dd.for.fit.tt <- data.frame(year=tt.df$YEAR, stratum=tt.df$Strata, catch.abundance=tt.df$totno, catch.abundance.corr=tt.df$totno.corr,  catch.biomass=tt.df$totwgt, catch.biomass.corr=tt.df$totwgt.corr,  yearly.estimate=tt.df$n, dist=tt.df$dist)

## keep only strata where there is at least 5 catch records 
my.tt <- table(subset(catch.df, totno != 0)$Strata)
my.st <- names(my.tt[my.tt >= 5])

dd.for.fit <- subset(dd.for.fit.tt, stratum %in% my.st)
dd.for.fit <- droplevels(dd.for.fit) # drop unused levels, otherwise mean and variance will be computed for all strata 

# remove years where the yearly estimate is zero
tt <- subset(dd.for.fit, yearly.estimate != 0)

dd.for.fit <- tt

# stupid linear model
my.lm1 <- lm(log(catch.abundance.corr+0.5)~log(yearly.estimate)*as.factor(stratum), data=dd.for.fit)

# Poisson GLM
my.glm1 <- glm(catch.abundance ~ yearly.estimate*as.factor(stratum), offset=(dist), family=poisson(link="log"),data=dd.for.fit)

# negative binomial GLM
my.glm2 <- glm.nb(catch.abundance ~ (yearly.estimate*as.factor(stratum)) + offset(dist), data=dd.for.fit, link=log, maxit=100)

# average abundance per stratum
my.n.mean.strat <- tapply(dd.for.fit$catch.abundance.corr, dd.for.fit$stratum, mean)
my.n.quan50.strat <- tapply(dd.for.fit$catch.abundance.corr, dd.for.fit$stratum, quantile, probs=c(0.5))
my.n.quan75.strat <- tapply(dd.for.fit$catch.abundance.corr, dd.for.fit$stratum, quantile, probs=c(0.75))
my.n.quan95.strat <- tapply(dd.for.fit$catch.abundance.corr, dd.for.fit$stratum, quantile, probs=c(0.95))

# median stratum abundance in years where the overall abundance was in the top 25%
top.25 <- quantile(my.df$n, probs=c(0.75))
top.25.yrs <- subset(my.df, n>= top.25)$year
dd.top25 <- subset(dd.for.fit, year %in% top.25.yrs)
my.n.median.strat.top25 <- tapply(dd.top25$catch.abundance.corr, dd.top25$stratum, median)

my.n.var.strat <- tapply(dd.for.fit$catch.abundance.corr, dd.for.fit$stratum, var)

} # end if DDHS

if(DDHS){return(list(strat=my.df, ddhs=list(lm1=my.lm1, glm1=my.glm1, glm2=my.glm2, strat.mean=my.n.mean.strat, strat.quan50=my.n.quan50.strat, strat.quan75=my.n.quan75.strat, strat.quan95=my.n.quan95.strat, strat.median.top25=my.n.median.strat.top25)))}
if(!DDHS){return(list(strat=my.df))}

} # end function

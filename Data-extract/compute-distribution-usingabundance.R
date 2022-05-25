##
##
distribution.usingabundance.fct <- function(catch.df) {
# catch.df <- read.csv(file.path(figdata.path, "/SS10_catch.csv"))

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
## turn into a long data frame
my.prop.df <- as.data.frame(my.prop)
st.t <- names(my.prop.df)

my.prop.df$year <- as.numeric(as.character(my.prop.df$Var1))

#my.prop.df <- tidyr::pivot_longer(my.prop.df, cols=all_of(st.t), names_to="stratum", values_to="prop")
names(my.prop.df) <- c("Var1","stratum","prop","year")

# names(my.prop.df) <- c("year", "stratum", "prop")

## strata statistics
qu <- paste("
select 
*
from
groundfish.gsstratum
", sep="")

strata.stats.df <- sqlQuery(chan, qu)

merged.df <- merge(my.prop.df, strata.stats.df, by.x="stratum", by.y="STRAT", all.x=TRUE, all.y=FALSE)

merged.df$area.occupied <- merged.df$prop * merged.df$AREA  * (1.852^2)

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

stratified.weighted.mat <- lapply(ll, function(i){
	# identify the strata for year i
	ss.tt <- subset(summer.strat, Strata %in% names(stratified.mat[[i]]))
	## now compute the yearly totals
	stratified.mat[[i]] * (ss.tt$area/sum(ss.tt$area))
	})

stratified.yearly <- lapply(ll, function(i){
	# identify the strata for year i
	ss.tt <- subset(summer.strat, Strata %in% names(stratified.mat[[i]]))
	## now compute the yearly totals
	sum(stratified.mat[[i]] * (ss.tt$area/sum(ss.tt$area)), na.rm=TRUE)
	})

yearly.stratified <- data.frame(year=yrs, n.strat=stratified.yearly)

st.area <- strata.stats.df[,c("STRAT","AREA")]
names(st.area) <- c("stratum", "area")

##
# nn <- length(yrs)
# df.to.fill <- data.frame(year=rep(-99,nn), area.surveyed=rep(-99,nn), D75=rep(-99,nn), D95=rep(-99,nn), Gini=rep(-99,nn))
# 
# D50, D75 and D95
yrs<- seq(min(catch.df$YEAR),max(catch.df$YEAR))
dwao.df <- data.frame(year=yrs, survey.area=rep(NA,length(yrs)), dwao=rep(NA,length(yrs)))
d50.d75.d95.df <- data.frame(year=yrs, survey.area=rep(NA,length(yrs)), D50=rep(NA,length(yrs)), D75=rep(NA,length(yrs)), D95=rep(NA,length(yrs)))

for(i in 1:length(yrs)){ # loop over years
  
  z <- catch.df[catch.df$YEAR==yrs[i],]
  n.tows <- aggregate(setno~Strata, data=z, length)
  names(n.tows)[2] <- "number.tows"
  
  ## survey area in current year
  ## limit to strata with 2 or more tows
  n.tows <- n.tows[n.tows$number.tows>1,]
  ss <- unique(n.tows$Strata)
  
  
  survey.area <- sum(st.area[st.area$stratum %in% ss, "area"]) * (1.852^2)
  
  z.out <- merge(merge(z, n.tows, by="Strata"), st.area, by.x="Strata", by.y="stratum")
  z.out$area.divided.by.tows <- z.out$area / z.out$number.tows
  z.out$I <- ifelse(z.out$totno.corr>0,1,0)
  DWAO <- sum(z.out$area.divided.by.tows * z.out$I) * (1.852^2)
  
  
  if(DWAO>0){
    ## loop over densities 
    #cc <- seq(0:(max(z.out$number.caught)+1))
    cc <- seq(0,(max(z.out$totno.corr)+1), length.out=200)
    res <- data.frame(c=cc, F=rep(NA,length(cc)), G=rep(NA,length(cc)))
    
    for(ii in 1:length(cc)){
      #z.out$I <- ifelse(z.out$number.caught<=cc[ii],1,0)
      z.out$I <- ifelse(z.out$totno.corr<=cc[ii],1,0)
      res[ii,"c"] <- cc[ii]
      #res[ii,"F"] <- 100 * (sum(z.out$area.divided.by.tows * z.out$number.caught * z.out$I) / sum(z.out$area.divided.by.tows * z.out$number.caught))
      res[ii,"F"] <- 100 * (sum(z.out$area.divided.by.tows * z.out$totno.corr * z.out$I) / sum(z.out$area.divided.by.tows * z.out$totno.corr))
      res[ii,"G"] <- sum(z.out$area.divided.by.tows * z.out$I)
    }
    
    itv <- findInterval(res$F, c(5, 25, 50))
    b95.ii <- c(table(itv)[1],table(itv)[1]+1)
    b75.ii <- c(table(itv)[1]+table(itv)[2],table(itv)[1]+table(itv)[2]+1)
    b50.ii <- c(table(itv)[1]+table(itv)[2]+table(itv)[3],table(itv)[1]+table(itv)[2]++table(itv)[3]+1)
    
    ## linear interpolation for D75 and D95, based on where the breaks occurs
    diff.95 <- 1- ((res[b95.ii[2], "F"] - 5) / (res[b95.ii[2], "F"] - res[b95.ii[1], "F"])) ## percentage of the interval below 5%
    diff.75 <- 1- ((res[b75.ii[2], "F"] - 25) / (res[b75.ii[2], "F"] - res[b75.ii[1], "F"])) ## percentage of the interval below 25%
    diff.50 <- 1- ((res[b50.ii[2], "F"] - 50) / (res[b50.ii[2], "F"] - res[b50.ii[1], "F"])) ## percentage of the interval below 50%
    
    ## 
    nr <- nrow(res)
    D.50 <- res[nr,"G"] - (res[b50.ii[1],"G"] + (diff.50*(res[b50.ii[2],"G"] - res[b50.ii[1],"G"])))
    D.75 <- res[nr,"G"] - (res[b75.ii[1],"G"] + (diff.75*(res[b75.ii[2],"G"] - res[b75.ii[1],"G"])))
    D.95 <- res[nr,"G"] - (res[b95.ii[1],"G"] + (diff.95*(res[b95.ii[2],"G"] - res[b95.ii[1],"G"])))
    
    dwao.df[dwao.df$year==yrs[i],"dwao"] <- DWAO
    dwao.df[dwao.df$year==yrs[i],"survey.area"] <- survey.area
    d50.d75.d95.df[d50.d75.d95.df$year==yrs[i],"survey.area"] <- survey.area
    d50.d75.d95.df[d50.d75.d95.df$year==yrs[i],"D50"] <- D.50 * (1.852^2)
    d50.d75.d95.df[d50.d75.d95.df$year==yrs[i],"D75"] <- D.75 * (1.852^2)
    d50.d75.d95.df[d50.d75.d95.df$year==yrs[i],"D95"] <- D.95 * (1.852^2)
  } else {
    
    dwao.df[dwao.df$year==yrs[i],"dwao"] <- 0
    dwao.df[dwao.df$year==yrs[i],"survey.area"] <- survey.area
    d50.d75.d95.df[d50.d75.d95.df$year==yrs[i],"survey.area"] <- survey.area
    d50.d75.d95.df[d50.d75.d95.df$year==yrs[i],"D50"] <- 0
    d50.d75.d95.df[d50.d75.d95.df$year==yrs[i],"D75"] <- 0
    d50.d75.d95.df[d50.d75.d95.df$year==yrs[i],"D95"] <- 0
    
  }
  
  
#   
#   
#   ## area surveyed
#   this.df <- catch.df[catch.df$YEAR==yrs[i],]
#   surveyed.area <- sum(summer.strat[summer.strat$Strata %in% unique(this.df$Strata),"area"]) * (1.852^2)
#   
#   
# #for(i in 1:(yrs-1969)){ # loop over years
# #print(i)
# #stratified.df <- as.data.frame(stratified.weighted.mat[,i])
# stratified.df <- as.data.frame(stratified.weighted.mat[[i]])
# 
# oo.desc <- rev(order(stratified.df)) # descending order, for Dx%
# oo.asc <- order(stratified.df) # ascending order, for Gini
# 
# oo.desc.strat.est <- stratified.df[oo.desc,]
# oo.desc.strat.names <- row.names(stratified.df)[oo.desc]
# 
# strat.est.desc.df <- na.omit(data.frame(strat=oo.desc.strat.names, strat.est=oo.desc.strat.est))
# 
# strat.est.desc.df$cumsum <- cumsum(strat.est.desc.df$strat.est)
# strat.est.desc.df$percent <- (strat.est.desc.df$cumsum / sum(strat.est.desc.df$strat.est)) * 100
# 
# tt.df <- merge(strat.est.desc.df, summer.strata.stats.df, by.x="strat", by.y="STRAT")
# oo.desc <- rev(order(tt.df$strat.est))
# tt.df <- tt.df[oo.desc,]
# tt.df$cumsum.area <- cumsum(tt.df$AREA)
# 
# #ifelse(length(which(tt.df$percent < 75)) == 0, 1, max(which(tt.df$percent < 75)))
# 
# d.75.i <- ifelse(length(which(tt.df$percent < 75)) == 0, 1, max(which(tt.df$percent < 75))) # max(which(tt.df$percent < 75))
# d.95.i <- ifelse(length(which(tt.df$percent < 95)) == 0, 1, max(which(tt.df$percent < 95))) # max(which(tt.df$percent < 95))
# 
# d.75 <- tt.df[d.75.i,]$cumsum.area * (1.852^2)
# d.95 <- tt.df[d.95.i,]$cumsum.area * (1.852^2)
# 
# 
# df.to.fill[i,"year"] <- yrs[i]
# df.to.fill[i,"area.surveyed"] <- surveyed.area
# df.to.fill[i,"D75"] <- d.75/1000
# df.to.fill[i,"D95"] <- d.95/1000
# 
# 
# 
# ## Gini
# oo.asc.strat.est <- stratified.df[oo.asc,]
# oo.asc.strat.names <- row.names(stratified.df)[oo.asc]
# 
# strat.est.asc.df <- na.omit(data.frame(strat=oo.asc.strat.names, strat.est=oo.asc.strat.est))
# strat.est.asc.df$cumsum <- cumsum(strat.est.asc.df$strat.est)
# strat.est.asc.df$percent <- (strat.est.asc.df$cumsum/sum(strat.est.asc.df$strat.est)) * 100
# 
# tt.df <- merge(strat.est.asc.df, summer.strata.stats.df, by.x="strat", by.y="STRAT")
# oo.asc <- order(tt.df$strat.est)
# tt.df <- tt.df[oo.asc,]
# tt.df$cumsum.area <- cumsum(tt.df$AREA)
# tt.df$perc.area <- tt.df$cumsum.area / sum(tt.df$AREA)
# tt.df$prop.area <- tt.df$AREA / sum(tt.df$AREA)
# 
# gini <- 2*(0.5 - sum(tt.df$prop.area*(tt.df$percent/100)))
# df.to.fill[i,"Gini"] <- gini

} # end loop over years


## final data frame to send back

#final.df <- data.frame(year=names(area.occ), area.surveyed=df.to.fill[,"area.surveyed"]/1000, area.occupied=area.occ/1000, D75=df.to.fill[,"D75"], D95=df.to.fill[,"D95"], Gini=df.to.fill[,"Gini"])
final.df <- data.frame(
  year=dwao.df$year, area.surveyed=dwao.df$survey.area/1000, DWAO=dwao.df$dwao/1000, D75=d50.d75.d95.df$D75/1000, D95=d50.d75.d95.df$D95/1000)

## if there are years with no catch, add NAs
all.years <- data.frame(year=1970:2020)

final.df <- merge(final.df, all.years, all.y=TRUE, by="year")

return(final.df)

} # end function

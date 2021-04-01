##
##
cat.in <- read.csv("DanTable.csv")

strata.stats <- read.csv("../DFO-strata-statistics.csv")
strata.stats <- strata.stats[strata.stats$STRAT %in% unique(cat.in$STRAT),]
strata.stats$total.area <- sum(strata.stats$AREA)
strata.stats$prop.area <- strata.stats$AREA/strata.stats$total.area

merged.df <- merge(cat.in, strata.stats[,c("STRAT","prop.area")], by="STRAT")

stratified.estimates <- aggregate(meanWGT*prop.area~year+SPEC, data=merged.df, sum)

ss.10 <- read.csv("../Figures-data/SS10_stratified.csv")

plot(ss.10$year, ss.10$b, type='b', lwd=2, pch=19, col="red", cex=2)
lines(stratified.estimates[stratified.estimates$SPEC==10, "year"], stratified.estimates[stratified.estimates$SPEC==10, 3], type='b', col="black", pch=19)

cor(ss.10$b, stratified.estimates[stratified.estimates$SPEC==10, 3])

ss.11 <- read.csv("../Figures-data/SS11_stratified.csv")

plot(ss.11$year, ss.11$b, type='b', lwd=2, pch=19, col="red", cex=2)
lines(stratified.estimates[stratified.estimates$SPEC==11, "year"], stratified.estimates[stratified.estimates$SPEC==11, 3], type='b', col="black", pch=19)

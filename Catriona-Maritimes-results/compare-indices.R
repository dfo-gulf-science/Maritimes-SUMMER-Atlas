##
##
cat.in <- read.csv(file.path(here::here(), "Catriona-Maritimes-results/DanTable.csv"))

species <- unique(cat.in$SPEC)


strata.stats <- read.csv(file.path(here::here(),"DFO-strata-statistics.csv"))
strata.stats <- strata.stats[strata.stats$STRAT %in% unique(cat.in$STRAT),]
strata.stats$total.area <- sum(strata.stats$AREA)
strata.stats$prop.area <- strata.stats$AREA/strata.stats$total.area

merged.df <- merge(cat.in, strata.stats[,c("STRAT","prop.area")], by="STRAT")

stratified.estimates <- aggregate(meanWGT*prop.area~year+SPEC, data=merged.df, sum)


## for all the species supplied by Catriona, generate a correlation plot for the computed biomass indices
for(s in species){
  DR.in <- read.csv(file.path(here::here(),paste0("Figures-data/SS", s, "_stratified.csv")))
  CAT.in <- stratified.estimates[stratified.estimates$SPEC==s, ]
  if(s==2550){CAT.in <- CAT.in[CAT.in$year>=1999,]}
  this.cor <- cor(DR.in$b, CAT.in$`meanWGT * prop.area`)
  print(paste0("Species code ", s, " correlation is ", this.cor))
}


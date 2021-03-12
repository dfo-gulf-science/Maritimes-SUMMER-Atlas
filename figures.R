## script that handles a call for each species and generates the appropriate figures 
## 
## 
## name and description of each figure
## "stratumabundance" figure: a figure containing 9 maps of the survey area and showing the stratum-level stratified estimate of catch abundance
## "depthpref" figure: cumulative distributions showing habitat preference of a species (as per Perry and Smith (1994)), this figure shows the distributions for depth
## "bottomtemppref" figure: cumulative distributions showing habitat preference of a species (as per Perry and Smith (1994)), this figure shows the distributions for bottom temperature
## "salinitypref" figure: cumulative distributions showing habitat preference of a species (as per Perry and Smith (1994)), this figure shows the distributions for bottom salinity
## "distribution-indices-usingN" figure: time-series plots of distribution indices, includes the Gini index, D50, D75 and D95
## "stratified" figure: time-series plots of stratified random estimates of catch abundance and catch biomass
## "lengthfreq" figure: length frequency distribution per 7-year period
## "LW" figure: length-weight relationship plot
## "condition" figure: time series of average fish condition
## "IDWabundance" figure: a figure containing 9 maps of the survey area and showing a geostatistical interpolation of catch abundance (inverse distance weighted)
## "IDWbiomass" figure: same as figure 10, using catch biomass
## "DDHS" figure: density-dependent habitat selection plot, beta vs. stratum-level density
## "stratifiedN"  figure: time-series plots of stratified random estimates of catch abundance (as per figure 6, but only abundance)
## "stratifiedB" figure: time-series plots of stratified random estimates of catch biomass (as per figure 6, but only biomass)
## "lengthfreqNAFO" figure: length frequency distribution per 7-year period, separately for NAFO 4X and NAFO 4VW (as per figure 7)
## "presence" figure: a single map showing the occurrences of a species, used for rare species with few records 
## "NvsD75corr" figure: correlation between abundance and distribution
## "distribution-indices-usingB" figure: time-series plots of distribution indices, includes the Gini index, D50, D75 and D95 (as per Figure 5, but using biomass)
## "BvsD75corr" figure: correlation between biomass and distribution
## "conditionNAFO" figure: timeseries of average fish condition, separately for NAFO 4X and NAFO 4VW (as per figure 9) and including the estimated parameters of the length-weight relationship
## "IDWabundanceSspecies" figure: a figure containing 4 maps of the survey area and showing a geostatistical interpolation of catch abundance (inverse distance weighted), from 1999 for inverts 
## "IDWbiomassSspecies" figure: same as figure 21, using catch biomass

make.figure <- function(fig.name, spec.num) {
  mes <- paste("Figure generation request for species code ", spec.num, " and for figure ID: ", fig.name, sep="")
  print(mes)
  ## switch statement to handle the different types of data extractions and statistics 
  switch(fig.name, 
         "stratumabundance" = {
           source(file.path(figcode.path, "stratumabundance.R"))
         },
         "depthpref" = {
           source(file.path(figcode.path, "depthpref.R"))
         },
         "bottomtemppref" = {
           source(file.path(figcode.path, "bottomtemppref.R"))
         },
         "salinitypref" = {
           source(file.path(figcode.path, "salinitypref.R"))
         },
         "distribution-indices-usingN" = {
           source(file.path(figcode.path, "distribution-indices-usingN.R"))
         },
         "stratified" = {
           source(file.path(figcode.path, "stratified.R"))
         },
         "lengthfreq" = {
           source(file.path(figcode.path, "lengthfreq.R"))
         },
         "LW" = {
           source(file.path(figcode.path, "LW.R"))
         },
         "condition" = {
           source(file.path(figcode.path, "condition.R"))
         },
         "IDWabundance" = {
           source(file.path(figcode.path, "IDWabundance.R"))
         },
         "IDWbiomass" = {
           source(file.path(figcode.path, "IDWbiomass.R"))
           ## PDF
           fig.name <- paste(paste("RV-4VWX", spec.num, fig.name, sep="-"), ".pdf", sep="")
           pdf(file.path(fig.path, fig.name), width=7.5, height=6.0)
           IDWbiomass.fig.fct(spec.num)
           dev.off()
           
         },
         "DDHS" = {
           source(file.path(figcode.path, "DDHS.R"))
         },
         "stratifiedN" = {
           source(file.path(figcode.path, "stratifiedN.R"))
         },
         "stratifiedB" = {
           source(file.path(figcode.path, "stratifiedB.R"))
         },
         "lengthfreqNAFO" = {
           source(file.path(figcode.path, "lengthfreqNAFO.R"))
         },
         "presence" = {
           source(file.path(figcode.path, "presence.R"))
           
           ## PDF
           fig.name <- paste(paste("RV-4VWX", spec.num, fig.name, sep="-"), ".pdf", sep="")
           pdf(file.path(fig.path, fig.name), width=7.6, height=5.5)
           #par(mar=c(4,4,1,1))
           presence.fig.fct(spec.num)
           dev.off()
           
         },
         "NvsD75corr" = {
           source(file.path(figcode.path, "NvsD75corr.R"))
         },
         "distribution-indices-usingB" = {
           source(file.path(figcode.path, "distribution-indices-usingB.R"))
         },
         "BvsD75corr" = {
           source(file.path(figcode.path, "BvsD75corr.R"))
         },
         "conditionNAFO" = {
           source(file.path(figcode.path, "conditionNAFO.R"))
         },
         "IDWabundanceSspecies" = {
           source(file.path(figcode.path, "IDWabundanceSspecies.R"))
         },
         "IDWbiomassSspecies" = {
           source(file.path(figcode.path, "IDWbiomassSspecies.R"))
         },
         ## case not defined
         {
           print("The character string for this type of figure is not recognised.")
         }
         
  ) # end switch
  
}
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
## "lengthfreqNAFO" figure: length frequency distribution per 7-year period, separately for 4X and 4VW
## "LW" figure: length-weight relationship plot
## "conditionNAFO" figure: time series of average fish condition, separately for NAFO 4X and NAFO 4VW
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
         "IDWbiomass" = {
           source(file.path(figcode.path, "IDWbiomass.R"))
           ## PDF
           fig.name <- paste(paste("RV-4VWX", spec.num, fig.name, sep="-"), ".pdf", sep="")
           pdf(file.path(fig.path, fig.name), width=8.5, height=6.0)
           IDWbiomass.fig.fct(spec.num)
           dev.off()
           
         },
         "stratifiedB" = {
           source(file.path(figcode.path, "stratifiedB.R"))
           ## PDF
           fig.name <- paste(paste("RV-4VWX", spec.num, fig.name, sep="-"), ".pdf", sep="")
           pdf(file.path(fig.path, fig.name), width=6.5, height=5.5)
           stratifiedB.fct(spec.num)
           dev.off()
         },
         "distribution-indices-usingB" = {
           source(file.path(figcode.path, "distribution-indices-usingB.R"))
           ## PDF
           fig.name <- paste(paste("RV-4VWX", spec.num, fig.name, sep="-"), ".pdf", sep="")
           pdf(file.path(fig.path, fig.name), width=6.5, height=5.5)
           distribution.usingB.fct(spec.num,which.measure=c('D'))
           dev.off()
           
         },
         "BvsD75corr" = {
           source(file.path(figcode.path, "BvsD75corr.R"))
           ## PDF
           fig.name <- paste(paste("RV-4VWX", spec.num, fig.name, sep="-"), ".pdf", sep="")
           pdf(file.path(fig.path, fig.name), width=6.5, height=5.5)
           BvsD75corr.fct(spec.num)
           dev.off()
         },
         "stratumabundance" = {
           source(file.path(figcode.path, "stratumabundance.R"))
         },
         "depthpref" = {
           source(file.path(figcode.path, "depthpref.R"))
           
           ## PDF
           fig.name <- paste(paste("RV-4VWX", spec.num, fig.name, sep="-"), ".pdf", sep="")
           pdf(file.path(fig.path, fig.name), width=6.5, height=4.5)
           depthpref.fct(spec.num)
           dev.off()
           
         },
         "bottomtemppref" = {
           source(file.path(figcode.path, "bottomtemppref.R"))
           
           ## PDF
           fig.name <- paste(paste("RV-4VWX", spec.num, fig.name, sep="-"), ".pdf", sep="")
           pdf(file.path(fig.path, fig.name), width=6.5, height=4.5)
           bottomtemppref.fct(spec.num)
           dev.off()
         },
         "salinitypref" = {
           source(file.path(figcode.path, "salinitypref.R"))
           
           ## PDF
           fig.name <- paste(paste("RV-4VWX", spec.num, fig.name, sep="-"), ".pdf", sep="")
           pdf(file.path(fig.path, fig.name), width=6.5, height=4.5)
           salinitypref.fct(spec.num)
           dev.off()
         },
         "distribution-indices-usingN" = {
           source(file.path(figcode.path, "distribution-indices-usingN.R"))
         },
         "stratified" = {
           source(file.path(figcode.path, "stratified.R"))
         },
         "lengthfreqNAFO" = {
           source(file.path(figcode.path, "lengthfreqNAFO.R"))
           ## PDF
           fig.name <- paste(paste("RV-4VWX", spec.num, fig.name, sep="-"), ".pdf", sep="")
           pdf(file.path(fig.path, fig.name), width=12, height=6)
           par(mfrow=c(1,2), omi=c(0,0,0,0), mai=c(0.75, 1.2, 0.1, 0.1), ps=16, cex=1, xpd=TRUE, lheight=0.5, xaxs="r", yaxs="r")
           lengthfreqNAFO.fct(spec.num)
           dev.off()
           
           
         },
         "LW" = {
           source(file.path(figcode.path, "LW.R"))
         },
         "conditionNAFO" = {
           source(file.path(figcode.path, "conditionNAFO.R"))
           ## PDF
           fig.name <- paste(paste("RV-4VWX", spec.num, fig.name, sep="-"), ".pdf", sep="")
           pdf(file.path(fig.path, fig.name), width=12, height=6)
           par(mfrow=c(1,2), omi=c(0,0,0,0), mai=c(0.75, 1.2, 0.1, 0.1), ps=16, cex=1, xpd=TRUE, lheight=0.5, xaxs="r", yaxs="r")
           conditionNAFO.fct(spec.num)
           dev.off()
           
           
         },
         "IDWabundance" = {
           source(file.path(figcode.path, "IDWabundance.R"))
         },
         "DDHS" = {
           source(file.path(figcode.path, "DDHS.R"))
           
           ## PDF
           fig.name <- paste(paste("RV-4VWX", spec.num, fig.name, sep="-"), ".pdf", sep="")
           pdf(file.path(fig.path, fig.name), width=12, height=6)
           DDHS.fct(spec.num, "mediantop25")
           dev.off()
           
         },
         "stratifiedN" = {
           source(file.path(figcode.path, "stratifiedN.R"))
         },
         "lengthfreqNAFO" = {
           source(file.path(figcode.path, "lengthfreqNAFO.R"))
         },
         "IDWbiomassSspecies" = {
           source(file.path(figcode.path, "IDWbiomassSspecies.R"))
           
           ## PDF
           fig.name <- paste(paste("RV-4VWX", spec.num, fig.name, sep="-"), ".pdf", sep="")
           pdf(file.path(fig.path, fig.name), width=7.5, height=7.5)
           IDWbiomassSspecies.fct(spec.num)
           dev.off()
           
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
         "conditionNAFO" = {
           source(file.path(figcode.path, "conditionNAFO.R"))
         },
         "IDWabundanceSspecies" = {
           source(file.path(figcode.path, "IDWabundanceSspecies.R"))
         },
         ## case not defined
         {
           print("The character string for this type of figure is not recognised.")
         }
         
  ) # end switch
  
}
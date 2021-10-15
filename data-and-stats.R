## main function to perform data extraction and generate data files to be used in the different figures and maps of the groundfish survey atlas
##
## this function write the appropriate text files that are to be used by the plotting routines and also generate a single Excel data file for each species
## Modification history:
## 2012-09-10: to accommodate the fact that not all analyses can be conducted for each species, I'm adding a switch to handle which data files are to be produced
## description
## extract "catch": catch abundance and biomass data
## extract "envpref": depth, temperature and salinity distribution 
## extract "dist": distribution indices
## extract "stratified": yearly stratified random estimates of abundance and weight
## extract "ddhs": density-dependent habitat selection
## extract "lf": length frequencies
## extract "lw": length-weight relationship
## extract "catchshort": catch abundance and biomass data from 1999 onwards, for inverts
## extract "stratifiedshort": yearly stratified random estimates of abundance and weight from 1999 onwards, for inverts
## extract "distshort":  distribution indices from 1999 onwards, for inverts


data.extract <- function(extract.name, spec.num) {
  mes <- paste("Data extraction request for species code ", spec.num, " and for data ID: ", extract.name, sep="")
  print(mes)
  
  ## switch statement to handle the different types of data extractions and statistics 
    switch(extract.name, 
           "catch" = { 	# catch abundance and biomass data
             source(file.path(dataextract.path, "data-extract-catch.R")) # extract the tow-level data
             
             ## catch abundance and biomass data
             catch.df <- extract.catch.fct(spec.num)
             fn <- paste("SS",spec.num,"_catch.csv",sep="")
             write.csv(catch.df, file.path(figdata.path, fn), row.names=FALSE)
           },
           "envpref" = { 	#
             source(file.path(dataextract.path, "generate-cumul-dist.R")) # uses the tow-level data to generate the cumulative distribution of catches and environmental variables
             
             ## depth distribution
             dat.fn <- paste("SS",spec.num,"_catch.csv",sep="")
             depth.dist.list <- generate.cumul.dist(read.csv(file.path(figdata.path, dat.fn)), "depth")
             depth.dist.df <- depth.dist.list[[1]]
             depth.dist.xt <- depth.dist.list[[2]]
             fn <- paste("SS",spec.num,"_depthdist.csv",sep="")
             fn.xt <- paste("SS",spec.num,"_depthdist.tex",sep="")
             write.csv(depth.dist.df, file.path(figdata.path, fn), row.names=FALSE)
             print.xtable(xtable(depth.dist.xt), type='latex', file=file.path(figdata.path, fn.xt),floating=FALSE)
             
             ## temperature distribution
             #temperature.dist.list <- generate.cumul.dist(extract.catch.fct(spec.num), "temperature")
             temperature.dist.list <- generate.cumul.dist(read.csv(file.path(figdata.path, dat.fn)), "temperature")
             temperature.dist.df <- temperature.dist.list[[1]]
             temperature.dist.xt <- temperature.dist.list[[2]]
             fn <- paste("SS",spec.num,"_temperaturedist.csv",sep="")
             fn.xt <- paste("SS",spec.num,"_temperaturedist.tex",sep="")
             write.csv(temperature.dist.df, file.path(figdata.path, fn), row.names=FALSE)
             print.xtable(xtable(temperature.dist.xt), type='latex', file=file.path(figdata.path, fn.xt),floating=FALSE)
             
             ## salinity distribution
             #salinity.dist.list <- generate.cumul.dist(extract.catch.fct(spec.num), "salinity")
             salinity.dist.list <- generate.cumul.dist(read.csv(file.path(figdata.path, dat.fn)), "salinity")
             salinity.dist.df <- salinity.dist.list[[1]]
             salinity.dist.xt <- salinity.dist.list[[2]]
             fn <- paste("SS",spec.num,"_salinitydist.csv",sep="")
             fn.xt <- paste("SS",spec.num,"_salinitydist.tex",sep="")
             write.csv(salinity.dist.df, file.path(figdata.path, fn), row.names=FALSE)
             print.xtable(xtable(salinity.dist.xt), type='latex', file=file.path(figdata.path, fn.xt),floating=FALSE)
             
             # single xtable with all three
             fn.xt <- paste("SS",spec.num,"_alldist.tex",sep="")
             my.df<-data.frame(Freq=depth.dist.xt[,1],Depth=depth.dist.xt[,2], Temp=temperature.dist.xt[,2], Sal=salinity.dist.xt[,2])
             print.xtable(xtable(my.df,digits=c(0,0,0,1,2)), type='latex', file=file.path(figdata.path, fn.xt), include.rownames=FALSE, floating=FALSE)
           },
           "dist" = { 	#
             ## distribution indices
             source(file.path(dataextract.path, "compute-distribution.R")) # uses the abundance tow-level data to generate yearly distribution indices
             source(file.path(dataextract.path, "compute-distribution-usingbiomass.R")) # uses the biomass tow-level data to generate yearly distribution indices
             
             source(file.path(dataextract.path, "data-extract-catch.R")) # extract the tow-level data
             
             distribution.df <- distribution.fct(extract.catch.fct(spec.num))
             fn <- paste("SS",spec.num,"_distribution.csv",sep="")
             write.csv(distribution.df, file.path(figdata.path, fn), row.names=FALSE)
             
             ## distribution indices using biomass
             distribution.df <- distribution.usingbiomass.fct(extract.catch.fct(spec.num))
             fn <- paste("SS",spec.num,"_distribution-usingbiomass.csv",sep="")
             write.csv(distribution.df, file.path(figdata.path, fn), row.names=FALSE)
             
           },
           "stratified" = { 	#
             ## yearly stratified random estimates of abundance and weight
             source(file.path(dataextract.path, "data-extract-catch.R")) # extract the tow-level data
             source(file.path(dataextract.path, "compute-stratified.R")) # uses the tow-level data to generate yearly stratified random estimates
             
             strat.list <- stratified.fct(extract.catch.fct(spec.num), DDHS=FALSE)
             stratified.df <- strat.list[[1]]
             fn <- paste("SS",spec.num,"_stratified.csv",sep="")
             write.csv(stratified.df, file.path(figdata.path, fn), row.names=FALSE)
           },
           "ddhs" = { 	#
             ## yearly stratified random estimates of abundance and weight and DDHS
             strat.list <- stratified.fct(extract.catch.fct(spec.num), DDHS=TRUE)
             stratified.df <- strat.list[[1]]
             fn <- paste("SS",spec.num,"_stratified.csv",sep="")
             write.csv(stratified.df, file.path(figdata.path, fn), row.names=FALSE)
             # DDHS
             DDHS.list <- strat.list[[2]]
             DDHS.df <- data.frame(par.name=names(coef(DDHS.list[[1]])), lm1.est = as.numeric(coef(DDHS.list[[1]])), glm.poisson.est = as.numeric(coef(DDHS.list[[2]])), glm.nb.est = as.numeric(coef(DDHS.list[[3]])) )
             fn <- paste("SS",spec.num,"_DDHS.csv",sep="")
             write.csv(DDHS.df, file.path(figdata.path, fn), row.names=FALSE)
             
             
             # slope estimates and average stratum abundance
             ll <- length(as.numeric(coef(DDHS.list[[3]])))
             ii <- c(2,seq((ll/2)+2,ll))
             
             slopes.df <- data.frame(stratum=names(DDHS.list[[4]]), mean.n=as.numeric(DDHS.list[[4]]), slope.glm.poisson = (coef(DDHS.list[[2]])[ii]), slope.glm.poisson.stderr = (summary(DDHS.list[[2]])$coefficients[,2][ii]), slope.glm.nb = (coef(DDHS.list[[3]])[ii]), strat.quan95=DDHS.list[[7]], strat.quan75=DDHS.list[[6]], strat.median.top25=DDHS.list[[8]])
             
             fn <- paste("SS",spec.num,"_DDHSslopes.csv",sep="")
             write.csv(slopes.df, file.path(figdata.path, fn), row.names=FALSE)
             
           },
           "lf" = { 	#
             ## length frequencies
             source(file.path(dataextract.path, "length-frequency.R")) # extract length frequency information
             
             lf.list <- lf.fct(spec.num)
             lf.df.all <- lf.list[[1]]
             fn <- paste("SS",spec.num,"_lf.csv",sep="")
             write.csv(lf.df.all, file.path(figdata.path, fn), row.names=FALSE)
             lf.df.4vw <- lf.list[[2]]
             fn <- paste("SS",spec.num,"_lf4vw.csv",sep="")
             write.csv(lf.df.4vw, file.path(figdata.path, fn), row.names=FALSE)
             lf.df.4x <- lf.list[[3]]
             fn <- paste("SS",spec.num,"_lf4x.csv",sep="")
             write.csv(lf.df.4x, file.path(figdata.path, fn), row.names=FALSE)
             
           },
           "lw" = { 	#
             ## length-weight relationship
             source(file.path(dataextract.path, "length-weight.R")) # extract length-weight information
             
             lw.list <- lw.fct(spec.num)
             lw.df <- lw.list[[1]]
             fn <- paste("SS",spec.num,"_lw.csv",sep="")
             write.csv(lw.df, file.path(figdata.path, fn), row.names=FALSE)
             
             lw.df.nafo4x <- lw.list[[2]]
             fn <- paste("SS",spec.num,"_lw4x.csv",sep="")
             write.csv(lw.df.nafo4x, file.path(figdata.path, fn), row.names=FALSE)
             
             lw.df.nafo4vw <- lw.list[[3]]
             fn <- paste("SS",spec.num,"_lw4vw.csv",sep="")
             write.csv(lw.df.nafo4vw, file.path(figdata.path, fn), row.names=FALSE)
             
           },
           "catchshort" = { 	#
             ## catch abundance and biomass data from 1999 onwards, for inverts
             source(file.path(dataextract.path, "data-extract-catch-short.R")) # extract the tow-level data, 1999 onwards
             
             catch.df <- extract.catch.short.fct(spec.num)
             fn <- paste("SS",spec.num,"_catch.csv",sep="")
             write.csv(catch.df, file.path(figdata.path, fn), row.names=FALSE)
             
           },
           "stratifiedshort" = { 	#
             ## yearly stratified random estimates of abundance and weight
             source(file.path(dataextract.path, "compute-stratified.R")) # uses the tow-level data to generate yearly stratified random estimates
             
             strat.list <- stratified.fct(extract.catch.short.fct(spec.num), DDHS=FALSE)
             stratified.df <- strat.list[[1]]
             fn <- paste("SS",spec.num,"_stratified.csv",sep="")
             write.csv(stratified.df, file.path(figdata.path, fn), row.names=FALSE)
           },
           "distshort" = { 	#
             ## distribution indices
             source(file.path(dataextract.path, "data-extract-catch-short.R")) # extract the tow-level data, 1999 onwards
             
             distribution.df <- distribution.fct(extract.catch.short.fct(spec.num))
             fn <- paste("SS",spec.num,"_distribution.csv",sep="")
             write.csv(distribution.df, file.path(figdata.path, fn), row.names=FALSE)
             
             ## distribution indices using biomass
             distribution.df <- distribution.usingbiomass.fct(extract.catch.short.fct(spec.num))
             fn <- paste("SS",spec.num,"_distribution-usingbiomass.csv",sep="")
             write.csv(distribution.df, file.path(figdata.path, fn), row.names=FALSE)
             
           },
           ## case not defined
           {
             print("The character string for this type of data extraction and computation is not recognised.")
           }
           
    ) # end switch
  
}

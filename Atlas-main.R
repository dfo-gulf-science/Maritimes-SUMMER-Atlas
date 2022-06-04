#--------------------------------------------------------------------------#
## main R script to generate the figures required in the DFO Maritimes groundfish survey atlas


# load required libraries

# list required libraries, and install if necessary
necessary <- c("PBSmapping","spatstat","zoo","classInt","RColorBrewer","gstat","maptools",
               "foreign","fields","spam","rgeos", "RODBC", 
               "xtable", "MASS", 
               #"xlsx", 
               "raster", "rgdal",
               "here","worms",
               "ncdf4","sf",
               "ggplot2")
installed <- necessary %in% installed.packages()[, 'Package']
if (length(necessary[!installed]) >=1) install.packages(necessary[!installed], repos='http://mirror.its.dal.ca/cran/')

## now load required packages
lapply(necessary, require, character.only=TRUE)

## since we are working from a git repository, use the "here" library to set the paths relative to the root path of the git repo
main.path <- here::here()


dataextract.path <- file.path(main.path, "Data-extract") ## data extraction scripts
figdata.path <- file.path(main.path, "Figures-data"); if(!dir.exists(figdata.path)) dir.create(figdata.path) ## where to store data for figures
figcode.path <- file.path(main.path, "Figures-code") ## where to find code for figures
fig.path <- file.path(main.path, "Figures-actual") ; if(!dir.exists(fig.path)) dir.create(fig.path) ## where to store figures (for report)
mapping.path <- file.path(main.path, "Mapping") ## mapping folder, maps of strata, etc.
report.path <- file.path(main.path, "Report-generation")
actualreport.path <- file.path(main.path, "TechReport-EN")


#####################################################################################################################################################  
## STEP 1 - GENERATE MAPS
source(file.path(mapping.path, "annotated-NAFO-maps.R")) ## map of the region with annotations, map of NAFO areas, ...
source(file.path(main.path, "chan.R"))
# chan.R edit to reflect database credentials, or alternatively use yours: chan <- odbcConnect(dsn='biobank', uid='', pwd='')
source(file.path(mapping.path, "SUMMER-strata-maps.R")) ## map of the SUMMER strata and map of the location of survey tows
RODBC::odbcClose(chan)

source(file.path(mapping.path, "Summer-timeline.R")) ## survey timeline inspired by Wikipedia graphic of music band members
#####################################################################################################################################################  


#####################################################################################################################################################  
## STEP 2 - DATA EXTRACTIONS
## open ODBC connection to Oracle database
source(file.path(main.path, "chan.R"))
# chan.R edit to reflect database credentials, or alternatively use yours: chan <- odbcConnect(dsn='biobank', uid='', pwd='')

## these 3 scripts churn the contents of the RV database, summarise tows, and identifies/ranks the species based on the number of records
## -> these files should be run only when a new year of data is available and the Atlas is to be updated
#source(file.path(main.path, "summaries.R")) ## summaries of tows, which is stored in the file "Report-generation/Atlas-summary-table-tows-by-year-stratum.csv" 
#source(file.path(main.path, "summaries-catch-records.R"), encoding="UTF-8") ## summaries of catch, which is stored in "Report-generation/species-list-for-report.csv" 
#source(file.path(main.path, "summaries-taxonomy.R"), encoding="UTF-8") ## summary of records with taxonomic details, which is stored in "Report-generation/species-list-for-report-APHIA.csv"

# this creates a CSV file with the strata statistics that is used for a table in the Tech Report
#source(file.path(dataextract.path, "strata-statistics.R"))

## this creates the figures of environmental CDFs for the survey
#source(file.path(dataextract.path, "Perry-Smith-summer.R"))


# source the code that defines the data extraction functions
source(file.path(main.path, "data-and-stats.R"))

spec.list <- read.csv(file.path(main.path, "species-list-for-report.csv"),header=TRUE) # this list is itself generated from the above "summaries.R", which requires database connection and connection to WORMS to get AphiaID

## test for John Dory
## data.extract(extract.name="stratified", spec.num=704)

## test for cod
## data.extract(extract.name="envpref", spec.num=10)
## data.extract(extract.name="catch", spec.num=10)

species.LF <- spec.list[spec.list$type=='LF',]$spec # long timeseries, using catch weights
species.SF <- spec.list[spec.list$type=='SF',]$spec # short timeseries
species.LR <- spec.list[spec.list$type=='LR',]$spec # long timeseries rare
species.SR <- spec.list[spec.list$type=='SR',]$spec # short timeseries rare
species.LI <- spec.list[spec.list$type=='LI',]$spec # intermediate species, using catch weights
species.LIn <- spec.list[spec.list$type=='LIn',]$spec # intermediate species, using catch numbers

## LF species
species.numbers <- species.LF
l.species.extracts <- c("catch","stratified","envpref","distusingB","ddhs","lf","lw")
print(paste("Starting data extracts, LF species: ", Sys.time()))
lapply(species.numbers, function(ss){lapply(l.species.extracts, function(de){data.extract(extract.name=de, spec.num=ss)})})
print(paste("End data extract, LF species: ", Sys.time()))

## SF species
species.numbers <- species.SF
s.species.extracts <- c("catchshort","stratifiedshort","distshort")
print(paste("Starting data extracts, SF species: ", Sys.time()))
lapply(species.numbers, function(ss){lapply(s.species.extracts, function(de){data.extract(extract.name=de, spec.num=ss)})})
print(paste("End data extract, SF species: ", Sys.time()))

## R species
species.numbers <- c(species.LR, species.SR)
r.species.extracts <- c("catch")
print(paste("Starting data extracts, R species: ", Sys.time()))
lapply(species.numbers, function(ss){lapply(r.species.extracts, function(de){data.extract(extract.name=de, spec.num=ss)})})
print(paste("End data extract, R species: ", Sys.time()))

## LI species
species.numbers <- species.LI
i.species.extracts <- c("catch","stratified","distusingB")
print(paste("Starting data extracts, I species: ", Sys.time()))
lapply(species.numbers, function(ss){lapply(i.species.extracts, function(de){data.extract(extract.name=de, spec.num=ss)})})
print(paste("End data extract, I species: ", Sys.time()))

## LIn species
species.numbers <- species.LIn
i.species.extracts <- c("catch","stratified","distusingN")
print(paste("Starting data extracts, I species: ", Sys.time()))
lapply(species.numbers, function(ss){lapply(i.species.extracts, function(de){data.extract(extract.name=de, spec.num=ss)})})
print(paste("End data extract, I species: ", Sys.time()))


## bottom organisms that we shouldn't catch
## 6500 - sand dollar
data.extract(extract.name="catch", spec.num=6500)
data.extract(extract.name="stratified", spec.num=6500)
data.extract(extract.name="dist", spec.num=6500)
# RODBC::odbcClose(chan)

#####################################################################################################################################################  


#####################################################################################################################################################  
## STEP 3 - GENERATE FIGURES
source(file.path(main.path, "figures.R"))

## LF species
species.numbers <- c(species.LF)
lf.figures <- c("IDWbiomass","stratifiedB","distribution-indices-usingB","BvsD75corr","lengthfreqNAFO","conditionNAFO","depthpref","bottomtemppref","salinitypref","DDHS")
print(paste("Starting figures, LF species: ", Sys.time()))
lapply(species.numbers, function(ss){lapply(lf.figures, function(ff){make.figure(fig.name=ff, spec.num=ss)})})
print(paste("End figures, LF species: ", Sys.time()))

## SF species
species.numbers <- c(species.SF)
sf.figures <- c("IDWbiomassSspecies","stratifiedB","distribution-indices-usingB","BvsD75corr")
print(paste("Starting figures, SF species: ", Sys.time()))
lapply(species.numbers, function(ss){lapply(sf.figures, function(ff){make.figure(fig.name=ff, spec.num=ss)})})
print(paste("End figures, SF species: ", Sys.time()))

## LI species
species.numbers <- c(species.LI)
li.figures <- c("IDWbiomass","stratifiedB","distribution-indices-usingB","BvsD75corr")
print(paste("Starting figures, LI species: ", Sys.time()))
lapply(species.numbers, function(ss){lapply(li.figures, function(ff){make.figure(fig.name=ff, spec.num=ss)})})
print(paste("End figures, LI species: ", Sys.time()))

## LIn species
species.numbers <- c(species.LIn)
li.figures <- c("IDWabundance","stratifiedN","distribution-indices-usingN","NvsD75corr")
print(paste("Starting figures, LI species: ", Sys.time()))
lapply(species.numbers, function(ss){lapply(li.figures, function(ff){make.figure(fig.name=ff, spec.num=ss)})})
print(paste("End figures, LI species: ", Sys.time()))

## R species
species.numbers <- c(species.LR, species.SR)
r.figures <- c("presence")
print(paste("Starting figures, R species: ", Sys.time()))
lapply(species.numbers, function(ss){lapply(r.figures, function(ff){make.figure(fig.name=ff, spec.num=ss)})})
print(paste("End figures, R species: ", Sys.time()))

## bottom organisms that we shouldn't catch
## not in the Atlas, mostly to investigate the 19ft wire
## on the Western IIA trawl
## 6500 - sand dollar
make.figure(fig.name="IDWbiomass", spec.num=6500)
make.figure(fig.name="stratifiedB", spec.num=6500)
make.figure(fig.name="distribution-indices-usingB", spec.num=6500)

#####################################################################################################################################################  

#####################################################################################################################################################  
## STEP 4 - GENERATE FILES FOR REPORT
source(file.path(main.path, "tech-report.R"), encoding="UTF-8")

#####################################################################################################################################################  


## at this stage, all data extractions have been performed, all the figures have been generated, and the dynamic contents of the Technical Report have been saved to files
## - it is now time to go in the TechReport-EN folder and render the document using csasdown

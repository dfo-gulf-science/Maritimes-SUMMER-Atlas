#--------------------------------------------------------------------------#
## main R script to generate the figures required in the DFO Maritimes groundfish survey atlas

# list required libraries, and install if necessary
necessary <- c("PBSmapping","spatstat","zoo","classInt","RColorBrewer","gstat","maptools",
               "foreign","fields","spam","rgeos", "RODBC", 
               "xtable", "MASS", "xlsx", "raster", "rgdal",
               "here","worms",
               "raster","ncdf4")
installed <- necessary %in% installed.packages()[, 'Package']
if (length(necessary[!installed]) >=1) install.packages(necessary[!installed], repos='http://mirror.its.dal.ca/cran/')

## now load required packages
lapply(necessary, require, character.only=TRUE)

## since we are working from a git repository, use the "here" library to set the paths relative to the root path of the git repo
main.path <- here::here()

report.path <- file.path(main.path, "Report-generation")
actualreport.path <- file.path(main.path, "TechReport-EN")
figdata.path <- file.path(main.path, "Figures-data") ## where to store data for figures
figcode.path <- file.path(main.path, "Figures-code") ## where to find code for figures
fig.path <- file.path(main.path, "Figures-actual") ## where to store figures (for report)
mapping.path <- file.path(main.path, "Mapping") ## mapping folder, maps of strata, etc.


#####################################################################################################################################################  
## STEP 1 - GENERATE MAPS
source(file.path(mapping.path, "annotated-NAFO-maps.R")) ## map of the region with annotations, map of NAFO areas, ...
source(file.path(main.path, "chan.R"))
# chan.R edit to reflect database credentials, or alternatively use yours: chan <- odbcConnect(dsn='biobank', uid='', pwd='')
source(file.path(mapping.path, "SUMMER-strata-maps.R")) ## map of the SUMMER strata and map of the location of survey tows
RODBC::odbcClose(chan)
#####################################################################################################################################################  


#####################################################################################################################################################  
## STEP 2 - DATA EXTRACTIONS
## open ODBC connection to Oracle database
source(file.path(main.path, "chan.R"))
# chan.R edit to reflect database credentials, or alternatively use yours: chan <- odbcConnect(dsn='biobank', uid='', pwd='')

## these 3 scripts churn the contents of the RV database, summarise tows, and identifies and ranks the species based on the number of records
## -> these files should be run only when a new year of data is available and the Atlas is to be updated
#source(file.path(main.path, "summaries.R")) ## summaries of tows, which is stored in the file "Report-generation/Atlas-summary-table-tows-by-year-stratum.csv" 
#source(file.path(main.path, "summaries-catch-records.R")) ## summaries of catch, which is stored in "Report-generation/species-list-for-report.csv" 
#source(file.path(main.path, "summaries-taxonomy.R")) ## summary of records with taxonomic details, which is stored in "Report-generation/species-list-for-report-APHIA.csv"

# source the code that defines the data extraction functions
source(file.path(main.path, "data-and-stats.R"))

## L species
l.species.extracts <- c("")
print(paste("Starting data extracts, L species: ", Sys.time()))
sapply(species.numbers, function(i){data.extract(extract.name=l.species.extracts, spec.num=i)})
print(paste("End data extract, L species: ", Sys.time()))

## R species
r.species.extracts <- c("")
print(paste("Starting data extracts, R species: ", Sys.time()))
sapply(species.numbers, function(i){data.extract(extract.name=r.species.extracts, spec.num=i)})
print(paste("End data extract, R species: ", Sys.time()))



#####################################################################################################################################################  


#####################################################################################################################################################  
## STEP 3 - GENERATE FIGURES
source(file.path(main.path, "figures.R"))
#####################################################################################################################################################  

#####################################################################################################################################################  
## STEP 4 - GENERATE FILES FOR REPORT
source(file.path(main.path, "tech-report.R"))

#####################################################################################################################################################  


## at this stage, all data extractions have been performed, all the figures have been generated, and the dynamic contents of the Technical Report have been saved to files
## - it is now time to go in the TechReport-EN folder and render the document using csasdown

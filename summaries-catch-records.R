before <- ls()

## tows
qu <- paste("
SELECT 
mission,
setno,
strat,
sdate,
TO_CHAR(sdate,'yyyy') YEAR,
TO_CHAR(sdate,'mm') MONTH,
TO_CHAR(sdate,'dd') DAY,
dmin,
dmax,
bottom_temperature,
bottom_salinity,
dist,
gear
FROM groundfish.gsinf
where
type=1
order by YEAR, mission, setno
", sep="")

tows.df <- sqlQuery(chan, qu, stringsAsFactors=FALSE)

tows.summer.df <- subset(
        tows.df, 
        (STRAT == '440' | STRAT == '441' | STRAT == '442' | STRAT == '443' | STRAT == '444' | STRAT == '445' | STRAT == '446' | STRAT == '447' | STRAT == '448' | STRAT == '449' | 
                 STRAT == '450' | STRAT == '451' | STRAT == '452' | STRAT == '453' | STRAT == '454' | STRAT == '455' | STRAT == '456' | STRAT == '457' | STRAT == '458' | STRAT == '459' | 
                 STRAT == '460' | STRAT == '461' | STRAT == '462' | STRAT == '463' | STRAT == '464' | STRAT == '465' | STRAT == '466' | 
                 STRAT == '470' | STRAT == '471' | STRAT == '472' | STRAT == '473' | STRAT == '474' | STRAT == '475' | STRAT == '476' | STRAT == '477' | STRAT == '478' | 
                 STRAT == '480' | STRAT == '481' | STRAT == '482' | STRAT == '483' | STRAT == '484' | STRAT == '485' | 
                 STRAT == '490' | STRAT == '491' | STRAT == '492' | STRAT == '493' | STRAT == '494' | STRAT == '495' ) 
        & (MONTH == 6 | MONTH == 7 | MONTH == 8)
        & (YEAR <= 2020)
)

## taxonomic summary
## list of species 

# catch records - this query returns a huge data frame, I guess some summaries could be done on the SQL side
qu <- paste("
select * from groundfish.gscat
where spec < 9000
", sep="")
all.catch <- sqlQuery(chan,qu, stringsAsFactors=FALSE)

# species list from user groundfish
qu <- paste("
select * from groundfish.gsspecies
where CODE < 9000 and
TRUNC(MOD(CODE/1000,4),1)!=1 
and CODE not in (6100,2100,2560,2522,2200,4500,2525,2519,2561,2520)
", sep="")
species.all <- sqlQuery(chan,qu, stringsAsFactors = FALSE)
colnames(species.all)[colnames(species.all) == 'SPEC'] <- 'SCIEN' 
colnames(species.all)[colnames(species.all) == 'CODE'] <- 'SPEC' 

m1.df <- merge(tows.summer.df, all.catch, 
               by.x=c('MISSION','SETNO'), 
               by.y=c('MISSION','SETNO'))


m2.df <- merge(m1.df, species.all, 
                      all.y=FALSE, 
                      by='SPEC')
catch.summer <- m2.df

catch.summer <- droplevels(catch.summer)
catch.by.species <- table(catch.summer$SPEC)
catch.by.species.df <- data.frame(catch.by.species)
catch.by.scien <- table(catch.summer$SCIEN)
catch.by.scien.df <- data.frame(catch.by.scien)

m1.df <- merge(species.all, catch.by.species.df, 
               by.x='SPEC', by.y='Var1')

m2.df <- merge(m1.df, catch.by.scien.df, 
                   by.x='SCIEN', 
                   by.y='Var1')
merged.df <- m2.df

oo<-order(merged.df$Freq.x, decreasing = TRUE)
ordered.df <- merged.df[oo,]

df.summary <- subset(ordered.df, Freq.x >= 10)
df.summary <- df.summary[c("SCIEN", "SPEC", "COMM", "NMFS", "Freq.x", "Freq.y")]
names(df.summary) <- c("scien","spec","comm","fao","nrecords","nrecords2")

## bring some taxonomic details from the itis table
qu <- paste("
select * from
groundfish.itis_gs_taxon
", sep="")
itis.all <- sqlQuery(chan,qu, stringsAsFactors=FALSE)


df.for.xtable <- merge(df.summary, itis.all, by.x="spec", 
                       by.y="GIVEN_SPEC_CODE")[,c(1,2,3,5,7,11,13,14:17,9,20,21)]


## 
# what category is the species, based on its taxonomy and number of records
df.for.xtable$type <- ifelse(df.for.xtable$nrecords <= 200 & 
                               df.for.xtable$ORDER=='Decapoda', "SR", 
                             ifelse(df.for.xtable$nrecords <= 200, "LR", 
                                    ifelse(df.for.xtable$ORDER=='Decapoda',"SF",
                                           ifelse(df.for.xtable$nrecords < 1500, "LI","LF")) ))
##
## manually clean up to remove undesired entries
df.for.xtable <- subset(df.for.xtable, !(spec %in% c(4521,4321,2210,642,2600,4320,4514,500,323)))

oo <- order(df.for.xtable$nrecords, decreasing=TRUE)

spec.xtable.df <- df.for.xtable[oo,c(5,13,14,1,4,10,9,8,15)]

## some common names are missing and others are wrong, fix these manually

# fix french names
spec.xtable.df[spec.xtable.df$spec==10,]$FAO_F_COMMON_NAME <- "Morue franche"
spec.xtable.df[spec.xtable.df$spec==11,]$FAO_F_COMMON_NAME <- "Aiglefin"
spec.xtable.df[spec.xtable.df$spec==41,]$FAO_F_COMMON_NAME <- "Plie grise"

spec.xtable.df[spec.xtable.df$spec==52,]$FAO_F_COMMON_NAME <- "Loup à tête large"
spec.xtable.df[spec.xtable.df$spec==300,]$FAO_F_COMMON_NAME <- "Chaboisseau à dix-huit épines"
spec.xtable.df[spec.xtable.df$spec==304,]$FAO_F_COMMON_NAME <- "Faux-trigle armé"
spec.xtable.df[spec.xtable.df$spec==160,]$FAO_F_COMMON_NAME <- "Grande argentine"
spec.xtable.df[spec.xtable.df$spec==400,]$FAO_F_COMMON_NAME <- "Baudroie d'Amérique"
spec.xtable.df[spec.xtable.df$spec==112,]$FAO_F_COMMON_NAME <- "Merluche à longues nageoires"

# fix english names
spec.xtable.df[spec.xtable.df$spec==15,]$FAO_E_COMMON_NAME <- "Cusk"

# fix both french and english names
spec.xtable.df[spec.xtable.df$spec==40,]$FAO_E_COMMON_NAME <- "American plaice"
spec.xtable.df[spec.xtable.df$spec==40,]$FAO_F_COMMON_NAME <- "Plie canadienne"

spec.xtable.df[spec.xtable.df$spec==16,]$FAO_E_COMMON_NAME <- "Pollock"
spec.xtable.df[spec.xtable.df$spec==16,]$FAO_F_COMMON_NAME <- "Goberge"

spec.xtable.df[spec.xtable.df$spec==320,]$FAO_E_COMMON_NAME <- "Sea raven"
spec.xtable.df[spec.xtable.df$spec==320,]$FAO_F_COMMON_NAME <- "Hémitriptère atlantique"

spec.xtable.df[spec.xtable.df$spec==201,]$FAO_E_COMMON_NAME <- "Thorny skate"
spec.xtable.df[spec.xtable.df$spec==201,]$FAO_F_COMMON_NAME <- "Raie épineuse"

spec.xtable.df[spec.xtable.df$spec==202,]$FAO_E_COMMON_NAME <- "Smooth skate"
spec.xtable.df[spec.xtable.df$spec==202,]$FAO_F_COMMON_NAME <- "Raie lisse"

spec.xtable.df[spec.xtable.df$spec==203,]$FAO_E_COMMON_NAME <- "Little skate"
spec.xtable.df[spec.xtable.df$spec==203,]$FAO_F_COMMON_NAME <- "Raie hérisson"

spec.xtable.df[spec.xtable.df$spec==204,]$FAO_E_COMMON_NAME <- "Winter skate"
spec.xtable.df[spec.xtable.df$spec==204,]$FAO_F_COMMON_NAME <- "Raie tachetée"

spec.xtable.df[spec.xtable.df$spec==241,]$FAO_E_COMMON_NAME <- "Atlantic hagfish"
spec.xtable.df[spec.xtable.df$spec==241,]$FAO_F_COMMON_NAME <- "Myxine du nord"

spec.xtable.df[spec.xtable.df$spec==610,]$FAO_E_COMMON_NAME <- "Sand lance"
spec.xtable.df[spec.xtable.df$spec==610,]$FAO_F_COMMON_NAME <- "Lançon"

spec.xtable.df[spec.xtable.df$spec==340,]$FAO_E_COMMON_NAME <- "Alligatorfish"
spec.xtable.df[spec.xtable.df$spec==340,]$FAO_F_COMMON_NAME <- "Poisson-alligator atlantique"

spec.xtable.df[spec.xtable.df$spec==647,]$FAO_E_COMMON_NAME <- "Vahl's eelpout"
spec.xtable.df[spec.xtable.df$spec==647,]$FAO_F_COMMON_NAME <- "Lycode à carreaux"

spec.xtable.df[spec.xtable.df$spec==410,]$FAO_E_COMMON_NAME <- "Marlin-spike grenadier"
spec.xtable.df[spec.xtable.df$spec==410,]$FAO_F_COMMON_NAME <- "Grenadier du Grand Banc"

spec.xtable.df[spec.xtable.df$spec==712,]$FAO_E_COMMON_NAME <- "White barracudina"
spec.xtable.df[spec.xtable.df$spec==712,]$FAO_F_COMMON_NAME <- "Lussion blanc"

spec.xtable.df[spec.xtable.df$spec==2527,]$FAO_F_COMMON_NAME <- "Crabe lyre araignée"
spec.xtable.df[spec.xtable.df$spec==2527,]$FAO_E_COMMON_NAME <- "Great spider crab"

spec.xtable.df[spec.xtable.df$spec==2521,]$FAO_F_COMMON_NAME <- "Crabe Hyas coarctatus"
spec.xtable.df[spec.xtable.df$spec==2521,]$FAO_E_COMMON_NAME <- "Arctic lyre crab"

spec.xtable.df[spec.xtable.df$spec==741,]$FAO_E_COMMON_NAME <- "Hatchetfishes"
spec.xtable.df[spec.xtable.df$spec==741,]$FAO_F_COMMON_NAME <- "Haches d'argent"

spec.xtable.df[spec.xtable.df$spec==619,]$ACCEPTED_SCIENT_NAME <- "Lycodes terraenovae"
spec.xtable.df[spec.xtable.df$spec==619,]$FAO_E_COMMON_NAME <- "Newfoundland eelpout"
spec.xtable.df[spec.xtable.df$spec==619,]$FAO_F_COMMON_NAME <- "Lycode du Labrador"
spec.xtable.df[spec.xtable.df$spec==619,]$CLASS_ <- "Actinopterygii"
spec.xtable.df[spec.xtable.df$spec==619,]$ORDER_ <- "Perciformes"
spec.xtable.df[spec.xtable.df$spec==619,]$FAMILY_ <- "Zoarcidae"
spec.xtable.df[spec.xtable.df$spec==619,]$type <- "LR"

spec.xtable.df[spec.xtable.df$spec==620,]$FAO_E_COMMON_NAME <- "Newfoundland eelpout"
spec.xtable.df[spec.xtable.df$spec==620,]$FAO_F_COMMON_NAME <- "Lycode du Labrador"

spec.xtable.df[spec.xtable.df$spec==142,]$FAO_E_COMMON_NAME <- "Fourspot flounder"
spec.xtable.df[spec.xtable.df$spec==142,]$FAO_F_COMMON_NAME <- "Cardeau à quatre ocelles"

spec.xtable.df[spec.xtable.df$spec==156,]$FAO_E_COMMON_NAME <- "Shortnose greeneye"
spec.xtable.df[spec.xtable.df$spec==156,]$FAO_F_COMMON_NAME <- "Éperlan du large"

spec.xtable.df[spec.xtable.df$spec==149,]$FAO_E_COMMON_NAME <- "Longnose greeneye"
spec.xtable.df[spec.xtable.df$spec==149,]$FAO_F_COMMON_NAME <- "Oeil-vert à long nez"

spec.xtable.df[spec.xtable.df$spec==412,]$FAO_E_COMMON_NAME <- "Roughnose grenadier"
spec.xtable.df[spec.xtable.df$spec==412,]$FAO_F_COMMON_NAME <- "Grenadier-scie"

spec.xtable.df[spec.xtable.df$spec==400,]$FAO_E_COMMON_NAME <- "Monkfish"

spec.xtable.df[spec.xtable.df$spec==742,]$FAO_E_COMMON_NAME <- "Atlantic batfish"
spec.xtable.df[spec.xtable.df$spec==742,]$FAO_F_COMMON_NAME <- "Malthe atlantique"

spec.xtable.df[spec.xtable.df$spec==150,]$FAO_E_COMMON_NAME <- "Lanternfishes"
spec.xtable.df[spec.xtable.df$spec==150,]$FAO_F_COMMON_NAME <- "Poissons-lanternes"

spec.xtable.df[spec.xtable.df$spec==63,]$FAO_F_COMMON_NAME <- "Éperlan arc-en-ciel"

spec.xtable.df[spec.xtable.df$spec==637,]$FAO_E_COMMON_NAME <- "Spotfin dragonet"
spec.xtable.df[spec.xtable.df$spec==637,]$FAO_F_COMMON_NAME <- "Dragonnet tacheté"

spec.xtable.df[spec.xtable.df$spec==630,]$FAO_E_COMMON_NAME <- "Wrymouth"
spec.xtable.df[spec.xtable.df$spec==630,]$FAO_F_COMMON_NAME <- "Terrassier tacheté"

spec.xtable.df[spec.xtable.df$spec==621,]$FAO_F_COMMON_NAME <- "Sigouine de roche"

spec.xtable.df[spec.xtable.df$spec==623,]$FAO_E_COMMON_NAME <- "Daubed shanny"
spec.xtable.df[spec.xtable.df$spec==623,]$FAO_F_COMMON_NAME <- "Lompénie tachetée"


spec.xtable.df[spec.xtable.df$spec==622,]$FAO_E_COMMON_NAME <- "Snakeblenny"
spec.xtable.df[spec.xtable.df$spec==622,]$FAO_F_COMMON_NAME <- "Lompénie-serpent"

spec.xtable.df[spec.xtable.df$spec==816,]$FAO_E_COMMON_NAME <- "Spottedfin tonguefish"
spec.xtable.df[spec.xtable.df$spec==816,]$FAO_F_COMMON_NAME <- "Langue fil noir"

spec.xtable.df[spec.xtable.df$spec==350,]$FAO_F_COMMON_NAME <- "Agone atlantique"

spec.xtable.df[spec.xtable.df$spec==341,]$FAO_F_COMMON_NAME <- "Poisson-alligator arctique"

spec.xtable.df[spec.xtable.df$spec==306,]$FAO_E_COMMON_NAME <- "Arctic hookear sculpin"
spec.xtable.df[spec.xtable.df$spec==306,]$FAO_F_COMMON_NAME <- "Hameçon neigeux"

spec.xtable.df[spec.xtable.df$spec==880,]$FAO_E_COMMON_NAME <- "Atlantic hookear sculpin"
spec.xtable.df[spec.xtable.df$spec==880,]$FAO_F_COMMON_NAME <- "Hameçon atlantique"

spec.xtable.df[spec.xtable.df$spec==301,]$FAO_E_COMMON_NAME <- "Shorthorn sculpin"
spec.xtable.df[spec.xtable.df$spec==301,]$FAO_F_COMMON_NAME <- "Chaboisseau à épines courtes"

spec.xtable.df[spec.xtable.df$spec==303,]$FAO_E_COMMON_NAME <- "Grubby"
spec.xtable.df[spec.xtable.df$spec==303,]$FAO_F_COMMON_NAME <- "Chaboisseau bronzé"

spec.xtable.df[spec.xtable.df$spec==501,]$FAO_E_COMMON_NAME <- "Lumpfish"

spec.xtable.df[spec.xtable.df$spec==502,]$FAO_E_COMMON_NAME <- "Atlantic spiny lumpsucker"
spec.xtable.df[spec.xtable.df$spec==502,]$FAO_F_COMMON_NAME <- "Petite poule de mer atlantique"

spec.xtable.df[spec.xtable.df$spec==503,]$FAO_E_COMMON_NAME <- "Atlantic seasnail"
spec.xtable.df[spec.xtable.df$spec==503,]$FAO_F_COMMON_NAME <- "Limace atlantique"

spec.xtable.df[spec.xtable.df$spec==512,]$FAO_F_COMMON_NAME <- "Limace marbée"

spec.xtable.df[spec.xtable.df$spec==505,]$FAO_E_COMMON_NAME <- "Gelatinous snailfish"
spec.xtable.df[spec.xtable.df$spec==505,]$FAO_F_COMMON_NAME <- "Limace gélatineuse"

spec.xtable.df[spec.xtable.df$spec==520,]$FAO_E_COMMON_NAME <- "Sea tadpole"
spec.xtable.df[spec.xtable.df$spec==520,]$FAO_F_COMMON_NAME <- "Petite limace de mer"

spec.xtable.df[spec.xtable.df$spec==307,]$FAO_F_COMMON_NAME <- "Cotte polaire"

spec.xtable.df[spec.xtable.df$spec==23,]$FAO_E_COMMON_NAME <- "Atlantic redfishes"
spec.xtable.df[spec.xtable.df$spec==23,]$FAO_F_COMMON_NAME <- "Sébastes de l'Atlantique"

spec.xtable.df[spec.xtable.df$spec==158,]$FAO_F_COMMON_NAME <- "Brossé améthyste"

spec.xtable.df[spec.xtable.df$spec==159,]$FAO_F_COMMON_NAME <- "Dragon-boa"

spec.xtable.df[spec.xtable.df$spec==704,]$FAO_E_COMMON_NAME <- "Silvery John dory"
spec.xtable.df[spec.xtable.df$spec==704,]$FAO_F_COMMON_NAME <- "Saint Pierre argenté"

spec.xtable.df[spec.xtable.df$spec==200,]$FAO_E_COMMON_NAME <- "Barndoor skate"
spec.xtable.df[spec.xtable.df$spec==200,]$FAO_F_COMMON_NAME <- "Grande raie"

spec.xtable.df[spec.xtable.df$spec==4512,]$FAO_E_COMMON_NAME <- "Longfin inshore squid"
spec.xtable.df[spec.xtable.df$spec==4512,]$FAO_F_COMMON_NAME <- "Calmar totam"

spec.xtable.df[spec.xtable.df$spec==2532,]$FAO_E_COMMON_NAME <- "Red deepsea crab"
spec.xtable.df[spec.xtable.df$spec==2532,]$FAO_F_COMMON_NAME <- "Crabe rouge"

spec.xtable.df[spec.xtable.df$spec==2523,]$FAO_E_COMMON_NAME <- "Atlantic king crab"
spec.xtable.df[spec.xtable.df$spec==2523,]$FAO_F_COMMON_NAME <- "Crabe épineux du nord"

spec.xtable.df[spec.xtable.df$spec==604,]$FAO_F_COMMON_NAME <- "Avocette ruban"

spec.xtable.df[spec.xtable.df$spec==625,]$FAO_F_COMMON_NAME <- "Ulvaire deux-lignes"

spec.xtable.df[spec.xtable.df$spec==641,]$FAO_F_COMMON_NAME <- "Lycode arctique"

spec.xtable.df[spec.xtable.df$spec==646,]$FAO_F_COMMON_NAME <- "Mollasse atlantique"

spec.xtable.df[spec.xtable.df$spec==603,]$FAO_F_COMMON_NAME <- "Lycode à tête longue"

spec.xtable.df[spec.xtable.df$spec==44,]$FAO_F_COMMON_NAME <- "Plie du Gulf Stream"

spec.xtable.df[spec.xtable.df$spec==351,]$FAO_E_COMMON_NAME <- "Alligatorfishes"
spec.xtable.df[spec.xtable.df$spec==351,]$FAO_F_COMMON_NAME <- "Poissons-alligator"

spec.xtable.df[spec.xtable.df$spec==314,]$FAO_E_COMMON_NAME <- "Spatulate sculpin"
spec.xtable.df[spec.xtable.df$spec==314,]$FAO_F_COMMON_NAME <- "Icèle spatulée"

spec.xtable.df[spec.xtable.df$spec==640,]$FAO_E_COMMON_NAME <- "Ocean pout"
spec.xtable.df[spec.xtable.df$spec==640,]$FAO_F_COMMON_NAME <- "Loquette d'Amérique"

spec.xtable.df[spec.xtable.df$spec==220,]$FAO_E_COMMON_NAME <- "Piked dogfish"

## fix the species that have an "unaccepted" status on WoRMS
# Loligo pealeii, 4512
# Zenopsis conchifera, 704
spec.xtable.df[spec.xtable.df$spec==4512,]$ACCEPTED_SCIENT_NAME <- "Doryteuthis pealeii"
spec.xtable.df[spec.xtable.df$spec==704,]$ACCEPTED_SCIENT_NAME <- "Zenopsis conchifer"

## fix names that are long and cause pagination problems
spec.xtable.df[spec.xtable.df$spec==4511,]$FAO_E_COMMON_NAME <- "North. shortfin squid"
spec.xtable.df[spec.xtable.df$spec==4511,]$FAO_F_COMMON_NAME <- "Encornet rouge nord."
spec.xtable.df[spec.xtable.df$spec==300,]$FAO_F_COMMON_NAME <- "Chaboisseau à 18 épines"
spec.xtable.df[spec.xtable.df$spec==410,]$FAO_F_COMMON_NAME <- "Grenadier Grand Banc"
spec.xtable.df[spec.xtable.df$spec==114,]$FAO_F_COMMON_NAME <- "Motelle à 4 barbillons"
spec.xtable.df[spec.xtable.df$spec==502,]$FAO_E_COMMON_NAME <- "Atl. spiny lumpsucker"
spec.xtable.df[spec.xtable.df$spec==502,]$FAO_F_COMMON_NAME <- "Petite poule de mer atl."
                           
# spec.xtable.df[spec.xtable.df$spec==200,]$type <- "LR"
# spec.xtable.df[spec.xtable.df$spec==340,]$type <- "I"
# spec.xtable.df[spec.xtable.df$spec==501,]$type <- "LR"
# spec.xtable.df[spec.xtable.df$spec==502,]$type <- "LR"
# spec.xtable.df[spec.xtable.df$spec==610,]$type <- "LI"

 ## species that were not weighted in early years 
 ## should be classified differently
spec.xtable.df[spec.xtable.df$spec==340,]$type <- "LIn"
spec.xtable.df[spec.xtable.df$spec==114,]$type <- "LIn"
spec.xtable.df[spec.xtable.df$spec==123,]$type <- "LIn"
spec.xtable.df[spec.xtable.df$spec==306,]$type <- "LIn"
spec.xtable.df[spec.xtable.df$spec==350,]$type <- "LIn"
spec.xtable.df[spec.xtable.df$spec==502,]$type <- "LIn"
spec.xtable.df[spec.xtable.df$spec==31,]$type <- "LIn"
spec.xtable.df[spec.xtable.df$spec==44,]$type <- "LIn"
spec.xtable.df[spec.xtable.df$spec==610,]$type <- "LIn"
spec.xtable.df[spec.xtable.df$spec==622,]$type <- "LIn"
spec.xtable.df[spec.xtable.df$spec==623,]$type <- "LIn"
spec.xtable.df[spec.xtable.df$spec==701,]$type <- "LIn"
spec.xtable.df[spec.xtable.df$spec==64,]$type <- "LIn"
spec.xtable.df[spec.xtable.df$spec==880,]$type <- "LIn"

 ## at this point, the data frame spec.xtable.df has everything we need for the report except an APHIA ID, so call worms to get those based on the scientific name that we have
 
 spec.xtable.df.final <- spec.xtable.df
 
 #spec.xtable.df.final <- na.omit(spec.xtable.df.final)

 ## keep our 104 species 
 spec.xtable.df.final <- spec.xtable.df.final[spec.xtable.df.final$spec 
                                              %in% c(241,240,604,156,149,712,720,60,62,61,10,11,12,16,13,112,15,114,17,410,412,414,14,19,400,742,150,160,64,63,610,50,51,52,637,630,122,621,70,623,622,625,626,701,640,647,641,619,620,646,603,816,44,142,40,41,42,43,30,31,143,340,350,341,300,304,306,880,301,314,303,501,502,320,503,512,505,520,307,23,123,158,741,159,704,201,202,204,203,200,221,220,4512,4511,2511,2513,2532,2523,2550,2526,2527,2521,2211), ] #,351

 # to allow french names to remain in the csv file i had to add the following lines of code as encoding="UTF-8" did not work
 csv.fn <- file.path(main.path, "species-list-for-report.csv")
 readr::write_csv(spec.xtable.df.final,
                   file=csv.fn,
                  col_names=T
           )
 
 file.copy(csv.fn, actualreport.path, overwrite=TRUE)
 
 
 rm(list= ls()[!(ls() %in% before)]) ## clean up after ourselves
 
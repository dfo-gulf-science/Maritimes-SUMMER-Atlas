## script to generate the summary table of number of tows per stratum-year for the tech report
## 
before <- ls()

qu <- paste("
select 
*
from
groundfish.gsstratum
", sep="")

strata.stats.df <- sqlQuery(chan, qu, stringsAsFactors=FALSE)

summer.strata.stats.df <- subset(
strata.stats.df, 
(STRAT == '440' | STRAT == '441' | STRAT == '442' | STRAT == '443' | STRAT == '444' | STRAT == '445' | STRAT == '446' | STRAT == '447' | STRAT == '448' | STRAT == '449' | 
STRAT == '450' | STRAT == '451' | STRAT == '452' | STRAT == '453' | STRAT == '454' | STRAT == '455' | STRAT == '456' | STRAT == '457' | STRAT == '458' | STRAT == '459' | 
STRAT == '460' | STRAT == '461' | STRAT == '462' | STRAT == '463' | STRAT == '464' | STRAT == '465' | STRAT == '466' | 
STRAT == '470' | STRAT == '471' | STRAT == '472' | STRAT == '473' | STRAT == '474' | STRAT == '475' | STRAT == '476' | STRAT == '477' | STRAT == '478' | 
STRAT == '480' | STRAT == '481' | STRAT == '482' | STRAT == '483' | STRAT == '484' | STRAT == '485' | 
STRAT == '490' | STRAT == '491' | STRAT == '492' | STRAT == '493' | STRAT == '494' | STRAT == '495' ) 
)

	
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
STRAT == '490' | STRAT == '491' | STRAT == '492' | STRAT == '493' | STRAT == '494' | STRAT == '495' ) & (MONTH == 6 | MONTH == 7 | MONTH == 8)
)
tt <- droplevels(tows.summer.df)
summary.table <- table(tt$STRAT, tt$YEAR)
summary.table.matrix <- as.matrix(summary.table)

oo <- order(summer.strata.stats.df$STRAT)
ss <- summer.strata.stats.df[oo,]
## turn area into square kilometers
summary.table.df <- data.frame(Stratum=as.character(ss$STRAT), NAFO=as.character(ss$NAME), Area=ss$AREA*3.434, 
y.1970=summary.table.matrix[,1],
y.1971=summary.table.matrix[,2],
y.1972=summary.table.matrix[,3],
y.1973=summary.table.matrix[,4],
y.1974=summary.table.matrix[,5],
y.1975=summary.table.matrix[,6],
y.1976=summary.table.matrix[,7],
y.1977=summary.table.matrix[,8],
y.1978=summary.table.matrix[,9],
y.1979=summary.table.matrix[,10],
y.1980=summary.table.matrix[,11],
y.1981=summary.table.matrix[,12],
y.1982=summary.table.matrix[,13],
y.1983=summary.table.matrix[,14],
y.1984=summary.table.matrix[,15],
y.1985=summary.table.matrix[,16],
y.1986=summary.table.matrix[,17],
y.1987=summary.table.matrix[,18],
y.1988=summary.table.matrix[,19],
y.1989=summary.table.matrix[,20],
y.1990=summary.table.matrix[,21],
y.1991=summary.table.matrix[,22],
y.1992=summary.table.matrix[,23],
y.1993=summary.table.matrix[,24],
y.1994=summary.table.matrix[,25],
y.1995=summary.table.matrix[,26],
y.1996=summary.table.matrix[,27],
y.1997=summary.table.matrix[,28],
y.1998=summary.table.matrix[,29],
y.1999=summary.table.matrix[,30],
y.2000=summary.table.matrix[,31],
y.2001=summary.table.matrix[,32],
y.2002=summary.table.matrix[,33],
y.2003=summary.table.matrix[,34],
y.2004=summary.table.matrix[,35],
y.2005=summary.table.matrix[,36],
y.2006=summary.table.matrix[,37],
y.2007=summary.table.matrix[,38],
y.2008=summary.table.matrix[,39],
y.2009=summary.table.matrix[,40],
y.2010=summary.table.matrix[,41],
y.2011=summary.table.matrix[,42],
y.2012=summary.table.matrix[,43],
y.2013=summary.table.matrix[,44],
y.2014=summary.table.matrix[,45],
y.2015=summary.table.matrix[,46],
y.2016=summary.table.matrix[,47],
y.2017=summary.table.matrix[,48],
y.2018=summary.table.matrix[,49],
y.2019=summary.table.matrix[,50],
y.2020=summary.table.matrix[,51],
stringsAsFactors=FALSE
)


ii <- dim(summary.table.df)[1]+1
summary.table.df[ii,] <- as.numeric(c("Total", "", sum(summary.table.df$Area), 
sum(summary.table.df$y.1970),
sum(summary.table.df$y.1971),
sum(summary.table.df$y.1972),
sum(summary.table.df$y.1973),
sum(summary.table.df$y.1974),
sum(summary.table.df$y.1975),
sum(summary.table.df$y.1976),
sum(summary.table.df$y.1977),
sum(summary.table.df$y.1978),
sum(summary.table.df$y.1979),
sum(summary.table.df$y.1980),
sum(summary.table.df$y.1981),
sum(summary.table.df$y.1982),
sum(summary.table.df$y.1983),
sum(summary.table.df$y.1984),
sum(summary.table.df$y.1985),
sum(summary.table.df$y.1986),
sum(summary.table.df$y.1987),
sum(summary.table.df$y.1988),
sum(summary.table.df$y.1989),
sum(summary.table.df$y.1990),
sum(summary.table.df$y.1991),
sum(summary.table.df$y.1992),
sum(summary.table.df$y.1993),
sum(summary.table.df$y.1994),
sum(summary.table.df$y.1995),
sum(summary.table.df$y.1996),
sum(summary.table.df$y.1997),
sum(summary.table.df$y.1998),
sum(summary.table.df$y.1999),
sum(summary.table.df$y.2000),
sum(summary.table.df$y.2001),
sum(summary.table.df$y.2002),
sum(summary.table.df$y.2003),
sum(summary.table.df$y.2004),
sum(summary.table.df$y.2005),
sum(summary.table.df$y.2006),
sum(summary.table.df$y.2007),
sum(summary.table.df$y.2008),
sum(summary.table.df$y.2009),
sum(summary.table.df$y.2010),
sum(summary.table.df$y.2011),
sum(summary.table.df$y.2012),
sum(summary.table.df$y.2013),
sum(summary.table.df$y.2014),
sum(summary.table.df$y.2015),
sum(summary.table.df$y.2016),
sum(summary.table.df$y.2017),
sum(summary.table.df$y.2018),
sum(summary.table.df$y.2019),
sum(summary.table.df$y.2020)
))

summary.table.df$totals <- rowSums(summary.table.df[c(4:54)])

##########################################################################################################
## all-important file, will be the table showing the number of tows for each year-stratum 
csv.fn <- file.path(report.path, "Atlas-summary-table-tows-by-year-stratum.csv") ## to use in csasdown
write.csv(summary.table.df, file=csv.fn, row.names = F)

file.copy(csv.fn, actualreport.path, overwrite=TRUE)

rm(list= ls()[!(ls() %in% before)]) ## clean up after ourselves

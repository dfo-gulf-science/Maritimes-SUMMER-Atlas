##
##
lf.fct <- function(spec.num) {

qu <- paste("
SELECT
d.mission,
d.setno,
i.strat,
TO_CHAR(i.sdate,'yyyy') YEAR,
TO_CHAR(i.sdate,'mm') MONTH,
TO_CHAR(i.sdate,'dd') DAY,
s.SPEC SCIEN,
d.fshno,
d.flen,
d.clen
FROM
groundfish.gsinf i,
groundfish.gsdet d,
groundfish.GSSPECIES s
WHERE
i.mission = d.mission AND
i.setno = d.setno AND
d.spec=s.CODE AND
s.CODE = '", spec.num, "'
order by year, month, day, setno,fshno
", sep="")

det.df <- sqlQuery(chan, qu)

det.summer.df <- 
subset(
det.df, 
(STRAT == '440' | STRAT == '441' | STRAT == '442' | STRAT == '443' | STRAT == '444' | STRAT == '445' | STRAT == '446' | STRAT == '447' | STRAT == '448' | STRAT == '449' | 
STRAT == '450' | STRAT == '451' | STRAT == '452' | STRAT == '453' | STRAT == '454' | STRAT == '455' | STRAT == '456' | STRAT == '457' | STRAT == '458' | STRAT == '459' | 
STRAT == '460' | STRAT == '461' | STRAT == '462' | STRAT == '463' | STRAT == '464' | STRAT == '465' | STRAT == '466' | 
STRAT == '470' | STRAT == '471' | STRAT == '472' | STRAT == '473' | STRAT == '474' | STRAT == '475' | STRAT == '476' | STRAT == '477' | STRAT == '478' | 
STRAT == '480' | STRAT == '481' | STRAT == '482' | STRAT == '483' | STRAT == '484' | STRAT == '485' | 
STRAT == '490' | STRAT == '491' | STRAT == '492' | STRAT == '493' | STRAT == '494' | STRAT == '495' )  & (MONTH == 6 | MONTH == 7 | MONTH == 8)
)

#length.breaks <- seq(0,max(det.summer.df$FLEN))
length.breaks <- seq(1,max(det.summer.df$FLEN),by=3) # the length bins used are 3cm bins

#tt <- subset(det.summer.df, MISSION =='ATC1971188' & SETNO==33)
#tt.fsh <- subset(tt, !is.na(FSHNO))
#tt.clen <- subset(tt, is.na(FSHNO))

#cuts.fsh <- cut(tt.fsh$FLEN, length.breaks, right=TRUE)
#fsh.df <- data.frame(table(cuts.fsh))

#cuts.clen <- cut(tt.clen$FLEN, length.breaks, right=TRUE)
#clen.df <- data.frame(cuts=cuts.clen, freq=tt.clen$CLEN)

#all.df <- merge(fsh.df, clen.df, by.x='cuts.fsh',by.y='cuts', all.x=TRUE)
#all.df[is.na(all.df$freq),]$freq <- 0
#all.df$realfreq <- all.df$Freq+all.df$freq

## be careful here, the data contains both individual fish measurements and fish counts per length classes
## 1970 to 1977
time1.fsh.df <- subset(det.summer.df, YEAR >= 1970 & YEAR < 1978 & !is.na(FSHNO)) # individual fish measurements
time1.clen.df <- subset(det.summer.df, YEAR >= 1970 & YEAR < 1978 & is.na(FSHNO)) # counts per length classes

fsh.df <- data.frame(table(cut(time1.fsh.df$FLEN, length.breaks, right=TRUE)))
clen.df <- data.frame(cuts=cut(time1.clen.df$FLEN, length.breaks, right=TRUE), freq=time1.clen.df$CLEN)
clen.df <- data.frame(freq=tapply(clen.df$freq, clen.df$cuts, sum))
clen.df$cuts <- row.names(clen.df)
names(clen.df) <- c("freq", "cuts")

all.df <- merge(fsh.df, clen.df, by.x='Var1',by.y='cuts', all.x=TRUE)
if(length(all.df[is.na(all.df$freq),]$freq)>0) {all.df[is.na(all.df$freq),]$freq <- 0}
all.df <- all.df[order(all.df$Var1),]
all.df$realfreq <- all.df$Freq+all.df$freq
rel.freq1 <- all.df$realfreq /sum(all.df$realfreq)


## 1978 to 1985
time2.fsh.df <- subset(det.summer.df, YEAR >= 1978 & YEAR < 1986 & !is.na(FSHNO)) # individual fish measurements
time2.clen.df <- subset(det.summer.df, YEAR >= 1978 & YEAR < 1986 & is.na(FSHNO)) # counts per length classes
fsh.df <- data.frame(table(cut(time2.fsh.df$FLEN, length.breaks, right=TRUE)))
clen.df <- data.frame(cuts=cut(time2.clen.df$FLEN, length.breaks, right=TRUE), freq=time2.clen.df$CLEN)
clen.df <- data.frame(freq=tapply(clen.df$freq, clen.df$cuts, sum))
clen.df$cuts <- row.names(clen.df)
names(clen.df) <- c("freq", "cuts")
all.df <- merge(fsh.df, clen.df, by.x='Var1',by.y='cuts', all.x=TRUE)
if(length(all.df[is.na(all.df$freq),]$freq)>0) {all.df[is.na(all.df$freq),]$freq <- 0}
all.df <- all.df[order(all.df$Var1),]
all.df$realfreq <- all.df$Freq+all.df$freq
rel.freq2 <- all.df$realfreq /sum(all.df$realfreq)

## 1986 to 1993
time3.fsh.df <- subset(det.summer.df, YEAR >= 1986 & YEAR < 1994 & !is.na(FSHNO)) # individual fish measurements
time3.clen.df <- subset(det.summer.df, YEAR >= 1986 & YEAR < 1994 & is.na(FSHNO)) # counts per length classes
fsh.df <- data.frame(table(cut(time3.fsh.df$FLEN, length.breaks, right=TRUE)))
clen.df <- data.frame(cuts=cut(time3.clen.df$FLEN, length.breaks, right=TRUE), freq=time3.clen.df$CLEN)
clen.df <- data.frame(freq=tapply(clen.df$freq, clen.df$cuts, sum))
clen.df$cuts <- row.names(clen.df)
names(clen.df) <- c("freq", "cuts")
all.df <- merge(fsh.df, clen.df, by.x='Var1',by.y='cuts', all.x=TRUE)
if(length(all.df[is.na(all.df$freq),]$freq)>0) {all.df[is.na(all.df$freq),]$freq <- 0}
all.df <- all.df[order(all.df$Var1),]
all.df$realfreq <- all.df$Freq+all.df$freq
rel.freq3 <- all.df$realfreq /sum(all.df$realfreq)

## 1994 to 2001
time4.fsh.df <- subset(det.summer.df, YEAR >= 1994 & YEAR < 2002 & !is.na(FSHNO)) # individual fish measurements
time4.clen.df <- subset(det.summer.df, YEAR >= 1994 & YEAR < 2002 & is.na(FSHNO)) # counts per length classes
fsh.df <- data.frame(table(cut(time4.fsh.df$FLEN, length.breaks, right=TRUE)))
clen.df <- data.frame(cuts=cut(time4.clen.df$FLEN, length.breaks, right=TRUE), freq=time4.clen.df$CLEN)
clen.df <- data.frame(freq=tapply(clen.df$freq, clen.df$cuts, sum))
clen.df$cuts <- row.names(clen.df)
names(clen.df) <- c("freq", "cuts")
all.df <- merge(fsh.df, clen.df, by.x='Var1',by.y='cuts', all.x=TRUE)
if(length(all.df[is.na(all.df$freq),]$freq)>0) {all.df[is.na(all.df$freq),]$freq <- 0}
all.df <- all.df[order(all.df$Var1),]
all.df$realfreq <- all.df$Freq+all.df$freq
rel.freq4 <- all.df$realfreq /sum(all.df$realfreq)

## 2002 to 2009
time5.fsh.df <- subset(det.summer.df, YEAR >= 2002 & YEAR < 2010 & !is.na(FSHNO)) # individual fish measurements
time5.clen.df <- subset(det.summer.df, YEAR >= 2002 & YEAR < 2010 & is.na(FSHNO)) # counts per length classes
fsh.df <- data.frame(table(cut(time5.fsh.df$FLEN, length.breaks, right=TRUE)))
clen.df <- data.frame(cuts=cut(time5.clen.df$FLEN, length.breaks, right=TRUE), freq=time5.clen.df$CLEN)
clen.df <- data.frame(freq=tapply(clen.df$freq, clen.df$cuts, sum))
clen.df$cuts <- row.names(clen.df)
names(clen.df) <- c("freq", "cuts")
all.df <- merge(fsh.df, clen.df, by.x='Var1',by.y='cuts', all.x=TRUE)
if(length(all.df[is.na(all.df$freq),]$freq)>0) {all.df[is.na(all.df$freq),]$freq <- 0}
all.df <- all.df[order(all.df$Var1),]
all.df$realfreq <- all.df$Freq+all.df$freq
rel.freq5 <- all.df$realfreq /sum(all.df$realfreq)

## 2010 to 2020
time6.fsh.df <- subset(det.summer.df, YEAR >= 2010 & YEAR < 2021 & !is.na(FSHNO)) # individual fish measurements
time6.clen.df <- subset(det.summer.df, YEAR >= 2010 & YEAR < 2021 & is.na(FSHNO)) # counts per length classes
fsh.df <- data.frame(table(cut(time6.fsh.df$FLEN, length.breaks, right=TRUE)))
clen.df <- data.frame(cuts=cut(time6.clen.df$FLEN, length.breaks, right=TRUE), freq=time6.clen.df$CLEN)
clen.df <- data.frame(freq=tapply(clen.df$freq, clen.df$cuts, sum))
clen.df$cuts <- row.names(clen.df)
names(clen.df) <- c("freq", "cuts")
all.df <- merge(fsh.df, clen.df, by.x='Var1',by.y='cuts', all.x=TRUE)
if(length(all.df[is.na(all.df$freq),]$freq)>0) {all.df[is.na(all.df$freq),]$freq <- 0}
all.df <- all.df[order(all.df$Var1),]
all.df$realfreq <- all.df$Freq+all.df$freq
rel.freq6 <- all.df$realfreq /sum(all.df$realfreq)


final.df.all <- data.frame(lengths=length.breaks[2:length(length.breaks)], lf.1970=as.vector(rel.freq1), lf.1978=as.vector(rel.freq2), lf.1986=as.vector(rel.freq3), lf.1994=as.vector(rel.freq4), lf.2002=as.vector(rel.freq5), lf.2010=as.vector(rel.freq6))

## now do the same but separately for NAFO 4X and for NAFO 4VW
nafo.4vw.strat <- c('440','441','442','443','444','445','446','447','448','449','450','451','452','453','454','455','456','457','458','459','460','461','462','463','464','465','466','467','468','469')
nafo.4x.strat <- c('470','471','472','473','474','475','476','477','478','480','481','482','483','484','485','490','491','492','493','494','495')


## NAFO 4VW
## 1970 to 1977
time1.fsh.df <- subset(det.summer.df, STRAT %in% nafo.4vw.strat & YEAR >= 1970 & YEAR < 1978 & !is.na(FSHNO)) # individual fish measurements
time1.clen.df <- subset(det.summer.df, STRAT %in% nafo.4vw.strat & YEAR >= 1970 & YEAR < 1978 & is.na(FSHNO)) # counts per length classes
fsh.df <- data.frame(table(cut(time1.fsh.df$FLEN, length.breaks, right=TRUE)))
clen.df <- data.frame(cuts=cut(time1.clen.df$FLEN, length.breaks, right=TRUE), freq=time1.clen.df$CLEN)
clen.df <- data.frame(freq=tapply(clen.df$freq, clen.df$cuts, sum))
clen.df$cuts <- row.names(clen.df)
names(clen.df) <- c("freq", "cuts")
all.df <- merge(fsh.df, clen.df, by.x='Var1',by.y='cuts', all.x=TRUE)
if(length(all.df[is.na(all.df$freq),]$freq)>0) {all.df[is.na(all.df$freq),]$freq <- 0}
all.df <- all.df[order(all.df$Var1),]
all.df$realfreq <- all.df$Freq+all.df$freq
rel.freq1 <- all.df$realfreq /sum(all.df$realfreq)


## 1978 to 1985
time2.fsh.df <- subset(det.summer.df, STRAT %in% nafo.4vw.strat & YEAR >= 1978 & YEAR < 1986 & !is.na(FSHNO)) # individual fish measurements
time2.clen.df <- subset(det.summer.df, STRAT %in% nafo.4vw.strat & YEAR >= 1978 & YEAR < 1986 & is.na(FSHNO)) # counts per length classes
fsh.df <- data.frame(table(cut(time2.fsh.df$FLEN, length.breaks, right=TRUE)))
clen.df <- data.frame(cuts=cut(time2.clen.df$FLEN, length.breaks, right=TRUE), freq=time2.clen.df$CLEN)
clen.df <- data.frame(freq=tapply(clen.df$freq, clen.df$cuts, sum))
clen.df$cuts <- row.names(clen.df)
names(clen.df) <- c("freq", "cuts")
all.df <- merge(fsh.df, clen.df, by.x='Var1',by.y='cuts', all.x=TRUE)
if(length(all.df[is.na(all.df$freq),]$freq)>0) {all.df[is.na(all.df$freq),]$freq <- 0}
all.df <- all.df[order(all.df$Var1),]
all.df$realfreq <- all.df$Freq+all.df$freq
rel.freq2 <- all.df$realfreq /sum(all.df$realfreq)

## 1986 to 1993
time3.fsh.df <- subset(det.summer.df, STRAT %in% nafo.4vw.strat & YEAR >= 1986 & YEAR < 1994 & !is.na(FSHNO)) # individual fish measurements
time3.clen.df <- subset(det.summer.df, STRAT %in% nafo.4vw.strat & YEAR >= 1986 & YEAR < 1994 & is.na(FSHNO)) # counts per length classes
fsh.df <- data.frame(table(cut(time3.fsh.df$FLEN, length.breaks, right=TRUE)))
clen.df <- data.frame(cuts=cut(time3.clen.df$FLEN, length.breaks, right=TRUE), freq=time3.clen.df$CLEN)
clen.df <- data.frame(freq=tapply(clen.df$freq, clen.df$cuts, sum))
clen.df$cuts <- row.names(clen.df)
names(clen.df) <- c("freq", "cuts")
all.df <- merge(fsh.df, clen.df, by.x='Var1',by.y='cuts', all.x=TRUE)
if(length(all.df[is.na(all.df$freq),]$freq)>0) {all.df[is.na(all.df$freq),]$freq <- 0}
all.df <- all.df[order(all.df$Var1),]
all.df$realfreq <- all.df$Freq+all.df$freq
rel.freq3 <- all.df$realfreq /sum(all.df$realfreq)

## 1994 to 2001
time4.fsh.df <- subset(det.summer.df, STRAT %in% nafo.4vw.strat & YEAR >= 1994 & YEAR < 2002 & !is.na(FSHNO)) # individual fish measurements
time4.clen.df <- subset(det.summer.df, STRAT %in% nafo.4vw.strat & YEAR >= 1994 & YEAR < 2002 & is.na(FSHNO)) # counts per length classes
fsh.df <- data.frame(table(cut(time4.fsh.df$FLEN, length.breaks, right=TRUE)))
clen.df <- data.frame(cuts=cut(time4.clen.df$FLEN, length.breaks, right=TRUE), freq=time4.clen.df$CLEN)
clen.df <- data.frame(freq=tapply(clen.df$freq, clen.df$cuts, sum))
clen.df$cuts <- row.names(clen.df)
names(clen.df) <- c("freq", "cuts")
all.df <- merge(fsh.df, clen.df, by.x='Var1',by.y='cuts', all.x=TRUE)
if(length(all.df[is.na(all.df$freq),]$freq)>0) {all.df[is.na(all.df$freq),]$freq <- 0}
all.df <- all.df[order(all.df$Var1),]
all.df$realfreq <- all.df$Freq+all.df$freq
rel.freq4 <- all.df$realfreq /sum(all.df$realfreq)

## 2002 to 2009
time5.fsh.df <- subset(det.summer.df, STRAT %in% nafo.4vw.strat & YEAR >= 2002 & YEAR < 2010 & !is.na(FSHNO)) # individual fish measurements
time5.clen.df <- subset(det.summer.df, STRAT %in% nafo.4vw.strat & YEAR >= 2002 & YEAR < 2010 & is.na(FSHNO)) # counts per length classes
fsh.df <- data.frame(table(cut(time5.fsh.df$FLEN, length.breaks, right=TRUE)))
clen.df <- data.frame(cuts=cut(time5.clen.df$FLEN, length.breaks, right=TRUE), freq=time5.clen.df$CLEN)
clen.df <- data.frame(freq=tapply(clen.df$freq, clen.df$cuts, sum))
clen.df$cuts <- row.names(clen.df)
names(clen.df) <- c("freq", "cuts")
all.df <- merge(fsh.df, clen.df, by.x='Var1',by.y='cuts', all.x=TRUE)
if(length(all.df[is.na(all.df$freq),]$freq)>0) {all.df[is.na(all.df$freq),]$freq <- 0}
all.df <- all.df[order(all.df$Var1),]
all.df$realfreq <- all.df$Freq+all.df$freq
rel.freq5 <- all.df$realfreq /sum(all.df$realfreq)

## 2010 to 2013
time6.fsh.df <- subset(det.summer.df, STRAT %in% nafo.4vw.strat & YEAR >= 2010 & YEAR < 2021 & !is.na(FSHNO)) # individual fish measurements
time6.clen.df <- subset(det.summer.df, STRAT %in% nafo.4vw.strat & YEAR >= 2010 & YEAR < 2021 & is.na(FSHNO)) # counts per length classes
fsh.df <- data.frame(table(cut(time6.fsh.df$FLEN, length.breaks, right=TRUE)))
clen.df <- data.frame(cuts=cut(time6.clen.df$FLEN, length.breaks, right=TRUE), freq=time6.clen.df$CLEN)
clen.df <- data.frame(freq=tapply(clen.df$freq, clen.df$cuts, sum))
clen.df$cuts <- row.names(clen.df)
names(clen.df) <- c("freq", "cuts")
all.df <- merge(fsh.df, clen.df, by.x='Var1',by.y='cuts', all.x=TRUE)
if(length(all.df[is.na(all.df$freq),]$freq)>0) {all.df[is.na(all.df$freq),]$freq <- 0}
all.df <- all.df[order(all.df$Var1),]
all.df$realfreq <- all.df$Freq+all.df$freq
rel.freq6 <- all.df$realfreq /sum(all.df$realfreq)

final.df.4vw <- data.frame(lengths=length.breaks[2:length(length.breaks)], lf.1970=as.vector(rel.freq1), lf.1978=as.vector(rel.freq2), lf.1986=as.vector(rel.freq3), lf.1994=as.vector(rel.freq4), lf.2002=as.vector(rel.freq5), lf.2010=as.vector(rel.freq6))

## replace NAs with zeroes
final.df.4vw <- replace(final.df.4vw, is.na(final.df.4vw), 0)


## NAFO 4x
## 1970 to 1977
time1.fsh.df <- subset(det.summer.df, STRAT %in% nafo.4x.strat & YEAR >= 1970 & YEAR < 1978 & !is.na(FSHNO)) # individual fish measurements
time1.clen.df <- subset(det.summer.df, STRAT %in% nafo.4x.strat & YEAR >= 1970 & YEAR < 1978 & is.na(FSHNO)) # counts per length classes
fsh.df <- data.frame(table(cut(time1.fsh.df$FLEN, length.breaks, right=TRUE)))
clen.df <- data.frame(cuts=cut(time1.clen.df$FLEN, length.breaks, right=TRUE), freq=time1.clen.df$CLEN)
clen.df <- data.frame(freq=tapply(clen.df$freq, clen.df$cuts, sum))
clen.df$cuts <- row.names(clen.df)
names(clen.df) <- c("freq", "cuts")
all.df <- merge(fsh.df, clen.df, by.x='Var1',by.y='cuts', all.x=TRUE)
if(length(all.df[is.na(all.df$freq),]$freq)>0) {all.df[is.na(all.df$freq),]$freq <- 0}
all.df <- all.df[order(all.df$Var1),]
all.df$realfreq <- all.df$Freq+all.df$freq
rel.freq1 <- all.df$realfreq /sum(all.df$realfreq)


## 1978 to 1985
time2.fsh.df <- subset(det.summer.df, STRAT %in% nafo.4x.strat & YEAR >= 1978 & YEAR < 1986 & !is.na(FSHNO)) # individual fish measurements
time2.clen.df <- subset(det.summer.df, STRAT %in% nafo.4x.strat & YEAR >= 1978 & YEAR < 1986 & is.na(FSHNO)) # counts per length classes
fsh.df <- data.frame(table(cut(time2.fsh.df$FLEN, length.breaks, right=TRUE)))
clen.df <- data.frame(cuts=cut(time2.clen.df$FLEN, length.breaks, right=TRUE), freq=time2.clen.df$CLEN)
clen.df <- data.frame(freq=tapply(clen.df$freq, clen.df$cuts, sum))
clen.df$cuts <- row.names(clen.df)
names(clen.df) <- c("freq", "cuts")
all.df <- merge(fsh.df, clen.df, by.x='Var1',by.y='cuts', all.x=TRUE)
if(length(all.df[is.na(all.df$freq),]$freq)>0) {all.df[is.na(all.df$freq),]$freq <- 0}
all.df <- all.df[order(all.df$Var1),]
all.df$realfreq <- all.df$Freq+all.df$freq
rel.freq2 <- all.df$realfreq /sum(all.df$realfreq)

## 1986 to 1993
time3.fsh.df <- subset(det.summer.df, STRAT %in% nafo.4x.strat & YEAR >= 1986 & YEAR < 1994 & !is.na(FSHNO)) # individual fish measurements
time3.clen.df <- subset(det.summer.df, STRAT %in% nafo.4x.strat & YEAR >= 1986 & YEAR < 1994 & is.na(FSHNO)) # counts per length classes
fsh.df <- data.frame(table(cut(time3.fsh.df$FLEN, length.breaks, right=TRUE)))
clen.df <- data.frame(cuts=cut(time3.clen.df$FLEN, length.breaks, right=TRUE), freq=time3.clen.df$CLEN)
clen.df <- data.frame(freq=tapply(clen.df$freq, clen.df$cuts, sum))
clen.df$cuts <- row.names(clen.df)
names(clen.df) <- c("freq", "cuts")
all.df <- merge(fsh.df, clen.df, by.x='Var1',by.y='cuts', all.x=TRUE)
if(length(all.df[is.na(all.df$freq),]$freq)>0) {all.df[is.na(all.df$freq),]$freq <- 0}
all.df <- all.df[order(all.df$Var1),]
all.df$realfreq <- all.df$Freq+all.df$freq
rel.freq3 <- all.df$realfreq /sum(all.df$realfreq)

## 1994 to 2001
time4.fsh.df <- subset(det.summer.df, STRAT %in% nafo.4x.strat & YEAR >= 1994 & YEAR < 2002 & !is.na(FSHNO)) # individual fish measurements
time4.clen.df <- subset(det.summer.df, STRAT %in% nafo.4x.strat & YEAR >= 1992 & YEAR < 2002 & is.na(FSHNO)) # counts per length classes
fsh.df <- data.frame(table(cut(time4.fsh.df$FLEN, length.breaks, right=TRUE)))
clen.df <- data.frame(cuts=cut(time4.clen.df$FLEN, length.breaks, right=TRUE), freq=time4.clen.df$CLEN)
clen.df <- data.frame(freq=tapply(clen.df$freq, clen.df$cuts, sum))
clen.df$cuts <- row.names(clen.df)
names(clen.df) <- c("freq", "cuts")
all.df <- merge(fsh.df, clen.df, by.x='Var1',by.y='cuts', all.x=TRUE)
if(length(all.df[is.na(all.df$freq),]$freq)>0) {all.df[is.na(all.df$freq),]$freq <- 0}
all.df <- all.df[order(all.df$Var1),]
all.df$realfreq <- all.df$Freq+all.df$freq
rel.freq4 <- all.df$realfreq /sum(all.df$realfreq)

## 2002 to 2009
time5.fsh.df <- subset(det.summer.df, STRAT %in% nafo.4x.strat & YEAR >= 2002 & YEAR < 2010 & !is.na(FSHNO)) # individual fish measurements
time5.clen.df <- subset(det.summer.df, STRAT %in% nafo.4x.strat & YEAR >= 2002 & YEAR < 2010 & is.na(FSHNO)) # counts per length classes
fsh.df <- data.frame(table(cut(time5.fsh.df$FLEN, length.breaks, right=TRUE)))
clen.df <- data.frame(cuts=cut(time5.clen.df$FLEN, length.breaks, right=TRUE), freq=time5.clen.df$CLEN)
clen.df <- data.frame(freq=tapply(clen.df$freq, clen.df$cuts, sum))
clen.df$cuts <- row.names(clen.df)
names(clen.df) <- c("freq", "cuts")
all.df <- merge(fsh.df, clen.df, by.x='Var1',by.y='cuts', all.x=TRUE)
if(length(all.df[is.na(all.df$freq),]$freq)>0) {all.df[is.na(all.df$freq),]$freq <- 0}
all.df <- all.df[order(all.df$Var1),]
all.df$realfreq <- all.df$Freq+all.df$freq
rel.freq5 <- all.df$realfreq /sum(all.df$realfreq)

## 2010 to 2020
time6.fsh.df <- subset(det.summer.df, STRAT %in% nafo.4x.strat & YEAR >= 2010 & YEAR < 2021 & !is.na(FSHNO)) # individual fish measurements
time6.clen.df <- subset(det.summer.df, STRAT %in% nafo.4x.strat & YEAR >= 2010 & YEAR < 2021 & is.na(FSHNO)) # counts per length classes
fsh.df <- data.frame(table(cut(time6.fsh.df$FLEN, length.breaks, right=TRUE)))
clen.df <- data.frame(cuts=cut(time6.clen.df$FLEN, length.breaks, right=TRUE), freq=time6.clen.df$CLEN)
clen.df <- data.frame(freq=tapply(clen.df$freq, clen.df$cuts, sum))
clen.df$cuts <- row.names(clen.df)
names(clen.df) <- c("freq", "cuts")
all.df <- merge(fsh.df, clen.df, by.x='Var1',by.y='cuts', all.x=TRUE)
if(length(all.df[is.na(all.df$freq),]$freq)>0) {all.df[is.na(all.df$freq),]$freq <- 0}
all.df <- all.df[order(all.df$Var1),]
all.df$realfreq <- all.df$Freq+all.df$freq
rel.freq6 <- all.df$realfreq /sum(all.df$realfreq)

final.df.4x <- data.frame(lengths=length.breaks[2:length(length.breaks)], lf.1970=as.vector(rel.freq1), lf.1978=as.vector(rel.freq2), lf.1986=as.vector(rel.freq3), lf.1994=as.vector(rel.freq4), lf.2002=as.vector(rel.freq5), lf.2010=as.vector(rel.freq6))
final.df.4x <- replace(final.df.4x, is.na(final.df.4x), 0)

final.list <- list(all=final.df.all, nafo4vw=final.df.4vw, nafo4x=final.df.4x)

return(final.list)
} # end function


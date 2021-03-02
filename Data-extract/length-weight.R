##
##
lw.fct <- function(spec.num) {

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
d.clen,
d.fwt
FROM
groundfish.gsinf i,
groundfish.gsdet d,
groundfish.GSSPECIES s
WHERE
i.type=1 and
d.flen is not null and
i.mission = d.mission AND
i.setno = d.setno AND
d.spec=s.CODE AND
s.CODE = '", spec.num, "'
order by year, month, day, setno,fshno

", sep="")

# d.fwt is not null and

det.df <- sqlQuery(chan, qu)

## keep only summer strata, collected in June-July-August and keep only individual fish measurements, not binned counts
det.summer.df <- 
subset(
det.df, 
(STRAT == '440' | STRAT == '441' | STRAT == '442' | STRAT == '443' | STRAT == '444' | STRAT == '445' | STRAT == '446' | STRAT == '447' | STRAT == '448' | STRAT == '449' | 
STRAT == '450' | STRAT == '451' | STRAT == '452' | STRAT == '453' | STRAT == '454' | STRAT == '455' | STRAT == '456' | STRAT == '457' | STRAT == '458' | STRAT == '459' | 
STRAT == '460' | STRAT == '461' | STRAT == '462' | STRAT == '463' | STRAT == '464' | STRAT == '465' | STRAT == '466' | 
STRAT == '470' | STRAT == '471' | STRAT == '472' | STRAT == '473' | STRAT == '474' | STRAT == '475' | STRAT == '476' | STRAT == '477' | STRAT == '478' | 
STRAT == '480' | STRAT == '481' | STRAT == '482' | STRAT == '483' | STRAT == '484' | STRAT == '485' | 
STRAT == '490' | STRAT == '491' | STRAT == '492' | STRAT == '493' | STRAT == '494' | STRAT == '495' )  & (MONTH == 6 | MONTH == 7 | MONTH == 8) & (CLEN == 1)
)

## now do the same but separately for NAFO 4X and for NAFO 4VW
nafo.4vw.strat <- c('440','441','442','443','444','445','446','447','448','449','450','451','452','453','454','455','456','457','458','459','460','461','462','463','464','465','466','467','468','469')
nafo.4x.strat <- c('470','471','472','473','474','475','476','477','478','480','481','482','483','484','485','490','491','492','493','494','495')

## NAFO 4X
det.summer.nafo4x.df <- subset(det.summer.df, STRAT %in% nafo.4x.strat)
## NAFO 4VW
det.summer.nafo4vw.df <- subset(det.summer.df, STRAT %in% nafo.4vw.strat)

## ALL
lw.fit <- lm(log(FWT)~log(FLEN),data=det.summer.df)
det.summer.df$pred.wgt <- exp(coef(lw.fit)[1]) * (det.summer.df$FLEN ^coef(lw.fit)[2] )
det.summer.df$condition <- det.summer.df$FWT / det.summer.df$pred.wgt

## NAFO 4X
lw.fit.nafo4x <- lm(log(FWT)~log(FLEN),data=det.summer.nafo4x.df)
det.summer.nafo4x.df$pred.wgt <- exp(coef(lw.fit.nafo4x)[1]) * (det.summer.nafo4x.df$FLEN ^coef(lw.fit.nafo4x)[2] )
det.summer.nafo4x.df$condition <- det.summer.nafo4x.df$FWT / det.summer.nafo4x.df$pred.wgt

## NAFO 4VW
lw.fit.nafo4vw <- lm(log(FWT)~log(FLEN),data=det.summer.nafo4vw.df)
det.summer.nafo4vw.df$pred.wgt <- exp(coef(lw.fit.nafo4vw)[1]) * (det.summer.nafo4vw.df$FLEN ^coef(lw.fit.nafo4vw)[2] )
det.summer.nafo4vw.df$condition <- det.summer.nafo4vw.df$FWT / det.summer.nafo4vw.df$pred.wgt

final.list <- list(all=det.summer.df, nafo4x=det.summer.nafo4x.df, nafo4vw=det.summer.nafo4vw.df)

return(final.list)
} # end function


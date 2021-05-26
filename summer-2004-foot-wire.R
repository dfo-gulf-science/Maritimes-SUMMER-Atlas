## checking the effect of having that 19' foot-wire on the trawl catches
## - following on conversation with BIll MacEachern

## details about the 2004 summer survey

qu <- paste0(
  "
  SELECT 
  *
  FROM
  GROUNDFISH.GSINF
  WHERE
  MISSION IN ('TEL2004529','TEL2004530')
  "
)

sets <- sqlQuery(chan,qu)

plot(sets$SDATE, as.factor(sets$TYPE), ylim=c(0,3), pch=19)


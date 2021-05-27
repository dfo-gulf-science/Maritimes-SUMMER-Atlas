SELECT 
extract(YEAR FROM i.SDATE),
c.length_units,
c.spec,
count(*)
FROM 
GROUNDFISH.GSINF i,
GROUNDFISH.GSCAT c 
WHERE 
i.mission = c.mission AND
i.setno = c.setno AND 
c.spec=60 AND
c.length_units IS NOT NULL
GROUP BY extract(YEAR FROM i.SDATE), c.length_units, c.spec 
ORDER BY extract(YEAR FROM i.SDATE)
;

SELECT
d.mission,
d.setno,
i.strat,
TO_CHAR(i.sdate,'yyyy') YEAR,
TO_CHAR(i.sdate,'mm') MONTH,
TO_CHAR(i.sdate,'dd') DAY,
s.SPEC SCIEN,
d.fshno,
c.length_units,
d.flen,
d.clen
FROM
groundfish.gsinf i,
groundfish.gscat c,
groundfish.gsdet d,
groundfish.GSSPECIES s
WHERE
i.mission = c.mission AND
i.setno = c.setno AND
i.mission = d.mission AND
i.setno = d.setno AND
d.spec=s.CODE AND
d.spec=c.spec AND
s.CODE = 60
order by year, month, day, setno,fshno
;

-- deep strata
SELECT
extract(YEAR FROM i.SDATE),
i.strat,
count(*)
FROM 
GROUNDFISH.GSINF i
WHERE i.strat IN ('501','502','503','504','505')
GROUP BY
extract(YEAR FROM i.SDATE), i.strat
ORDER BY
extract(YEAR FROM i.SDATE) DESC, i.strat
;

SELECT 
m.YEAR,
m.VESEL,
m.PURPOSE, 
count(*)
FROM
groundfish.GSMISSION_LIST m
WHERE 
m.FK_SERIES_ID = 'SUMMER'
GROUP BY 
m.YEAR,
m.VESEL,
m.PURPOSE 
ORDER BY 
m.YEAR
;

SELECT 
i.mission,
extract(YEAR FROM i.sdate),
i.gear,
g.geardesc,
count(*)
FROM 
groundfish.GSMISSION_LIST m,
GROUNDFISH.GSINF i,
GROUNDFISH.GSGEAR g 
WHERE m.PK_MISSION = i.mission AND i.gear=g.gear AND m.FK_SERIES_ID = 'SUMMER'
GROUP BY
i.mission, extract(YEAR FROM i.sdate), i.gear, g.geardesc
ORDER BY
extract(YEAR FROM i.sdate)
;

SELECT 
i.mission,
extract(YEAR FROM i.sdate),
i.gear,
g.geardesc,
count(*)
FROM 
groundfish.GSMISSIONS m,
GROUNDFISH.GSINF i,
GROUNDFISH.GSGEAR g 
WHERE 
m.mission = i.mission AND 
i.gear=g.gear AND 
m.SEASON = 'SUMMER'
GROUP BY
i.mission, extract(YEAR FROM i.sdate), i.gear, g.geardesc
ORDER BY
extract(YEAR FROM i.sdate)
;



  SELECT 
  MISSION,
  EXTRACT(YEAR FROM sdate),
  EXTRACT(MONTH FROM sdate),
  count(*)
  FROM
  GROUNDFISH.GSINF
  WHERE
  EXTRACT(YEAR FROM sdate) = 2004
  GROUP BY 
  MISSION,
    EXTRACT(YEAR FROM sdate),
  EXTRACT(MONTH FROM sdate)
  ;

 
 SELECT 
  *
  FROM
  GROUNDFISH.GSINF
  WHERE
  MISSION IN ('TEL2004529','TEL2004530')
  ;
  
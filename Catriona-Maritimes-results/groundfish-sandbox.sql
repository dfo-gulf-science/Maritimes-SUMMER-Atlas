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
m.SEASON,
i.mission,
extract(YEAR FROM i.sdate),
extract(MONTH FROM i.sdate),
i.gear,
g.geardesc,
count(*)
FROM 
groundfish.GSMISSIONS m,
GROUNDFISH.GSINF i,
GROUNDFISH.GSGEAR g 
WHERE
i.TYPE=1 AND
m.mission = i.mission AND 
i.gear=g.gear AND 
m.SEASON = 'SUMMER'
GROUP BY
m.SEASON, i.mission, extract(YEAR FROM i.sdate), extract(MONTH FROM i.sdate), i.gear, g.geardesc
ORDER BY
extract(YEAR FROM i.sdate), extract(MONTH FROM i.sdate)
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
  
 SELECT 
  I.*,
  C.*
  FROM
  GROUNDFISH.GSINF I, 
  GROUNDFISH.GSCAT C
  WHERE
  I.MISSION=C.MISSION AND 
  I.SETNO=C.SETNO AND
  I.MISSION IN ('TEL2004529','TEL2004530')
  ;
 
 
 SELECT  
 * 
 FROM 
 GROUNDFISH.GSSPECIES  
 WHERE 
 SPEC = 10
 ;
 
SELECT
i.mission,
extract(YEAR FROM i.sdate),
extract(MONTH FROM i.sdate),
i.gear,
count(*)
FROM 
GROUNDFISH.GSINF i 
WHERE
i.type=1 
GROUP BY
i.mission, extract(YEAR FROM i.sdate), extract(MONTH FROM i.sdate), i.gear
ORDER BY
extract(YEAR FROM i.sdate), extract(MONTH FROM i.sdate)
;

 SELECT  
 * 
 FROM 
 GROUNDFISH.GSINFP70  
 WHERE 
SLON AND 
SLAT > 45
 ;

SELECT 
extract(YEAR FROM i.sdate) AS YEAR,
count(*) 
FROM 
groundfish.GSINF i,
groundfish.GSCAT c 
WHERE 
  I.MISSION=C.MISSION AND 
  I.SETNO=C.SETNO AND
c.SPEC = 123
GROUP BY 
extract(YEAR FROM i.sdate)
ORDER BY extract(YEAR FROM i.sdate)
;


SELECT 
MIN(EXTRACT(YEAR FROM g.sdate))
FROM
GROUNDFISH.GSINFP70 g 
;

SELECT *
FROM 
GROUNDFISH.GSCAT g 
WHERE 
g.SPEC = 11 AND 
g.mission='NED2003002' AND 
g.SETNO=2
;


SELECT
extract(YEAR FROM i.sdate),
extract(MONTH FROM i.sdate),
i.gear,
g.GEARDESC, 
count(*)
FROM 
GROUNDFISH.GSINF i,
GROUNDFISH.GSGEAR g
WHERE 
i.gear=g.gear
GROUP BY
i.mission, extract(YEAR FROM i.sdate), extract(MONTH FROM i.sdate), i.gear, g.GEARDESC
ORDER BY
extract(YEAR FROM i.sdate) DESC, extract(MONTH FROM i.sdate)
;
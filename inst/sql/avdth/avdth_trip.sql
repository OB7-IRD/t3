SELECT
	'fr.ird.avdth.entities.data.Trip#'
		& format(b.C_BAT, '0000')
		& '#'
		& YEAR(m.D_DBQ)
		& format(MONTH(m.D_DBQ), '00')
		& format(DAY(m.D_DBQ), '00') AS trip_id
	,p.C_ISO3166_A3 AS fleet
	,Format(m.D_DEPART, "yyyy-mm-dd") & ' ' & Format(m.D_DEPART, "Long Time") AS departure_date
	,Format(m.D_DBQ, "yyyy-mm-dd") & ' ' & Format(m.D_DBQ, "Long Time") AS landing_date
	,m.F_ENQ AS logbook_availability
	,m.F_CAL_VID AS fish_hold_empty
	,b.c_bat AS vessel_id
	,ttb.L_TYPE_TYPE_B AS vessel_type
FROM
	((((MAREE m
	INNER JOIN BATEAU b ON m.C_BAT = b.C_BAT)
	INNER JOIN TYPE_BATEAU tb ON b.C_TYP_B = tb.C_TYP_B)
	INNER JOIN TYPE_TYPE_BATEAU ttb ON tb.C_TYP_TYP_B = ttb.C_TYP_TYPE_B)
	INNER JOIN PAYS p ON b.C_PAYS = p.C_PAYS)
WHERE
	m.D_DBQ BETWEEN ?begin_period AND ?end_period
	AND p.C_ISO3166_A3 IN (?countries)
	AND ttb.C_TYP_TYPE_B IN (1)
	AND 'fr.ird.avdth.entities.data.Trip#'
		& format(b.C_BAT, '0000')
		& '#' & YEAR(m.D_DBQ)
		& format(MONTH(m.D_DBQ), '00')
		& format(DAY(m.D_DBQ), '00') IN (SELECT DISTINCT 
											'fr.ird.avdth.entities.data.Trip#'
												& format(a.C_BAT, '0000')
												& '#'
												& YEAR(a.D_DBQ)
												& format(MONTH(a.D_DBQ), '00')
												& format(DAY(a.D_DBQ), '00') AS trip_id
										FROM
											((ACTIVITE a
											INNER JOIN BATEAU b ON a.C_BAT = b.C_BAT)
											INNER JOIN PAYS p ON b.C_PAYS = p.C_PAYS)
										WHERE
											a.D_DBQ BETWEEN ?begin_period AND ?end_period
											AND p.C_ISO3166_A3 IN (?countries)
											AND a.C_OCEA IN (?oceans))
ORDER BY
	m.C_BAT
	,m.D_DBQ
;

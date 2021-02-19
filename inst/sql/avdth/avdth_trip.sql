SELECT
	'fr.ird.t3.entities.data.Trip#'
		& format(b.C_BAT, '0000')
		& '#'
		& YEAR(m.D_DBQ)
		& format(MONTH(m.D_DBQ), '00')
		& format(DAY(m.D_DBQ), '00') AS trip_id
	,p.C_ISO3166_A3 AS fleet
	,m.D_DEPART AS departure_date
	,m.D_DBQ AS landing_date
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
	AND 'fr.ird.t3.entities.data.Trip#'
		& format(b.C_BAT, '0000')
		& '#' & YEAR(m.D_DBQ)
		& format(MONTH(m.D_DBQ), '00')
		& format(DAY(m.D_DBQ), '00') IN (SELECT DISTINCT 
											'fr.ird.t3.entities.data.Trip#'
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

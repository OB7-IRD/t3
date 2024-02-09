SELECT
	'fr.ird.avdth.entities.data.Trip#'
		& format(b.C_BAT, '0000')
		& '#'
		& YEAR(m.D_DBQ)
		& format(MONTH(m.D_DBQ), '00')
		& format(DAY(m.D_DBQ), '00') AS trip_id
	,p.C_PAYS AS fleet_code
	,p.C_ISO3166_A3 AS fleet_label
	,Format(m.D_DEPART, "yyyy-mm-dd") & ' ' & Format(m.D_DEPART, "Long Time") AS departure_date
	,Format(m.D_DBQ, "yyyy-mm-dd") & ' ' & Format(m.D_DBQ, "Long Time") AS trip_end_date
	,m.F_ENQ AS logbook_availability_code
	,m.F_CAL_VID AS landing_well_content_code
	,b.c_bat AS vessel_code
	,tb.C_TYP_B AS vessel_type_code
FROM
	(((MAREE m
	INNER JOIN BATEAU b ON m.C_BAT = b.C_BAT)
	INNER JOIN TYPE_BATEAU tb ON b.C_TYP_B = tb.C_TYP_B)
	INNER JOIN PAYS p ON b.C_PAYS = p.C_PAYS)
WHERE
	m.D_DBQ BETWEEN ?begin_time_period AND ?end_time_period
	AND p.C_PAYS IN (?fleet_codes)
	AND tb.C_TYP_B IN (?vessel_type_codes)
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
											(((ACTIVITE a
											INNER JOIN BATEAU b ON a.C_BAT = b.C_BAT)
											INNER JOIN TYPE_BATEAU tb ON b.C_TYP_B = tb.C_TYP_B)
											INNER JOIN PAYS p ON b.C_PAYS = p.C_PAYS)
										WHERE
											a.D_DBQ BETWEEN ?begin_time_period AND ?end_time_period
											AND p.C_PAYS IN (?fleet_codes)
											AND a.C_OCEA IN (?ocean_codes)
											AND tb.C_TYP_B IN (?vessel_type_codes))
ORDER BY
	m.C_BAT
	,m.D_DBQ
;

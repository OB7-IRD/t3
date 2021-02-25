SELECT
	'fr.ird.avdth.entities.data.Trip#'
		& format(a.C_BAT, '0000')
		& '#'
		& YEAR(a.D_DBQ)
		& format(MONTH(a.D_DBQ), '00')
		& format(DAY(a.D_DBQ), '00') AS trip_id
	,'fr.ird.avdth.entities.data.Activity#'
		& format(a.C_BAT, '0000')
		& '#'
		& YEAR(a.D_DBQ)
		& format(MONTH(a.D_DBQ), '00')
		& format(DAY(a.D_DBQ), '00')
		& format(a.D_ACT, 'YYYYMMDD')
		& format(a.N_ACT, '00') AS activity_id
	,a.C_OCEA AS ocean
	,a.D_ACT AS activity_date
	,a.N_ACT AS activity_number
	,a.V_LON AS activity_longitude
	,a.V_LAT AS activity_latitude
	,IIf(a.C_OPERA IN (0,1,2,14),
		a.V_NB_OP,
		0) AS set_count
	,a.C_TBANC AS school_type
	,a.C_OPERA AS activity_code
	,o.L_OPERA AS activity_name
	,a.V_TMER AS time_at_sea
FROM
	(((ACTIVITE a
	INNER JOIN OPERA o ON a.C_OPERA = o.C_OPERA)
	INNER JOIN BATEAU b ON a.C_BAT = b.C_BAT)
	INNER JOIN PAYS p ON b.C_PAYS = p.C_PAYS)
WHERE
	a.D_DBQ BETWEEN ?begin_period AND ?end_period
	AND p.C_ISO3166_A3 IN (?countries)
	AND 'fr.ird.avdth.entities.data.Trip#'
			& format(a.C_BAT, '0000')
			& '#'
			& YEAR(a.D_DBQ)
			& format(MONTH(a.D_DBQ), '00')
			& format(DAY(a.D_DBQ), '00') IN (SELECT DISTINCT 
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
;

SELECT
	'fr.ird.avdth.entities.data.Activity#'
		& format(ce.C_BAT, '0000')
		& '#' 
		& YEAR(ce.D_DBQ)
		& format(MONTH(ce.D_DBQ), '00')
		& format(DAY(ce.D_DBQ), '00')
		& format(ce.D_ACT, 'YYYYMMDD')
		& format(ce.N_ACT, '00') AS activity_id
	,'fr.ird.avdth.entities.data.ElementaryCatch#'
		& format(ce.C_BAT, '0000')
		& '#'
		& YEAR(ce.D_DBQ)
		& format(MONTH(ce.D_DBQ), '00')
		& Format(DAY(ce.D_DBQ), '00')
		& ce.N_ACT
		& '.'
		& ce.N_CAPT AS elementarycatch_id
	,a.C_OCEA AS ocean
	,a.C_TBANC AS school_type
	,ce.C_CAT_T AS logbook_category
	,ct.L_CAT_T AS logbook_category_name
	,ce.C_ESP AS specie_code
	,e.C_ESP_3L AS specie_code3l
	,SUM(ce.V_POIDS_CAPT) AS catch_weight
FROM
	(((((CAPT_ELEM ce
	INNER JOIN ACTIVITE a ON a.C_BAT = ce.C_BAT AND a.D_DBQ = ce.D_DBQ AND a.D_ACT = ce.D_ACT AND a.N_ACT = ce.N_ACT)
	INNER JOIN CAT_TAILLE ct ON ce.C_ESP = ct.C_ESP AND ce.C_CAT_T = ct.C_CAT_T)
	INNER JOIN ESPECE e ON ce.C_ESP = e.C_ESP)
	INNER JOIN BATEAU b ON a.C_BAT = b.C_BAT)
	INNER JOIN PAYS p ON b.C_PAYS = p.C_PAYS)
WHERE
	a.D_DBQ BETWEEN ?begin_period AND ?end_period
	AND p.C_ISO3166_A3 IN (?countries)
	AND 'fr.ird.avdth.entities.data.Trip#'
			& format(ce.C_BAT, '0000')
			& '#'
			& YEAR(ce.D_DBQ)
			& format(MONTH(ce.D_DBQ), '00')
			& format(DAY(ce.D_DBQ), '00') IN (SELECT DISTINCT 
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
GROUP BY
	activity_id
	,elementarycatch_id
	,ocean
	,school_type
	,logbook_category
	,logbook_category_name
	,specie_code
	,specie_code3l
;

SELECT
	'fr.ird.avdth.entities.data.Trip#'
		& format(lc.C_BAT,'0000')
		& '#'
		& YEAR(lc.D_DBQ)
		& format(MONTH(lc.D_DBQ), '00')
		& format(DAY(lc.D_DBQ), '00') AS trip_id
	,'fr.ird.avdth.entities.data.ElementaryLanding#'
		& format(lc.C_BAT,'0000')
		& '#'
		& YEAR(lc.D_DBQ)
		& format(MONTH(lc.D_DBQ), '00')
		& format(DAY(lc.D_DBQ), '00')
		& "."
		& lc.N_LOT
		& lc.C_ESP
		& lc.C_CAT_C AS elementarylanding_id
	,lc.C_CAT_C AS landing_category
	,cc.L_CC_SOV AS landing_category_name
	,lc.C_ESP AS specie_code
	,e.C_ESP_3L AS specie_code3l
	,SUM(lc.V_POIDS_LC) AS landing_weight
FROM
	((((LOT_COM lc
	INNER JOIN CAT_COM cc ON lc.C_ESP = cc.C_ESP AND lc.C_CAT_C = cc.C_CAT_C)
	INNER JOIN ESPECE e ON lc.C_ESP = e.C_ESP)
	INNER JOIN BATEAU b ON lc.C_BAT = b.C_BAT)
	INNER JOIN PAYS p ON b.C_PAYS = p.C_PAYS)
WHERE
	lc.D_DBQ BETWEEN ?begin_period AND ?end_period
	AND p.C_ISO3166_A3 IN (?countries)
	AND 'fr.ird.t3.entities.data.Trip#'
			& format(lc.C_BAT, '0000')
			& '#'
			& YEAR(lc.D_DBQ)
			& format(MONTH(lc.D_DBQ), '00')
			& format(DAY(lc.D_DBQ), '00') IN (SELECT DISTINCT 
												'fr.ird.t3.entities.data.Trip#'
													& format(a.C_BAT,'0000')
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
	trip_id
	,elementarylanding_id
	,landing_category
	,landing_category_name
	,specie_code
	,specie_code3l
;

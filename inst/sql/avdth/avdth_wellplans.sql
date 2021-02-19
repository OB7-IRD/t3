SELECT
	'fr.ird.t3.entities.data.WellPlan#'
		& format(cc.C_BAT, '0000')
		& '#'
		& YEAR(cc.D_DBQ)
		& format(MONTH(cc.D_DBQ), '00')
		& format(DAY(cc.D_DBQ), '00') AS wellplan_id
	,'fr.ird.t3.entities.data.Well#'
		& format(cc.C_BAT, '0000')
		& '#'
		& YEAR(cc.D_DBQ)
		& format(MONTH(cc.D_DBQ), '00')
		& format(DAY(cc.D_DBQ), '00')
		& '.'
		& cc.N_CUVE
		& cc.F_POS_CUVE AS well_id
	,'fr.ird.t3.entities.data.Activity#'
		& format(cc.C_BAT, '0000')
		& '#'
		& YEAR(cc.D_DBQ)
		& format(MONTH(cc.D_DBQ), '00')
		& format(DAY(cc.D_DBQ), '00')
		& format(cc.D_ACT, 'YYYYMMDD')
		& format(cc.N_ACT, '00') AS activity_id
	,'fr.ird.t3.entities.data.Sample#'
		& format(cc.C_BAT, '0000')
		& '#'
		& YEAR(cc.D_DBQ)
		& format(MONTH(cc.D_DBQ), '00')
		& format(DAY(cc.D_DBQ), '00')
		& '.'
		& cc.N_CUVE
		& cc.F_POS_CUVE
		& '.' 
		& e.N_ECH AS sample_id
	,es.C_ESP AS specie_code
	,es.C_ESP_3L AS specie_code3l
	,cc.V_POIDS AS wellplan_weight
	,cc.V_NB AS wellplan_number
	,cc.C_CAT_POIDS AS wellplan_weigth_category_code
	,cp.L_CAT_POIDS AS wellplan_weigth_category_label
FROM
	(((((CUVE_CALEE cc
	LEFT JOIN ECHANTILLON e ON (cc.F_POS_CUVE = e.F_POS_CUVE) AND (cc.N_CUVE = e.N_CUVE) AND (cc.D_DBQ = e.D_DBQ) AND (cc.C_BAT = e.C_BAT))
	INNER JOIN ESPECE es ON cc.C_ESP = es.C_ESP)
	INNER JOIN CAT_POIDS cp ON cc.C_CAT_POIDS = cp.C_CAT_POIDS)
	INNER JOIN BATEAU b ON cc.C_BAT = b.C_BAT)
	INNER JOIN PAYS p ON b.C_PAYS = p.C_PAYS)
WHERE
	cc.D_DBQ BETWEEN ?begin_period AND ?end_period
	AND p.C_ISO3166_A3 IN (?countries)
	AND 'fr.ird.t3.entities.data.Trip#'
			& format(cc.C_BAT, '0000')
			& '#'
			& YEAR(cc.D_DBQ)
			& format(MONTH(cc.D_DBQ), '00')
			& format(DAY(cc.D_DBQ), '00') IN (SELECT DISTINCT 
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
	wellplan_id
	,well_id
	,activity_id
	,sample_id
	,specie_code
	,wellplan_weigth_category_code
;

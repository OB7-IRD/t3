SELECT
	'fr.ird.avdth.entities.data.WellPlan#'
		& format(cc.C_BAT, '0000')
		& '#'
		& YEAR(cc.D_DBQ)
		& format(MONTH(cc.D_DBQ), '00')
		& format(DAY(cc.D_DBQ), '00') AS wellplan_id
	,'fr.ird.avdth.entities.data.Well#'
		& format(cc.C_BAT, '0000')
		& '#'
		& YEAR(cc.D_DBQ)
		& format(MONTH(cc.D_DBQ), '00')
		& format(DAY(cc.D_DBQ), '00')
		& '.'
		& cc.N_CUVE
		& cc.F_POS_CUVE AS well_id
	,'fr.ird.avdth.entities.data.Activity#'
		& format(cc.C_BAT, '0000')
		& '#'
		& YEAR(cc.D_DBQ)
		& format(MONTH(cc.D_DBQ), '00')
		& format(DAY(cc.D_DBQ), '00')
		& format(cc.D_ACT, 'yyyymmdd')
		& format(cc.N_ACT, '00') AS activity_id
	,'fr.ird.avdth.entities.data.Sample#'
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
	,es.C_ESP AS species_code
	,es.C_ESP_3L AS species_fao_code
	,cc.V_POIDS AS wellplan_weight
	,cc.V_NB AS wellplan_count
	,cc.C_CAT_POIDS AS weight_category_code
	,cp.L_CAT_POIDS AS weight_category_label
FROM
	((((((CUVE_CALEE cc
	LEFT JOIN ECHANTILLON e ON (cc.F_POS_CUVE = e.F_POS_CUVE) AND (cc.N_CUVE = e.N_CUVE) AND (cc.D_DBQ = e.D_DBQ) AND (cc.C_BAT = e.C_BAT))
	INNER JOIN ESPECE es ON cc.C_ESP = es.C_ESP)
	INNER JOIN CAT_POIDS cp ON cc.C_CAT_POIDS = cp.C_CAT_POIDS)
	INNER JOIN BATEAU b ON cc.C_BAT = b.C_BAT)
	INNER JOIN TYPE_BATEAU tb ON b.C_TYP_B = tb.C_TYP_B)
	INNER JOIN PAYS p ON b.C_PAYS = p.C_PAYS)
WHERE
	cc.D_DBQ BETWEEN ?begin_time_period AND ?end_time_period
	AND p.C_PAYS IN (?fleet_code)
	AND tb.C_TYP_B IN (?vessel_type_code)
	AND 'fr.ird.avdth.entities.data.Trip#'
			& format(cc.C_BAT, '0000')
			& '#'
			& YEAR(cc.D_DBQ)
			& format(MONTH(cc.D_DBQ), '00')
			& format(DAY(cc.D_DBQ), '00') IN (SELECT DISTINCT 
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
												a.D_DBQ BETWEEN ?begin_time_period AND ?end_time_period
												AND p.C_PAYS IN (?fleet_code)
												AND a.C_OCEA IN (?ocean_code))
ORDER BY
	wellplan_id
	,well_id
	,activity_id
	,sample_id
	,species_code
	,weight_category_code
;

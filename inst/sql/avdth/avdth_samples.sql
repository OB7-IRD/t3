SELECT
	'fr.ird.avdth.entities.data.Trip#'
		& format(e.C_BAT, '0000')
		& '#'
		& YEAR(e.D_DBQ)
		& format(MONTH(e.D_DBQ), '00')
		& format(DAY(e.D_DBQ), '00') AS trip_id
	,'fr.ird.avdth.entities.data.Well#'
		& format(e.c_bat, '0000')
		& '#'
		& YEAR(e.D_DBQ)
		& format(MONTH(e.D_DBQ), '00')
		& format(DAY(e.D_DBQ), '00')
		& '.'
		& e.N_CUVE
		& e.F_POS_CUVE AS well_id
	,e.V_POIDS_M10 AS well_minus10_weigth
	,e.V_POIDS_P10 AS well_plus10_weigth
	,e.V_POIDS_ECH AS well_global_weigth
	,'fr.ird.avdth.entities.data.Sample#'
		& format(e.c_bat, '0000')
		& '#'
		& YEAR(e.D_DBQ)
		& format(MONTH(e.D_DBQ), '00')
		& format(DAY(e.D_DBQ), '00')
		& '.'
		& e.N_CUVE
		& e.F_POS_CUVE
		& '.'
		& e.N_ECH AS sample_id
	,e.F_S_ECH AS sub_sample_id
	,'fr.ird.t3.entities.data.SampleSpeciesFrequency#'
		& format(e.c_bat, '0000')
		& '#'
		& YEAR(e.D_DBQ)
		& format(MONTH(e.D_DBQ), '00')
		& format(DAY(e.D_DBQ), '00')
		& '.'
		& e.N_CUVE
		& e.F_POS_CUVE
		& '.'
		& e.N_ECH AS elementarysampleraw_id
	,e.C_QUAL_ECH AS sample_quality
	,e.C_TYP_ECH AS sample_type
	,ee.C_ESP AS specie_code
	,es.C_ESP_3L AS specie_code3l
	,ee.F_LDLF AS length_type
	,ee.V_NB_TOT AS sample_total_count
	,ef.V_EFF AS sample_number_measured
	,ef.V_LONG AS sample_length_class
FROM
	(((((((ECHANTILLON e
	INNER JOIN ECH_ESP ee ON e.C_BAT = ee.C_BAT AND e.D_DBQ = ee.D_DBQ AND e.N_ECH = ee.N_ECH)
	INNER JOIN ECH_FREQT ef ON ee.C_BAT = ef.C_BAT AND ee.D_DBQ = ef.D_DBQ AND ee.N_ECH =ef.N_ECH AND ee.N_S_ECH = ef.N_S_ECH AND ee.C_ESP = ef.C_ESP AND ee.F_LDLF = ef.F_LDLF)
	INNER JOIN ESPECE es ON es.C_ESP = ee.C_ESP)
	INNER JOIN BATEAU b ON e.C_BAT = b.C_BAT)
	INNER JOIN TYPE_BATEAU tb ON b.C_TYP_B = tb.C_TYP_B)
	INNER JOIN TYPE_TYPE_BATEAU ttb ON tb.C_TYP_TYP_B = ttb.C_TYP_TYPE_B)
	INNER JOIN PAYS p ON b.C_PAYS = p.C_PAYS)
WHERE
	e.D_DBQ BETWEEN ?begin_period AND ?end_period
	AND p.C_ISO3166_A3 IN (?countries)
	AND e.C_TYP_ECH IN (?sample_type)
	AND ttb.C_TYP_TYPE_B IN (1)
	AND 'fr.ird.avdth.entities.data.Trip#'
			& format(e.C_BAT, '0000')
			& '#'
			& YEAR(e.D_DBQ)
			& format(MONTH(e.D_DBQ), '00')
			& format(DAY(e.D_DBQ), '00') IN (SELECT DISTINCT 
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
	trip_id
	,sample_id
	,well_id
	,sub_sample_id
	,specie_code
	,length_type
	,sample_length_class
;

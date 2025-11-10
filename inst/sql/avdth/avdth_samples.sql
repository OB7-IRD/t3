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
	,e.V_POIDS_M10 AS well_minus10_weight
	,e.V_POIDS_P10 AS well_plus10_weight
	,e.V_POIDS_ECH AS well_global_weight
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
	,ee.N_S_ECH AS sub_sample_id
	,'fr.ird.t3.entities.data.SampleSpecies#'
		& format(e.c_bat, '0000')
		& '#'
		& YEAR(e.D_DBQ)
		& format(MONTH(e.D_DBQ), '00')
		& format(DAY(e.D_DBQ), '00')
		& '.'
		& e.N_ECH
		& '.'
		& ee.N_S_ECH
		& '.'
		& ee.C_ESP
		& '.'
		& ee.F_LDLF AS sub_sample_total_count_id
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
	,e.C_QUAL_ECH AS sample_quality_code
	,e.C_TYP_ECH AS sample_type_code
	,ee.C_ESP AS species_code
	,es.C_ESP_3L AS species_fao_code
	,ee.F_LDLF AS size_measure_type_code
	,ee.V_NB_TOT AS sample_total_count
	,ef.V_EFF AS sample_number_measured
	,ef.V_LONG AS sample_length_class
FROM
	((((((ECHANTILLON e
	INNER JOIN ECH_ESP ee ON e.C_BAT = ee.C_BAT AND e.D_DBQ = ee.D_DBQ AND e.N_ECH = ee.N_ECH)
	INNER JOIN ECH_FREQT ef ON ee.C_BAT = ef.C_BAT AND ee.D_DBQ = ef.D_DBQ AND ee.N_ECH =ef.N_ECH AND ee.N_S_ECH = ef.N_S_ECH AND ee.C_ESP = ef.C_ESP AND ee.F_LDLF = ef.F_LDLF)
	INNER JOIN ESPECE es ON es.C_ESP = ee.C_ESP)
	INNER JOIN BATEAU b ON e.C_BAT = b.C_BAT)
	INNER JOIN TYPE_BATEAU tb ON b.C_TYP_B = tb.C_TYP_B)
	INNER JOIN PAYS p ON b.C_PAYS = p.C_PAYS)
WHERE
	e.D_DBQ BETWEEN ?begin_time_period AND ?end_time_period
	AND p.C_ISO3166_A3 IN (?flag_codes)
	AND e.C_TYP_ECH IN (?sample_type_codes)
	AND tb.C_TYP_B IN (?vessel_type_codes)
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
												a.D_DBQ BETWEEN ?begin_time_period AND ?end_time_period
												AND p.C_ISO3166_A3 IN (?flag_codes)
												AND a.C_OCEA IN (?ocean_codes))
ORDER BY
	trip_id
	,sample_id
	,well_id
	,sub_sample_id
	,species_code
	,size_measure_type_code
	,sample_length_class
;

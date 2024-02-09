SELECT
	'fr.ird.avdth.entities.data.Trip#'
		& format(ec.C_BAT, '0000')
		& '#'
		& YEAR(ec.D_DBQ)
		& format(MONTH(ec.D_DBQ), '00')
		& format(DAY(ec.D_DBQ), '00') AS trip_id
	,'fr.ird.avdth.entities.data.Activity#'
		& format(ec.C_BAT, '0000')
		& '#'
		& YEAR(ec.D_DBQ)
		& format(MONTH(ec.D_DBQ), '00')
		& format(DAY(ec.D_DBQ), '00')
		& format(ec.D_ACT, 'yyyymmdd')
		& format(ec.N_ACT, '00') AS activity_id
	,'fr.ird.avdth.entities.data.Well#'
		& format(ec.C_BAT, '0000')
		& '#'
		& YEAR(ec.D_DBQ)
		& format(MONTH(ec.D_DBQ), '00')
		& format(DAY(ec.D_DBQ), '00')
		& '.'
		& e.N_CUVE
		& e.F_POS_CUVE AS well_id
	,'fr.ird.avdth.entities.data.Sample#'
		& format(ec.C_BAT, '0000')
		& '#'
		& YEAR(ec.D_DBQ)
		& format(MONTH(ec.D_DBQ), '00')
		& format(DAY(ec.D_DBQ), '00')
		& '.'
		& e.N_CUVE
		& e.F_POS_CUVE
		& '.' 
		& e.N_ECH AS sample_id
	,ec.V_POND AS well_set_weighted_weight
FROM
	((((ECH_CALEE ec
	INNER JOIN ECHANTILLON e ON e.C_BAT = ec.C_BAT AND ec.D_DBQ = e.D_DBQ AND ec.N_ECH = e.N_ECH)
	INNER JOIN BATEAU b ON ec.C_BAT = b.C_BAT)
	INNER JOIN TYPE_BATEAU tb ON b.C_TYP_B = tb.C_TYP_B)
	INNER JOIN PAYS p ON b.C_PAYS = p.C_PAYS)
WHERE
	ec.D_DBQ BETWEEN ?begin_time_period AND ?end_time_period
	AND p.C_PAYS IN (?fleet_codes)
	AND tb.C_TYP_B IN (?vessel_type_codes)
	AND 'fr.ird.avdth.entities.data.Trip#'
			& format(ec.C_BAT, '0000')
			& '#'
			& YEAR(ec.D_DBQ)
			& format(MONTH(ec.D_DBQ), '00')
			& format(DAY(ec.D_DBQ), '00') IN (SELECT DISTINCT 
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
;

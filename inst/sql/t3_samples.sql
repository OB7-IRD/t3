SELECT
	s.trip::text AS trip_id
	,s.well::text AS well_id
	,s.minus10weight::integer AS well_minus10_weigth
	,s.plus10weight::integer AS well_plus10_weigth
	,s.globalweight::integer AS well_global_weigth
	,s.topiaid::text AS sample_id
	,ss.subsamplenumber::integer AS sub_sample_id
	,sq.code::integer AS sample_quality
	,st.code::integer AS sample_type
	,sp.code3l::text AS specie_code3l
	,ss.ldlfflag::integer AS length_type
	,ss.totalcount::integer AS sample_total_count
	,ssf.number::integer AS sample_number_measured
	,ssf.lengthclass::integer AS sample_length_class
FROM
	public.sample s 
	JOIN public.trip t ON (t.topiaid = s.trip)
	JOIN public.vessel v ON (t.vessel = v.topiaid)
	JOIN public.country c ON (v.fleetcountry = c.topiaid)
	JOIN public.samplespecies ss ON (ss.sample = s.topiaid)
	JOIN public.species sp ON (sp.topiaid = ss.species)
	JOIN public.samplespeciesfrequency ssf ON (ssf.samplespecies = ss.topiaid)
	JOIN public.samplequality sq ON (s.samplequality = sq.topiaid)
	JOIN public.sampletype st ON (s.sampletype = st.topiaid)
WHERE
	t.landingdate BETWEEN ?begin_period AND ?end_period
	AND c.codeiso3 IN (?countries)
	AND st.code IN (?sample_type)
ORDER BY
	trip_id
	,sample_id
	,well_id
	,sub_sample_id
	,specie_code3l
	,length_type
	,sample_length_class
;

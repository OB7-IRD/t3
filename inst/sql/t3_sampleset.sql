SELECT
	s.trip::text AS trip_id
	,sset.activity::text AS activity_id
	,s.well::text AS well_id
	,s.topiaid::text AS sample_id
	,sset.weightedweight::numeric AS well_set_weighted_weight
FROM
	public.sampleset sset
	JOIN public.sample s ON (sset.sample = s.topiaid)
	JOIN public.trip t ON (s.trip = t.topiaid)
	JOIN public.vessel v ON (t.vessel = v.topiaid)
	JOIN public.country c ON (v.flagcountry = c.topiaid)
WHERE
	t.landingdate BETWEEN ?begin_period AND ?end_period
	AND c.codeiso3 IN (?countries)
	AND t.topiaid IN (?trips_selected)
;

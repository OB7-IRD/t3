SELECT
	s.trip::text AS trip_id
	,s.topiaid::text AS sample_id
	,ss.subsamplenumber::integer AS sub_sample_id
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
WHERE
	t.landingdate BETWEEN '2016-10-01' AND '2018-03-01'
	AND c.codeiso3 IN ('FRA')
ORDER BY
	trip_id
	,sample_id
	,sub_sample_id
	,specie_code3l
	,length_type
	,sample_length_class
;

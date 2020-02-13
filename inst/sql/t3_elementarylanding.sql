SELECT
	t.topiaid::text AS trip_id
	,el.topiaid::text AS elementarylanding_id
	,wcla.code::integer AS landing_category
	,wcla.sovlibelle::text AS landing_category_name
	,s.code3l::text AS specie_code3l
	,sum(el.weight)::numeric AS landing_weight
FROM
	public.elementarylanding el
	JOIN public.weightcategorylanding wcla ON (el.weightcategorylanding = wcla.topiaid)
	JOIN public.species s ON (wcla.species = s.topiaid)
	JOIN public.trip t ON (el.trip = t.topiaid)
	JOIN public.vessel v ON (t.vessel = v.topiaid)
	JOIN public.country c ON (v.fleetcountry = c.topiaid)
WHERE
	t.landingdate BETWEEN ?begin_period AND ?end_period
	AND c.codeiso3 IN (?countries)
GROUP BY
	trip_id
	,elementarylanding_id
	,landing_category
	,landing_category_name
	,specie_code3l
;

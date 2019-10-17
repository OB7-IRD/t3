SELECT
	t.topiaid::text AS trip_id
	,t.landingdate::date AS landing_date
	,v.code::integer AS vessel_id
	,wcla.sovlibelle::text AS landing_category
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
	t.landingdate BETWEEN '2016-10-01' AND '2018-03-01'
	AND c.codeiso3 IN ('FRA')
GROUP BY
	trip_id
	,landing_date
	,vessel_id
	,landing_category
	,specie_code3l
ORDER BY
	vessel_id
	,landing_date
;

WITH ocean_selection AS (
SELECT DISTINCT
	t.topiaid::text AS trip_id
FROM
	public.activity a
	JOIN public.route r ON (a.route = r.topiaid)
	JOIN public.trip t ON (r.trip = t.topiaid)
	JOIN public.vessel v ON (t.vessel = v.topiaid)
	JOIN public.country c ON (v.flagcountry = c.topiaid)
	JOIN public.harbour h ON (t.landingharbour = h.topiaid)
	JOIN public.ocean o ON (h.ocean = o.topiaid)
WHERE
	t.landingdate BETWEEN ?begin_period AND ?end_period
	AND c.codeiso3 IN (?countries)
	AND o.code IN (?oceans)
	AND t.topiaid IN (?trips_selected)
)
SELECT
	t.topiaid::text AS trip_id
	,el.topiaid::text AS elementarylanding_id
	,wcla.code::integer AS landing_category
	,wcla.sovlibelle::text AS landing_category_name
	,s.code::integer AS specie_code
	,s.code3l::text AS specie_code3l
	,sum(el.weight)::numeric AS landing_weight
FROM
	public.elementarylanding el
	JOIN public.weightcategorylanding wcla ON (el.weightcategorylanding = wcla.topiaid)
	JOIN public.species s ON (wcla.species = s.topiaid)
	JOIN public.trip t ON (el.trip = t.topiaid)
	JOIN public.vessel v ON (t.vessel = v.topiaid)
	JOIN public.country c ON (v.flagcountry = c.topiaid)
WHERE
	t.landingdate BETWEEN ?begin_period AND ?end_period
	AND c.codeiso3 IN (?countries)
	AND t.topiaid IN (SELECT trip_id FROM ocean_selection)
GROUP BY
	trip_id
	,elementarylanding_id
	,landing_category
	,landing_category_name
	,specie_code
	,specie_code3l
;

SELECT
	a.topiaid::text AS activity_id
	,ec.topiaid::TEXT AS elementarycatch_id
	,o.code::integer AS ocean
	,st.code::integer AS school_type
	,wcl.code::integer AS logbook_category
	,wcl.label1::text AS logbook_category_name
	,s.code::integer AS specie_code
	,s.code3l::text AS specie_code3l
	,sum(ec.catchweight)::numeric AS catch_weight
FROM
	public.elementarycatch ec
	JOIN public.weightcategorylogbook wcl ON (ec.weightcategorylogbook = wcl.topiaid)
	JOIN public.species s ON (wcl.species = s.topiaid)
	JOIN public.activity a ON (ec.activity = a.topiaid)
	JOIN public.ocean o ON (a.ocean = o.topiaid)
	JOIN public.schooltype st ON (a.schooltype = st.topiaid)
	JOIN public.route r ON (a.route = r.topiaid)
	JOIN public.trip t ON (r.trip = t.topiaid)
	JOIN public.vessel v ON (t.vessel = v.topiaid)
	JOIN public.country c ON (v.fleetcountry = c.topiaid)
WHERE
	t.landingdate BETWEEN ?begin_period AND ?end_period
	AND c.codeiso3 IN (?countries)
	AND t.topiaid IN (?trips_selected)
GROUP BY
	activity_id
	,elementarycatch_id
	,o.code
	,school_type
	,logbook_category
	,logbook_category_name
	,specie_code
	,specie_code3l
;

SELECT
	o.code::integer AS ocean
	,t.topiaid::text AS trip_id
	,t.landingdate::date AS landing_date
	,r.date::date AS activity_date
	,a.number::integer AS activity_number
	,st.code::integer AS school_type
	,wcl.code::integer AS logbook_category
	,wcl.label1::text AS logbook_category_name 
	,v.code::integer AS vessel_id
	,s.code3l::text AS specie_code3l
	,sum(ec.catchweight)::integer AS catch_weight
FROM
	public.elementarycatch ec
	JOIN public.weightcategorylogbook wcl ON (ec.weightcategorylogbook = wcl.topiaid)
	JOIN public.species s ON (wcl.species = s.topiaid)
	JOIN public.activity a ON (ec.activity = a.topiaid)
	JOIN public.route r ON (a.route = r.topiaid)
	JOIN public.trip t ON (r.trip = t.topiaid)
	JOIN public.vessel v ON (t.vessel = v.topiaid)
	JOIN public.country c ON (v.fleetcountry = c.topiaid)
	JOIN public.ocean o ON (a.ocean = o.topiaid)
	JOIN public.schooltype st ON (a.schooltype = st.topiaid)
WHERE
	t.landingdate BETWEEN '2016-10-01' AND '2018-03-01'
	AND c.codeiso3 IN ('FRA')
GROUP BY
	o.code
	,trip_id
	,landing_date
	,activity_date
	,activity_number
	,school_type
	,logbook_category
	,logbook_category_name
	,vessel_id
	,specie_code3l
ORDER BY
	vessel_id
	,landing_date
;

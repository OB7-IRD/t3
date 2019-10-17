SELECT
	t.topiaid::TEXT AS trip_id
	,t.landingdate::date AS landing_date
	,r.date::date AS activity_date
	,a.number::integer AS activity_number
	,wcl.label1::text AS logbook_category 
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
WHERE
	t.landingdate BETWEEN '2016-10-01' AND '2018-03-01'
	AND c.codeiso3 IN ('FRA')
GROUP BY
	trip_id
	,landing_date
	,activity_date
	,activity_number
	,logbook_category
	,vessel_id
	,specie_code3l
ORDER BY
	vessel_id
	,landing_date
;

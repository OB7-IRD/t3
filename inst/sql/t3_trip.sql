SELECT
	t.topiaid::text AS trip_id
	,c.codeiso3::text AS fleet
	,t.departuredate::text AS departure_date
	,t.landingdate::text AS landing_date
	,t.logbookavailability::integer AS logbook_availability
	,t.fishholdempty::integer AS fish_hold_empty
	,v.code::integer AS vessel_id
FROM 
	public.trip t
	JOIN public.vessel v ON (t.vessel = v.topiaid)
	JOIN public.country c ON (v.fleetcountry = c.topiaid)
WHERE
	t.landingdate BETWEEN ?begin_period AND ?end_period
	AND c.codeiso3 IN (?countries)
ORDER BY
	vessel_id
	,landing_date
;

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
	,c.codeiso3::text AS fleet
	,t.departuredate::text AS departure_date
	,t.landingdate::text AS landing_date
	,t.logbookavailability::integer AS logbook_availability
	,t.fishholdempty::integer AS fish_hold_empty
	,v.code::integer AS vessel_id
	,v3.label1::text AS vessel_type
FROM 
	public.trip t
	JOIN public.vessel v ON (t.vessel = v.topiaid)
	JOIN public.vesseltype v2 ON (v.vesseltype = v2.topiaid)
	JOIN public.vesselsimpletype v3 ON (v2.vesselsimpletype = v3.topiaid)
	JOIN public.country c ON (v.flagcountry = c.topiaid)
WHERE
	t.landingdate BETWEEN ?begin_period AND ?end_period
	AND c.codeiso3 IN (?countries)
	AND t.topiaid IN (SELECT trip_id FROM ocean_selection)
ORDER BY
	vessel_id
	,landing_date
;

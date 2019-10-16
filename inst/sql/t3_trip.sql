SELECT
	t.topiaid::text AS trip_id
	,t.landingdate::date AS landing_date
	,t.fishingtime::integer AS fishing_time
	,t.timeatsea::integer AS time_at_sea
	,t.logbookavailability::integer AS logbook_availability
	,t.fishholdempty::integer AS fish_hold_empty
	,v.code::integer AS vessel_id
	,t.landingharbour::text AS landing_harbour_id
	,t.departureharbour::text AS departure_harbour_id
FROM 
	public.trip t
	JOIN public.vessel v ON (t.vessel = v.topiaid)
WHERE
	t.landingdate BETWEEN '2016-10-01' AND '2018-03-01'
ORDER BY
	vessel_id
	,landing_date
;

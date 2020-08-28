WITH ocean_selection AS (
SELECT DISTINCT
	t.topiaid::text AS trip_id
FROM
	public.activity a
	JOIN public.route r ON (a.route = r.topiaid)
	JOIN public.trip t ON (r.trip = t.topiaid)
	JOIN public.vessel v ON (t.vessel = v.topiaid)
	JOIN public.country c ON (v.flagcountry = c.topiaid)
	JOIN public.ocean o ON (a.ocean = o.topiaid)
WHERE
	t.landingdate BETWEEN ?begin_period AND ?end_period
	AND c.codeiso3 IN (?countries)
	AND o.code IN (?oceans)
	AND t.topiaid IN (?trips_selected)
)
SELECT
	t.topiaid::text AS trip_id
	,a.topiaid::text AS activity_id
	,o.code::integer AS ocean
	,r.date::date AS activity_date
	,a.number::integer AS activity_number
	,a.longitude::numeric AS activity_longitude
	,a.latitude::numeric AS activity_latitude
	,a.setcount::integer AS set_count
	,st.code::integer AS school_type
	,va.code::integer AS activity_code
	,va.label1::text AS activity_name
	,a.timeatsea::integer AS time_at_sea
FROM 
	public.activity a
	JOIN public.route r ON (a.route = r.topiaid)
	JOIN public.trip t ON (r.trip = t.topiaid)
	JOIN public.vessel v ON (t.vessel = v.topiaid)
	JOIN public.country c ON (v.flagcountry = c.topiaid)
	JOIN public.ocean o ON (a.ocean = o.topiaid)
	JOIN public.schooltype st ON (a.schooltype = st.topiaid)
	JOIN public.vesselactivity va ON (a.vesselactivity = va.topiaid)
WHERE
	t.landingdate BETWEEN ?begin_period AND ?end_period
	AND c.codeiso3 IN (?countries)
	AND t.topiaid IN (SELECT trip_id FROM ocean_selection)
;

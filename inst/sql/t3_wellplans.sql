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
	wp.topiaid::text AS wellplan_id
	,wp.well::text AS well_id
	,wp.activity::text AS activity_id
	,s.topiaid::text AS sample_id
	,sp.code::integer AS specie_code
	,sp.code3l::text AS specie_code3l
	,wp.weight::numeric AS wellplan_weight
	,wp.number::integer AS wellplan_number
	,wcwp.code::integer AS wellplan_weigth_category_code
	,wcwp.label1::text AS wellplan_weigth_category_label
FROM
	public.wellplan wp
	JOIN public.activity a ON (wp.activity = a.topiaid)
	JOIN public.route r ON (a.route = r.topiaid)
	JOIN public.trip t ON (r.trip = t.topiaid)
	JOIN public.vessel v ON (t.vessel = v.topiaid)
	JOIN public.vesseltype vt on (v.vesseltype = vt.topiaid)
	JOIN public.vesselsimpletype vst ON (vt.vesselsimpletype = vst.topiaid)
	JOIN public.country c ON (v.flagcountry = c.topiaid)
	JOIN public.species sp ON (wp.species = sp.topiaid)
	JOIN public.weightcategorywellplan wcwp ON (wcwp.topiaid = wp.weightcategorywellplan)
	LEFT JOIN public.sample s ON (wp.well = s.well)
WHERE
	t.landingdate BETWEEN ?begin_period AND ?end_period
	AND c.codeiso3 IN (?countries)
	AND vst.code IN (1)
	AND t.topiaid IN (SELECT trip_id FROM ocean_selection)
ORDER BY
	wellplan_id
	,well_id
	,activity_id
	,sample_id
	,specie_code
	,wellplan_weigth_category_code
;

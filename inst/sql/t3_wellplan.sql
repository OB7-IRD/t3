SELECT
	wp.topiaid::text AS wellplan_id
	,wp.well::text AS well_id
	,wp.activity::text AS activity_id
	,s.topiaid::text AS sample_id
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
	JOIN public.country c ON (v.fleetcountry = c.topiaid)
	JOIN public.species sp ON (wp.species = sp.topiaid)
	JOIN public.weightcategorywellplan wcwp ON (wcwp.topiaid = wp.weightcategorywellplan)
	LEFT JOIN public.sample s ON (wp.well = s.well)
WHERE
	t.landingdate BETWEEN '2016-10-01' AND '2018-03-01'
	AND c.codeiso3 IN ('FRA')
ORDER BY
	wellplan_id
	,well_id
	,activity_id
	,sample_id
	,specie_code3l
	,wellplan_weigth_category_code
;

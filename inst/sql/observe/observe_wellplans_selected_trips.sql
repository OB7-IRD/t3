select
	was.topiaid::text as wellplan_id
	,w.topiaid:: text as well_id
	,a.topiaid::text as activity_id
	,sc.code::text as school_type_code
	,sc.label1::text as school_type_label
	,s.topiaid::text as sample_id
	,sp.code::integer as species_code
	,sp.faocode::text as species_fao_code
	,was.weight::numeric as wellplan_weight
	--,was.count::integer as wellplan_count
	,wc.code::text as weight_category_code
	,wc.label1::text as weight_category_label
from
	ps_logbook.wellactivityspecies was
	join ps_logbook.wellactivity wa on (was.wellactivity = wa.topiaid)
	join ps_logbook.well w on (wa.well = w.topiaid)
	join ps_logbook.activity a on (wa.activity = a.topiaid)
	join ps_logbook.route r on (a.route = r.topiaid)
	left join ps_common.schooltype sc on (s.topiaid = a.schooltype)
	join ps_common.trip t on (r.trip = t.topiaid)
	join common.vessel v on (t.vessel = v.topiaid)
	join common.vesseltype vt on (v.vesseltype = vt.topiaid)
	join common.country c on (v.flagcountry = c.topiaid)
	join common.ocean o on (t.ocean = o.topiaid)
	join common.species sp on (was.species = sp.topiaid)
	join ps_common.weightcategory wc on (was.weightcategory = wc.topiaid)
	left join ps_logbook.sample s on (w.well = s.well AND s.trip = t.topiaid)
where
	t.topiaid in (?trip_ids)
;

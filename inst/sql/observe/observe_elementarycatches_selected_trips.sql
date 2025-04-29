select
	a.topiaid::text as activity_id
	,c.topiaid::text as elementarycatch_id
	,o.code::integer as ocean_code
	,o.label1::text as ocean_label
	,s.code::integer as school_type_code
	,s.label1::text as school_type_label
	,w.code::text as weight_category_code
	,w.label1::text as weight_category_label
	,w.minweight::numeric as weight_category_min
	,w.maxweight::numeric as weight_category_max
  ,sp.faocode::text as species_fao_code
	,c.weight::numeric as catch_weight
	,c.count::integer as catch_count
	,sf.code::integer as species_fate_code
	,sf.label1::text as species_fate_label
from
	ps_logbook.catch c
	join ps_logbook.activity a on (c.activity = a.topiaid)
	join ps_logbook.route r on (a.route = r.topiaid)
	join ps_common.trip t on (r.trip = t.topiaid)
	join common.vessel v on (t.vessel = v.topiaid)
	join common.vesseltype vt on (v.vesseltype = vt.topiaid)
	join common.country co on (v.flagcountry = co.topiaid)
	join common.ocean o on (t.ocean = o.topiaid)
	left join ps_common.schooltype s on (s.topiaid = a.schooltype)
	left join ps_common.weightcategory w on (c.weightcategory = w.topiaid)
	join common.species sp on (c.species = sp.topiaid)
	join ps_common.speciesfate sf on (c.speciesfate = sf.topiaid)
where
	t.topiaid in (?trip_ids)
;

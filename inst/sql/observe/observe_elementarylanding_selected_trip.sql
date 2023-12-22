select
	t.topiaid::text as trip_id
	,l.topiaid::text as elementarylanding_id
	,w.code::text as weight_category_code
	,w.label1::text as weight_category_label
	,sp.code::integer as species_code
	,sp.faocode::text as species_fao_code
	,l.weight::numeric as landing_weight
from
	ps_landing.landing l
	join ps_common.trip t on (l.trip = t.topiaid)
	join common.species sp on (l.species = sp.topiaid)
	left join ps_common.weightcategory w on (l.weightcategory = w.topiaid)
	join common.vessel v on (t.vessel = v.topiaid)
	join common.vesseltype vt on (v.vesseltype = vt.topiaid) 
	join common.country co on (v.fleetcountry = co.topiaid)
	join common.ocean o on (t.ocean = o.topiaid)
where
	t.topiaid in (?trip_id)
;

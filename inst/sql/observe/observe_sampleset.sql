select
	t.topiaid::text as trip_id
	,a.topiaid::text as activity_id
	,w.topiaid::text as well_id
	,sa.sample::text as sample_id
	,sa.weightedweight::numeric as well_set_weighted_weight
from
	ps_logbook.sampleactivity sa
	join ps_logbook.activity a on (sa.activity = a.topiaid)
	join ps_logbook.route r on (a.route = r.topiaid)
	join ps_common.trip t on (t.topiaid = r.trip)
	join common.vessel v on (t.vessel = v.topiaid)
	join common.vesseltype vt on (v.vesseltype = vt.topiaid) 
	join common.country c on (v.fleetcountry = c.topiaid)
	join common.ocean o on (t.ocean = o.topiaid)
	join ps_logbook.sample s on (sa.sample = s.topiaid)
	left join ps_logbook.well w on (t.topiaid = w.trip and s.well = w.well)
where
	t.enddate between ?begin_time_period and ?end_time_period
	and c.code in (?fleet_code)
	and o.code in (?ocean_code)
	and vt.code in (?vessel_type_code)
;

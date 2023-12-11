select
	t.topiaid::text as trip_id
	,a.topiaid:: text as activity_id
	,o.code::integer as ocean_code
	,o.label1::text as ocean_label
	,r.date::text as activity_date
	,a.number::integer as activity_number
	,a.longitude::numeric as activity_longitude
	,a.latitude::numeric as activity_latitude
	,a.setcount::integer as set_count
	,s.code::integer as school_type_code
	,s.label1::text as school_type_label
	,va.code::integer as activity_code
	,va.label1::text as activity_label
	,r.timeatsea::integer as time_at_sea
from
	ps_logbook.activity a
	join ps_logbook.route r on (a.route = r.topiaid)
	join ps_common.trip t on (t.topiaid = r.trip)
	join common.vessel v on (t.vessel = v.topiaid)
	join common.vesseltype vt on (v.vesseltype = vt.topiaid) 
	join common.country c on (v.fleetcountry = c.topiaid)
	join common.ocean o on (t.ocean = o.topiaid)
	left join ps_common.schooltype s on (s.topiaid = a.schooltype)
	join ps_common.vesselactivity va on (a.vesselactivity = va.topiaid)
where
	t.topiaid in (?trip_id)
;

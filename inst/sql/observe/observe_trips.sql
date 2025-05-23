select
	t.topiaid::text as trip_id
	,c.iso3code::text as flag_code
	,t.startdate::text as departure_date
	,t.enddate::text as trip_end_date
	,v.code::integer as vessel_code
	,vt.code::integer as vessel_type_code
	,vt.label1::text as vessel_type_label
	,acqs.code::integer as logbook_availability_code
	,acqs.label1::text as logbook_availability_label
	,w.code::integer as landing_well_content_code
	,w.label1::text as landing_well_content_label
	,o.code::integer as ocean_code
	,o.label1::text as ocean_label
from
	ps_common.trip t
	join common.vessel v on (t.vessel = v.topiaid)
	join common.vesseltype vt on (v.vesseltype = vt.topiaid)
	join common.country c on (v.flagcountry = c.topiaid)
	join common.ocean o on (t.ocean = o.topiaid)
	join ps_common.acquisitionstatus acqs on (t.logbookacquisitionstatus = acqs.topiaid)
	left join ps_logbook.wellcontentstatus w on (t.landingwellcontentstatus = w.topiaid)
where
	t.enddate between ?begin_time_period and ?end_time_period
	and c.iso3code in (?flag_codes)
	and o.code in (?ocean_codes)
	and vt.code in (?vessel_type_codes)
	and t.logbookprogram is not null
order by
	vessel_code,
	trip_end_date
;

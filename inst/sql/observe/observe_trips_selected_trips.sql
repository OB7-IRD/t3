---------------------------------------------------------------------------------------------------------------------------
-- Domain: purse seiner (PS)
-- Subdomain: logbooks
--
-- Title: Trips with full details for selected trips topiaid.
--
-- Description: Return all records in ps_logbook.trips with maximum of surrounding data, for selected trips.
--
-- WHERE clause filter parameters: trip_ids (character).
--
-- Supported data models: 9.3 - 9.5
--
-- Author: M.Depetris
-- Date: 2024-02-09
--
-- Updates:
-- 2025-01-21 - J.Clément - Change type of  filter parameters flag_code from integer to character (three letters FAO codes).
-- 2026-04-10 - J.Clément - Add observe_logbook_program_codes as filter parameters used in WHERE clause.
----------------------------------------------------------------------------------------------------------------------------

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
	,prog.code::text as logbook_program_code
	,prog.label1::text as logbook_program_label
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
	left join ps_common.program prog on (t.logbookprogram = prog.topiaid)
where
	t.topiaid in (?trip_ids)
order by
	vessel_code,
	trip_end_date
;

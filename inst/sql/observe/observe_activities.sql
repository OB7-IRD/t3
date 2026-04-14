---------------------------------------------------------------------------------------------------------------------------
-- Domain: purse seiner (PS)
-- Subdomain: logbooks
--
-- Title: activities with full details
--
-- Description: Return records in ps_logbook.activity, with maximum of surrounding data.
--
-- WHERE clause filter parameters: begin_time_period (date), end_time_period (date),
--                                 flag_codes (character), ocean_codes (integer),
--                                 vessel_type_codes (integer),
--                                 observe_logbook_program_codes (character).
--
-- Supported data models: 9.3 - 9.5
--
-- Author: M.Depetris
-- Date: 2024-02-09
--
-- Updates:
-- 2024-04-30 - M.Depetris - Update fleet argument to flag.
-- 2024-10-31 - J.Clément - Add objectoperation_code and objectoperation_label in outputs
-- 2024-11-08 - J.Clément - Add objectoperation_id in outputs. Then multiple object operations associated with the same activity are handled by combining the codes, labels and topiaid of the object operations, separated by commas to obtain a single row for each activity_id, in the function `object_model_data.R`.
-- 2025-01-21 - J.Clément - Change type of  filter parameters flag_code from integer to character (three letters FAO codes).
-- 2025-03-21 - J.Clément - Change type of object_operation_code from integer to text.
-- 2026-04-10 - J.Clément - Add observe_logbook_program_codes as filter parameters used in WHERE clause.
----------------------------------------------------------------------------------------------------------------------------

select
	t.topiaid::text as trip_id
	,a.topiaid:: text as activity_id
	,fob.topiaid::text as objectoperation_id
	,o.code::integer as ocean_code
	,o.label1::text as ocean_label
	,r.date::text as activity_date
	,a.number::integer as activity_number
	,a.longitude::numeric as activity_longitude
	,a.latitude::numeric as activity_latitude
	,a.setcount::integer as set_count
	,sss.code::integer as set_success_status_code
	,sss.label1::text as set_success_status_label
	,s.code::integer as school_type_code
	,s.label1::text as school_type_label
	,va.code::integer as activity_code
	,va.label1::text as activity_label
	,obo.code::text as objectoperation_code
	,obo.label1::text as objectoperation_label
	,r.timeatsea::integer as time_at_sea
from
	ps_logbook.activity a
	join ps_logbook.route r on (a.route = r.topiaid)
	join ps_common.trip t on (t.topiaid = r.trip)
	join common.vessel v on (t.vessel = v.topiaid)
	join common.vesseltype vt on (v.vesseltype = vt.topiaid)
	join common.country c on (v.flagcountry = c.topiaid)
	join common.ocean o on (t.ocean = o.topiaid)
	left join ps_common.schooltype s on (s.topiaid = a.schooltype)
	join ps_common.vesselactivity va on (a.vesselactivity = va.topiaid)
	left join ps_logbook.floatingobject fob on (fob.activity=a.topiaid)
	left join ps_common.objectoperation obo on (obo.topiaid = fob.objectoperation)
	left join ps_logbook.setsuccessstatus sss on (a.setsuccessstatus = sss.topiaid)
	left join ps_common.program prog on (t.logbookprogram = prog.topiaid)
where
	t.enddate between ?begin_time_period and ?end_time_period
	and c.iso3code in (?flag_codes)
	and o.code in (?ocean_codes)
	and vt.code in (?vessel_type_codes)
	and prog.code in (?observe_logbookprogram_codes)
;

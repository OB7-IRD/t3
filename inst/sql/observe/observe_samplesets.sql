---------------------------------------------------------------------------------------------------------------------------
-- Domain: purse seiner (PS)
-- Subdomain: logbooks
--
-- Title: samplesets with full details
--
-- Description: Return records in ps_logbook.sampleactivity, with maximum of surrounding data.
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
-- 2025-01-21 - J.Clément - Change type of  filter parameters flag_code from integer to character (three letters FAO codes).
-- 2026-04-10 - J.Clément - Add observe_logbook_program_codes as filter parameters used in WHERE clause.
-- 2026-05-04 - J.Clément - Add well_label in outputs.
----------------------------------------------------------------------------------------------------------------------------
select
	t.topiaid::text as trip_id
	,s.well::text as well_label
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
	join common.country c on (v.flagcountry = c.topiaid)
	join common.ocean o on (t.ocean = o.topiaid)
	join ps_logbook.sample s on (sa.sample = s.topiaid)
	left join ps_logbook.well w on (t.topiaid = w.trip and s.well = w.well)
	left join ps_common.program prog on (t.logbookprogram = prog.topiaid)
where
	t.enddate between ?begin_time_period and ?end_time_period
	and c.iso3code in (?flag_codes)
	and o.code in (?ocean_codes)
	and vt.code in (?vessel_type_codes)
	and prog.code in (?observe_logbookprogram_codes)
;

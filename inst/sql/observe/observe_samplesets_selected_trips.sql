---------------------------------------------------------------------------------------------------------------------------
-- Domain: purse seiner (PS)
-- Subdomain: logbooks
--
-- Title: samplesets for selected trips with full details
--
-- Description: Return all records in ps_logbook.sampleactivity, for selected trips, with maximum of surrounding data.
--
-- WHERE clause filter parameters: trip_ids (character).
--
-- Supported data models: 9.3 - 9.5
--
-- Author: M.Depetris
-- Date: 2024-02-09
--
-- Updates:
-- 2024-04-30 - M.Depetris - Update fleet argument to flag.
-- 2026-05-04 - J.Clément - Add well_label in outputs.
----------------------------------------------------------------------------------------------------------------------------
select
	t.topiaid::text as trip_id
	,vt.code::integer as vessel_type_code
	,a.topiaid::text as activity_id
	,w.topiaid::text as well_id
	,s.well::text as well_label
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
where
	t.topiaid in (?trip_ids)
;

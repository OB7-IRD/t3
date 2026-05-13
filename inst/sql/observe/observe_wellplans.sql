-- Domain: purse seiner (PS)
-- Subdomain: logbooks
--
-- Title: wellplans with full details
--
-- Description: Return records in ps_logbook.wellactivityspecies with maximum of surrounding data.
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
-- 2025-01-21 - J.Clément - Change type of  filter parameters flag_code from integer to character (three letters FAO codes).
-- 2025-10-23 - J.Clément - Add school type code in outputs.
-- 2026-04-10 - J.Clément - Add observe_logbook_program_codes as filter parameters used in WHERE clause.
-- 2026-05-04 - J.Clément - Add well_label in outputs.
----------------------------------------------------------------------------------------------------------------------------
select
	was.topiaid::text as wellplan_id
	,w.topiaid:: text as well_id
	,w.well::text as well_label
	,t.topiaid::text as trip_id
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
  left join ps_common.schooltype sc on (sc.topiaid = a.schooltype)
	join ps_common.trip t on (r.trip = t.topiaid)
	join common.vessel v on (t.vessel = v.topiaid)
	join common.vesseltype vt on (v.vesseltype = vt.topiaid)
	join common.country c on (v.flagcountry = c.topiaid)
	join common.ocean o on (t.ocean = o.topiaid)
	join common.species sp on (was.species = sp.topiaid)
	join ps_common.weightcategory wc on (was.weightcategory = wc.topiaid)
	left join ps_logbook.sample s on (w.well = s.well AND s.trip = t.topiaid)
	left join ps_common.program prog on (t.logbookprogram = prog.topiaid)
where
	t.enddate between ?begin_time_period and ?end_time_period
	and c.iso3code in (?flag_codes)
	and o.code in (?ocean_codes)
	and vt.code in (?vessel_type_codes)
	and prog.code in (?observe_logbookprogram_codes)
;

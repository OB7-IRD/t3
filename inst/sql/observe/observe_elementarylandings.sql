---------------------------------------------------------------------------------------------------------------------------
-- Domain: purse seiner (PS)
-- Subdomain: landing
--
-- Title: landings with full details
--
-- Description: Return records in ps_landing.landing, with maximum of surrounding data.
--
-- WHERE clause filter parameters: begin_time_period (date), end_time_period (date),
--                                 flag_codes (character), ocean_codes (integer),
--                                 vessel_type_codes (integer),
--                                 observe_logbook_program_codes (character),
--
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
----------------------------------------------------------------------------------------------------------------------------
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
	join common.country co on (v.flagcountry = co.topiaid)
	join common.ocean o on (t.ocean = o.topiaid)
	left join ps_common.program prog on (t.logbookprogram = prog.topiaid)
where
	t.enddate between ?begin_time_period and ?end_time_period
	and co.iso3code in (?flag_codes)
	and o.code in (?ocean_codes)
	and vt.code in (?vessel_type_codes)
	and prog.code in (?observe_logbookprogram_codes)
;

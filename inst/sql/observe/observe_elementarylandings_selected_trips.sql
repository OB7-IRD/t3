---------------------------------------------------------------------------------------------------------------------------
-- Domain: purse seiner (PS)
-- Subdomain: logbooks
--
-- Title: landings for selected trips with full details
--
-- Description: Return all records in  ps_landing.landing, for selected trips, with maximum of surrounding data.
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
where
	t.topiaid in (?trip_ids)
;

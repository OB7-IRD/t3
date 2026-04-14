---------------------------------------------------------------------------------------------------------------------------
-- Domain: purse seiner (PS)
-- Subdomain: logbooks
--
-- Title: samples for selected trips with full details
--
-- Description: Return records in ps_logbook.sample, for selected trips, with maximum of surrounding data.
--
-- WHERE clause filter parameters: trip_ids (character),
--                                 sp.faocode in ('YFT', 'SKJ', 'BET', 'ALB', 'MIX','TUN', 'LOT', 'BLT', 'FRI', 'FRZ', 'LTA', 'KAW')
--
-- Supported data models: 9.3 - 9.5
--
-- Author: M.Depetris
-- Date: 2024-02-09
--
-- Updates:
-- 2024-04-30 - M.Depetris - Update fleet argument to flag.
-- 2025-04-14 - J.Clément - Fix typo (weigth insteaf of weight).
-- 2026-01-29 - J.Clément - Add condition sp.faocode in ('YFT', 'SKJ', 'BET', 'ALB', 'MIX','TUN', 'LOT', 'BLT', 'FRI', 'FRZ', 'LTA', 'KAW') in sample's query.
----------------------------------------------------------------------------------------------------------------------------
select
	t.topiaid::text as trip_id
	,w.topiaid::text as well_id
	,s.smallsweight::numeric as well_minus10_weight
	,s.bigsweight::numeric as well_plus10_weight
	,s.totalweight::numeric as well_global_weight
	,s.topiaid::text as sample_id
	,ss.subsamplenumber::integer as sub_sample_id
	,ss.topiaid::text as sub_sample_total_count_id
	,ssm.topiaid::text as elementarysampleraw_id
	,sq.code::integer as sample_quality_code
	,sq.label1::text as sample_quality_label
	,st.code::integer as sample_type_code
	,st.label1::text as sample_type_label
	,sp.code::integer as species_code
	,sp.faocode::text as species_fao_code
	,smt.code::text as size_measure_type_code
	,smt.label1::text as size_measure_type_label
	,ss.totalcount::integer as sample_total_count
	,ssm.count::integer as sample_number_measured
	,ssm.sizeclass::numeric as sample_length_class
from
	ps_logbook.sample s
	join ps_common.trip t on (s.trip = t.topiaid)
	join common.vessel v on (t.vessel = v.topiaid)
	join common.vesseltype vt on (v.vesseltype = vt.topiaid)
	join common.country c on (v.flagcountry = c.topiaid)
	join common.ocean o on (t.ocean = o.topiaid)
	join ps_logbook.samplespecies ss on (s.topiaid = ss.sample)
	join ps_logbook.samplespeciesmeasure ssm on (ss.topiaid = ssm.samplespecies)
	join ps_logbook.samplequality sq on (s.samplequality = sq.topiaid)
	join ps_common.sampletype st on (s.sampletype = st.topiaid)
	join common.species sp on (ss.species = sp.topiaid)
	join common.sizemeasuretype smt on (ss.sizemeasuretype = smt.topiaid)
	left join ps_logbook.well w on (t.topiaid = w.trip and s.well = w.well)
where
	t.topiaid in (?trip_ids)
	and sp.faocode in ('YFT', 'SKJ', 'BET', 'ALB', 'MIX','TUN', 'LOT', 'BLT', 'FRI', 'FRZ', 'LTA', 'KAW')
order by
	trip_id
	,sample_id
	,well_id
	,sub_sample_id
	,species_code
	,size_measure_type_code
	,sample_length_class
;

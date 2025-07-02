select
	o.code::integer as ocean_code
	,o.label1::text as ocean_label
	,s.code::integer as species_code
	,s.faocode::text as species_fao_code
	,l.lengthweightformula::text as length_weight_formula
	,split_part(split_part(l.coefficients, ':', 1), '=', 2)::numeric AS lwr_a
	,split_part(split_part(l.coefficients, ':', 2), '=', 2)::numeric AS lwr_b
  ,l.startdate::text as formula_startdate
	,l.enddate::text as formula_enddate
	,l.source::text as formula_source
	,l.topiaid::text as lwr_id

from
	common.lengthweightparameter l
	join common.species s on (l.species = s.topiaid)
	join common.ocean o on (l.ocean = o.topiaid)
	where
	l.startdate < ?begin_time_period
	and coalesce(l.enddate, ?end_time_period) >= ?end_time_period
;

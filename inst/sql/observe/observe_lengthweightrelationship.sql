select
	o.code::integer as ocean_code
	,o.label1::text as ocean_label
	,s.code::integer as species_code
	,s.faocode::text as species_fao_code
	,l.lengthweightformula::text as length_weight_formula
	,split_part(split_part(l.coefficients, ':', 1), '=', 2)::numeric AS lwr_a
	,split_part(split_part(l.coefficients, ':', 2), '=', 2)::numeric AS lwr_b
from 
	common.lengthweightparameter l
	join common.species s on (l.species = s.topiaid)
	join common.ocean o on (l.ocean = o.topiaid)
;

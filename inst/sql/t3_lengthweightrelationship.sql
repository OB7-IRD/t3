SELECT
	o.code::integer AS ocean
	,s.code::integer AS specie_code
	,s.code3l::text AS specie_code3l
	,split_part(split_part(lwc.coefficients, ':', 1), '=', 2)::numeric AS lwr_a
	,split_part(split_part(lwc.coefficients, ':', 2), '=', 2)::numeric AS lwr_b
FROM
	public.lengthweightconversion lwc
	JOIN public.species s ON (lwc.species = s.topiaid)
	JOIN public.ocean o ON (lwc.ocean = o.topiaid)
ORDER by
	o.code
	,specie_code
;

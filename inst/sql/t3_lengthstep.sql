SELECT
	o.code::integer AS ocean
	,s.code3l::text AS specie_code3l
	,sls.ld1class::integer * power(10, -1) AS ld1_class
	,sls.lfclass::integer AS lf_class
	,sls.ratio::integer AS ratio
FROM
	public.specieslengthstep sls
	JOIN public.ocean o ON (sls.ocean = o.topiaid)
	JOIN public.species s ON (sls.species = s.topiaid)
ORDER BY
	ocean
	,specie_code3l
	,ld1_class
	,lf_class
;

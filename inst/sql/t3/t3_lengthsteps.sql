SELECT
	o.code::integer AS ocean
	,s.code::integer AS specie_code
	,s.code3l::text AS specie_code3l
	--careful, ld1 in mm and lf in cm
	,sls.ld1class::numeric * power(10, -1) AS ld1_class
	,sls.lfclass::integer AS lf_class
	,sls.ratio::numeric AS ratio
FROM
	public.specieslengthstep sls
	JOIN public.ocean o ON (sls.ocean = o.topiaid)
	JOIN public.species s ON (sls.species = s.topiaid)
ORDER BY
	ocean
	,specie_code
	,specie_code3l
	,ld1_class
	,lf_class
;

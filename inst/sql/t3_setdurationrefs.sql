SELECT
	sd.year::integer AS year
	,c.codeiso3::text AS country
	,o.code::integer AS ocean
	,st.code::integer AS school_type
	,sd.parametera::numeric AS parameter_a
	,sd.parameterb::numeric AS parameter_b
	,sd.nullsetvalue::numeric AS null_set_value
FROM 
	public.setduration sd
	JOIN public.country c ON (sd.country = c.topiaid)
	JOIN public.ocean o ON (sd.ocean = o.topiaid)
	JOIN public.schooltype st ON (sd.schooltype = st.topiaid)
WHERE
	sd.year IN (?period)
	AND c.codeiso3 IN (?countries)
;

SELECT
	l.C_OCEAN  AS ocean
	,l.C_ESP AS specie_code
	,e.C_ESP_3L AS specie_code3l
	,l.LD1 AS ld1_class
	,l.LF AS lf_class
	,l.COEF_LF * 100 AS ratio
FROM 
	(LD1_LF l
	INNER JOIN ESPECE e ON l.C_ESP = e.C_ESP)
ORDER BY
	l.C_OCEAN,
	l.C_ESP,
	e.C_ESP_3L,
	l.LD1,
	l.LF
;

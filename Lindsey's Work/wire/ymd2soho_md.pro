;soho mission day
function ymd2soho_md, yymmdd
	y=long(yymmdd)/10000L
	m=(yymmdd-y*10000L)/100L
	d=yymmdd-y*10000L-m*100L
	if y gt 50 then y=y+1900L else y=y+2000L
	return, julday(m,d,y)-julday(12,1,1995)
end
;**************************************************************

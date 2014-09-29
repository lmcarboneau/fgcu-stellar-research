;read fits header according to keyword as string
pro read_header_str, header, keyword, var
ind=where(strmid(header,0,8) eq keyword,cnt)
if cnt ne 1 then begin
	print, 'READ_HEADER: no such keyword: ',keyword
	var=!values.F_NaN
	return
endif
comment_pos=strpos(header(ind(0)),'/',9)
if comment_pos gt 0 then $
	var_string=strmid(header(ind(0)),9,comment_pos-10L) $
else var_string=strmid(header(ind(0)),9,24)
var_string=strtrim(var_string,2)
var=float(strmid(var_string,1,strlen(var_string)-2))
return
end
;**************************************************************

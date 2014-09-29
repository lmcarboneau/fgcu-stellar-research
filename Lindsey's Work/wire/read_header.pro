;read fits header according to keyword as value
pro read_header, header, keyword, var
ind=where(strmid(header,0,8) eq keyword,cnt)
if cnt ne 1 then begin
	print, 'READ_HEADER: no such keyword: ',keyword
	var=''
	return
endif
comment_pos=strpos(header(ind(0)),'/',9)
if comment_pos gt 0 then $
	var_string=strmid(header(ind(0)),9,comment_pos-10L) $
else var_string=strmid(header(ind(0)),9,24)
reads,var_string,var
return
end
;**************************************************************

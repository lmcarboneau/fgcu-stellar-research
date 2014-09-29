pro read_level1P,filename,day,value,flag
;Procedure to read daily PMO6V FITS-Files
;returns 1440 values of TSI and flag
  d_temp=readfits(filename,header,exten=1,/silent)
  read_header,header,'NAXIS2  ',tempd
  if strmid(filename,1,2,/revers) eq '00' then $
  	read_header,header,'TNULL1  ',notval else $
  	read_header_str,header,'TNULL1  ',notval
  n=long(tempd)
  date=long(strmid(filename,11,6,/revers))
  day=dindgen(n)/tempd+double(ymd2soho_md(date))+22.d0/86400.d0
  value=double(ftget(header,d_temp,'PMO6_VA '))
  flag=ftget(header,d_temp,'DQ_PMO6_VA')
  inval=where(value le notval,cnt)
;  if cnt gt 0 then value(inval)=!values.D_NaN
; For us better to set it to zero
  if cnt gt 0 then value(inval)=0.0D0
end
;**************************************************************

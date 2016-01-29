PRO wireq,st,full=full

m4_get_basedir, base

default9,full,0
default9,cdir,base + 'wire/collection/'

restore,base + 'wire/wire_essential/xy_positions4.idl'

w = where(strmatch(wireinfo.object,'*' + st + '*',/fold),c)

print,''

if c ge 1 then begin
 mat = strarr(c)
 for k=0,c-1 do begin
  x = findfile(cdir + '*' + wireinfo(w(k)).object + '*',count=cnt)
  if cnt eq 1 then begin
    y = strsplit(x,'/',/extract) & ny = n_elements(y)
    mat(k) = y(ny-1)
  endif
 endfor
endif

if full eq 0 then $
for i=0,c-1 do print,$
 string(w(i),format='(I2)') + ': ' + $
 wireinfo(w(i)).object + '   wireg,"'+mat(i)+'"' else $
for i=0,c-1 do print,$
 string(w(i),format='(I2)') + ': ',$
 wireinfo(w(i)),  '   wireg,"'+mat(i)+'"' 

if (c eq 0) or (w(0) eq -1) then print,' %%% No matches for string: *' + st + '*' else $
 print,''

END

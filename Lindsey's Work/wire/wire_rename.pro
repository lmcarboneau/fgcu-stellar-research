; Rename files ...

spawnrob,'ls -1 /home/bruntt/wire/data/feb05/r*',l
spawnrob,'ls -1 /home/bruntt/wire/data/feb05/sigmagem/r*',l

n = n_elements(l)

for i=0,n-1 do begin

 fil = l(i)

 if strmatch(fil,'*gz*') eq 1 then begin
  spawnrob,'gzip -d ' + fil
  g = strsplit(fil,'.',/extract)
  newfil = g(0)
  if n_elements(g) eq 3 then newfil = newfil + '.' + g(1)
 endif else begin
  newfil = fil
 endelse

 spawnrob,'head -50 ' + newfil, h

 gg = n_elements(h)

 w = where(strmatch(h,'*:*') eq 1,c)

 if gg le 5 then begin
  print,' %%% Corrupt file? ---> ' + newfil
  goto, skip_fil
 endif
 
 p = h(w(5))
 s = strsplit(p,'-',/extract)

 year = s(0)
 day = s(1)

 t = strsplit(s(2),':',/extract)
 
 time = long(t(0))
 timen = strcompress(time,/remove_all)
 if time le 9 then timen = '0' + strcompress(time,/remove_all)

 minut = long(t(1))
 minutn = strcompress(minut,/remove_all)
 if minut le 9 then minutn = '0' + strcompress(minut,/remove_all)

 dayno = long(day)
 if dayno ge 100 then $
  dayname = strcompress(day,/remove_all)
 if dayno ge 10 and dayno le 99 then $
   dayname = '0' + strcompress(day,/remove_all)
 if dayno le 9 then $
   dayname = '00' + strcompress(day,/remove_all)



 filename = 'wire_200' + strcompress(fix(year),/remove_all) + $
      '_d' + dayname + $
      '_t' + timen + '_' + minutn + '.dat'

 command = 'mv ' + newfil + ' ' + filename
 spawnrob,command
 spawnrob,'gzip ' + filename

 print,command

skip_fil:

endfor

END

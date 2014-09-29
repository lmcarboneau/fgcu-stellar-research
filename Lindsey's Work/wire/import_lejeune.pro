PRO import_lejeune, file, l

; import_lejeune, '/home/bruntt/evol/lejeune/modc008.dat', l

openr,1,file

cnt = 0L
k = 0L

n = 51
l = replicate( {teff: fltarr(n), l:fltarr(n), m:-1.}, 60)

dum = ''
for j=0,1 do readf,1,dum

while not eof(1) do begin
 readf,1,dum

 if strcompress(dum,/remove_all) eq '' then begin 
   cnt = cnt + 1 
   k = 0 
 endif else begin

s = strsplit(dum,' ',/extract)
ns = n_elements(s)
if ns ge 10 then begin
 l(cnt).teff(k) = float(s(4)) ; log teff
 l(cnt).l(k)    = float(s(3)) ; log lumn
 k=k+1
endif
if (ns eq 1) and (float(s(0)) gt .1)  then begin
 l(cnt).m = float(s(0))
 readf,1,dum
endif

endelse

endwhile

close,1
; ================================================

; Purge unused
l = l(0:cnt-1)

;w = where(l.m gt .01,c)
;if c ge 1 then l = l(w)


END

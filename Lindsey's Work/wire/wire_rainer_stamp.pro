PRO wire_rainer_stamp, lcfile, lcfile2, stampfile 

; Example:
; lcfile  = '/home/bruntt/wire/procyon/RAINER/ProcyonRERED.dat'
; lcfile2 = '/home/bruntt/wire/procyon/RAINER/ProcyonREREDs.dat'
; stampfile = 'RAINER_wire__001.idl'  

restore,stampfile
readcol,lcfile,hjd, adu, format='D,D'

n = n_elements(hjd)
stamp = strarr(n)

tlim = double(1e-6)

for j=0L,n-1 do begin
 for k=0,4 do begin
 w = where(abs(wire.hjd(k)- hjd(j)) lt tlim, c)
 if c eq 1 then stamp(j) = wire(w).stamp(0)
 endfor
endfor

w = where(stamp eq '',c)
if c ge 1 then begin
 print,' %%% Some stamps not available: ',c
 hitme,s9
endif

openw,1,lcfile2
for j=0L,n-1 do begin
 x = strsplit(stamp(j),'_',/extract)
 printf,1,hjd(j), x(0), x(1), adu(j), format='(D16.7, A10, A15, I9)'
endfor
close,1

print,' %%% Wrote file: '+lcfile2


END

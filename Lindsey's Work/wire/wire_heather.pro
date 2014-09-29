; Import heather's results for giant stars
; June 18th 2005

file = '/home/bruntt/wire/giants/giants2.dat'

ns = 25
nmax = 10

openr,1,file
temp = ''
h = replicate( {f:fltarr(nmax), n:0B, hd:0L,$
                a:fltarr(nmax), snr:fltarr(nmax)}, ns)

cnt = 0
i   = 0B
init = 0B

while not eof(1) do begin

 readf,1,temp

 if strmatch(temp,'*%*') then goto,reread

 if strcompress(temp,/remove_all) eq '' then readf,1,temp
 if (init ne 0) and strmatch(temp,'HD*') then begin
   h(cnt).n = i 
   i = 0B
   cnt = cnt + 1
   y = strsplit(temp,' ',/extract)
   h(cnt).hd = long(y(1))
   goto,reread
 endif

 if (init eq 0) and strmatch(temp,'HD*') then begin
   y = strsplit(temp,' ',/extract)
   h(cnt).hd = long(y(1))
   readf,1,temp
   init = 1B
 endif
 
print,temp

 x = strsplit(temp,' ',/extract)
 if (n_elements(x) ge 3) and (strmatch(temp,'*F*')) then begin
  h(cnt).f(i)   = float(x(1))
  h(cnt).a(i)   = float(x(2))
  h(cnt).snr(i) = float(x(3))
  i = i + 1
 endif

 reread:
endwhile

close,1
h(cnt).n = i 

h = h(0:cnt)
print,' %%% Imported frequencies for ' + string(cnt,format='(I3)') + ' stars'

END

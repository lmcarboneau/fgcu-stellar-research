PRO wire_decor_insert,decorfile,wirefile,star=star,debug=debug

default9, star, 0
default9, debug, 0B

; 1. Import DECOR file from wire_decor.pro
; 2. Import original WIRE structure
; 3. Replace DATA ID numbers with new decor'ed data
; 4. Save new WIRE structure

; 1. Import DECOR file from wire_decor.pro
readcol,decorfile,$
 time,decor,phase,correction,dataid,$
 format='d,d,f,d,f'

n = n_elements(time)

; 2. Import original WIRE structure
 restore,wirefile

; 3. Replace DATA ID numbers with new decor'ed data

orgdata = wireult.mag(star)
norg = n_elements(wireult)
usedata = bytarr(norg)

if debug then begin
  rr = robust_sigma(orgdata)
  mm = median(orgdata)
  plot,orgdata,psym=3,yr=[-1,1]*0.05 + mm,xr=[0,1000]
endif

for i=0L, n-1 do begin
 wireult(dataid(i)).mag(star) = orgdata(dataid(i)) - correction(i)
 usedata(dataid(i)) = 1B
endfor

print,' %%% Data that was not decorrelated is set to bad magnitude!'
wbad = where(usedata eq 0, cbad) ; this data was not corrected ---> set to bad value
if cbad ge 1 then $
 wireult(wbad).mag(star) = -9.99

if debug then begin
  col=getcolor(/load)
  oplot,wireult.mag(star)+0.01,psym=3,col=col.sky
endif

; 4. Save new WIRE structure
outfile = wirefile + '.decor'
 save,filename=outfile,wireult,hjd_converted

print,' %%% Saved file: ' + outfile


END

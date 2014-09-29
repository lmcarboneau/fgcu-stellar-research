; Read Retter's data:

t0 = 51480D

if n_elements(d2) eq 0 then begin
readcol,'/usr/users/bruntt/wire/retter_altair_series02.dat',$
 skipline=23,t2,d2,w2,format='D,D,F'
 t2 = t2 - 10.298 + t0     ; approx offset in time!
 d2 = d2 - median(d2) 
endif

np = n_elements(d2)

; From wire_merge3.pro:

n_max = np
nstar = 2

   wirer = replicate({hjd:0D, $
                      mag:fltarr(nstar),$                  
                     fwhm:fltarr(nstar),$
                       gc:fltarr(2,nstar), $
                        x:intarr(nstar), $
                        y:intarr(nstar)}, n_max)

wirer.hjd = t2
wirer.mag(0) = 11.918 -1D * d2 ; same delta-mag convention as ... Bruntt!

restore,'/ai39/bruntt/wire/altair/altair_merged_allslots_31_decor1.idl' ; 3.10.03

nn = n_elements(wire3)

   wirer2 = replicate({hjd:0D, $
                      mag:fltarr(nstar),$                  
                     fwhm:fltarr(nstar),$
                       gc:fltarr(2,nstar), $
                        x:intarr(nstar), $
                        y:intarr(nstar)}, nn)

wirer2.hjd = wire3.hjd
wirer2.mag(0) = interpol(wirer.mag(0),wirer.hjd,wire3.hjd)

plot,wirer.hjd-51480D,wirer.mag(0),xr=[-.05,.0],psym=1,symsi=.5
oplot,wirer2.hjd-51480D,wirer2.mag(0),col=col.red,psym=1,symsi=.5

plot,wire3.hjd-51480D,wire3.mag(0) - wirer2.mag(0),psym=3,yr=[-1,1]*0.005,xr=[-1,1]*.2-5

wire4 = wire3
wire4.mag(1) = wirer2.mag(0)

print,' wirep,wire4 '

end

; Have a look at the photometry for different aperture sizes

if n_elements(wire2) eq 0 then $
 restore,'/ai39/bruntt/wire/altair/altair_wire_003.idl1'
if n_elements(col) eq 0 then $
 col = getcolor(/load)

;plot,wire2.hjd(0)-51470.,alog10(wire2.flux1(0))+25.,psym=3,$
; xr=[4.9,5.6],yr=[30.1,30.25],xsty=3,ysty=3,$
; /nodata


; oplot,wire2.hjd(0)-51470.,alog10(wire2.p(0,3))+25.0,psym=3,col=col.yellow
; oplot,wire2.hjd(0)-51470.,alog10(wire2.p(0,4))+25.0,psym=3,col=col.red

colx = [col.red,col.aqua,col.yellow,col.green,col.navy,col.charcoal,$
        col.beige,col.gray,col.magenta,col.sky,col.orchid]

;if n_elements(a) eq 0 then begin ; compute offset?
 a = fltarr(9) ; offset array

 for i=0,8 do begin
  m = alog10(wire2.p(0,i))+25.0
  w = where(m gt 25. and $
            wire2.hjd(0) gt 51473.0 and wire2.hjd(0) lt 51476.0,c)
  a(i) = median(wire2(w).p(0,i))
 endfor

 a = alog10(a)+25.0
 ss = a & ss(*) = -999.99
 
 for i=0,8 do begin
  m = alog10(wire2.p(0,i))+25.0
  w = where(m gt 25. and $
            wire2.hjd(0) gt 51473.0 and wire2.hjd(0) lt 51476.0,c)
  ptp_robust_fin,m(w)-a(i),noise,1
  ss(i) = noise ; robust_sigma(m(w))
 endfor


; endif

plot,wire2.hjd(0)-51470.,alog10(wire2.flux1(0))+25.,psym=3,$
 xr=[3.5,5.6],yr=[-.04,.04],xsty=3,ysty=3,$
 /nodata

print,' %%% Tast en tast!'

for i=0,8 do begin
 oplot,wire2.hjd(0)-51470.,alog10(wire2.p(0,i))+25.0-a(i),psym=3,col=colx(i)
 s = get_kbrd(1)
endfor 

for i=0,8 do $
 xyouts,4.9,0.0 + 0.003 * i, $
  strcompress(string(i),/remove_all)+' '+$
  strcompress(string(ss(i),format='(F6.4)'),/remove_all), $
  col=colx(i),charsi=1.3

print,' %%% Point-to-Point Noise for each aperture:'
for i=0,8 do $
 print,i,ss(i),format='(I2,F9.5)'

end

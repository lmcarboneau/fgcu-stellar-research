; Find median x,y position of Altair + median FWHM of profile:
; Find limits on x,y, and FWHM! Use the median x,y,FWHM in new reduction!

nstar = 5
xx9 = fltarr(2,nstar)
yy9 = fltarr(2,nstar)
fw9 = fltarr(2,nstar)

if n_elements(wire3) eq 0 then $
 restore,'/ai39/bruntt/wire/altair/altair_merged_allslots_31_decor1.idl' ; 3.10.03

for ss=0,4 do begin ; for each star

plot,wire3.gc(0,ss),wire3.gc(1,ss),$
 psym=3,xr=[2,5],yr=[2,5],xtit='x pos',ytit='y pos'

;wx = where(wire3.gc(1,ss) lt 3.2 and wire3.gc(1,ss) gt 3.1 and $
;           wire3.fwhm(ss) gt 1.84 and wire3.fwhm(ss) lt 1.88,cx)
wx = where(wire3.gc(1,ss) lt 4.0 and wire3.gc(1,ss) gt 3.0 and $
           wire3.fwhm(ss) gt 1.0 and wire3.fwhm(ss) lt 3.0,cx)

; Altair:
xx = median(wire3(wx).gc(0,ss))
yy = median(wire3(wx).gc(1,ss))
fw = median(wire3(wx).fwhm(ss))


plots,xx,yy,psym=1,col=col.yellow,symsi=3
print,' %%% Hit any key ... '  &  s = get_kbrd(1)

plot,wire3.gc(1,ss),wire3.fwhm(ss),$
 psym=3,xr=[3,4],yr=[1,2.5],xtit='y pos',ytit='fwhm'
plots,yy,fw,psym=1,col=col.yellow,symsi=3
print,' %%% Hit any key ... '  &  s = get_kbrd(1)


rr_x  = robust_sigma(wire3(wx).gc(0,ss))
rr_y  = robust_sigma(wire3(wx).gc(1,ss))
rr_fw = robust_sigma(wire3(wx).fwhm(ss))

xx9(0,ss) = xx   & yy9(0,ss) = yy   & fw9(0,ss) = fw
xx9(1,ss) = rr_x & yy9(1,ss) = rr_y & fw9(1,ss) = rr_fw

; debug: rr_y = rr_y * 3.0

plot,wire3.gc(0,ss),wire3.gc(1,ss),psym=3,symsi=.1,yr=[2.0,4],ysty=3,xr=[2.0,4], $
 xtit='x position',ytit='y position'
plots,xx,yy,psym=1,col=col.red,symsi=3,thick=3
tvellipse,4.*rr_x,4.*rr_y,xx,yy,/data,col=col.red

; dist = ((wire3.gc(0,ss)-xx) / rr_x)^2.0 + ((wire3.gc(1,ss)-yy) / rr_y)^2.0
dist = ((wire3.gc(0,ss)-xx) / rr_x)^2.0 + ((wire3.gc(1,ss)-yy) / rr_y)^2.0 + $
       ((wire3.fwhm(ss)-fw) / rr_fw)^2.0

dist = sqrt(dist)

wg = where(dist lt 4.0, cg)
oplot,wire3(wg).gc(0,ss),wire3(wg).gc(1,ss),psym=3,col=col.red

plots,xx,yy,psym=1,col=col.yellow,symsi=3,thick=3

print,xx,yy,fw,format='(3F9.5)'
print,rr_x,rr_y,rr_fw,format='(3F9.5)'

w = where(wire3.mag(ss) gt 3.,c)
print,'Number of points: ',c, cg / float(c)

print,' %%% Hit any key ... '  &  s = get_kbrd(1)

plot,wire3.gc(1,ss),wire3.fwhm(ss),psym=3,symsi=.1,yr=[1,3],ysty=3,xr=[2.0,4], $
 xtit='y position',ytit='fwhm'
oplot,wire3(wg).gc(1,ss),wire3(wg).fwhm(ss),psym=3,col=col.red
plots,yy,fw,psym=1,col=col.yellow,symsi=3,thick=3
tvellipse,4.*rr_y,4.*rr_fw,yy,fw,/data,col=col.red

print,' %%% Hit any key ... '  &  s = get_kbrd(1)

mmag = median(wire3(wg).mag(ss)) & rrmag = robust_sigma(wire3(wg).mag(ss))
plot,wire3.hjd-51480D,wire3.mag(ss)-mmag,yr=[-1,1]*rrmag*5.0,psym=3,xr=[0,.4] +6.5
oplot,wire3(wg).hjd-51480D,wire3(wg).mag(ss)-mmag+0.001,psym=3,col=col.red

print,' %%% Hit any key ... '  &  s = get_kbrd(1)

endfor

; ,xr=[-5,0]

END

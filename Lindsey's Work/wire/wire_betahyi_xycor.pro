; wire_betahyi_xycor
; 12th of January 2006

; I removed 566 bad points :

 file = "/mnt/sdb6/wire/rawdata/nov2005/rfd/BetaHyi/BetaHyi_wire31.idl"
 restore,file  &  target = "BetaHyi"
 wireult_org = wireult

;;  wirep,wireult,file,target,1 ; ,$
dx = wireult.x(0)-round(wireult.x(0))
dy = wireult.y(0)-round(wireult.y(0))

x0 = 0.28546662  & y0 =     0.35355268
plot,wireult.x(0)-round(wireult.x(0)),wireult.y(0)-round(wireult.y(0)),psym=3,xr=[.2,.5],yr=[.2,.5]
w = where(abs(dx-x0) lt .06 and abs(dy-y0) lt .027)
oplot,dx(w),dy(w),psym=3,col=col.sky

wireult = wireult_org(w)

xx = wireult.x(0) - 263.0
yy = wireult.y(0) - 261.0
mm = wireult.mag(0)

med = median(mm)
dmag = 0.01 / 25. & dmag2 = dmag / 10. ; edge effects

mag_min = 13.728
mag_max = 13.80
x_min = .24
x_max = .37
y_min = .32
y_max = .358

plot, mm, xx, psym=3,yr=[.2,.4],xr=[mag_min-.01,mag_max+.01]
oplot,mm,yy,psym=3, col=col.sky ; ,xr=[mag_min-.01,mag_max+.01],yr=[.3,.4]

plots,!x.crange,y_max,col=col.red
plots,!x.crange,y_min,col=col.red
plots,!x.crange,x_max,col=col.yellow
plots,!x.crange,x_min,col=col.yellow

mag = mag_min
cor = dblarr(4,5000)
cnt = 0

while mag le mag_max do begin
 wm = where(abs(mm-mag) lt (dmag+dmag2) and $
            mm gt mag_min and mm lt mag_max and $
            xx gt x_min and xx lt x_max,cm)
 if cm ge 10 then begin
  resistant_mean, mm(wm), 3, me, sd, nr 
  resistant_mean, xx(wm), 3, mex, sdx, nrx 
  cor(0,cnt) = mag ; correction value 
  cor(1,cnt) = mex ; mean x-value
  cor(2,cnt) = me ; mean value = correction
  cor(3,cnt) = sd ; error on mean
  oplot,mm(wm),xx(wm),col=col.red,psym=3
  cnt = cnt + 1
 endif

 mag = mag + dmag
endwhile
; =========================================================

; =========================================================
cor = cor(*,0:cnt-1)
plot, mm, xx, psym=3,yr=[.2,.4],xr=[mag_min,mag_max]+[-1,1]*0.01
oplot,cor(2,6:*),cor(1,6:*),psym=7,col=col.red,symsi=.6
cor2 = cor(*,6:cnt-4)
n = n_elements(cor2(0,*))
cor3 = fltarr(4,n+2)
cor3(*,1:n) = cor2
cor3(0,0) = cor2(0,0) - 0.01 ; go to a lower magnitude

xp = robust_poly_fit(cor2(0,0:19), cor2(1,0:19), 2, myfit)
cor3(1,0) = poly(cor3(0,0), xp )
xp2 = robust_poly_fit(cor2(0,0:19), cor2(2,0:19), 2, myfit2)
cor3(2,0) = poly(cor3(0,0), xp2 )

cor3(0,n+1) = cor2(0,n-1) + 0.01 ; go to a lower magnitude

exp = robust_poly_fit(cor2(0,n-20:n-1), cor2(1,n-20:n-1), 1, myfit)
cor3(1,n+1) = poly(cor3(0,n+1), exp )
exp2 = robust_poly_fit(cor2(0,0:19), cor2(2,0:19), 1, myfit2)
cor3(2,n+1) = poly(cor3(0,n+1), exp2 )

; Hard limits for fitting 2nd deg. poly's
mlim0 = [13.73,  13.740, 13.750, 13.760]
mlim1 = [13.740,  13.750, 13.760, 13.79 ]
; For interpolating to extreme values... use these limits:
; Should be identical to mlim0 and mlim1 above, but change
; entry (mlimt0(0) and the last entry in mlimt1
mlimt0 = [13.700,  13.740, 13.750, 13.760]
mlimt1 = [13.740,  13.750, 13.760, 13.81 ]

nsec = n_elements(mlim0)
nx = 200
cor4 = fltarr(4,nx)
cor4(0,*) = (min(mlimt) ) + (findgen(nx)/(nx-1.)) * (max(mlimt) - min(mlimt) )
cor4(2,*) = cor4(0,*) ; also magnitudes

for i=0,nsec-1 do begin
 w = where(cor2(0,*) ge mlim0(i) and cor2(0,*) le mlim1(i),c)

 pp = robust_poly_fit(cor2(0,w),cor2(1,w),2,myfit)
 wn = where(cor4(0,*) ge mlimt0(i) and cor4(0,*) le mlimt1(i),cn)
 cor4(1,wn) = poly(cor4(0,wn),pp)
endfor

;!P.multi[0,1,2]
plot,cor2(0,*),cor2(1,*),ysty=3,xsty=3
oplot,cor4(0,*),cor4(1,*),psym=7,col=col.sky
;!P.multi=0


;



plot,cor3(0,*),cor3(1,*),psym=-2,symsi=.5,xr=[13.7,13.83],ysty=3,/nodata,yr=[.2,.35] ; old cor
oplot, mm, xx, psym=3,col=col.red
; oplot,xall,poly(xall, exp),line=2,col=col.red
oplot, mm, xx, psym=3,col=col.red
oplot,cor4(0,*),cor4(1,*),psym=-6,symsi=.7 ; new cor
plots,mlim0(0),!y.crange,line=2

; Use polynomial approximation:
; cor3 = cor4


; =========================================================

; =========================================================
; Devide the dataset in two parts, before and after "twoparts"
; =========================================================
twoparts = 13.76
meanmag = 13.75 ; magn. of star set to this magnitude (most data points have this value)
colx = [col.sky, col.yellow]
plot, mm, xx, psym=3,yr=[.2,.4]

mstart = 13.75 ; min(cor3(0,*))
mstart = min(cor3(0,*))
mend   = max(cor3(0,*))
mstep  = 0.00075
mfuz = mstep * 2.
mmag = mstart

use = bytarr(n_elements(mm))
cord = dblarr(n_elements(mm))

; =========================================================
while mmag lt mend do begin

if mmag lt twoparts then begin
   wcor = where(mm le twoparts and $
              (mm ge mmag and mm lt mmag+mstep) and $
               xx gt x_min and xx lt x_max and $
               yy gt y_min and yy lt y_max and $ 
               mm gt mag_min and use eq 0,ccor)
   wcor2 = where(mm le twoparts and $
              (mm ge (mmag-mfuz) and mm lt (mmag+mstep+mfuz)) and $
               xx gt x_min and xx lt x_max and $
               yy gt y_min and yy lt y_max and $ 
               mm gt mag_min,ccor2)
   w2   = where(cor3(0,*) le twoparts,c2)
endif else begin
   wcor = where(mm gt twoparts and $
              (mm ge mmag and mm lt mmag+mstep) and $
               xx gt x_min and xx lt x_max and $
               yy gt y_min and yy lt y_max and $ 
               mm gt mag_min and use eq 0,ccor)
   wcor2 = where(mm gt twoparts and $
              (mm ge (mmag-mfuz) and mm lt (mmag+mstep+mfuz)) and $
               xx gt x_min and xx lt x_max and $
               yy gt y_min and yy lt y_max and $ 
               mm gt mag_min,ccor2)
   w2   = where(cor3(0,*) gt twoparts,c2)
endelse

;plot, xx,        mm,         psym=3,xr=[.2,.4]
;oplot,xx(wcor),  mm(wcor),   psym=3,col=col.sky
;oplot,cor3(1,w2),cor3(2,w2), psym=7, col=col.magenta

x = sort(cor3(1,w2))
if ccor ge 10 then begin
 resistant_mean,xx(wcor2), 3, meanx, sd, nr
 correction = interpol(cor3(0,w2(x)), cor3(1,w2(x)), meanx ) - meanmag

; use(wcor) = 1B ; already corrected
 cord(wcor) = correction
endif


;plot,xx(wcor), mm(wcor),psym=3,yr=13.78+[-1,1]*0.03
;oplot,cor3(1,w2(x)), cor3(2,w2(x)),psym=2,col=col.sky
;oplot,xx(wcor), mm(wcor) - correction + .03, psym=3,col=col.red

mmag = mmag + mstep

endwhile
; =========================================================

plot,xx,mm,psym=3,xr=[0,.4]
plot,xx,mm,psym=3,xr=[.2,.4]
oplot,xx,mm - cord,psym=3,col=col.red

wgood = where(xx gt x_min and xx lt x_max and $
              yy gt y_min and yy lt y_max and $ 
              mm gt mag_min and mm lt mag_max,cgood)
ntot = n_elements(mm)

print,' %%% Data points within x,y and mag limits: ',cgood
print,' %%% Percent good points: ' + string(100. * float(cgood) / ntot,format='(F6.1)')

stop

wbad = where(abs(mm-cord - median(mm-cord)) gt .015,cbad)

wireult.mag(0) = mm - cord
wireult(wbad).mag(0) = 99.9 ; bad data

file = '/mnt/sdb6/wire/rawdata/nov2005/rfd/BetaHyi/BetaHyi_wire31_XYCOR.idl'
save,filename=file,wireult,mmag,err,t0
 restore,file  &  target = "BetaHyi"
 wirep,wireult,file,target,1 ; ,$


END

dops = 1
outname  = '/ai40/bruntt/wire/wire_eps/altair_complete_lc.eps'
outname2 = '/ai40/bruntt/wire/wire_eps/altair_complete_lc_zoom.eps'

fiton = 0. ; show artif. lc ?

if dops eq 1 then begin
 a4e,x=20,y=25,name = outname
endif

tzoom1 = 7.5 ; -.5 + 7
tzoom2 = 8.5 ; 0.5 + 7

; readcol,'~/wire/wire_lc/wire_ALTAIR_09APR.dat',hjd,dat,wei,format='(D,D,F)'
; inputname = '~/wire/wire_periods/altair_bruntt_f8_09APR04.per'

readcol,'/ai40/bruntt/wire/wire_lc/wire_Altair_11jun04.dat',hjd,dat,wei,format='(D,D,F)'
inputname = '/ai40/bruntt/wire/wire_per/Altair_clean_11jun04_p98.per'


min_t = min(floor(hjd))
max_t = max(ceil(hjd))

days_per_plot = 2.
datlim = 0.1 ; magnitude limits
multy = 1e3 ; 1000.
yrr = 0.002 * multy
datlim = datlim * multy
t0 = 51480.0

nd = max_t - min_t + 1

xx = 1. ; number of plots horizontal direction
yy = ceil ( nd / ( days_per_plot * xx ) )

getpos,xx,yy,0.02,0.00,0,pos

dat = dat * multy

emp =[' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ']
emp2 = ''
; emp2 =[' ','1',' ','0',' ','1',' ',' ',' ',' ',' ',' ']
emp2 =['-2',' ','0',' ','2',' ',' ',' ',' ',' ',' ',' ']



   readcol,inputname,$
           dummy,p98f,p98a,p98p,$
           format='A,D,D,D'
   nfreq = n_elements(p98f) & np = n_elements(dat)

   npf = 10000
   fit = fltarr(2,npf)
   fit(0,*) = min_t + (max_t - min_t) * (findgen(npf) / (npf-1.))

    for pp=0, nfreq-1 do $
     fit(1,*) = fit(1,*) + p98a(pp) * $
      sin(2. * !DPI * p98f(pp) * (fit(0,*)) + p98p(pp) * (2. * !DPI)  ) 

    fit(1,*) = fit(1,*) * multy




plot,[0,1],/nodata,xsty=6,ysty=6

;!P.xthick=2
;!P.ythick=2
;!P.charsize=1.5


for i=0,yy-1 do begin

t1 = min_t +  i    * days_per_plot
t2 = min_t + (i+1) * days_per_plot


w = where(hjd ge t1 and hjd le t2 and abs(dat) lt datlim,c)

wart = where(fit(0,*) ge t1 and fit(0,*) le t2,cart)


 if c ge 2 then begin


 if i ne (yy-1) then $
 plot,/noerase,hjd(w),dat(w),psym=3,position=pos(*,i),$
  xr = [t1,t2],xsty=1,ysty=1,yr=[-1,1]*yrr,$
  xtickname=emp,ytickname=emp,$
  xthick=2,ythick=2  else $
 plot,/noerase,hjd(w),dat(w),psym=3,position=pos(*,i),$
  xr = [t1,t2],xsty=1,ysty=1,yr=[-1,1]*yrr,$
  xthick=2,ythick=2, $
  xtit='!3HJD - 24' + strcompress(string(t0,format='(D10.1)'),/remove_all), $
  charsi=0.9,charthick=2, $
  ytickname = emp2, $
  ytit='!4D!3m [mmag]' ; 'Amplitude [mmag]'

 if fiton eq 1 and cart ge 2 then oplot,fit(0,wart),fit(1,wart)

 pp1 = ' ' & pp2 = ' ' & pp3 = ' ' & pp4 = ' '


; Mark zoom range for altair
if t1 gt (tzoom1-days_per_plot*1.5) and t2 lt (tzoom2+days_per_plot*1.5) then begin
 oplot,tzoom1*[1.,1],[-1.,1]*yrr,line=2,thick=3
 oplot,tzoom2*[1.,1],[-1.,1]*yrr,line=2,thick=3
endif

print,t1,t2


; if t1 lt 0. then begin
;  pp1 = '(' & pp2 = ')'
; endif

; if t2 lt 0. then begin
;  pp3 = '(' & pp4 = ')'
; endif
 
;  '!4D!3t =' + $

 xyouts,t1 + days_per_plot * 0.05,yrr * 0.55,charsi=0.8,charthick=2,$
  pp1 + strcompress(string(t1,format='(I7)'),/remove_all) + pp2 + ' to ' + $
  pp3 + strcompress(string(t2,format='(I7)'),/remove_all) + pp4

endif else begin
  print,'No data in this range: ',t1,t2,c
endelse

endfor

if dops ge 1 then begin
 device,/close
 set_plot,'x'
 print,' $  ggv ' + outname + '  & '
endif



; ================ ZOOM PLOT ====================

if dops eq 1 then begin
 a4e,x=20,y=12,name = outname2
endif

wz = where(hjd ge tzoom1 and hjd le tzoom2 and abs(dat) lt datlim,c)
wz_art = where(fit(0,*) ge tzoom1 and fit(0,*) le tzoom2,cz_art)

plotsym,0,/fill

plot,hjd(wz),dat(wz),psym=8,symsi=.7,$
  xr = [tzoom1,tzoom2],xsty=1,ysty=1,yr=[-1,1]*yrr,$
  xthick=2,ythick=2, $
  xtit='!3HJD - 24' + strcompress(string(t0,format='(D10.1)'),/remove_all), $
  charsi=1.5,charthick=2, $
  ytickname = emp2, $
  ytit='!4D!3m [mmag]' ; 'Amplitude [mmag]', nodata
oplot,fit(0,wz_art),fit(1,wz_art),thick=5,col=80
oplot,hjd(wz),dat(wz),psym=8,symsi=.7

if dops ge 1 then begin
 device,/close
 set_plot,'x'
 print,' $  ggv ' + outname2 + '  & '
endif


end

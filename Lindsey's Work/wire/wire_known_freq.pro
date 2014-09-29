PRO   wire_known_freq,p98_file,tt,t0, dd, dmag, debug


   readcol,p98_file,dummy,p98f,p98a,p98p,format='A,D,D,D'

   nfreq = n_elements(p98f) & np = 8000

   dmag = fltarr(n_elements(tt))

   for pp=0, nfreq-1 do $
     dmag = dmag + p98a(pp) * sin(2. * !DPI * p98f(pp) * (tt-t0) + p98p(pp) * (2. * !DPI)  ) 
 
   d0 = median(dd)
   nn = robust_sigma(dd)
   ww = where(abs(dd-d0) lt 10. * nn,cc)
   d0 = median(dd(ww))


;   plot,tt-t0,dd - d0,yr=[-1,1]*0.005,psym=3,xr = [-9.0,-8.3],xsty=1
;   oplot,tt-t0,dmag+offs,psym=3,col=col.red

   offs = median(dd(ww) - dmag(ww) - d0)
 
if debug eq 1 then begin

!P.multi = [0,1,2]
!P.charsize=1.0
   col=getcolor(/load)
   plot,tt-t0,dmag,yr=[-1,1]*0.005,/nodata,$
    tit='WIRE: Red line = Known Oscillation',xtit='!4D!3 HJD',ytit='!4D!3 mag' , $
     xr = [-9.0,-8.7],xsty=1
   oplot,tt-t0,dd-median(dd),psym=3
   oplot,tt-t0,-dmag,col=col.red,psym=3

   plot,tt-t0,dd-dmag,yr=[-1,1]*0.005,psym=3,$
     tit='WIRE: After subtration of oscillation',xtit='!4D!3 HJD',ytit='!4D!3 mag', $
     xr = [-9.0,-8.7],xsty=1

   print,' Hit to stop wire_known_freq.pro' & s = get_kbrd(1) & if s eq 'x' then stop

!P.multi = 0

endif




END

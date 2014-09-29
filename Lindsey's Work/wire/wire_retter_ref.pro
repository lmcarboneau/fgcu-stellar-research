; Read Retter's data and use his HJD as reference times 
; (c) Bruntt ca. Oct/Nov 2003
; Adjusted for USAFA / new Bruntt light curve on 29 MAR 04

t0 = 51480D
doff = 11.9179
col = getcolor(/load)

; Read Retter's data:
if n_elements(d2) eq 0 then begin
; readcol,'/usr/users/bruntt/wire/retter_altair_series02.dat',$
 readcol,'~bruntt/wire/altair/retter_altair_series02.dat',$ ; USAFA
 skipline=23,t2,d2,w2,format='D,D,F'
 t2 = t2 - 10.298 + t0     ; approx offset in time!
 d2 = d2 - median(d2) + doff
endif

nmerge = 1

; wire_merge_ref_time,time_ref,dd,t0,nmerge,ttref,ddref

if n_elements(ttref) eq 0 then $
 wire_merge_ref_time,t2,d2,t0,nmerge,ttref,ddref

plot,t2-t0,d2,psym=3,yr=[-1,1]*0.005+doff
oplot,ttref(0,*)-t0,ddref,psym=3,col=col.green

nref = n_elements(ttref(0,*))

plot,t2-t0,d2,psym=3,yr=[-1,1]*0.005+doff,xr=[0.02,0.06]
for i=0,nref-1 do plots,ttref(1,i)-t0,!y.crange,col=col.red
for i=0,nref-1 do plots,ttref(2,i)-t0,!y.crange,col=col.green

;if n_elements(wire3) eq 0 then $
; restore,'/ai39/bruntt/wire/altair/altair_merged_allslots_31_decor1.idl' ; 3.10.03

; fil = "/ai43/bruntt/wire/altair/altair_merged_allslots_51_decor1.idl" ; Ca. 10.11.03

fil = "/data2/bruntt/wire/dat/AltairA7V/data/data_merged_allslots_31.idl" ; USAFA 29 MAR 04
restore, fil

nstar = n_elements(wireult(0).mag)

; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>
; STOLEN FROM wire_merge3.pro:

; ----------------------------------------------------------------------------
; Arrays for merged results:
; ----------------------------------------------------------------------------
   ndat = n_elements(ttref(0,*))
   dat3 = fltarr(nstar,ndat)
   gc3  = fltarr(2,nstar,ndat)
    x3  = fltarr(nstar,ndat)
    y3  = fltarr(nstar,ndat)
  fwhm3 = fltarr(nstar,ndat)
  back3 = fltarr(nstar,ndat)

   dat3(*,*)   =    -9.9
    gc3(*,*,*) =  -100.
      x3(*,*)  = -1000.
      y3(*,*)  = -1000.
   fwhm3(*,*) = -5.5
   back3(*,*) = -99.
; ----------------------------------------------------------------------------
   
; ----------------------------------------------------------------------------
 for star=0,nstar-1 do begin
; ----------------------------------------------------------------------------

; Find good and bad data points!
   wg   = where(wireult.mag(star) gt 5. and wireult.hjd gt 45000.,cg)

; debug
;   wg = where(dat(star,*) gt 10. and wire2.hjd gt 51475. and $
;              wire2.hjd lt 51477.,c0)

   wbad = where(wireult.mag(star)  le 5. or wireult.hjd  le 45000. or $
                wireult.fwhm(star) lt 1.3 or wireult.fwhm(star) gt 3.0,cbad)

; Set up arrays for results ---> Mark bad points
   ; times(star,wg)  = wire2(wg).hjd ; 
   wireult(wbad).mag(star)  =   -9.9
   wireult(wbad).gc(*,star) =  -99.9 ; gaussian (x,y)-center position
    wireult(wbad).x(star)   = -900.
    wireult(wbad).y(star)   = -900.
 wireult(wbad).fwhm(star)   = -2.5
 wireult(wbad).back(star)   = -99.

; Calculate noise in light curve
   me = median(wireult(wg).mag(star))
   noise = robust_sigma(wireult(wg).mag(star))
   fivesigma = 5. * noise ; 5. * robust_sigma(dat)
   extsigma = 15. * noise
   mmag(1,star) = noise
   mmag(0,star) = me

; ----------------------------------------------------------------------------
   tt      = reform(wireult(wg).hjd)      
   dd      = reform(wireult(wg).mag(star)) 
   gc_temp = reform(wireult(wg).gc(*,star))
    x_temp = reform(wireult(wg).x(star))      
    y_temp = reform(wireult(wg).y(star))
 fwhm_temp = reform(wireult(wg).fwhm(star))
 back_temp = reform(wireult(wg).back(star))
; ----------------------------------------------------------------------------

; -xxx---------------------------------------------------------------------------
; Merge data points (last two parameters: weights=1, debug=0)
;   wire_merge_dat_any,tt,dd,gc_temp,ttref, ttn, ddn, gcn, nmerge, 1, 0
   wire_merge_dat_any,$
    tt , dd , x_temp, y_temp, gc_temp, fwhm_temp, back_temp, ttref, $
    ttn, ddn, xn    , yn    , gcn    , fwhmn    , backn,     nmerge, $
    dmag, $
    t0, $
    0.10, $ 
    1, 0

; Put merged data in temporary result arrays
  dat3(star,*) = ddn ; put merged data in result arrays
  for k=0,1 do gc3(k,star,*) = gcn(k,*)
   x3(star,*)  = xn
   y3(star,*)  = yn
fwhm3(star,*)  = fwhmn
back3(star,*)  = backn
; ----------------------------------------------------------------------------

; ----------------------------------------------------------------------------
!P.multi = [0,2,3]
!P.charsize=1.3
!P.charthick=1.5       

   t000 = min(tt)
   plot,tt-t000,dd,psym=3,yr=[-1,1]*0.01+mmag(0,0),xr=[0,1]*0.4,$
    tit='Red = Merged points; White = org. lc',xtit='HJD',ytit='!4D!3 mag'
   oplot,ttref(0,*)-t000,ddn,psym=1,symsi=.2,col=col.red

   plot,fwhm_temp,dd,psym=3,yr=[-1,1]*0.01+mmag(0,0),xr=[1.75,2.5],$
    tit='FWHM',xtit='FWHM [pixels]',ytit='!4D!3 mag'
   oplot,fwhm3(star,*),ddn,psym=1,symsi=.2,col=col.red

   plot,tt-t000,fwhm_temp,psym=3,yr=[1.75,2.5],xr=[0,1]*0.1,$
    tit='FWHM',xtit='HJD',ytit='FWHM [pixels]'
   oplot,ttref(0,*)-t000,fwhm3(0,*),psym=1,symsi=.2,col=col.red

   plot,gc_temp(0,*)-3.5,dd,psym=3,$
    xr=[-1,1]*0.5, $
    yr=[-1,1]*0.02+mmag(0,0), $
    tit='x-y - center of stellar profile',ytit='!4D!3 mag',xtit='!4D!3 x'
   oplot,gc3(0,star,*)-3.5,dat3(star,*),psym=1,symsi=.2,col=col.red

   plot,gc_temp(1,*)-3.5,dd,psym=3,$
    xr=[-1,1]*0.5, $
    yr=[-1,1]*0.02+mmag(0,0), $
    tit='x-y - center of stellar profile',ytit='!4D!3 mag',xtit='!4D!3 y'
   oplot,gc3(1,star,*)-3.5,dat3(star,*),psym=1,symsi=.2,col=col.red

   plot,gc_temp(0,*)-3.5,gc_temp(1,*)-3.5,psym=3,$
    yr=[-1,1]*.6,xr=[-1,1]*.6,$
    tit='x-y - center of stellar profile',xtit='x',ytit='y'
   oplot,gc3(0,star,*)-3.5,gc3(1,star,*)-3.5,psym=3,col=col.red

   ; Avg. Row/Column Position:
   ;   plot,tt-t000,x_temp,psym=3,yr=[-1,1]*5.0+median(x_temp),$
   ;    xr=[0,1]*0.4,$
   ;    tit='Red = Merged points; White = org. lc',xtit='HJD',ytit='!4D!3 mag'
   ;   oplot,ttref(0,*)-t000,x3(star,*),psym=1,symsi=.2,col=col.red
   
   ;   plot,tt-t000,gc_temp(0,*),psym=3,yr=[-1,1]*2.5+median(gc_temp(0,*)),$
   ;    xr=[0,1]*0.4,$
   ;    tit='Red = Merged points; White = org. lc',xtit='HJD',ytit='!4D!3 x'
   ;   oplot,ttref(0,*)-t000,gc3(0,star,*),psym=1,symsi=.2,col=col.red

!P.multi=0
; ----------------------------------------------------------------------------

   endfor ; go to next star and compress the LC
; ----------------------------------------------------------------------------

; ----------------------------------------------------------------------------
; Merge the data for all stars
; ----------------------------------------------------------------------------
 cnt3 = 0L
 ; wire4 = wireult ; lots of entries ...

 wire5 = wireult
 wire_merge_ww3,ttref(0,*),nstar,dat3,x3,y3,gc3,fwhm3,back3,cnt3,wire5
 wire5 = wire5(0:cnt3-1) ; remove unused entries! 


 npp = n_elements(wire5)
 wire4 = replicate({hjd:0D, mag:fltarr(nstar+1), fwhm:fltarr(nstar+1),$
                   gc:fltarr(2,nstar+1), x:fltarr(nstar+1), y:fltarr(nstar+1), $
                   angle:fltarr(nstar+1), temp:0., back:fltarr(nstar+1)},npp) ; angle, temp, back added 29MAR04
 wire4.hjd = wire5.hjd
 wire4.mag(0:nstar-1)  = wire5.mag(0:nstar-1)
 wire4.fwhm(0:nstar-1) = wire5.fwhm(0:nstar-1)
 wire4.gc(*,0:nstar-1) = wire5.gc(*,0:nstar-1)
 wire4.x(0:nstar-1)    = wire5.x(0:nstar-1)
 wire4.y(0:nstar-1)    = wire5.y(0:nstar-1)
 wire4.angle(0:nstar-1)   = wire5.angle(0:nstar-1)
 wire4.back(0:nstar-1)    = wire5.back(0:nstar-1)
 wire4.temp               = wire5.temp

; ----------------------------------------------------------------------------

; ----------------------------------------------------------------------------
plot,wire4.hjd-t0,wire4.mag(0),psym=3,$
 xtit='HJD',ytit='!4D!3 mag',yr=[-1,1]*0.05+mmag(0,0),xr=[1.1,2.2]+.2
oplot,wireult.hjd-t0,wireult.mag(0),psym=3,col=col.green

; Progress plot:
refstar = 0
 plot,wire4(0:cnt3-1).hjd-51480D,wire4(0:cnt3-1).mag(refstar)-median(wire4(0:cnt3-1).mag(refstar)),$
  psym=3,xr=[-12,10],yr=[-0.9,0.9],ysty=1,xsty=3,xtit='!4D !3HJD',ytit='!4D!3m',$
  tit='Progress Plot For All Stars',/nodata
 for s=0,nstar-1 do oplot,wire4(0:cnt3-1).hjd-51480.,$
  wire4(0:cnt3-1).mag(s)-median(wire4(0:cnt3-1).mag(s))+0.75-0.34*s,psym=3
 for s=0,nstar-1 do xyouts,-12.1,0.78-0.34*s,'Star '+strcompress(string(s),/remove_all),charsi=1.0

 for s=0,nstar-1 do oplot,wireult.hjd-51480.,$
  wireult.mag(s)-median(wireult.mag(s))+0.75-0.4*s,psym=3,col=col.red

; ----------------------------------------------------------------------------
 print,' %%% Merged points so far: ',cnt3
 print,''
; ----------------------------------------------------------------------------

; wireult = wire4(0:cnt3-1) ; remove unused data points
; outfile = '/ai39/bruntt/wire/altair/altair_retter_times.idl'

outfile = "/data2/bruntt/wire/dat/AltairA7V/data/data_merged_retter_times_31.idl" ; USAFA 29 MAR 04


wire4.mag(5) = -d2 + 2. * doff + 1.0 * 0.0005 ; retters light curve !!

wireult = wire4

save, filename = outfile, mmag, wireult, decor_star, posmap, map, err, t0

print,' %%% Saved "restore" file: restore,"'+outfile+'"'
print,' Try this in idl:   wirep,wireult'
; ----------------------------------------------------------------------------




END

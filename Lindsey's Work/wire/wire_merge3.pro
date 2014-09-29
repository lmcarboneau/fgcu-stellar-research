; (c) Early September 2003 by Hans Bruntt

; Purpose: 1. Compress data: "nmerge" parameter describes compression =
;          number of data points to combine to a single data point --
;          either using a sigma-clipped average or weights.
;          2. Merge different wire result files for all observed stars.

; Note: This program is used after wire_pos.pro

PRO wire_merge3, input_dir, target, position_file

; ----------------------------------------------------------------------------
; Directory with aperture results from wire_pos.pro:
; input_dir = '/ai39/bruntt/wire/altair/*.idl1' ; aperture results from wire_pos.pro
;input_dir = wire_merge3,'/ai39/bruntt/wire/HD113226/data/data_wire_*.idl1x' 
; wire_merge3,'/ai39/bruntt/wire/beta_UMi/data/data_wire_*.idl1x' 
; wire_merge3,'/data2/bruntt/wire/dat/HR6596F5V/data//data_wire_010.idl1x','HR6596', '/data1/bruntt/wire/xy_positions.idl'
; wire_merge3,'/data2/bruntt/wire/dat/HR6596F5V/data//data_wire_*.idl1x','HR6596', '/data1/bruntt/wire/xy_positions.idl'
; ----------------------------------------------------------------------------

; ----------------------------------------------------------------------------
; run_mode = 'Determine Correlation' ; altair
; run_mode = 'Process All Frames and Decorrelate'
; run_mode = 'Determine Correlation nmerge=51' ; HD113226 + beta_Umi
run_mode = 'nmerge=51' ; standard reduction since MARCH 3rd 2004
; ----------------------------------------------------------------------------

; ----------------------------------------------------------------------------
; ap_number = 5 ; use 5 or 6 (4 is too small) ; use program wire_test.pro to determine ap. size
n_max = 10000. * 30.
; nstar = 5 
refstar = 0   ; Slot number of main target 



; ----------------------------------------------------------------------------
restore,position_file
;position_file = '/data1/bruntt/wire/xy_positions2.idl'
; Set up the xy positon file:
; xymax = 200 ; max 150 WIRE observing runs
; nst = 40
; xyinfo = replicate( {xy:fltarr(2,nst), exy:fltarr(2,nst), $
;                       xy2:fltarr(2,nst), flux:fltarr(2,nst), $
;                       fwhm:fltarr(2,nst), angle:fltarr(10), $
;                       distcen:fltarr(10), xc:fltarr(2), $
;                       extra:fltarr(6,nst), $
;                       object:strarr(nst), aperture:intarr(nst)}, xymax)
; wireinfo = replicate( {t0:0D, dir:'', nstars:0, object:''}, xymax )
; save,filename=position_file,xyinfo, wireinfo

winf = where(strmatch(wireinfo.object,'*' + target + '*',/fold_case) eq 1,cinf)
if cinf ne 1 then begin
 print,' *** WARNING: The target: '+target+ ' ... is not in the xyinfo structure!'
 print,'              You must enter target, t0, xy-positons manually ... '
 print,'              Use the program wire_getpos.pro to get the information.'
 help,winf,xyinfo,wireinfo
 stop
endif

wall = where(strmatch(wireinfo.object,'*' + target + '*',/fold_case) eq 1,cinf)

t0 = wireinfo(winf).t0
xy = xyinfo(winf).xy(*,0:wireinfo(winf).nstars-1)

print,'wireinfo on this object: '
print, wireinfo(winf)
print,'Number of stars: ',wireinfo(winf).nstars
print,'Apertures to use: ',xyinfo(winf).aperture(0:wireinfo(winf).nstars-1)

; ----------------------------------------------------------------------------
if n_elements(t0) eq 0 then begin
 print,' *** Missing t0 parameter !! '
 stop
endif
if t0 lt 51000 then begin
 print,' %%% Was the star observed before WiRE was launched ???'
 stop
endif
; ----------------------------------------------------------------------------

; ----------------------------------------------------------------------------
if run_mode eq 'nmerge=51' then begin
 nmerge = 51 ; number of points to merge (if time is within == timestep * nmerge)
 filmax = 99 ; max number of files to import (eg. 0-2 has a large range in x,y pos)
endif

; ----------------------------------------------------------------------------

print,' %%% '
print,' %%% ' + run_mode
print,' %%% ' + $ 
                ' -- nmerge = '+ strcompress(nmerge,/remove_all) + $
                ' -- filmax = '+ strcompress(filmax,/remove_all)
print,' %%% '



; ----------------------------------------------------------------------------

cnt3 = 0L ; counter
if n_elements(col) eq 0 then col=getcolor(/load)

; ----------------------------------------------------------------------------
spawnrob,'ls -1 '+input_dir,aa

na = n_elements(aa)
if na eq 0 or aa(0) eq '' then stop
; ----------------------------------------------------------------------------

; ----------------------------------------------------------------------------
window,0,title='WIRE Progress Window',xsize=580,ysize=650
!P.charsize=1.5
; ----------------------------------------------------------------------------

if n_elements(xy) eq 0 then begin
 print,''
 print,' *** No xy array restored: x,y positions of stars in wire frame not known!'
 print,' *** investigate the wire3.x wire3.y positions after the first iteration ...'
 print,' *** eg. use prg. wire_getpos.pro (need wire3 structure: restore approp. file).'
 print,''
endif

; ----------------------------------------------------------------------------
if na ge filmax then na = filmax
; ----------------------------------------------------------------------------

; ----------------------------------------------------------------------------
for fil = 0,na-1 do begin ; for each idl1x file (from wire_pos.pro)

 restore,aa(fil) ; read the wire data structure (from program wire_pos.pro)
 nslot = n_elements(wire2(0).x)


 if n_elements(xy) ne 0 then nstar = n_elements(xy(0,*)) else nstar = nslot

 ; plot,wire2.gc(0,0)-3.5,wire2.gc(1,0)-3.5,psym=3,xr=[-1,1],yr=[-1,1]
 ; oplot,wire3.gc(0,0)-3.5,wire3.gc(1,0)-3.5,psym=3,col=col.red
 ; wxxx = where( (wire2.gc(1,0)-3.5) gt -0.2, cxxx) & help,cxxx

 if fil eq 0 then $
   wire3 = replicate({hjd:0D, $
                      mag:fltarr(nstar),$                  
                     fwhm:fltarr(nstar),$
                       gc:fltarr(2,nstar), $
                        x:intarr(nstar), $
                        y:intarr(nstar)}, n_max)

; ----------------------------------------------------------------------------
; Determine which aperture to use:
; ----------------------------------------------------------------------------
ap_number = (xyinfo(winf).aperture(0:nstar-1)) ; found using wire_getpos.pro
wbadap = where(ap_number le 1,cbadap)
if cbadap ge 1 then begin
 print,' %%% I am suspecting the apertures are not correctly chosen?'
 stop
endif
; ----------------------------------------------------------------------------
 
; Swaps slots --- use absolute CCD x,y position:
if n_elements(xy) ne 0 then begin
 wire_swap_all, xy, wire2, wire2a, ap_number, 1

 ; Remove data with the same times:
 wire2c = wire2a
 wire2c.p(*,*) = -9.
 wire2c.fwhm(*) = -1. ; bad data
 wire2c.x(*) = -1. ; bad data
 wire2c.y(*) = -1. ; bad data
 wire2c.hjd(*) = -99.
 
 for k=0,nstar-1 do begin

  ss = sort(wire2a.hjd(k))
  wire2b = wire2a(ss)

  wok99 = where(wire2b.p(k,ap_number(k)) lt 100.,c99)
  wire2b(wok99).hjd(k) = -99. ; bad data

  uu = uniq(wire2b.hjd(k))
  nuu = n_elements(uu)
  wire2c(0:nuu-1).p(k,*)  = wire2b(uu).p(k,*)
  wire2c(0:nuu-1).a(k,*)  = wire2b(uu).a(k,*)
  wire2c(0:nuu-1).co(*,k) = wire2b(uu).co(*,k)
  wire2c(0:nuu-1).gc(*,k) = wire2b(uu).gc(*,k)
  wire2c(0:nuu-1).fwhm(k) = wire2b(uu).fwhm(k)
  wire2c(0:nuu-1).x(k)       = wire2b(uu).x(k)
  wire2c(0:nuu-1).y(k)       = wire2b(uu).y(k)
  wire2c(0:nuu-1).hjd(k)     = wire2b(uu).hjd(k)
  wire2c(0:nuu-1).backgr(k)  = wire2b(uu).backgr(k)
  wire2c(0:nuu-1).backgr2(k) = wire2b(uu).backgr2(k)
  wire2c(0:nuu-1).col(k)     = wire2b(uu).col(k)
  wire2c(0:nuu-1).row(k)     = wire2b(uu).row(k)
  wire2c(0:nuu-1).flux1(k)   = wire2b(uu).flux1(k)
  wire2c(0:nuu-1).flux2(k)   = wire2b(uu).flux2(k)

 endfor 


;plot_io,wire2a.hjd(0)-t0,wire2a.p(0,4,*),min_value=1000.,psym=3,xr=[-20,-10],yr=[1000,1e5]
;oplot,wire2a.hjd(1)-t0,wire2a.p(1,4,*),psym=3,col=col.red
;oplot,wire2a.hjd(2)-t0,wire2a.p(2,4,*),psym=3,col=col.green
;oplot,wire2a.hjd(3)-t0,wire2a.p(3,4,*),psym=3,col=col.sky
;oplot,wire2a.hjd(4)-t0,wire2a.p(4,4,*),psym=3,col=col.yellow

 wire2 = wire2c
 wire2a = 0B &  wire2b = 0B & wire2c = 0B
 print,' %%% Data sorted by x,y position! Bad data purged!'
endif



  if nstar ge 2 then begin

      ndat = n_elements(wire2)
      dat = fltarr(nstar,ndat)
      times = dblarr(nstar,ndat)
      flag = fltarr(nstar,ndat)

      ; Select the best aperture:
      for j=0,nstar-1 do dat(j,*)  = wire2.p(j,ap_number(j),*) ; light curve data
      for j=0,nstar-1 do flag(j,*) = wire2.a(j,ap_number(j),*) ; bad/good data flag
      
  endif else begin

; Need to debug this part ... if there is only one star !!!
      dat   = wire2.p(0,ap_number(0),*)
      np    = n_elements(dat) ; number of data points
      times = dblarr(np)     ; hjd
      flag  = wire2.a(0,ap_number(0),*) 
  endelse

;;   weight = fltarr(nstar,np)  ; weights
;;   weight(*,*) = 0.0         
                               
     mmag = fltarr(2,nstar)  ; mean magn. and noise
     gc = wire2.gc           ; x,y center position 
      x = wire2.x            ; column & row position on CCD
      y = wire2.y            ; see eg. /ai38/bruntt/wire/data/991019.3142.1AltairA7V.data
   fwhm = wire2.fwhm         ; fwhm == measured seeing ==> decorrelation!
; ----------------------------------------------------------------------------

; ----------------------------------------------------------------------------
    if nstar ge 2 then $
    w0 = where(dat(refstar,*) gt 10. and $
      abs(wire2.hjd(refstar,*) - t0) lt 800.,c0 ) else $
    w0 = where(dat gt 10. and $
       abs(wire2.hjd - t0) lt 800.,c0) ;
; ----------------------------------------------------------------------------
    if float(c0) / n_elements(dat(refstar,*)) lt 0.5 then begin
      print,' *** I removed a lot of points ... check it out!'
      print, float(c0) / n_elements(dat(refstar,*))
      ; if c0 lt 100 then stop
    endif

    if c0 eq 0 then begin
      print,' *** No valid data points in file: ' + aa(fil)
      goto,skip_file
    endif

; ----------------------------------------------------------------------------
; Calculate DAOPHOT-like magnitude, then merge data! 
; ----------------------------------------------------------------------------
   print,' %%% Computing ref. time points from LC of ref. star: (0--100)'

   if nstar ge 2 then begin
    dd = reform( -2.5 * alog10 ( dat(refstar,w0) ) + 25.0 )
    time_ref = wire2(w0).hjd(refstar)
   endif else begin
    dd = reform( -2.5 * alog10 ( dat(w0) ) + 25.0 )
    time_ref = wire2(w0).hjd(0)
   endelse
; ----------------------------------------------------------------------------

; ----------------------------------------------------------------------------
; debug: (the "if sentence"!) BE CAREFUL IF YOU'RE USING MORE FILES !!!
; if (n_elements(ttref) eq 0) or (filmax ne 1) then $
   wire_merge_ref_time,time_ref,dd,t0,nmerge,ttref,ddref
; ----------------------------------------------------------------------------
;   wire_merge_ref_time,time_ref,dd,t0,150,ttref,ddref
;   wire_merge_ref_time,time_ref,dd,t0,15,ttref,ddref

; plot,time_ref-t0,dd-median(dd),psym=3,yr=[-1,1]*0.005
; oplot,ttref(0,*)-t0,ddref-median(ddref),psym=3,col=col.red

; ----------------------------------------------------------------------------
; Arrays for merged results:
; ----------------------------------------------------------------------------
if nstar ge 2 then begin
   ndat = n_elements(ttref(0,*))
   dat3 = fltarr(nstar,ndat)
   gc3  = fltarr(2,nstar,ndat)
    x3  = fltarr(nstar,ndat)
    y3  = fltarr(nstar,ndat)
  fwhm3 = fltarr(nstar,ndat)
; =============================
   dat3(*,*)   =    -9.9
    gc3(*,*,*) =  -100.
      x3(*,*)  = -1000.
      y3(*,*)  = -1000.
   fwhm3(*,*) =     -5.5
endif else begin
   ndat = n_elements(ttref(0,*))
   dat3 = fltarr(ndat)
   gc3  = fltarr(2,ndat)
    x3  = fltarr(ndat)
    y3  = fltarr(ndat)
  fwhm3 = fltarr(ndat)
; =============================
   dat3(*)      =    -9.9
    gc3(*,*) =  -100.
      x3(*)     = -1000.
      y3(*)     = -1000.
   fwhm3(*)     =    -5.5
endelse

; ----------------------------------------------------------------------------
   
; ----------------------------------------------------------------------------
   tx = median(ttref(0,*))
   err = robust_sigma(ddref)
   plot,ttref(0,*)-tx,ddref,psym=3,yr=[-1,1]*err*10.0 + median(ddref),/nodata,$
    tit='Reference Light Curve (Time Points)'
   oplot,wire2(w0).hjd(refstar)-tx,dd,psym=3,col=col.red
   oplot,ttref(0,*)-tx,ddref,psym=3

;   tm = min(ttref(0,*))-t0 ; plot a single night?
;   plot,$ ; position=[.8,.8,.95,.95],/noerase,$
;        wire2(w0).hjd(refstar)-tx,dd,psym=3,col=col.red,$
;        xr=[0,.1]+tm,yr=[-1,1]*0.01 + median(ddref)
; ----------------------------------------------------------------------------

; ----------------------------------------------------------------------------
 for star=0,nstar-1 do begin
; ----------------------------------------------------------------------------
fwhml = xyinfo(winf).fwhm(*,star)
fwhml_min = fwhml(0) - 10. * fwhml(1) ; accept 10 sigma deviation
fwhml_max = fwhml(0) + 10. * fwhml(1) ;
mag = xyinfo(winf).flux(*,star)
mag_min = mag(0) - 10. * mag(1) - 3.0
mag_max = mag(0) + 10. * mag(1) + 3.0
magn = -2.5 * alog10(dat(star,*)) + 25.0

; Find good and bad data points!
;;   if nstar ge 2 then begin
    wg   = where(magn gt mag_min and magn lt mag_max and $
                 abs(wire2.hjd(star,*)-t0) lt 800.0 and $
                 fwhm(star,*) ge fwhml_min and fwhm(star,*) le fwhml_max,cg); and $
;                 flag(star,*) eq 0, cg)  
    wbad = where(magn le mag_min or magn ge mag_max or $
                 abs(wire2.hjd(star,*)-t0) ge 800.0 or $
                 fwhm(star,*) lt fwhml_min or fwhm(star,*) gt fwhml_max,cbad); or $
;                 flag(star,*) eq 1, cbad)  
;;   endif else begin
;;
;;    wg   = where(magn gt mag_min and magn lt mag_max and $
;;                 abs(wire2.hjd(star,*)-t0) lt 800.0 and $
;;                 fwhm(star,*) ge fwhml_min and fwhm(star,*) le fwhml_max,cg)

;;    wbad = where(magn le mag_min or magn ge mag_max or $
;;                 abs(wire2.hjd(star,*)-t0) ge 800.0 or $
;;                 fwhm(star,*) lt fwhml_min or fwhm(star,*) gt fwhml_max,cbad)

;;   endelse

if cg lt 20 then begin
 print,' %%% Abort star! No good data points for star: '+string(star,format='(I2)')
 dd = 0B & tt = 0B ; beware: previously, last successful star was used!
 goto, fail_star
endif


if nstar ge 2 then begin 

    ; Set up arrays for results ---> Mark bad points

    if cbad ge 1 then begin
       dat(star,wbad)  =   -9.9
       gc(*,star,wbad) =  -99.9 ; gaussian (x,y)-center position
        x(star,wbad)   = -900.
        y(star,wbad)   = -900.
     fwhm(star,wbad)   = -2.5
    endif
    
    if cg ge 20 then begin
       times(star,wg)  = wire2(wg).hjd(star) ; 
    
    ; Convert fluxes to delta magnitudes
       dat(star,wg) = -2.5 * alog10(dat(star,wg)) + 25.0
       mmag(0,star) = median(dat(star,wg))
    
    ; Calculate noise in light curve
       resistant_mean,dat(star,wg),3,me,sd_of_mean,nr   
       ; me = median(dat(star,wg))
       ; ptp_robust_fin,dat(star,wg),noise,0 ; this is a bad idea for a classical variable!
       noise = robust_sigma(dat(star,wg))
       fivesigma = 5. * noise ; 5. * robust_sigma(dat)
       extsigma = 15. * noise
       mmag(1,star) = noise
    ; ----------------------------------------------------------------------------
       tt      = reform(times(star,wg))      
       dd      = reform(dat(star,wg)) 
       gc_temp = reform(gc(*,star,wg))
        x_temp = reform(x(star,wg))      
        y_temp = reform(y(star,wg))
     fwhm_temp = reform(fwhm(star,wg))
    ; ----------------------------------------------------------------------------
    endif

endif else begin
;;    ; Set up arrays for results ---> Mark bad points
;;
    if cbad ge 1 then begin
       dat(wbad)  =   -9.9
       gc(*,wbad) =  -99.9 ; gaussian (x,y)-center position
        x(wbad)   = -900.
        y(wbad)   = -900.
     fwhm(wbad)   = -2.5
    endif

    if cg ge 10 then begin
    ; Convert fluxes to delta magnitudes
       dat(wg) = -2.5 * alog10(dat(wg)) + 25.0
       mmag(0,star) = median(dat(wg))
    ; Calculate noise in light curve
       me = median(dat(wg))
       noise = robust_sigma(dat(wg))
       fivesigma = 5. * noise ; 5. * robust_sigma(dat)
       extsigma = 15. * noise
       mmag(1,star) = noise
    
       times(wg)  = wire2(wg).hjd(star) ; 
    ; ----------------------------------------------------------------------------
       tt      = reform(times(wg))      
       dd      = reform(dat(wg)) 
       gc_temp = reform(gc(*,wg))
        x_temp = reform(x(wg))      
        y_temp = reform(y(wg))
     fwhm_temp = reform(fwhm(wg))
    ; ----------------------------------------------------------------------------
    endif

endelse ; only one star ...

; Calculate Stetson weights
;   astet = 0.7 & bstet = 6.0 ; Stetson outlier weights !
;   fudge_weight = (1. + (abs(dat(star,wg)-me)/(astet*extsigma))^bstet)^(-1.)
;   fudge_weight = fudge_weight / total(fudge_weight)
;   weight(star,wg) = fudge_weight

; ----------------------------------------------------------------------------
;   !P.multi=[0,1,2]
;   plot,dat(star,wg)-me,weight(star,wg),psym=1,xr=[-1,1]*extsigma,symsi=.1,$
;    tit='Stetson Weights (Sec. Plot: Red = Ref. Star)'
;   plot,times(star,wg)-t0,dat(star,wg)-me,yr=[-1,1]*extsigma,psym=1,symsi=.1,$
;    tit='Star '+strcompress(string(star))+' mag = '+string(me,format='(F5.2)')+ $
;    ' np = '+string(cg,format='(I6)'),xr=[-1,1]*0.2
; Plot the reference star for comparison?
;   mg = -2.5 * alog10(wire2.p(refstar,ap_number)) + 25.
;   oplot,wire2.hjd(refstar,*)-t0,mg-median(mg),psym=3,col=col.red

;   !P.multi = 0
; ----------------------------------------------------------------------------

 ; end of setting up arrays (weights, lc)

; ----------------------------------------------------------------------------
;;; if decor ---> subtract known freqs. !!
; if decor eq 1 and star eq 0 $
;   and strmatch(input_dir,'*altair*') eq 1 then begin 
;   print,' Importing known periods in the light curve of Altair -- dmag array!'
;   wire_known_freq,'/ai39/bruntt/wire/altair/altair_t0_51480.per',tt,t0,dd, dmag, 0
; endif else begin
   dmag = fltarr(n_elements(tt)) & dmag(*) = 0.0
;;; endelse
; ----------------------------------------------------------------------------

; Merge data points (last two parameters: weights=1, debug=0)
   wire_merge_dat_any,$
    tt , dd , x_temp, y_temp, gc_temp, fwhm_temp, ttref, $
    ttn, ddn, xn    , yn    , gcn    , fwhmn    , nmerge, $
    dmag, $
    t0, $
    0.10, $ ; how close must (x,y) pos be within each (nmerge) block (0.01-0.1 pix?)
    1, 0 ; last parameter: 1, 0 ... change to 1, 1 to debug! x = exit

; Put merged data in temporary result arrays
if nstar ge 2 then begin
      dat3(star,*) = ddn ; put merged data in result arrays
      for k=0,1 do gc3(k,star,*) = gcn(k,*)
       x3(star,*)  = xn
       y3(star,*)  = yn
    fwhm3(star,*)  = fwhmn
endif else begin
      dat3 = ddn ; put merged data in result arrays
      for k=0,1 do gc3(k,*) = gcn(k,*)
       x3  = xn
       y3  = yn
    fwhm3  = fwhmn
endelse
; ----------------------------------------------------------------------------

; ----------------------------------------------------------------------------
!P.multi = [0,2,3]
!P.charsize=1.3
!P.charthick=1.5       

   t000 = min(tt)
   err = robust_sigma(dd)

   plot,tt-t000,dd,psym=3,yr=[-1,1]*err*10.0+mmag(0,0),xr=[0,1]*0.4,$
    tit='Red = Merged points; White = org. lc',xtit='HJD',ytit='!4D!3 mag'
   oplot,ttref(0,*)-t000,ddn,psym=1,symsi=.2,col=col.red

   plot,fwhm_temp,dd,psym=3,yr=[-1,1]*err*10.0+mmag(0,0),xr=[1.75,2.5],$
    tit='FWHM',xtit='FWHM [pixels]',ytit='!4D!3 mag'
   oplot,fwhm3(star,*),ddn,psym=1,symsi=.2,col=col.red

   plot,tt-t000,fwhm_temp,psym=3,yr=[1.75,2.5],xr=[0,1]*0.1,$
    tit='FWHM',xtit='HJD',ytit='FWHM [pixels]'
   oplot,ttref(0,*)-t000,fwhm3(star,*),psym=1,symsi=.2,col=col.red

   plot,gc_temp(0,*)-3.5,dd,psym=3,$
    xr=[-1,1]*0.5, $
    yr=[-1,1]*xyinfo(winf).flux(1,star)*10.+mmag(0,0), $
    tit='x-y - center of stellar profile',ytit='!4D!3 mag',xtit='!4D!3 x'
   oplot,gc3(0,star,*)-3.5,dat3(star,*),psym=1,symsi=.2,col=col.red

   plot,gc_temp(1,*)-3.5,dd,psym=3,$
    xr=[-1,1]*0.5, $
    yr=[-1,1]*xyinfo(winf).flux(1,star)*10.+mmag(0,0), $
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

fail_star:

   endfor ; go to next star and compress the LC
; ----------------------------------------------------------------------------

; !P.multi=0
;plot,ttref(0,*),dat3(4,*),psym=3,ysty=3
;print,' %%% Stop = x ' & s = get_kbrd(1) & if s eq 'x' then stop


; ----------------------------------------------------------------------------
; Merge the data for all stars
; ----------------------------------------------------------------------------
 wire_merge_ww3,ttref(0,*),nstar,dat3,x3,y3,gc3,fwhm3,cnt3,wire3
; ----------------------------------------------------------------------------

; ----------------------------------------------------------------------------
yyr = robust_sigma(wire3(0:cnt3-1).mag(0)) * 10.0
dd0 = median(wire3(0:cnt3-1).mag(0))
plot,wire3(0:cnt3-1).hjd-t0,wire3(0:cnt3-1).mag(0),psym=3,/nodata,$
 xtit='HJD',ytit='!4D!3 mag',yr=[-1,1]*yyr+dd0
oplot,wire2.hjd(0)-t0,-2.5*alog10(wire2.p(0,ap_number(0)))+25.0,psym=3,col=col.red
oplot,wire3(0:cnt3-1).hjd-t0,wire3(0:cnt3-1).mag(0),psym=1,symsi=.1

;a = interpol(wire3(0:cnt3-1).mag(0),wire3(0:cnt3-1).hjd-t0,wire2.hjd(0)-t0)
;plot,wire2.hjd(0)-t0,-2.5*alog10(wire2.p(0,ap_number(0)))+25.0-dd0,psym=3,yr=[-1,1]*err*10.0,xr=[2,3]
;oplot,wire2.hjd(0)-t0,a-dd0,psym=3,col=col.green
;plot,wire2.hjd(0)-t0,$
;     ( -2.5*alog10(wire2.p(0,ap_number(0)))+25.0-dd0 ) - ( a-dd0 ), psym=3,xr=[2,3],yr=[-1,1]*err*10.0

; Progress plot:
 plot,wire3(0:cnt3-1).hjd-t0,wire3(0:cnt3-1).mag(refstar)-median(wire3(0:cnt3-1).mag(refstar)),$
  psym=3,xr=[-25,25],yr=[-1.5,1.5],ysty=1,xsty=3,xtit='!4D !3HJD',ytit='!4D!3m',$
  tit='Progress Plot For All Stars',/nodata
 for s=0,nstar-1 do oplot,wire3(0:cnt3-1).hjd-t0,$
  wire3(0:cnt3-1).mag(s)-median(wire3(0:cnt3-1).mag(s)) +1.2-0.34*s,psym=3
 for s=0,nstar-1 do xyouts,-12.1,1.25-0.34*s,'Star '+strcompress(string(s),/remove_all),charsi=1.0

skip_file:

; ----------------------------------------------------------------------------
 print,' %%% Merged points so far: ',cnt3
 print,''
; ----------------------------------------------------------------------------

endfor                          ; next restore file is read
; ----------------------------------------------------------------------------

wire3 = wire3(0:cnt3-1) ; remove unused data points

g = strsplit(input_dir,'/',/extract) & ng = n_elements(g)
outfile = '/'
for k=0,ng-2 do outfile = outfile + g(k) + '/'
; outfile = outfile + g(k-1) + '_merged_slot'+strcompress(string(star),/remove_all)+'.idl'
outfile = outfile + g(k-1) + '_merged_allslots_' + $
 strcompress(string(nmerge,format='(I7)'),/remove_all) + '.idl'


save, filename = outfile, wire3, mmag, err, t0
print,' %%% Saved "restore" file: '+outfile
; ----------------------------------------------------------------------------



END

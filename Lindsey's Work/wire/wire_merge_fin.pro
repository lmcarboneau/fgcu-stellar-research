; (c) Early September 2003 by Hans Bruntt
;     Improved version to import info from wire_flat.pro: MARCH 2004

; Purpose: 1. Compress data: "nmerge" parameter describes compression =
;          number of data points to combine to a single data point --
;          either using a sigma-clipped average or weights.
;          2. Merge different wire result files for all observed stars.

; Note: This program is used after wire_pos.pro, wire_getpos AND wire_flat.pro

PRO wire_merge_fin, input_dir, target, position_file,$
 progress_on=progress_on, magvar=magvar, refstar=refstar, nmer=nmer

; ============
; Keywords:
; ============
; nmer: number of data points to be merged. Default if 31!
; ============

; ----------------------------------------------------------------------------
; Examples:
; wire_merge_fin,'/data2/bruntt/wire/dat/EpsilonPeg/data/data_wire_*.idl1.fin','EpsilonPeg', '/data1/bruntt/wire/xy_positions2.idl'
; ----------------------------------------------------------------------------

; ----------------------------------------------------------------------------
; run_mode = 'Determine Correlation' ; altair
; run_mode = 'Process All Frames and Decorrelate'
; run_mode = 'Determine Correlation nmerge=51' ; HD113226 + beta_Umi
run_mode = 'nmerge=31' ; standard reduction since MARCH 3rd 2004
; ----------------------------------------------------------------------------

default9, progress_on, 0B
default9, magvar, 0.5

; ----------------------------------------------------------------------------
n_max = 10000. * 40. ; max number of data points
if n_elements(refstar) eq 0 then $ 
 refstar = 0   ; Slot number of main target 

; ----------------------------------------------------------------------------
restore,position_file ; info in wireinfo and xyinfo struct.s is from wire_getpos.pro
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

winf = where(strmatch(wireinfo.object,'*' + target + '*') eq 1,cinf)
if cinf eq 0 then begin
 print,' *** WARNING: The target: '+target+ ' ... is not in the wireinfo structure!'
 print,'              You must enter target, t0, xy-positons manually ... '
 print,'              Use the program wire_getpos.pro to get the information.'
 help,winf,xyinfo,wireinfo
 stop
endif

if cinf ge 2 then begin
 print,' *** WARNING: The target: '+target+ ' ... found several times in wireinfo!'
 for u=0,cinf-1 do $
  print,strcompress(u) + ': ' + wireinfo(winf(u)).object
 print,''
 print,' >>> Pick one: '
 sell = get_kbrd(1)
 if sell eq 'x' then stop
 sell = fix(sell)
 winf = winf(sell)
 cinf = 1
endif

; ==========================================================
print,' =========================================================='
print,' %%% wireinfo name is: ' + wireinfo(winf).object
print,' =========================================================='
print,' %%% If you want to add anything to this name, do it here...'
print,' %%% e.g. type "July2004" to change id to ' + wireinfo(winf).object + '_' + 'July2004'
print,' %%% This may help remove confusion on selection of secondary targets ... '
print, ''
addname = ''
read,' >>> Add to name: ',addname
addname = strcompress(addname,/remove_all)
; ==========================================================

; ==========================================================
if addname ne '' then begin
 wireinfo(winf).object = wireinfo(winf).object + '_' + addname
 save,filename=position_file,wireinfo,xyinfo
 print,' %%% Saved file: ' + position_file
endif
; ==========================================================

; ==========================================================
print,''
print,' %%% New name: ' + wireinfo(winf).object
print,''
; ==========================================================



t0 = wireinfo(winf).t0
xy = xyinfo(winf).xy(*,0:wireinfo(winf).nstars-1)

print,'wireinfo on this object: '
print, wireinfo(winf)
print,'Number of stars: ',wireinfo(winf).nstars
print,'Apertures to use: ',xyinfo(winf).aperture(0:wireinfo(winf).nstars-1)

refmag = xyinfo(winf).flux(0,refstar) ; 31MAR04

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
if run_mode eq 'nmerge=31' then begin
 nmerge = 31 ; number of points to merge (if time is within == timestep * nmerge)
 filmax = 99 ; max number of files to import (eg. 0-2 has a large range in x,y pos)
endif

; 29MAR2004: ... the 1999 data set has a lot of points ...
if strmatch(target,'*alphaCen*') eq 1 then begin
 nmerge = 151 ; number of points to merge (if time is within == timestep * nmerge)
 filmax = 199 ; max number of files to import (eg. 0-2 has a large range in x,y pos)
 print,' %%% Alpha Cen --- Many Data Points!! --- nmerge set to: ',nmerge
endif

if strmatch(target,'*betaUMi*') eq 1 then begin
 nmerge = 101 ; number of points to merge (if time is within == timestep * nmerge)
 filmax = 199 ; max number of files to import (eg. 0-2 has a large range in x,y pos)
 print,' %%% Beta UMi --- Many Data Points!! --- nmerge set to: ',nmerge
endif


; 29MAR2004:
a = strsplit(input_dir,'*',/extract)
if n_elements(a) eq 2 then begin
if strmatch(a(0),'*AlphaUMi*') eq 1 and $ 
  strmatch(a(1),'*obj2.idl1.fin') eq 1 then begin
 nmerge = 31 ; number of points to merge (if time is within == timestep * nmerge)
 filmax = 99 ; max number of files to import (eg. 0-2 has a large range in x,y pos)
 print,' %%% Alpha UMi --- Many Data Points!! --- nmerge set to: ',nmerge
endif
endif

if n_elements(nmer) eq 1 then nmerge = nmer ; keyword given?
if n_elements(nmerge) eq 0 then stop

; ----------------------------------------------------------------------------

print,' %%% '
print,' %%% Run mode: ' + run_mode
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

if na eq 0 or aa(0) eq '' then begin
  print,' %%% No data files found in dir: '+input_dir  
  RETURN
endif
; ----------------------------------------------------------------------------

; ----------------------------------------------------------------------------
print,''
print,' %%% I will merge these files: '
print,''
for kk=0,na-1 do print,aa(kk)
print,''
; ----------------------------------------------------------------------------

; ----------------------------------------------------------------------------
 window,0,title='WIRE Merging Process Window',xsize=500,ysize=350
 !P.charsize=1.2

if progress_on eq 1 then begin
 window,0,title='WIRE Progress Window',xsize=580,ysize=650
 !P.charsize=1.5
endif
; ----------------------------------------------------------------------------

if n_elements(xy) eq 0 then begin
 print,''
 print,' *** No xy array restored: x,y positions of stars in wire frame not known!'
 print,' *** investigate the wireult.x wireult.y positions after the first iteration ...'
 print,' *** eg. use prg. wire_getpos.pro (need wireult structure: restore approp. file).'
 print,''
endif

; ----------------------------------------------------------------------------
if na ge filmax then na = filmax
; ----------------------------------------------------------------------------

; ----------------------------------------------------------------------------
 for fil = 0,na-1 do begin ; for each idl1x file (from wire_pos.pro)

 print,''
 print,' %%% Restoring: ' + aa(fil)
 print,''

 restore,aa(fil) ; read the wire data structure (from program wire_pos.pro)
 nslot = n_elements(wirefin(0).x) ; number of stars put in slots

 if n_elements(xy) ne 0 then nstar = n_elements(xy(0,*)) else nstar = nslot

 ; plot,wirefin.gc(0,0)-3.5,wirefin.gc(1,0)-3.5,psym=3,xr=[-1,1],yr=[-1,1]
 ; oplot,wireult.gc(0,0)-3.5,wireult.gc(1,0)-3.5,psym=3,col=col.red
 ; wxxx = where( (wirefin.gc(1,0)-3.5) gt -0.2, cxxx) & help,cxxx

 if fil eq 0 then $
   wireult = replicate({hjd:0D, $
                      mag:fltarr(nstar),$                  
                     fwhm:fltarr(nstar),$
                    angle:fltarr(nstar),$
                     temp:0., $
                     back:fltarr(nstar),$ 
                       gc:fltarr(2,nstar), $
                        x:fltarr(nstar), $
                        y:fltarr(nstar)}, n_max)

; ----------------------------------------------------------------------------
; Determine which aperture to use:
; ----------------------------------------------------------------------------
ap_number = (xyinfo(winf).aperture(0:nstar-1)) ; found using wire_getpos.pro
wbadap = where(ap_number le 0,cbadap)
if cbadap ge 1 then begin
 print,' %%% I am suspecting the apertures are not correctly chosen?'
 stop
endif
; ----------------------------------------------------------------------------


; --------------------------------------------------------------------------
; Swap deleted ...

  if nstar ge 2 then begin

      ndat = n_elements(wirefin)

      dat = fltarr(nstar,ndat)
      times = dblarr(nstar,ndat)
      flag = fltarr(nstar,ndat)

      ; Select the best aperture:
      for j=0,nstar-1 do dat(j,*)  = wirefin.p(j,ap_number(j),*) ; light curve data
      for j=0,nstar-1 do flag(j,*) = wirefin.a(j,ap_number(j),*) ; bad/good data flag

     mmag = fltarr(2,nstar)  ; mean magn. and noise
     gc = wirefin.gc           ; x,y center position 
      x = wirefin.x            ; column & row position on CCD
      y = wirefin.y            ; see eg. /ai38/bruntt/wire/data/991019.3142.1AltairA7V.data
   fwhm = wirefin.fwhm         ; fwhm == measured seeing ==> decorrelation!
   back = wirefin.backgr2      ; measured background!
      
  endif else begin

; Need to debug this part ... if there is only one star !!!
      dat   = reform(wirefin.p(0,ap_number(0),*))
      ndat  = n_elements(dat)    ; number of data points
      times = dblarr(ndat)       ; hjd
      flag  = reform(wirefin.a(0,ap_number(0),*) )
     mmag = fltarr(2)  ; mean magn. and noise
     gc = reform(wirefin.gc)           ; x,y center position 
      x = reform(wirefin.x)            ; column & row position on CCD
      y = reform(wirefin.y)            ; see eg. /ai38/bruntt/wire/data/991019.3142.1AltairA7V.data
   fwhm = reform(wirefin.fwhm)         ; fwhm == measured seeing ==> decorrelation!
   back = reform(wirefin.backgr2)      ; measured seeing for each star --> decorrelation!

  endelse

; ----------------------------------------------------------------------------

; ----------------------------------------------------------------------------
    if nstar ge 2 then $
    w0 = where(dat(refstar,*) gt 10. and $
      abs(wirefin.hjd(refstar,*) - t0) lt 800.,c0 ) else $
    w0 = where(dat gt 10. and $
       abs(wirefin.hjd - t0) lt 800.,c0) ;


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
; Calculate DAOPHOT-like magnitude and then merge data! 
; ----------------------------------------------------------------------------
   print,' %%% Computing ref. time points from LC of ref. star: (0--100)'

   if nstar ge 2 then begin
    dd = reform( -2.5 * alog10 ( dat(refstar,w0) ) + 25.0 )
    time_ref = wirefin(w0).hjd(refstar)
   endif else begin
    dd = reform( -2.5 * alog10 ( dat(w0) ) + 25.0 )
    time_ref = wirefin(w0).hjd(0)
   endelse

; plot,time_ref-t0,dd,psym=3


; ----------------------------------------------------------------------------

; ----------------------------------------------------------------------------
; debug: (the "if sentence"!) BE CAREFUL IF YOU'RE USING MORE FILES !!!
; if (n_elements(ttref) eq 0) or (filmax ne 1) then $

   wire_merge_ref_time,time_ref,dd,t0,refmag,nmerge,ttref,ddref, aborting, magvar=magvar
   if aborting then goto,skip_file


; plot,time_ref-t0,dd,psym=3,yr=11.37 + [-1,1]*0.02,xr=[0.045,.05]; 
; for i=0,3500 do plots,ttref(0,i)-t0,!y.crange

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
  back3 = fltarr(nstar,ndat)
; =============================
   dat3(*,*)   =    -9.9
    gc3(*,*,*) =  -100.
      x3(*,*)  = -1000.
      y3(*,*)  = -1000.
   fwhm3(*,*) =     -5.5
   back3(*,*) = -199.
endif else begin
   ndat = n_elements(ttref(0,*))
   dat3 = fltarr(ndat)
   gc3  = fltarr(2,ndat)
    x3  = fltarr(ndat)
    y3  = fltarr(ndat)
  fwhm3 = fltarr(ndat)
  back3 = fltarr(ndat)
; =============================
   dat3(*)      =    -9.9
    gc3(*,*) =  -100.
      x3(*)     = -1000.
      y3(*)     = -1000.
   fwhm3(*)     =    -5.5
   back3(*)     = -199.
endelse

; ----------------------------------------------------------------------------
   
; ----------------------------------------------------------------------------
   tx = median(ttref(0,*))
   err = robust_sigma(ddref)
if progress_on eq 1 then begin
   plot,ttref(0,*)-tx,ddref,psym=3,yr=[-1,1]*err*10.0 + median(ddref),/nodata,$
    tit='Reference Light Curve (Time Points)'
   oplot,wirefin(w0).hjd(refstar)-tx,dd,psym=3,col=col.red
   oplot,ttref(0,*)-tx,ddref,psym=3
endif

;   tm = min(ttref(0,*))-t0 ; plot a single night?
;   plot,$ ; position=[.8,.8,.95,.95],/noerase,$
;        wirefin(w0).hjd(refstar)-tx,dd,psym=3,col=col.red,$
;        xr=[0,.1]+tm,yr=[-1,1]*0.01 + median(ddref)
; ----------------------------------------------------------------------------

; ----------------------------------------------------------------------------
 for star=0,nstar-1 do begin
; ----------------------------------------------------------------------------


; New stragegy, November 8th 2004:
fw = wirefin.fwhm(star) & gg_fw = where(fw gt .5 and fw lt 5,c_fw)
factor_fw =  float(c_fw) / n_elements(back(0,*))
if factor_fw lt 0.1 then begin
 print, ' %%% Very few valid FWHM: ',factor_fw
 goto,fail_star
endif

fwhml = median(fw(gg_fw)) 
limit_fwhm = 10. * robust_sigma(fw(gg_fw))
if limit_fwhm lt 0.1 then limit_fwhm = 0.1
fwhml_min = fwhml(0) - limit_fwhm ; accept 10 sigma deviation
fwhml_max = fwhml(0) + limit_fwhm
mm = 25. -2.5*alog10(wirefin.p(star,ap_number(star))) 
gg_mm = where(mm gt 5 and mm lt 25,cc_gg_mm)
if cc_gg_mm ge 100 then begin ; at least 100 reasonable points
  mag = fltarr(2) & mag(0) = median(mm(gg_mm)) & mag(1) = robust_sigma(mm(gg_mm))
  mag_min = mag(0) - 10. * mag(1) - 3.0 ; Max / min allowed magnitude range!
  mag_max = mag(0) + 10. * mag(1) + 3.0
endif else begin ; BAD DATA!
  stop
  mag = fltarr(2) & mag(0) = 15 & mag(1) = 10
  mag_min = mag(0) - 10. 
  mag_max = mag(0) + 10. 
endelse

if mag(1) lt 0 then begin ; Nov. 2006
 print,' %%% STDEV is negative !! '
 stop
endif

;fwhml = xyinfo(winf).fwhm(*,star)
;limit_fwhm = 10. * fwhml(1)
;if limit_fwhm lt 0.3 then limit_fwhm = 0.3
;fwhml_min = fwhml(0) - limit_fwhm ; accept 10 sigma deviation
;fwhml_max = fwhml(0) + limit_fwhm
;mag = xyinfo(winf).flux(*,star)
;mag_min = mag(0) - 10. * mag(1) - 3.0
;mag_max = mag(0) + 10. * mag(1) + 3.0

; Find good and bad data points!
   if nstar ge 2 then begin
    magn = reform(-2.5 * alog10(dat(star,*)) + 25.0)
    wg   = where(magn gt mag_min and magn lt mag_max and $
                 abs(wirefin.hjd(star,*)-t0) lt 800.0 and $
                 back(star,*) gt -20. and $
                 fwhm(star,*) ge fwhml_min and fwhm(star,*) le fwhml_max,cg)

    wbad = where(magn le mag_min or magn ge mag_max or $
                 abs(wirefin.hjd(star,*)-t0) ge 800.0 or $
                 back(star,*) le -20 or $
                 fwhm(star,*) lt fwhml_min or fwhm(star,*) gt fwhml_max,cbad)

   if cbad / float(cg) gt .1 then begin
      print,' %%% More than 10% bad points for star: ',star, cbad / float(cg)
      if cbad / float(cg) gt .4 then begin ; more than 40% bad points?
         plot,wirefin.hjd(star,*)-t0,magn,$
          yr=mag(0) + [-1,1]*mag(1)*6.,psym=3,xr=[-1,1]*2.
          ref = -2.5 * alog10(dat(0,*)) + 25.0 & ref = ref - median(ref)
          oplot,wirefin.hjd(0,*)-t0,ref + mag(0),psym=2,symsi=.5,col=col.sky ; The main target LC
          print,' %%% White dots = current star. Blue points = main target'
;         s9='' & hitme,mess=' TO STOP HERE = press x',s9 & if s9 eq 'x' then stop
      endif
   endif


  endif else begin ;;; updated 01 APR 2004
    magn = -2.5 * alog10(dat) + 25.0
    wg   = where(magn gt mag_min and magn lt mag_max and $
                 abs(wirefin.hjd - t0) lt 800.0 and $
                 back gt - 20. and $
                 fwhm ge fwhml_min and fwhm le fwhml_max,cg)

    wbad = where(magn le mag_min or magn ge mag_max or $
                 abs(wirefin.hjd - t0) ge 800.0 or $
                 back le -20 or $
                 fwhm lt fwhml_min or fwhm gt fwhml_max,cbad)
  endelse

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
     back(star,wbad)   = -199.
    endif
    
    if cg ge 20 then begin
       times(star,wg)  = wirefin(wg).hjd(star) ; 
    
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
     back_temp = reform(back(star,wg))
    ; ----------------------------------------------------------------------------
 endif

;; November 2006:
;; if strmatch(aa(fil),'*ThetaCen_007.obj1*') and star eq 4 then stop


endif else begin ; only one star present?

    if cbad ge 1 then begin
       dat(wbad)  =   -9.9
       gc(*,wbad) =  -99.9 ; gaussian (x,y)-center position
        x(wbad)   = -900.
        y(wbad)   = -900.
     fwhm(wbad)   = -2.5
     back(wbad)   = -199.
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
    
       times(wg)  = wirefin(wg).hjd(star) ; 
    ; ----------------------------------------------------------------------------
       tt      = reform(times(wg))      
       dd      = reform(dat(wg)) 
       gc_temp = reform(gc(*,wg))
        x_temp = reform(x(wg))      
        y_temp = reform(y(wg))
     fwhm_temp = reform(fwhm(wg))
     back_temp = reform(back(wg))
    ; ----------------------------------------------------------------------------
    endif

endelse ; only one star ...

; plot,tt-t0,dd,psym=1,symsi=.2,ysty=3,xr=[-1,1]*0.005 - 19.385
; oplot,wirefin.hjd(0)-t0,-2.5*alog10(wirefin.p(0,4))+25.0,psym=4,symsi=.2,col=col.green

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
;   mg = -2.5 * alog10(wirefin.p(refstar,ap_number)) + 25.
;   oplot,wirefin.hjd(refstar,*)-t0,mg-median(mg),psym=3,col=col.red

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

init = 0B & if fil eq 0 then init = 1B

; Merge data points (last two parameters: weights=1, debug=0)
   wire_merge_dat_any,$
    tt , dd , x_temp, y_temp, gc_temp, fwhm_temp, back_temp, ttref, $
    ttn, ddn, xn    , yn    , gcn    , fwhmn    , backn,     nmerge, $
    dmag, $
    t0, $
    0.10, $ ; how close must (x,y) pos be within each (nmerge) block (0.01-0.1 pix?)
    1, 9, $ ; last parameter: 1, 0 ... change to 1, 1 to debug! x = exit
    init, star, wireult ; first entry: init = the first structure to import
            ; ie. 
            ; it may have several bad points from the previous WIRE primary target
; 

; Put merged data in temporary result arrays
if nstar ge 2 then begin
      dat3(star,*) = ddn ; put merged data in result arrays
      for k=0,1 do gc3(k,star,*) = gcn(k,*)
       x3(star,*)  = xn
       y3(star,*)  = yn
    fwhm3(star,*)  = fwhmn
    back3(star,*)  = backn
endif else begin
      dat3 = ddn ; put merged data in result arrays
      for k=0,1 do gc3(k,*) = gcn(k,*)
       x3  = xn
       y3  = yn
    fwhm3  = fwhmn
    back3  = backn
endelse
; ----------------------------------------------------------------------------

; ----------------------------------------------------------------------------
!P.multi = [0,2,3]
!P.charsize=1.3
!P.charthick=1.5       

   t000 = min(tt)
   err = robust_sigma(dd)

if progress_on eq 1 then begin
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
endif

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
 wire_merge_ww3,ttref(0,*),nstar,dat3,x3,y3,gc3,fwhm3,back3,cnt3,abort_mode,wireult
; ----------------------------------------------------------------------------

if abort_mode eq 1 then begin
 print,' %%% Array wire3 too small. Aborted merging. Try setting nmerge or n_max higher!'
 stop
endif

; ----------------------------------------------------------------------------
if cnt3 le 5 then begin
 print,' %%% Too few valid data points found in wire_merge_fin.pro !'
 goto,skip_file
endif

yyr = robust_sigma(wireult(0:cnt3-1).mag(0)) * 10.0
dd0 = median(wireult(0:cnt3-1).mag(0))
if progress_on eq 1 then begin
 plot,wireult(0:cnt3-1).hjd-t0,wireult(0:cnt3-1).mag(0),psym=3,/nodata,$
  xtit='HJD',ytit='!4D!3 mag',yr=[-1,1]*yyr+dd0
 oplot,wirefin.hjd(0)-t0,-2.5*alog10(wirefin.p(0,ap_number(0)))+25.0,psym=3,col=col.red
 oplot,wireult(0:cnt3-1).hjd-t0,wireult(0:cnt3-1).mag(0),psym=1,symsi=.1
endif

;a = interpol(wireult(0:cnt3-1).mag(0),wireult(0:cnt3-1).hjd-t0,wirefin.hjd(0)-t0)
;plot,wirefin.hjd(0)-t0,-2.5*alog10(wirefin.p(0,ap_number(0)))+25.0-dd0,psym=3,yr=[-1,1]*err*10.0,xr=[2,3]
;oplot,wirefin.hjd(0)-t0,a-dd0,psym=3,col=col.green
;plot,wirefin.hjd(0)-t0,$
;     ( -2.5*alog10(wirefin.p(0,ap_number(0)))+25.0-dd0 ) - ( a-dd0 ), psym=3,xr=[2,3],yr=[-1,1]*err*10.0

; Progress plot:
if progress_on eq 1 then begin
 plot,wireult(0:cnt3-1).hjd-t0,wireult(0:cnt3-1).mag(refstar)-median(wireult(0:cnt3-1).mag(refstar)),$
  psym=3,xr=[-25,25],yr=[-1.5,1.5],ysty=1,xsty=3,xtit='!4D !3HJD',ytit='!4D!3m',$
  tit='Progress Plot For All Stars',/nodata
 for s=0,nstar-1 do oplot,wireult(0:cnt3-1).hjd-t0,$
  wireult(0:cnt3-1).mag(s)-median(wireult(0:cnt3-1).mag(s)) +1.2-0.34*s,psym=3
 for s=0,nstar-1 do xyouts,-12.1,1.25-0.34*s,'Star '+strcompress(string(s),/remove_all),charsi=1.0
endif

skip_file:

; ----------------------------------------------------------------------------
 print,' %%% Merged points so far: ',cnt3
 print,''
; ----------------------------------------------------------------------------

endfor                          ; next restore file is read
; ----------------------------------------------------------------------------

if cnt3 le 10 then begin
 print,' %%% Only ',cnt3,' valid points ... something is wrong here!'
 stop
endif

wireult = wireult(0:cnt3-1) ; remove unused data points

g = strsplit(input_dir,'/',/extract) & ng = n_elements(g)
outfile = '/'
for k=0,ng-2 do outfile = outfile + g(k) + '/'
; outfile = outfile + g(k-1) + '_merged_slot'+strcompress(string(star),/remove_all)+'.idl'
outfile = outfile + target + '_wire' + $
 strcompress(string(nmerge,format='(I7)'),/remove_all) + '.idl'


save, filename = outfile, wireult, mmag, err, t0
print,' %%% Saved "restore" file: '+outfile
; ----------------------------------------------------------------------------


; ----------------------------------------------------------------------------
nl = ' ============================================'
print,''
print,nl
print,' %%% To process in wire analysis GUI:'
print,nl
print,' file = "'+outfile+'"'
print,' restore,file  &  target = "'+target+'"'
print,' wirep,wireult,file,target,1 ; ,$'
print,'  ; f="~/wire/wire_process/wirep_"'+target+'".idl"'
print,''
; ----------------------------------------------------------------------------

print,' *** JD --> HJD ? '
print,''

END

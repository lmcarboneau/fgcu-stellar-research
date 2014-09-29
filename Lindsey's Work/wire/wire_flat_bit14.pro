PRO wire_flat_bit14, file, phot_mode, position_file, target, seeing_mode, fixed_fwhm

bit14 = long(2.^14.)

seeing_mode = 'variable_fwhm'
if fixed_fwhm(0) gt .5 then begin
 print,' WARNING! Using a fixed FWHM for each star ...'
 print,fixed_fwhm
 seeing_mode = ''
 print,'Hit any key go to on ... '  &  s = get_kbrd(1)
endif

stop

; ---------
; Example:
; ---------
; After running wire_all.pro and wire_pos.pro + wire_getpos.pro to get
; the xy position of each star on the CCD (to be used below) ... do
; this:
; ---------
 
; basedir = '/data2/bruntt/wire/dat/ProcyonF5IV-V/'
; spawnrob,'ls -1 '+basedir + 'data/data_wire_*.idl',files
; wire_flat,files, 1, '/data1/bruntt/wire/xy_positions.idl', 'ProcyonF5IV-V'
; ---------

; Debug:
; spawnrob,'ls -1 /data2/bruntt/wire/dat/ProcyonF5IV-V/data/data_wire_*.idl',file
; spawnrob,'ls -1 /data2/bruntt/wire/dat/ProcyonF5IV-V/data/data_wire_006.idl',file
; target = 'ProcyonF5IV-V'
; phot_mode = 1
; position_file = '/data1/bruntt/wire/xy_positions.idl'
;;; merge_file = '/data2/bruntt/wire/dat/ProcyonF5IV-V/data/data_merged_allslots_51.idl'


; ---------------------------
; mode = 'determine_ff' 
mode = 'get_lc'
; ---------------------------

; Purpose:
; ---------
; Read wire result file(s) from the prg. wire_all.pro & the merged
; result from wire_merge3.pro: RE ANALYSE DATA --- TAKE INTO ACCOUNT
;                                                  THE (UNKNWON) FLAT FIELD

; AN ATTEMPT TO MEASURE THE FLAT FIELD: IMAGE(x,y) = [PSF(x,y) + BG(x,y)]* FLAT(x,y)
; Run this after wire_merge3.pro: need info on times, fwhm & positions.
; merge_file = result from wire_pos.pro

; Examples:
; ----------

; 8x8 array: PSF residuals IM_0 = IM_org - PSF(gauss) - BG
; restore,'~bruntt/wire/wire_psf_residual.idl'
; note that your must scale with the flux level:
; For Procyon the median flux in aperture 7 was: 285664. ADU
; A random PSF had total(psf) = 286090. ADU ... so this is a good estimate.
;;; psf_residual = psf_residual 

; 8x8 array: Flat field? array is: flat_residual
; restore,'~bruntt/wire/wire_flat_residual.idl'

!P.charthick=1.0
!P.multi=0
!P.charsize=1.5
 !x.thick = 1
 !y.thick = 1

nf = n_elements(file)
if nf eq 0 then begin
 print,' *** NO input files!'
 return
endif

repair = 0L
edge = intarr(8,8)
edge(0,*) = 1 & edge(*,0) = 1 & edge(7,*) = 1 & edge(*,7) = 1
edge(1,1) = 1 & edge(6,1) = 1 & edge(1,6) = 1 & edge(6,6) = 1
wedge = where(edge eq 1,cedge)

backccd = intarr(8,8)
backccd(0:1,0) = 1    &  backccd(0,0:1) = 1
backccd(6:7,0) = 1    &  backccd(0,6:7) = 1
backccd(7,0:1) = 1    &  backccd(0:1,7) = 1
backccd(6:7,7) = 1    &  backccd(7,6:7) = 1
wback = where(backccd eq 1,cback)


repcnt = 0L & repmax = 200000 & repbad = lonarr(repmax)


; FINAL FLAT FIELD:
flat = fltarr(512,512)
flat(*,*) = -13.
; TEMP FLAT FIELD FOR SUMMING UP THE FLAT FIELDS
cnu_max  = 50 & flat_all  = fltarr(cnu_max ,512,512) & flat_all(*,*,*)  = -99.
cnu_max2 = 50 & flat_all2 = fltarr(cnu_max2,512,512) & flat_all2(*,*,*) = -99.
cn_all2 = 0

; ==================================
wrap2 = 0B ; double warp check is off (default)
; ==================================
; Make sure to only wrap star at the center !!
 xp = 260
 yp = 260

if strmatch(file(0),'*Procyon*') eq 1 then begin ; saturation -- double trapped numbers!
 wrap2 = 1B
 wrapval = 3800.
 wraplim = 3500.
 x11 = 3 & x22 = 4
 y11 = 3 & y22 = 4 
endif

; NEW alpha cen measurements are WRAPPED ... but not the old ones
; ... texp was only 0.1 sec back then, in 1999 !!
if strmatch(file(0),'*AlphaUMi*obj2*') eq 1 then begin ; saturation -- double trapped numbers!
 wrap2 = 1B
 wrapval = 15000.
 wraplim = 20000. ; will this work?
 x11 = 3 & x22 = 4
 y11 = 3 & y22 = 4
endif


; saturation -- double trapped numbers!
if strmatch(file(0),'*AlphaBoo*') eq 1 then begin 
 wrap2 = 1B
 wrapval = 24000.
 wraplim =  5000.
 x11 = 3 & x22 = 4
 y11 = 3 & y22 = 3 
endif

; saturation -- double trapped numbers! SOMEONE HAS CHANGED THE DATA ALREADY?
;if strmatch(file(0),'*AlphaCen*') eq 1 then begin 
; wrap2 = 1B
; wrapval = 24000.
; wraplim =  5000.
; x11 = 3 & x22 = 4
; y11 = 3 & y22 = 3 
;endif


; ==================================

; ==================================
cn = findgen(2,8,8)
for i=0,7 do begin
 for j=0,7 do begin
  cn(0,i,j) = i
  cn(1,i,j) = j
 endfor
endfor
; ==================================


; Aperture photometry mode:
;  phot_mode = 1 ; faster ... less resistant
;  phot_mode = 2 ; slower, more resistant?
;  phot_mode = 3 ; even slower, more resistant?
if n_elements(phot_mode) eq 0 then phot_mode = 1

case phot_mode of
 1: print,' %%% Photometry mode: faster ... less resistant'
 2: print,' %%% Photometry mode: slower, more resistant?'
 3: print,' %%% Photometry mode: even slower, more resistant?'
endcase

col = getcolor(/load)
progress_on = 9
; 0 = all off, 1 = plot for every star, 2 = more progress plots (slower!)

; recommended settings: 2 = show first part of LC on screen
;                       9 = do a .ps plot for every 1e4th image
;                       ... contour + surface plots
; 11 = contours on screen of selected data-index range

if progress_on ge 1 and progress_on le 2 then begin
  !P.multi = [0,2,2] ; multiple plots = progress
  window,0,xsize=450,ysize=450,title='WIRE - Flat field determination'
endif

if progress_on eq 9 then begin
 !P.multi = [0,2,5]
 x = 18 & y = 26 & set_plot, 'ps'
 qq = strsplit(file(0),'/',/extract)
 psname = '/' & for zz=0,n_elements(qq)-2 do psname = psname + qq(zz) + '/'
 psname = psname + target+ '_wire_reduc.ps'
 print,' %%% Wire reduction .ps file : '
 print,' $  ggv  '+psname + '  &  '
 device, xsize=x, ysize=y-0.4, yoffset=26.9-y, filename=psname
endif


; ----------------------------------------------------------------------------

 for ff=0,nf-1 do begin

print,' %%% Restoring file '+file(ff)+' ...'
if file(ff) eq '' then begin
 print,' %%% Null filename ... returning from wire_pos.pro'
 !P.multi=0
 if progress_on ge 9 then begin
  device,/close
  set_plot,'x'
 endif
 RETURN ; terminate program
endif


; debug: normally this should be turned on!!
restore,file(ff)

ndp = n_elements(wire)
nslot = 5
; debug:
; ndp = 9000
; dp_start = 10000
dp_start = 0L
if ndp lt 10 then stop

; Counter for final wire results array

; The original aperutes ... first Altair reduction (must be the same
wire_get_apertures, nap, apertures, apuse ; apusexx: don't do all apertures!

; If position_file is given as input, use only the best aperture!
 restore,position_file

 winf = where(strmatch(wireinfo.object,'*' + target + '*') eq 1,cok)

 if cok ne 1 then begin
  print,'' &  print,' *** Warning: Star not found in wireinfo structure:'
  print,' *** Did you run wire_getpos.pro for this star?' &  print,''
  !P.multi=0
  if progress_on ge 9 then begin
   device,/close
   set_plot,'x'
  endif
  RETURN
 endif


 aps = xyinfo(winf).aperture(0:wireinfo(winf).nstars-1)
 a = sort(aps)
 aps = aps(a)
 g = uniq(aps)
 g2 = aps(g) ; only do the apertures listed here ...
 apuse(*) = 0
 apuse(g2) = 1

 t0 = wireinfo(winf).t0
 xy = xyinfo(winf).xy(*,0:wireinfo(winf).nstars-1)

 nstars = wireinfo(winf).nstars
 cn_fin = lonarr(nstars)


 print,'wireinfo on this object: '
 print, wireinfo(winf)
 print,'Number of stars: ' ,wireinfo(winf).nstars
 print,'Apertures to use: ',xyinfo(winf).aperture(0:wireinfo(winf).nstars-1)

; In wire3 the saturated pixels are increased by 2^16.,
; thus the counts are correct, and the flux can be calculated!
wiref   = replicate({d:fltarr(nslot,8,8)}, ndp)
; wiref.d = wire.d
; debug:
wiref.d = wire(dp_start:dp_start+ndp-1).d

; Correct for counts << .0
add_digital_sat = 2.^16
for i=0,nslot-1 do begin ; for each CCD-cut out (wire == 5)
 for j=0L,ndp-1 do begin ; for each star (usually of the order 100.000)

    cut = reform(wire(j).d(i,*,*)) ; the original data

    w = where(cut lt -100 and cut gt -66000.,c)
    if c ge 1 then begin
       cut(w) = cut(w) + add_digital_sat 
       wiref(j).d(i,*,*) = cut 
       ; add 65536 if data is saturated (beware of low background counts)
    endif

   ; device,/close & set_plot,'x' & !P.multi = 0

    if wrap2 eq 1 and $
      abs(wire(j).x(i) - xp) lt 10. and $
      abs(wire(j).y(i) - yp) lt 10. then begin ; Procyon / ACen double wrap!
      w2 = where( cn(0,*,*) ge x11 and cn(0,*,*) le x22 and $
                  cn(1,*,*) ge y11 and cn(1,*,*) le y22 and $
                  cut gt 100. and $
                  abs(cut - wrapval) le wraplim, c2)
       if c2 ge 1 then begin
          cut(w2) = cut(w2) + add_digital_sat 
          wiref(j).d(i,*,*) = cut 
       endif

     endif

 endfor 
endfor




wire_get_wirearr, wire2, nslot, nap, ndp

; Overall x,y position!
;wire2.x = wire.x 
;wire2.y = wire.y
;wire2.hjd = wire.hjd
; debug
wire2.x   = wire(dp_start:dp_start+ndp-1).x
wire2.y   = wire(dp_start:dp_start+ndp-1).y
wire2.hjd = wire(dp_start:dp_start+ndp-1).hjd

xm = fltarr(nslot)
ym = fltarr(nslot)

for i=0,nslot-1 do begin ; overall x,y position of the 5 stars
  xm(i) = median(wire2.x(i,*))
  ym(i) = median(wire2.y(i,*))
endfor

; Loop to compute centre of light and overall flux level
print,''

for i=0,nslot-1 do begin
;debug:
;for i=0,0 do begin
;for i=0,2 do begin

; Progress indication:
 print,''
 print,'Star number = '+string(i,format='(I2)')
 print,'Data point out of '+string(ndp,format='(I6)') + ':'

if xm(i) le 0. or ym(i) le 0. then begin
 print,' *** Star ',i,' seems to have no valid xy positions ... '
 goto,fail_star
endif


; for j=0L,ndp-1 do begin
; Debug:
 for j=dp_start,ndp+dp_start-1 do begin
  if j mod 5000 eq 0 then print,j,format='(I7,$)'

  j2 = j - dp_start ; replace wire2(j and wiref(j with ... (j2

; >>> Determine the centre of light in each frame
  colight_x = 0.
  colight_y = 0.
  
  for x1 =0,7 do begin
   for y1 = 0,7 do begin
     colight_x = colight_x + (x1+0.5-4.) * wiref(j2).d(i,x1,y1)
     colight_y = colight_y + (y1+0.5-4.) * wiref(j2).d(i,x1,y1)     
   endfor
  endfor

  flux = total(wiref(j2).d(i,*,*)) ; ignoring background --> buzasi photometry

  colight_x = (colight_x / flux) + 3.5
  colight_y = (colight_y / flux) + 3.5

; End of centre of light <<<

;       a(0) = A0 = constant term.
;       a(1) = A1 = scale factor.
;       a(2) = a = width of gaussian in X direction.
;       a(3) = b = width of gaussian in Y direction.
;       a(4) = h = center X location.
;       a(5) = k = center Y location.
;       a(6) = T = Theta the rotation of the ellipse from the X axis
;               in radians, counterclockwise.

; Fit a gaussian ... starting parameters:
 ccd = float( reform(wiref(j2).d(i,*,*)) )

; Make sure max pixels are near the center of the ccd
 if max(ccd(wedge)) / max(ccd(2:5,2:5)) gt 0.9 then begin
     wire2(j2).flux1(i) = -17.7
     wire2(j2).fwhm(i) = -5
    goto,skip_data
 endif

; plot,wire2(0:j2-1).p(i,best),yr=[-2000,2000]+median(wire2(0:j2-1).p(i,best)),psym=3,xr=[2500,5000]


if progress_on eq 11 and j2 gt 2500 and j2 lt 15200 then begin
  if j2 mod 25 eq 0 then begin

 !P.multi=[0,4,1]
 !P.charsize= 1.5

    divv = 10000.
    mx = (round(max(ccd)/100.) * 100.) 
    lev = (findgen(10) * mx ) / 10.
    lev = lev / divv

 contour,ccd/divv,charsi=2,$
  levels = lev,c_labels=[1,0,1,0,1,0,1,0,1,0],xsty=1,ysty=1, $
  title = string(aa1(2),format='(F7.3)') + ' ' + $
   string(aa1(3),format='(F7.3)') 

 contour,wire(j2).d(0,*,*)/divv,charsi=2,$ ; org data
  levels = lev,c_labels=[1,0,1,0,1,0,1,0,1,0],xsty=1,ysty=1, $
  title = ' ORG DATA '




 surface,ccd,charsi=3.0,zr=[0,1e5],$
  title = string(background2,format='(I5)') + ' ' + $
   string(background5,format='(I7)') 

 wi = where(wire2.p(0,4) gt 500.,c)
 plot,wire2(wi).p(0,4),psym=3,ysty=3,yr=[5.0,5.5]*1e5,charsi=2.5


 print,j2
 print,' Hit me ... '   &  s = get_kbrd(1)

 endif 
endif

; I use a more refined method below !!!
; max_cen = max(ccd(2:5,2:5))
; max_oth = max(ccd)
; if max_oth / max_cen gt 1.1 then begin ; central pixels do not contain the maximum!
;   wire2(j2).flux1(i) = -19.9
;   goto,skip_data
; endif

 ; -------------------------------------------------
 ; Program with new format: often 65000+ pixels occur!
 ; ------------------------------------------------- 
 wmax = where(ccd gt 65000., cmax)
 wmin = where(ccd lt 65000. and ccd gt 30000,cmin)
 wed  = where(ccd gt 65000. and edge eq 1,ced)

 if ced ge 1 and (repcnt+ced-1) lt repmax then begin
  repbad(repcnt:repcnt+ced-1) = ccd(wed)
  repcnt = repcnt + ced
 endif


 if cmax ge 7 then begin ; many spurious points 
    wire2(j2).flux1(i) = -19.9
    goto,skip_data
 endif

 if (cmax ge 1 and cmin eq 0) or (ced ge 1) then begin

   ; Take the CCD value along the edge of the CCD:
   rep = [ reform(ccd(0,*)), reform(ccd(7,*)), ccd(1:6,0), ccd(1:6,7) ] 
   ; Make sure the values are not very high ...
   wrr = where(rep lt 2000,crr)
   ; If ok, replace by the median value of the edges ...
   if (crr ge 10) and (ced lt 7) and (ced gt 1) then begin
    replace   = median(rep(wrr))
    ccd(wmax) = replace ;; + randomn(seed

    repair    = repair + 1

    endif else goto,skip_data

endif
 ; -------------------------------------------------



 aa = fltarr(7)
 aa(0) = avg([ccd(0,0),ccd(0,7),ccd(7,0),ccd(7,7)]) ; median(wiref(j2).d(i,*,*))
 aa(1) = (max(ccd(2:5,2:5)) - aa(0)) * 0.95
 aa(2) = 0.9 & aa(3) = 0.9             ; typical gaussian sigma = FWHM / 2.35 
 aa(4) = colight_x & aa(5) = colight_y ; good guess at x,y center

 ss = sort(ccd)
 uu = uniq(ccd(ss))
 difcnt =  aa(1) - aa(0) ; max pixel value - background
 totcnt = total(ccd) - 64. * aa(0) ; approx. flux value - background

 if (n_elements(uu) lt 8) or $
        difcnt lt 50. or $
        totcnt lt 100 then begin ; bad data 
     wire2(j2).co(0,i)  = -9.9
     wire2(j2).co(1,i)  = -9.9
     wire2(j2).flux1(i) = -9.9
     wire2(j2).flux2(i) = -9.9
     goto,skip_data ; bad data     
 endif

 if (aa(1) lt (0.5 * aa(0))) or (aa(1) lt 50.0) then $
  goto,skip_data ; very low counts!

; device,/close & set_plot,'x' & !P.multi = 0
; ccd = float( reform(wiref(j2).d(i,*,*)) )
; surface,ccd,charsize=3

  if strmatch(file(ff),'*AlphaUMi_wire_AlphaUMi_001.obj2*') then begin
   if i eq 4 and j2 gt 21768 and j2 lt 22120 then goto, skip_data
   if i eq 3 and j2 gt 19480 and j2 lt 19630 then goto, skip_data
   if i eq 1 and j2 gt 59760 and j2 lt 99999 then goto, skip_data
  endif

  if strmatch(file(ff),'*AlphaUMi_wire_AlphaUMi_002.obj2*') then begin
   if i eq 0 and j2 gt  8235 and j2 lt 10840 then goto, skip_data
   if i eq 0 and j2 gt 45610 and j2 lt 50420 then goto, skip_data
;   if i eq 2 and j2 gt 31540 and j2 lt 50420 then goto, skip_data
  endif

  if strmatch(file(ff),'*AlphaUMi_wire_AlphaUMi_002.obj2*') then begin
   if i eq 0 and j2 gt  8235 and j2 lt 10840 then goto, skip_data
   if i eq 0 and j2 gt 45610 and j2 lt 50420 then goto, skip_data
;   if i eq 2 and j2 gt 31540 and j2 lt 50420 then goto, skip_data
  endif

  if strmatch(file(ff),'*AlphaUMi_wire_AlphaUMi_003.obj2*') then begin
   if i eq 2 and j2 gt 49100 and j2 lt 50101 then goto, skip_data
;   if i eq 2 and j2 gt 31540 and j2 lt 50420 then goto, skip_data
  endif


  if strmatch(file(ff),'*epsilonCyg/data_wire_002.idl*') then begin
   if i eq 4 and j2 ge 16530 and j2 lt 16635 then goto, skip_data
  endif


; Exception for epsilonUMa
  if strmatch(file(ff),'*epsilonUMa*') and $
     strmatch(file(ff),'*data_wire_019.idl*') and $
   i eq 0 and $
   j2 gt 70480 and j2 lt 70500. then goto, skip_data

  if strmatch(file(ff),'*betaLeo*') then begin
    if  strmatch(file(ff),'*data_wire_005.idl*') and $
    i eq 2 and $
    j2 gt 39534 and j2 lt 39566 then goto, skip_data

    if  strmatch(file(ff),'*data_wire_005.idl*') and $
    i eq 4 and $
    j2 gt 96981 and j2 lt 96986 then goto, skip_data
  endif

;     device,/close & set_plot,'x' & !P.multi=0 & wset,0
;     surface, wiref(j2+300).d(i,*,*)

; Exception for 
  if strmatch(file(ff),'*NSV9189*') then begin
     if  strmatch(file(ff),'*data_wire_001.idl*') and $
     i eq 0 and $
     j2 gt 12596 and j2 lt 12606 then goto, skip_data

     if  strmatch(file(ff),'*data_wire_001.idl*') and $
     i eq 1 and $
     j2 gt 3992 and j2 lt 5645 then goto, skip_data

     if  strmatch(file(ff),'*data_wire_001.idl*') and $
     i eq 3 and $
     j2 gt 6126 and j2 lt 6150 then goto, skip_data

     if  strmatch(file(ff),'*data_wire_001.idl*') and $
     i eq 3 and $
     j2 gt 6457 and j2 lt 6800 then goto, skip_data

     if  strmatch(file(ff),'*data_wire_001.idl*') and $
     i eq 4 and $
     j2 gt 3646 and j2 lt 3750 then goto, skip_data

     if  strmatch(file(ff),'*data_wire_001.idl*') and $
     i eq 4 and $
     j2 gt 4420 and j2 lt 5920 then goto, skip_data

     if  strmatch(file(ff),'*data_wire_003.idl*') and $
     i eq 2 and $
     j2 gt 93886 and j2 lt 96190 then goto, skip_data

     if  strmatch(file(ff),'*data_wire_003.idl*') and $
     i eq 1 and $
     j2 gt 85000 and j2 lt 97000 then goto, skip_data

  endif

  if strmatch(file(ff),'*ZetaOph*') then begin
     if  strmatch(file(ff),'*ZetaOph_wire_ZetaOph_008.obj2*') eq 1 and $
     i eq 1 and $
     j2 gt 9467 and j2 lt 9535 then goto, skip_data
  endif

  if strmatch(file(ff),'*ZetaOph*') then begin
     if  strmatch(file(ff),'*ZetaOph_wire_ZetaOph_009.obj2*') eq 1 then begin

     if i eq 0 and j2 gt 41674 and j2 lt 41700 then goto, skip_data
     if i eq 4 and j2 gt 50243 and j2 lt 50275 then goto, skip_data

     endif

     if  strmatch(file(ff),'*ZetaOph_wire_ZetaOph_012.obj2*') eq 1 then begin

       if i eq 1 and j2 gt 47331 and j2 lt 47350 then goto, skip_data
       if i eq 2 and j2 gt 31165 and j2 lt 31238 then goto, skip_data
       if i eq 3 and j2 gt 14194 and j2 lt 15295 then goto, skip_data

     endif

  endif


 aa1 = aa ; initial guesses ...
 gg1 = gauss2dfit(ccd,aa1) ; fit all pixels

 ; first fit bad? ---> try again, but only if max counts in star > background

 fwhm_1 = avg(abs([aa1(2:3)]))
 if ( (fwhm_1 lt 0.5) or (fwhm_1) gt 5.5 or $
      (abs((aa1(1)-aa(1))/aa(1)) gt 2.5) ) and $
    (aa(1) gt aa(0)) then begin ; first fit bad, but there's plenty of signal!

;  ccd = float( reform(wiref(j2+15).d(i,*,*)) )

; Exception for Procyon:
  if strmatch(file(ff),'*ProcyonF5IV-V*') and $
     strmatch(file(ff),'*data_wire_007.idl*') and $
   i eq 3 and $
   j2 gt 30802 and j2 lt 30808 then goto, skip_data

; Exception for betaLeo:
  if strmatch(file(ff),'*betaLeo*') then begin

   if strmatch(file(ff),'*data_wire_001.idl*') and $
   i eq 1 and $
   j2 gt 45005 and j2 lt 45012 then goto, skip_data

   if strmatch(file(ff),'*data_wire_004.idl*') and $
   i eq 1 and $
   j2 gt 7558 and j2 lt 7701 then goto, skip_data

  endif


; Exception for :
  if strmatch(file(ff),'*epsilonUMa*') and $
     strmatch(file(ff),'*data_wire_016.idl*') and $
   i eq 0 and $
   j2 gt 35375 and j2 lt 35400. then goto, skip_data

;  print, fix( reform(wiref(j2+findgen(10) * 100 + 1300).d(i,*,*)) )

  aa2 = aa
  aa2(4) = aa2(4) - 1.
  aa2(5) = aa2(5) - 1. ; offset central position one pixel
  gg2    = gauss2dfit(ccd(1:6,1:6),aa2) ; fits only central 6x6 pixels
  fwhm_2 = avg(abs([aa2(2:3)]))

      if (fwhm_2 lt 0.5) or (fwhm_2 gt 5.5) or $
         (abs((aa2(1)-aa(1))/aa(1)) gt 2.5) then begin
           wire2(j2).flux1(i) = -99.9 ; second fit was also bad
           goto,skip_data
      endif

  aa    = aa2        ; second fit was ok
  aa(4) = aa(4) + 1.
  aa(5) = aa(5) + 1. ; remove (1,1)-offset central position one pixel
  gg = gg2
 endif else begin

 ; low signal ... gaussfit2d often fails!
  if (aa(1) lt aa(0)) then goto, skip_data
  aa = aa1 ; first fit was ok
  gg = gg1

 endelse

 background = aa(0) ; gaussian fit ==> sky background

; bb = [ccd(0,0),ccd(0,7),ccd(7,0),ccd(7,7)]
; background5 = avg(bb) ; bg = corners of ccd

; ADDED 2-APR-2004: 12 pixels instead of only 4 ... 
 bb33 = ccd(wback)
 resistant_mean,bb33,3,background2,sd,nr
 background2_store = background2

; BEWARE:
;  if strmatch(file(0),'*AlphaUMi*obj2*') eq 1 then begin
;    if abs(wire2(j2).x(i)-260.) lt 15. and $
;       abs(wire2(j2).y(i)-260.) lt 15. then background2 = 0.
;  endif

 ; if abs((background2 - background5) / background2) gt 1.1 then stop

 wire2(j2).co(0,i)  = colight_x
 wire2(j2).co(1,i)  = colight_y ; center of light

 wire2(j2).gc(0,i)  = aa(4)
 wire2(j2).gc(1,i)  = aa(5) ; gaussian center (x,y)

; =================================================================================
 npix = n_elements(gg)

;; ccd2 = long(ccd)
;; w14 = where(ccd2 ge bit14 and (ccd2 XOR bit14) ne (ccd2-bit14),c14) NOTE gg is the
;; gaussian fit!!!

 wire2(j2).flux1(i) = flux - 64. * background2
 wire2(j2).flux2(i) = total(gg)      - npix * background2 ; subtract background



; wire2(j2).flux1(i) = total(gg(w14)) - c14  * background2 ; subtract background
; =================================================================================

 wire2(j2).backgr(i)  = background  ; gaussian background
 wire2(j2).backgr2(i) = background2_store ; use median of four corners of CCD!

; Do "fixed aperture" photometry:
 ap_flux = fltarr(nap) ; fluxes in each aperture
  ss1 = abs( aa(2) ) ; may be negative ...
  ss2 = abs( aa(3) ) ; gaussian sigma
  fwhm = avg([ss1,ss2]) * 2.35 ; convert sigma to FWHM
  ap_max = 0

  use_x = aa(4)
  use_y = aa(5) ; x,y coordinates to use ... it seems the gaussian fit is better

  if use_x gt 5.5 or use_x lt 2.5 or $
     use_y gt 5.5 or use_y lt 2.5 then begin
       ; coordinates may be wrong!!?
       use_x = colight_x
       use_y = colight_y
  endif

;  if strmatch(file(0),'*AlphaUMi*obj2*') eq 1 then begin
;    if abs(wire2(j2).x(i)-260.) lt 15. and $
;       abs(wire2(j2).y(i)-260.) lt 15. then fwhm = 3.00
;  endif

 wire2(j2).fwhm(i) = fwhm
 
 if (fwhm lt 0.5 or fwhm gt 5.) and j2 gt 100 then begin ; currupt r_ap
   w78 = where(wire2.fwhm(i) gt .3,c78)
   if c78 ge 15 then begin
     wire2(j2).fwhm(i) = median(wire2(w78).fwhm(i)) 
   endif else begin
     wire2(j2).flux1(i) = -99.9 ; second fit was also bad
     goto,skip_data
   endelse
 endif

 doap = where(apuse eq 1,cdoap)




 for p2 = 0,cdoap-1 do begin
  pp = doap(p2)
  r_ap = apertures(pp) * fwhm

; ============ PHOTMODE 1 =================
if phot_mode eq 1 then begin
  dist = ccd ; result array == dist
  dist_circle, dist, 8, use_x, use_y
  wap = where(dist lt r_ap,cap)
  if cap ge 1 then $
    ap_flux(pp) = total(ccd(wap)) - background2 * cap ; background subtraction!
endif
; =========================================

; ============ PHOTMODE 2 =================
if phot_mode ge 2 then begin
  if phot_mode eq 2 then n1 = 2 ; total number of apertures = 2 * n1 + 1
  if phot_mode eq 3 then n1 = 5 ; 

  cnt_p = 0

  photres = fltarr(2,1+n1 * 2) ; radii & fluxes

  dist = ccd ; result array == dist
  dist_circle, dist, 8, use_x, use_y

  if phot_mode eq 2 then r_adj = ((findgen(n1*2+1)/(n1*2.))-0.5) * 0.2
  if phot_mode eq 3 then r_adj = ((findgen(n1*2+1)/(n1*2.))-0.5) * 0.3

; ---------------------------------------------------------------------------------
;      TOO LITTLE RANGE IN THE APERTURE SIZES!
; ---------------------------------------------------------------------------------
  r_use_all = r_ap + r_adj * r_ap
  wr = where(r_use_all gt 1.0,cwr) ; number of ap. sizes > 1.0

  if (cwr / (2.*n1+1) lt 0.5) then begin ; too few aps. with r > 1.0
       wap = where(dist lt r_ap,cap)
       if cap ge 1 then $
       ap_flux(pp) = total(ccd(wap)) - background2 * cap ; background subtraction!
       goto,next_ap
  endif 
; ---------------------------------------------------------------------------------
 
  n1_l = 2*n1 + 1
  for apm=0,n1_l-1 do begin ; for each sub-aperture!
    r_use = r_ap + r_adj(apm) * r_ap
    if r_use lt 1 then r_use = 1.
    wap = where(dist lt r_use,cap)
     photres(0,cnt_p) = r_use
     if cap ge 1 then $
      photres(1,cnt_p) = total(ccd(wap)) - background2 * cap ; aperture flux
    cnt_p = cnt_p + 1 ; increase aperture counter!
  endfor

; Fit a 2nd degree polynomial!
  ll_fit = robust_poly_fit(photres(0,*),photres(1,*),2,lin_myfit)

   ap_flux(pp) = lin_myfit(n1)

endif
; =========================================

  if (use_x + r_ap) gt 7. or (use_x - r_ap) lt 0. or $
     (use_y + r_ap) gt 7. or (use_y - r_ap) lt 0. then $
       wire2(j2).a(i,*) = 1 ; mark aperture as being outside!

next_ap:

endfor

  wire2(j2).p(i,*) = ap_flux ; j = frame number, i = star number

; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>.
; FOR the new alpha cen obs. try summing the whole ccd - background ...
;  if strmatch(file(0),'*AlphaUMi*obj2*') eq 1 then begin
;    if abs(wire2(j2).x(i)-260.) lt 15. and $
;       abs(wire2(j2).y(i)-260.) lt 15. then $
;    wire2(j2).p(i,*) = total(wiref(j2).d(i,*,*)) ; flux ; do not subtract background !!!
;  endif
; <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<


if progress_on eq 9 then begin
 if (j2 mod 6e3) eq 0 then begin

    ; surface, ccd, charsi=2.5
    !P.charsize = 0.5

    titl =  'Sl:' + strcompress(string(i          ,format='(I7)'),/remove_all) + $
           ' Im:' + strcompress(string(j2         ,format='(I7)'),/remove_all) + $
           ' Fl:' + strcompress(string(wire2(j2).flux1(i)/1e3,format='(I7)'),/remove_all) + $
           ' FW:' + strcompress(string(wire2(j2).fwhm(i) ,format='(F5.2)'),/remove_all)
   
    mx = (round(max(ccd)/100.) * 100.) 
    lev = (findgen(10) * mx ) / 10.

    divv = 10.
    if mx gt 1e3 then divv = 1e2
    if mx gt 1e4 then divv = 1e3
    if mx gt 4e4 then tivv = 1e4
  
    lev = lev / divv

    contour,ccd/divv,levels=lev,c_labels=[1,0,1,0,1,0,1,0,1,0],xsty=1,ysty=1,$
       tit = titl,charsi=1.5,$
               xcharsi=.5,ycharsi=.5
    plots,wire2(j2).gc(0,i),wire2(j2).gc(1,i),psym=7,thick=3,symsi=3
    tvellipse, wire2(j2).fwhm(i), wire2(j2).fwhm(i), $
               wire2(j2).gc(0,i),wire2(j2).gc(1,i),0,/data,thick=3,linestyle=2
    xyouts,.3,.5,alignment=0,/data,'S:'+$
      strcompress(string(divv,format='(I7)'),/remove_all),charsi=1.5
   
    
;    mx = max(ccd) & mi = min(ccd)
;    usemin = wire2(j2).backgr2(i)
;    showim, ccd, usemin, mx * 0.4 ; , 0, 0, '','','' ; titl
;    plotsym,0
;    plots,wire2(j2).gc(0,i),wire2(j2).gc(1,i),psym=8,symsi=2 * wire2(j2).fwhm(i),$
;     thick=3,color=0,/data

endif ; progress_on = 9

endif






; >>> Progress plot >>>
if progress_on ge 2 and progress_on le 5 then begin
 if (i ge 0)   and j le 26000 and $
    ((j mod 500) eq 0) and j ne 0 then begin ; plot a progress plot of a LC!

  !P.multi = [0,1,3]
   best = ceil(nap/2.) ; select a good aperture size (?)
   wpp = where(apuse eq 1,c_wpp) & if c_wpp le 2 then best = max(wpp)

   ptp_robust_fin,-2.5*alog10(wire2(0:j2-1).p(i,nap-1))+25.,noise,0
   tt = wire2(0:j2-1).hjd(i) & wt = where(tt gt 10000.,ct)
   mt = median(tt(wt))
   t1 = 86400.*(min(tt(wt))-mt) & t2 = 86400.*(max(tt(wt))-mt)
   plot,86400.*(wire2(0:j2-1).hjd(i)-mt),$
                wire2(0:j2-1).p(i,nap-1),$
        psym=3,symsi=.5,yr=[-2000,2000]+median(wire2(0:j2-1).p(i,nap-1)),$
        tit='Star: '+string(i,format='(I2)')+$
        ' -- Aperture 8: '+string(noise,format='(F9.4)'),$
        charsi=1.5,xsty=3,ysty=3,xr=[t1,t2]
   ptp_robust_fin,-2.5*alog10(wire2(0:j2-1).p(i,best))+25.,noise,0
   plot,86400.*(wire2(0:j2-1).hjd(i)-mt),$
                wire2(0:j2-1).p(i,best),$
        psym=3,symsi=.5,yr=[-2000,2000]+median(wire2(0:j2-1).p(i,best)),$
        tit='Aperture 5: '+string(noise,format='(F9.4)'),$
        charsi=1.5,xsty=3,ysty=3,xr=[t1,t2]
   ptp_robust_fin,-2.5*alog10(wire2(0:j2-1).flux1(i))+25.,noise,0
   plot,86400.*(wire2(0:j2-1).hjd(i)-mt),$
                wire2(0:j2-1).flux1(i),$
        psym=3,symsi=.5,yr=[-2000,2000]+median(wire2(0:j2-1).flux1(i)),$
        tit='Buzasi Phot: '+string(noise,format='(F9.4)'),$
        charsi=1.5,xsty=3,ysty=3,xr=[t1,t2]
  !P.multi=[0,2,2]
 endif

;  plot,wire.d(0,3,3),psym=3,ysty=3,yr=[2.4,2.7]*1e4,charsi=2
; for j2 = 1000,2200,15 do print,reform( round (wiref(j2).d(0,1:6,1:6)) ), 'ha'
; for j2 = 1000,2200,15 do contour,( round (wiref(j2).d(0,1:6,1:6)) )

endif
; <<< end of progress plot <<<

  skip_data:

 endfor ; next data point

if progress_on eq 1 then begin
 print,''
 plot,xm,ym,xr=[0,1000],yr=[0,1000],psym=4,tit='CCD position of stars'

 ; plot,wire2.backgr2(0) / wire2.backgr(0),psym=3,yr=[-5,5]*3
 ; plot,wire2.p(0,4) / wire2.flux1(0),psym=3,yr=[-5,5]*3
 ; plot,wire2.p(0,4) / wire2.flux1(0) - .985,psym=3,yr=[-1,1]*0.01


 ;plot,wire2.hjd(0),wire2.x(0,*),psym=3
 ;plot,wire2.hjd(0),wire2.x(1,*),psym=3
 ;plot,wire2.hjd(0),wire2.x(2,*),psym=3

 plot,wire2.co(1,0),wire2.co(0,0),psym=3,xr=[2,5],yr=[2,5],$
  tit='Center of Light Pos',xsty=1,ysty=1
  oplot,wire2.co(1,1),wire2.co(0,1),psym=3,col=col.red
  oplot,wire2.co(1,2),wire2.co(0,2),psym=3,col=col.yellow
  oplot,wire2.co(1,3),wire2.co(0,3),psym=3,col=col.green
  oplot,wire2.co(1,4),wire2.co(0,4),psym=3,col=col.sky 

 plot,wire2.hjd(0)-median(wire2.hjd(0)),wire2.flux2(0),psym=3,$
  tit='Time vs. Flux for Main Target',xr = [-1,1]*0.5,$
   min_value = 5.2e5,ysty=3,charsi=2

 plot,wire2.flux1(0),wire2.flux2(0),psym=3,tit='Flux1 vs. Flux2 for Main Target'
endif

fail_star:


;plot,wire2.p(0,4),yr=[5.1,5.5]*1e5,psym=3
; oplot,wire2.flux2(0) , psym=3,col=col.red


endfor ; next star


fl = fltarr(nslot) ; median flux of each star
fl(*) = -99.9 ; assume bad data!
cnt_max = 0L

 for i=0,nslot-1 do begin
   fl_all = wire2.flux2(i)       ; all measured fluxes
   ww = where(fl_all gt 0.0,cok) ; reject bad points
   if cok ge (ndp*0.25) then fl(i) = median(fl_all(ww))
   cnt_new = max(ww)
   if cnt_new gt cnt_max then cnt_max = cnt_new
 endfor


if cnt_max ge 10 then begin ; any valied data points?

; Beware:
;  wire2 = wire2(0:cnt_max-1) ; remove unused data entries!
;  wiref = wiref(0:cnt_max-1)

wire_get_wirearr, wire2a, nstars, nap, ndp ; obs! nstars ... may be > 5 stars !!
cn_star = lonarr(nstars)
wiref2   = replicate({d:fltarr(nstars,8,8)}, ndp)
wire2a.hjd(*) = 1e9


; ====================================================
;  Flat Field determination >>>>>>>>>>>>>>>>
; ====================================================


; First of all ... sort wire array be the right star 
; (identified by x,y position relative to the central star).

dist_cen_lim = 5.0    ; not squared!
dist_xy_lim  = 50.^2. ; squared distance!


for star  = 0,nstars-1 do begin ; for every star in the result array (wire3)
 for slot = 0,nslot-1 do begin ; for each slot in original data set

xy_pos     = transpose( [ [wire2.x(slot)] , $
                          [wire2.y(slot)] ] )

xy_pos2    = transpose( [ [wire2.x(slot) + wire2.gc(0,slot)] , $
                          [wire2.y(slot) + wire2.gc(1,slot)] ] )

hjd_pos = wire.hjd(slot)

  ; Get the right star .. sort by unique star x,y positions !!

  dist_all = reform( (xy_pos2(0,*) - xy(0,star))^2.0 + (xy_pos2(1,*) - xy(1,star))^2.0 )
  dist_cen = reform( (xy_pos2(0,*) - xy(0,0)   )^2.0 + (xy_pos2(1,*) - xy(1,0)   )^2.0 )
  dist_nom = (xy(0,star) - xy(0,0))^2.0 + (xy(1,star) - xy(1,0))^2.0
  abstand  = abs(sqrt(dist_cen) - sqrt(dist_nom))

  phot = wire2.p(slot,xyinfo(winf).aperture(star))
  fwhm = wire2.fwhm(slot)

  ws = where(dist_all lt dist_xy_lim and $
             abstand lt dist_cen_lim and $
             fwhm gt .3 and $
             phot gt 10., cs)

  if cs ge 5 then begin

   wire2a(cn_star(star):cn_star(star)+cs-1).x(star)       = wire2(ws).x(slot)
   wire2a(cn_star(star):cn_star(star)+cs-1).y(star)       = wire2(ws).y(slot)  
   wire2a(cn_star(star):cn_star(star)+cs-1).hjd(star)     = wire2(ws).hjd(slot)   
   wire2a(cn_star(star):cn_star(star)+cs-1).co(*,star)    = wire2(ws).co(*,slot) ;
   wire2a(cn_star(star):cn_star(star)+cs-1).flux1(star)   = wire2(ws).flux1(slot)    
   wire2a(cn_star(star):cn_star(star)+cs-1).flux2(star)   = wire2(ws).flux2(slot)   
   wire2a(cn_star(star):cn_star(star)+cs-1).gc(*,star)    = wire2(ws).gc(*,slot) ;
   wire2a(cn_star(star):cn_star(star)+cs-1).backgr(star)  = wire2(ws).backgr(slot)    
   wire2a(cn_star(star):cn_star(star)+cs-1).backgr2(star) = wire2(ws).backgr2(slot)    
   wire2a(cn_star(star):cn_star(star)+cs-1).p(star,*)     = wire2(ws).p(slot,*) ;
   wire2a(cn_star(star):cn_star(star)+cs-1).a(star,*)     = wire2(ws).a(slot,*) ;
   wire2a(cn_star(star):cn_star(star)+cs-1).fwhm(star)    = wire2(ws).fwhm(slot)   
   wire2a(cn_star(star):cn_star(star)+cs-1).col(star)     = wire2(ws).col(slot)  
   wire2a(cn_star(star):cn_star(star)+cs-1).row(star)     = wire2(ws).row(slot)   

   wiref2(cn_star(star):cn_star(star)+cs-1).d(star,*,*) = wiref(ws).d(slot,*,*)
   cn_star(star) = cn_star(star) + cs    

   ; Thoroughly tested!

  endif
  
  endfor
endfor

; Sanity check:
; surface,wiref2(0).d(star,*,*)
; print,wire2a(0).gc(*,star)
 
; Sort by times ... for the first star ...
 ;a = sort(wire2a.hjd(0))
 ;wire2a = wire2a(a)
 ;wiref2 = wiref2(a)
; Reset the bad times ... no I need HIGH times for the bad points
; (I will by times later on !)
; for star=0,nstars-1 do begin
;  wb = where(wire2a.hjd(star) gt 1e8,cb)
;  if cb ge 1 then wire2a(wb).hjd(star) = -1e6
; endfor

; ==============================================================================
; A new wire result array (hopefully the last one!)
wire_get_wirearr_bit14, wirefin, nstars, nap, ndp
; ==============================================================================

; ==============================================================================
;  Take mean value of FWHM, positions within these groups:
; ==============================================================================
ngroup_fwhm = 20 ; take mean of +- ngroup_fwhm to get robust FWHM
ngroup_flux =  7 ; take mean of +- ngroup_flux to get robust FLUX; 1 exp == 0.5 sec.
ngroup_pos  = 20 ; take mean of +- ngroup_pos  to get robust POSITION
ngroup_back = 20 ; take mean of +- ngroup_back to get robust BACKGROUND ESTIMATE
; ==============================================================================

; ==============================================================================
;  Redetermine FWHM, backgr, position ---> More robust photometry!!
; ==============================================================================

for star = 0,nstars-1 do begin ; for every star in the result array (wire2a)
; ==============================================================================
print,' '
print,' *** Processing (redetermine FWHM, backgr, position --> Flux) for star: ' + $
 string(star,format='(I3)')

; a = sort(wire2a.hjd(star))
; wire2a = wire2a(a)
; wiref2 = wiref2(a)

 dfwhm      = abs(wire2a.fwhm(star) - xyinfo(winf).fwhm(0,star))
 dfwhm_lim  = xyinfo(winf).fwhm(1,star)
 phot       = wire2a.p(star,xyinfo(winf).aperture(star))
 hjd        = wire2a.hjd(star)
 gc         = wire2a.gc(*,star) 

if mode eq 'determine_ff' then begin
    ; For secondary stars: need stars very near 3.5, 3.5 -- other wise PSF
    ; residual look-up table will not be valid
    ; gclim = 0.10 ; percent of data that can be used: 
    ; gc = 0.10; ... (.10*2)^2. =  4%
    gc = 0.15; ... (.15*2)^2. =  9%
    ; gc = 0.25; ... (.25*2)^2. = 25%
     wg = where( wire2a.fwhm(star) gt .3 and gc(0,*) gt 2. and $
                 abs(gc(0,*)- 3.5) lt gclim and abs(gc(1,*)-3.5) lt gclim and $
                 dfwhm lt  5.0 and phot gt 10. and hjd gt 4e4 and hjd lt 6e4,cg)
endif 

if mode eq 'get_lc' then begin
     wg = where( wire2a.fwhm(star) gt .3 and gc(0,*) gt 2. and gc(1,*) gt 2. and $
                 gc(0,*) lt 5. and gc(1,*) lt 5. and $
                 dfwhm lt  5.0 and phot gt 10. and hjd gt 4e4 and hjd lt 6e4,cg)
endif

                                ; must construct the PSF model
                                ; ... IMAGE = (PSF + BG) * FLAT
                                ; ... need the profile !!

; if star eq 0 and cg lt 5 then stop ; debug this ... what's wrong with this star?

 cng = 0L
 cnf = 0L
 cnp = 0L
 cnback = 0L

 if cg le 5 then begin
;  print,' %%% No valid points for star ' + string(star,format='(I2)') + $
;              ' xy = ' + string(xy(*,star),format='(F5.1,X,F5.1)') + ' file '+file(ff) 
  goto,failed_star
 endif

 wire2g = wire2a(wg) ; flux, x-y position, FWHM, background of each image ...
 wirefg = wiref2(wg) ; CCD 8x8 pixels for each star

 all_fwhm = fltarr(cg)
 all_flux = fltarr(cg)
 all_pos  = fltarr(6,cg)
 all_back = fltarr(cg)

 dt_lim = median( wire2g(1:cg-1).hjd(star) - wire2g(0:cg-2).hjd(star) ) * 6.
 if dt_lim eq 0 then dt_lim = (0.5 / 86400.) * 6D ; Problem for dt = 0.1 sek

 ; *** MEASURE FWHM ***
 while cng lt cg do begin

 cnt_fwhm1 = cng - ngroup_fwhm & if cnt_fwhm1 lt  0 then cnt_fwhm1 = 0
 cnt_fwhm2 = cng + ngroup_fwhm & if cnt_fwhm2 ge cg then cnt_fwhm2 = cg-1

 dt_fwhm = abs(wire2g(cnt_fwhm1:cnt_fwhm2).hjd(star) - wire2g(cng).hjd(star))
 dat_fwhm = wire2g(cnt_fwhm1:cnt_fwhm2).fwhm(star)
 wg_fwhm = where(dt_fwhm lt dt_lim and dat_fwhm gt .3 and dat_fwhm lt 3.0,cg_fwhm) 
   ; must be within lime limit
 if cg_fwhm ge 1 then begin
    use_fwhm = dat_fwhm(wg_fwhm)
    if cg_fwhm gt 5 then $
     resistant_mean,use_fwhm,3,me_fwhm,sd,nr $
    else me_fwhm = median(use_fwhm) 
 endif else begin ; illegal FWHM?
  goto,failed_fwhm
 endelse
   all_fwhm(cng) = me_fwhm 
   failed_fwhm:
   cng = cng + 1 ; next 8x8 image
  endwhile
  ; *** END OF: MEASURE FWHM ***


 ; *** MEASURE FLUX ***
 while cnf lt cg do begin

 cnt_flux1 = cnf - ngroup_flux & if cnt_flux1 lt  0 then cnt_flux1 = 0
 cnt_flux2 = cnf + ngroup_flux & if cnt_flux2 ge cg then cnt_flux2 = cg-1

 dt_flux = abs(wire2g(cnt_flux1:cnt_flux2).hjd(star) - wire2g(cnf).hjd(star))
 dat_flux = wire2g(cnt_flux1:cnt_flux2).p(star,xyinfo(winf).aperture(star))
 wg_flux = where(dt_flux lt dt_lim and dat_flux gt 10. and dat_flux lt 1e9,cg_flux) 
   ; must be within lime limit
 if cg_flux ge 1 then begin
    use_flux = dat_flux(wg_flux)
    if cg_flux gt 5 then $
     resistant_mean,use_flux,3,me_flux,sd,nr $
    else me_flux = median(use_flux) 
 endif else begin ; illegal FLUX?
  goto,failed_flux
 endelse
   all_flux(cnf) = me_flux 
   failed_flux:
   cnf = cnf + 1 ; next 8x8 image
 endwhile
  ; *** END OF: MEASURE FLUX ***


 ; *** MEASURE background ***
 while cnback lt cg do begin

 cnt_back1 = cnback - ngroup_back & if cnt_back1 lt  0 then cnt_back1 = 0
 cnt_back2 = cnback + ngroup_back & if cnt_back2 ge cg then cnt_back2 = cg-1

 dt_back  = abs(wire2g(cnt_back1:cnt_back2).hjd(star) - wire2g(cnback).hjd(star))
 dat_back = wire2g(cnt_back1:cnt_back2).backgr2(star)
 wg_back  = where(dt_back lt dt_lim and dat_back gt 10. and dat_back lt 1e4,cg_back) 
   ; must be within time limit
 if cg_back ge 1 then begin
    use_back = dat_back(wg_back)
    if cg_back gt 5 then $
     resistant_mean,use_back,3,me_back,sd,nr $
    else me_back = median(use_back) 
 endif else begin ; illegal back?
  goto,failed_back
 endelse
   all_back(cnback) = me_back 
   failed_back:
   cnback = cnback + 1 ; next 8x8 image
 endwhile
  ; *** END OF: MEASURE back ***





 ; *** MEASURE POS ***
 while cnp lt cg do begin

 cnt_pos1 = cnp - ngroup_pos & if cnt_pos1 lt  0 then cnt_pos1 = 0
 cnt_pos2 = cnp + ngroup_pos & if cnt_pos2 ge cg then cnt_pos2 = cg-1
 npos = cnt_pos2 - cnt_pos1 + 1

 dt_pos = abs(wire2g(cnt_pos1:cnt_pos2).hjd(star) - wire2g(cnp).hjd(star))
 dat_pos = fltarr(6,npos)

 dat_pos(0,*) = wire2g(cnt_pos1:cnt_pos2).x(star)
 dat_pos(1,*) = wire2g(cnt_pos1:cnt_pos2).y(star)
 dat_pos(2,*) = wire2g(cnt_pos1:cnt_pos2).gc(0,star)
 dat_pos(3,*) = wire2g(cnt_pos1:cnt_pos2).gc(1,star)
 dat_pos(4,*) = wire2g(cnt_pos1:cnt_pos2).gc(0,star) + wire2g(cnt_pos1:cnt_pos2).x(star)
 dat_pos(5,*) = wire2g(cnt_pos1:cnt_pos2).gc(1,star) + wire2g(cnt_pos1:cnt_pos2).y(star)

 xy_cen = fltarr(2) &  xy_cen(0) = wire2g(cnp).x(star) & xy_cen(1) = wire2g(cnp).y(star)

 wg_pos = where(dt_pos lt dt_lim and $
                dat_pos(0,*) eq xy_cen(0) and dat_pos(1,*) eq xy_cen(1) and $ ; identical pixels!
                dat_pos(0,*) gt 3. and dat_pos(0,*) lt 508. and $
                dat_pos(1,*) gt 3. and dat_pos(1,*) lt 508. and $
                dat_pos(2,*) gt 2. and dat_pos(2,*) lt 5. and $
                dat_pos(3,*) gt 2. and dat_pos(3,*) lt 5.,cg_pos) 
   ; must be within lime limit
 if cg_pos ge 1 then begin
    use_pos = dat_pos(*,wg_pos)
    if cg_pos gt 5 then begin
      resistant_mean,use_pos(2,*),3,me_pos_x,sd,nr 
      resistant_mean,use_pos(3,*),3,me_pos_y,sd,nr 
     endif else begin
      me_pos_x = median(use_pos(2,*)) 
      me_pos_y = median(use_pos(3,*)) 
    endelse

 endif else begin ; illegal POS?
  goto,failed_pos
 endelse
   all_pos(0:1,cnp) = xy_cen
   all_pos(2:3,cnp) = [me_pos_x, me_pos_y]
   all_pos(4:5,cnp) = [me_pos_x, me_pos_y] + xy_cen

   failed_pos:
   cnp = cnp + 1 ; next 8x8 image
 endwhile
  ; *** END OF: MEASURE POS ***


; Subtract the PSF and see whats left


 cna = 0L
 ; *** MEASURE POS ***
 while cna lt cg do begin

  bias = 0. ; 370. ; I need to adjust the bias level !

;  .r 
;  for bias=0,400,40 do begin

;if mode eq 'determine_ff' then begin
  image = reform( wirefg(cna).d(star,*,*) )
  background = all_back(cna)
  psf = fltarr(8,8)
  sigsqr2 = 2. * (all_fwhm(cna) / 2.35) ^ 2.0 ; 2 * sigma^2
  for i=0,7 do $
  for j=0,7 do $
    psf(i,j) = exp( - ((all_pos(2,cna) - i)^2. + (all_pos(3,cna) - j)^2. ) / sigsqr2)
;endif

  ; Flux must be scaled to the same aperture size:
  if seeing_mode eq 'variable_fwhm' then $
   r_ap = apertures(xyinfo(winf).aperture(star)) * all_fwhm(cna) else $
   r_ap = apertures(xyinfo(winf).aperture(star)) * fixed_fwhm(star)

  dist = psf ; result array == dist
  use_x = all_pos(2,cna) & use_y = all_pos(3,cna) 
  dist_circle, dist, 8, use_x, use_y

  wap = where(dist lt r_ap,cap)

  image2 = long(image)
  wap2 = where(dist lt r_ap AND ( (image2 XOR bit14) ne (image2-bit14) ),cap2)

  w_bit14 = where(dist lt r_ap AND ( (image2 XOR bit14) eq (image2-bit14) ),c14)

  ; if cap ne cap2 then stop

  if cap ge 1 then begin
     aperture_flux = total(psf(wap)) 
     aperture_flux_image = total(image(wap)) - background * cap
     aperture_flux_image_bit14 = total(image(wap2)) - background * cap2
  endif else begin
    print,' *** No valid aperture_flux: '+file(ff),star
    goto,skip_cna
  endelse

  scalepsf = all_flux(cna) / aperture_flux 
  psf = psf * scalepsf

  diff = image - background - psf
  rr = robust_sigma(diff)
  mx_psf = max(psf) ;; & ph_noise = 1. / sqrt(mx_psf * 15. )
                    ;; lim_err = mx_psf * ph_noise
  ratio_flux_image = aperture_flux_image / all_flux(cna)
  org_flux = wire2g(cna).p(star,xyinfo(winf).aperture(star))
  ratio_flux_image2 = aperture_flux_image / org_flux
  back_diff = abs(background - wire2g(cna).backgr2(star))

  baddat = 0B
  if max(abs(diff)) gt 2500. or $
     abs(ratio_flux_image -1.0) gt .15 or $
     abs(ratio_flux_image2-1.0) gt .15 then baddat = 1B

   wirefin(cn_fin(star)).x(star)       = all_pos(0,cna) + use_x
   wirefin(cn_fin(star)).y(star)       = all_pos(1,cna) + use_y
   wirefin(cn_fin(star)).hjd(star)     = wire2g(cna).hjd(star)
   wirefin(cn_fin(star)).co(*,star)    = [use_x,use_y]
   wirefin(cn_fin(star)).flux1(star)   = org_flux
   wirefin(cn_fin(star)).flux2(star)   = all_flux(cna)
   wirefin(cn_fin(star)).gc(0,star)    = use_x
   wirefin(cn_fin(star)).gc(1,star)    = use_y
   wirefin(cn_fin(star)).backgr(star)  = wire2g(cna).backgr(star)
   wirefin(cn_fin(star)).backgr2(star) = background


; Bit 14 mode:
   wirefin(cn_fin(star)).p2(star,xyinfo(winf).aperture(star)) = aperture_flux_image
   wirefin(cn_fin(star)).p(star,xyinfo(winf).aperture(star)) = aperture_flux_image_bit14
   wirefin(cn_fin(star)).c14(star) = c14


   wirefin(cn_fin(star)).a(star,xyinfo(winf).aperture(star)) = baddat 
   wirefin(cn_fin(star)).fwhm(star)    = all_fwhm(cna)
   wirefin(cn_fin(star)).col(star)     = all_pos(0,cna)
   wirefin(cn_fin(star)).row(star)     = all_pos(1,cna)
   cn_fin(star) = cn_fin(star) + 1


     
;   print,' Percentage of light in aperture: ', aperture_flux / total(psf)

;   flat_cut = (image - bias) / (psf + background )
;   flat_cut = flat_cut / median(flat_cut) ; it this a good idea ... normalize to 1.000

if mode eq 'determine_ff' then begin
   psf_residual_use = psf_residual * (all_flux(cna) /  285664.)

   flat_cut = (image - bias) / (psf + background + psf_residual_use)
   flat_cut = flat_cut / median(flat_cut) ; it this a good idea ... normalize to 1.000

;    flat_cut = image - bias - psf - background 
;    flat_cut = image - bias - psf - background - psf_residual_use

   ; a, lo, hi, xoffset, yoffset, xtit, ytit, tit

;   showim,flat_cut,.9,1.1,0,0,'x','y','(image - bias) / (psf + background)'
;   s = get_kbrd(1)
;   aa = image - background - psf
;   showim,aa,-20,20,0,0,'x','y','image - background - psf'
;   s = get_kbrd(1)
;   surface,image-background-psf,charsi=2,tit='image-background-psf'
;   s = get_kbrd(1)



                                ; the all pos x,y positions are
                                ; estimates of the central pixel of
                                ; the star ... I subtract 4,4 to get
                                ; the lower left corner of the CCD cut-out
   x1 = all_pos(0,cna) - 4 
;   y1 = all_pos(1,cna) - 4
   y1 = 512. - all_pos(1,cna) - 4 ; y1 increses from top in wire




   unused = 0 & cnu = -1
   ; Find a vacant spot

   while unused eq 0 do begin
    cnu = cnu + 1
    used = reform( flat_all(cnu,x1:x1+7,y1:y1+7) )
    wu = where(used gt -5,cu)
    if cu eq 0 then unused = 1


   if (cnu ge (cnu_max-1)) or $
       ( (cna ge (cg-1)) and (unused eq 1) ) then begin ; stop ; time to sum up the flat fields
  
      for k=0,511 do begin
       for j=0,511 do begin
          aok = where(abs(flat_all(*,k,j)-1.) lt .6,cok) ; true FF
;           aok = where(abs(flat_all(*,k,j)) lt 5e3,cok) ; get PSF residuals

          if cok ge 1 then flat_all2(cn_all2,k,j) = median(flat_all(aok,k,j))
      endfor ; next x
     endfor ; next y

;     contour,flat_all2(cn_all2,x1-3:x1+10,y1-3:y1+10),min_value = -5, $
;      levels = [.9,.95,.975,1.,1.025,1.05,1.1], c_labels = [1,1,1,1,1,1,1]

; contour,flat_all2(0,x1:x1+10,y1:y1+10),min_value=-5,$
;  levels=[.9,.95,.975,1.,1.025,1.05,1.1], c_labels=[1,1,1,1,1,1,1]

     cn_all2 = cn_all2 + 1
     if cn_all2 ge cnu_max2 then begin
      print,' %%% Temporary flat_all2 array is filled up: ',cnu_max2
      cna = cg ; terminate while loop
     endif

    ; Reset temp. flat field storage
     flat_all(*,*,*) = -16.
     cnu = 0
     unused = 1  
  
   endif

endwhile

                        ; cnu == 1 ?



; Only store pixels with high signal
;store_flat = intarr(8,8) & store_flat(*,*) = 100
;store_flat(3:4,3:4) = 1
;store_flat2 = intarr(8,8)
;store_flat2(*,*) = 100
;store_flat2(2:5,2:2) =1
;store_flat2(2:5,5:5) =1
;store_flat2(2:2,2:5) =1
;store_flat2(5:5,2:5) =1

;  flat_all(cnu,x1:x1+7,y1:y1+7) = flat_cut
   flat_all(cnu,x1+3:x1+4,y1+3:y1+4) = flat_cut(3:4,3:4)

; contour,flat_all(cnu,x1-3:x1+10,y1-3:y1+10),min_value = -5, $
;  levels = [.95,.975,1.,1.025,1.05], c_labels = [1,1,1,1,1]

;  .r
; for i=0,cnu-1 do begin
; contour,flat_all(i,55:70,55:70),min_value = -5, $
;  levels = [.95,.975,1.,1.025,1.05], c_labels = [1,1,1,1,1]
; s = get_kbrd(1)
; endfor
; end

;  print,bias, median(flat)
 
;  endfor
;  end

; contour,image
; plots,all_pos(2:3,cna),psym=7
;  stop


endif ; determine ff ?
; -----------------------------------------------------------------------

skip_cna:

   cna = cna + 1
   if cna mod 1e4 eq 0 then print,cna,format='(I6,$)'
endwhile




if mode eq 'determine_ff' then begin

; .r 

; Sum the (sum of flat fields)
   if cn_all2 ge 1 then begin  
      for k=0,511 do begin
       for j=0,511 do begin
          aok = where(abs(flat_all2(*,k,j)-1.0) lt .3 and flat(k,j) lt -5,cok) ; True FF
;           aok = where(abs(flat_all2(*,k,j)) lt 5e3,cok) ; PSF residuals

          if cok ge 1 then flat(k,j) = median(flat_all2(aok,k,j))
      endfor ; next x
     endfor ; next y

; end
; showim,reform(flat(x1:x1+7,y1:y1+7)),.9,1.1 
; showim,reform(flat_all2(0,x1:x1+7,y1:y1+7)),.9,1.1
; showim,reform(flat_all2(35,x1:x1+7,y1:y1+7)) / reform(flat(x1:x1+7,y1:y1+7)),.9,1.1
; for i=0,49 do showim,reform(flat_all2(i,x1:x1+7,y1:y1+7)) / reform(flat(x1:x1+7,y1:y1+7)),.9,1.1
; for i=0,49 do showim,reform(flat_all2(i,x1:x1+7,y1:y1+7)),.95,1.05
; for i=0,49 do showim,reform(flat_all2(i,x1:x1+7,y1:y1+7)),-100,100

; ff 8x8 array: PSF residuals IM_0 = IM_org - PSF(gauss) - BG
; psf_residual = reform(flat(x1:x1+7,y1:y1+7))
; save,filename='~bruntt/wire/wire_psf_residual.idl',psf_residual

; flat_residual = reform(flat(x1:x1+7,y1:y1+7))
; flat_residual = flat_residual / median(flat_residual)
; save,filename='~bruntt/wire/wire_flat_residual.idl',flat_residual

     ; Reset the flat temp array
     cn_all2 = 0
     flat_all2(*,*,*) = -18.
   endif

endif ; determine ff?



failed_star:

endfor ; next star







; ====================================================
; ====================================================


; Sort array according to times using the reference star!
; a = sort(wire2a.hjd(0))
; wire2a = wire2a(a)
; wiref2 = wiref2(a)  

  ; ====================================================
  outfile = ''
  a = strsplit(file(ff),'.',/extract) & na = n_elements(a)

  for i=0,na-2 do outfile = outfile + a(i) + '.'
   if mode eq 'get_lc' then $
    outfile = outfile + 'idl'+string(phot_mode,format='(I1)') + '.fin'
   if mode eq 'determine_ff' then $
    outfile = outfile + 'idl'+string(phot_mode,format='(I1)') + '.flat'

  xm = fltarr(nstars) & xm(*) = -9.
  ym = fltarr(nstars) & ym(*) = -9.
  fl = fltarr(nstars) & fl(*) = -9.
  ap = intarr(nstars) & ap(*) = -1

  for jj9 = 0,nstars-1 do begin
   wg9 = where(wirefin.flux1(jj9) gt 10,cg9)
   if cg9 ge 100 then begin
    xm(jj9) = median(wirefin(wg9).x(jj9))
    ym(jj9) = median(wirefin(wg9).y(jj9))
    aper    = xyinfo(winf).aperture(jj9)
    ap(jj9) = aper
    phot    = -2.5 * alog10(wirefin(wg9).p(jj9,aper)) + 25.0
    fl(jj9) = median(phot)
   endif
  endfor
 
  mxfin = max(cn_fin)
  if mxfin eq 0 then begin
   print,' %%% Apparently there is no valid data for the file: '+file(ff)
   goto,skip_saving
  endif

  wirefin = wirefin(0:mxfin-1) ; purge unused data
  xyp = xy ; the input x,y positions
  pts = cn_fin

  readme = strarr(7)
  readme(0) = 'The file you just restored has the following arrays & structures:'
  readme(1) = 'wirefin: processed image by image photometry: plot,wirefin.p(star,ap(star))
  readme(2) = 'fl,xm,ym: median flux, x and y position of each star'
  readme(3) = 'ap: the best aperture for each star, eg. ap(2) is the best ap. for star 3'
  readme(4) = 'xyp: the input x,y positions used to IDENTIFY the stars'
  readme(5) = 'pts: the number of valid data points in the wirefin structure for each star,'
  readme(6) = '     eg. use data points: d = wirefin(0:pts(star)-1).p(star,ap(star))'

  save,filename=outfile,wirefin,fl,xm,ym,ap,xyp,pts,/compress
  
  print,''
  print,' %%% Wire output file saved as: ' + outfile
  print,''
  ; ====================================================
endif else begin                ; any valid data points to save?
  print,''
  print,' *** No valid data points for file: ' + file(ff)
  print,''
endelse

skip_saving:

; contour,flat(60:80,45:60),levels=[.8,.9,1,1.1,1.2],c_labels=[1,1,1,1,1]
; contour,flat(290:300,290:300),levels=[.8,.9,1,1.1,1.2],c_labels=[1,1,1,1,1]

endfor

      ; next wire file (from the program "wire_all.pro") is read

!P.multi = 0
; wdelete,0 ; delete window

print, ' %%% Try to run .r wire_merge_fin.pro now!'

if progress_on eq 9 then begin
 device,/close
 set_plot,'x'
 !P.multi=0
 print,' %%% Wire reduction .ps file : '
 print,' $  ggv  '+psname + '  &  '
endif


if repcnt gt 10 then begin
 repout = '~/wire/badpixels.idl'
 print,' %%% Saved structure of bad pixels: '+repout
 repbad = repbad(0:repcnt-1) ; purge !!
 save,filename=repout, repair, repbad
endif

end


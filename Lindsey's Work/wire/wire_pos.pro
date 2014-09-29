PRO wire_pos,file,phot_mode,position_file, target

; Purpose:
; ---------
; Read wire result file(s) from the prg. wire_all.pro
; Then measure flux, sky, x-y centroid (2d-gauss fit), & center of light

; Examples:
; ----------
; Single wire result file:
;   wire_pos,'/ai38/bruntt/wire/data/991019.3142.1AltairA7V.idl',1,'',''
; Multiple wire result files:
;   spawnrob,'ls -1 /ai39/bruntt/wire/altair/altair_wire_*.idl',fil
;   wire_pos,fil,1,'',''

; Experimental photometry technique (slower):
;   spawnrob,'ls -1 /ai39/bruntt/wire/altair/altair_wire_003.idl',fil
;   wire_pos,fil,2,'',''

; flux2 = gaussian - background2 * n_pix
; flux1 = total flux - background2 * n_pix
; p = aperture photometry ... ap. size scales with FWHM ("seeing")
; ==========================================================

; ==========================================================
!P.charthick=1.0
!P.multi=0
!P.charsize=1.5
 !x.thick = 1
 !y.thick = 1
col = getcolor(/load)

; ==========================================================
; Get info on the location of fundamental files:
; ==========================================================
old_position_file = position_file
wire_red_setup,position_file,target_file
ref_position_file = position_file
position_file = old_position_file ; determines MODE of program
; ==========================================================


; ==========================================================
; Possible progress modes (progress_on parameter):
; 0 = all of, 1 = plot for every star, 2 = more progress plots
progress_on = 2 
; ==========================================================

; ==========================================================
nf = n_elements(file)
if nf eq 0 then begin
 print,' *** NO input files!'
 return
endif
; ==========================================================

; ==========================================================
repair = 0L
edge = intarr(8,8)
edge(0,*) = 1 & edge(*,0) = 1 & edge(7,*) = 1 & edge(*,7) = 1
edge(1,1) = 1 & edge(6,1) = 1 & edge(1,6) = 1 & edge(6,6) = 1
wedge = where(edge eq 1,cedge)
; ==========================================================

; ==========================================================
repcnt = 0L & repmax = 200000 & repbad = lonarr(repmax)
; Repbad contain the NUMBER of bad pixels along edges of ccd
; ==========================================================

; ==========================================================
wrap2 = 0B ; double warp check is off (default)
; ==========================================================

if strmatch(file(0),'*Procyon*') eq 1 then begin ; saturation -- double trapped numbers!
 wrap2 = 1B
 wrapval = 4500.
 wraplim = 3000.
 x11 = 3 & x22 = 4
 y11 = 3 & y22 = 4 
endif

; NEW alpha cen measurements are WRAPPED ... but not the old ones
; ... texp was only 0.1 sec back then, in 1999 !! That's AlphaCen
if strmatch(file(0),'*AlphaUMi*obj2*') eq 1 then begin ; saturation -- double trapped numbers!
 wrap2 = 1B
 wrapval = 20000.
 wraplim = 25000. ; will this work?
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

if wrap2 ne 0 then print,' ************ WRAPPING PIXELS ************* '




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


if progress_on ge 1 then begin
  !P.multi = [0,2,2] ; multiple plots = progress
  window,1,xsize=450,ysize=450,$
   title='WIRE - Basic CCD Reduction - Progress Plots'
endif

 for ff=0,nf-1 do begin
;;; for ff=9,nf-1 do begin ; debug

print,' %%% Restoring file '+file(ff)+' ...'
if file(ff) eq '' then begin
 print,' %%% Null filename ... returning from wire_pos.pro;
 RETURN
endif

restore,file(ff)

ndp = n_elements(wire)
nslot = 5

; The original aperutes ... first Altair reduction
nap = 9 ; number of apertures
inc = 1.25 & inc = sqrt(inc)
aperture = fltarr(nap)
aperture(0) = 0.7 ; size of the first aperture!
apuse = intarr(nap) & apuse(*) = 1 ; do all apertures == DEFAULT


for i=0,nap-2 do aperture(i+1) = aperture(i) * inc
; The area of the aperture will increase by "inc", eg 1.25 ==> 25%.

;nap = 9 ; number of apertures
;inc = 1.4 & inc = sqrt(inc)
;aperture = fltarr(nap)
;aperture(0) = 0.6 ; size of the first aperture!
;for i=0,nap-2 do aperture(i+1) = aperture(i) * inc
; The area of the aperture will increase by "inc", eg 1.25 ==> 25%.

; -----------------------------------------------------------------
; If position_file is given as input, use only the best aperture!
; This is used when you run ALL the data files of a given target!
; -----------------------------------------------------------------
if position_file ne '' then begin
 restore,position_file
 if n_elements(wireinfo) le 0 then begin
  print,' *** position file not found: '+position_file
  RETURN
 endif


 wok = where(strmatch(wireinfo.object,'*' + target + '*') eq 1,cok)
 aps = xyinfo(wok).aperture(0:wireinfo(wok).nstars-1)
 
 a = sort(aps)
 aps = aps(a)
 g = uniq(aps)
 g2 = aps(g) ; only do the apertures listed here ...
 apuse(*) = 0
 apuse(g2) = 1

; nslot = wireinfo(wok).nstars
; if nslot eq 1 then nslot = 2 ; problem with arrays fltarr(2,1) --> fltarr(2)

endif
; -----------------------------------------------------------------

; -----------------------------------------------------------------
;  Retrieve the RA / DEC for conversion of JD ---> HJD
; -----------------------------------------------------------------
; targetlist = '~/wire/wire_process/targets_wire.txt' ; USAFA
; targetlist = '/ai40/bruntt/wire/wire_process/targets_wire.txt' ; IFA
targetlist = target_file

wire_target_info, targetlist, info
wtarget = where(strmatch(info.object,'*'+target+'*',/fold) eq 1,ctarget)
if ctarget ne 1 then begin
 print,' %%% Target is not in your list: '+targetlist, ctarget
 stop
endif
ra  = info(wtarget).ra2
dec = info(wtarget).dec2
; -----------------------------------------------------------------

; -----------------------------------------------------------------
wire2 = replicate({x:fltarr(nslot),y:fltarr(nslot),hjd:dblarr(nslot),$
                  co:fltarr(2,nslot),flux1:fltarr(nslot),flux2:fltarr(nslot),$
                  gc:fltarr(2,nslot),backgr:fltarr(nslot),backgr2:fltarr(nslot),$
                  p:fltarr(nslot,nap), a:bytarr(nslot,nap), fwhm:fltarr(nslot), $
                  col:intarr(nslot), row:intarr(nslot)}, ndp)
; -----------------------------------------------------------------


; -----------------------------------------------------------------
; In wire3 the saturated pixels are increased by 2^16.,
; thus the counts are correct, and the flux can be calculated!
; -----------------------------------------------------------------
wire3 = replicate({d:fltarr(nslot,8,8)}, ndp)
wire3.d = wire.d

; Overall x,y position!
wire2.x = wire.x 
wire2.y = wire.y
wire2.hjd = wire.hjd

; Correct for counts << 0.
add_digital_sat = 2.^16
for i=0,nslot-1 do begin ; for each CCD-cut out (wire == 5)
 for j=0L,ndp-1 do begin ; for each star (usually of the order 100.000)

    cut = reform(wire(j).d(i,*,*)) ; the original data
    w = where(cut lt -100 and cut gt -65600.,c)
    if c ge 1 then begin
       cut(w) = cut(w) + add_digital_sat 
       wire3(j).d(i,*,*) = cut 
    ; add 65536 if data is saturated (beware of low background counts)
    endif


    if wrap2 eq 1 and $
      abs(wire(j).x(i)-260.) lt 15. and $
      abs(wire(j).y(i)-260.) lt 15. then begin ; Procyon / ACen double wrap!

      w2 = where( cn(0,*,*) ge x11 and cn(0,*,*) le x22 and $
                  cn(1,*,*) ge y11 and cn(1,*,*) le y22 and $
                  cut gt 100. and $
                 abs(cut - wrapval) le wraplim, c2)

        if c2 ge 1 then begin
           cut(w2) = cut(w2) + add_digital_sat 
           wire3(j).d(i,*,*) = cut 
        endif
    endif



 endfor 
endfor
; -----------------------------------------------------------------

; -----------------------------------------------------------------
; Compute median x, y position of each slot
; -----------------------------------------------------------------
xm = fltarr(nslot)
ym = fltarr(nslot)

for i=0,nslot-1 do begin ; overall x,y position of the 5 stars
  xm(i) = median(wire2.x(i,*))
  ym(i) = median(wire2.y(i,*))
endfor
; -----------------------------------------------------------------

; Debug:
; plot,wire.d(0,3,3) - wire3.d(0,3,3),psym=3


; -----------------------------------------------------------------
; Loop to compute centre of light and overall flux level
; -----------------------------------------------------------------
print,''
for i=0,nslot-1 do begin

; Progress indication:
 print,''
 print,'Star number = '+string(i,format='(I2)')
 print,'Data point out of '+string(ndp,format='(I6)') + ':'

if xm(i) le 0. or ym(i) le 0. then begin
 print,' *** Star ',i,' seems to have no valid xy positions ... '
 goto,fail_star
endif

 for j=0L,ndp-1 do begin
  if j mod 2500 eq 0 then print,j,format='(I7,$)'

; >>> Determine the centre of light in each frame
  colight_x = 0.
  colight_y = 0.
  
  for x1 =0,7 do begin
   for y1 = 0,7 do begin
     colight_x = colight_x + (x1+0.5-4.) * wire3(j).d(i,x1,y1)
     colight_y = colight_y + (y1+0.5-4.) * wire3(j).d(i,x1,y1)     
   endfor
  endfor

  flux = total(wire3(j).d(i,*,*))

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
 ccd = float( reform(wire3(j).d(i,*,*)) )

 ; -------------------------------------------------
 ; Program with new format: often 65000+ pixels occur!
 ; -------------------------------------------------
 
 wmax = where(ccd gt 65000., cmax)
 wmin = where(ccd lt 65000. and ccd gt 30000,cmin)
 wed  = where(ccd gt 65000. and edge eq 1,ced)

 if (cmax ge 1 and cmin eq 0) or (ced ge 1) then begin

   ; Take the CCD value along the edge of the CCD:
   rep = [ reform(ccd(0,*)), reform(ccd(7,*)), ccd(1:6,0), ccd(1:6,7) ] 
   ; Make sure the values are not very high ...
   wrr = where(rep lt 2000,crr)
   ; If ok, replace by the median value of the edges ...
   if (crr ge 10) and (ced lt 7) and (ced gt 1) then begin
    replace   = median(rep(wrr))
    ccd(wmax) = replace ;; + randomn(seed

    if ced ge 1 and (repcnt+ced-1) lt repmax then begin
     repbad(repcnt:repcnt+ced-1) = ced ; ccd(wed)
     repcnt = repcnt + ced
    endif

    repair    = repair + 1
    endif else goto,skip_data
endif
 ; -------------------------------------------------



 max_cen = max(ccd(2:5,2:5))
 max_oth = max(ccd)
 if max_oth / max_cen gt 1.1 then begin ; central pixels do not contain the maximum!
  wire2(j).flux1(i) = -19.9
  goto,skip_data
 endif

 aa = fltarr(7)
 aa(0) = avg([ccd(0,0),ccd(0,7),ccd(7,0),ccd(7,7)]) ; median(wire3(j).d(i,*,*))
 aa(1) = (max(wire3(j).d(i,2:5,2:5)) - aa(0)) * 0.95
 aa(2) = 0.9 & aa(3) = 0.9             ; typical gaussian sigma = FWHM / 2.35 
 aa(4) = colight_x & aa(5) = colight_y ; good guess at x,y center
 
   ; sss = sort(ccd) & uuu = uniq(ccd(sss)) & nnn = n_elements(uuu) 
   ; if (nnn/64.) lt 0.5 then begin ; TOO SLOW!
 if median(ccd) eq 0. then begin ; bad data == eg. all entries are zero!
     wire2(j).co(0,i)  = -9.9
     wire2(j).co(1,i)  = -9.9
     wire2(j).flux1(i) = -9.9
     wire2(j).flux2(i) = -9.9
     goto,skip_data ; bad data     
 endif

 if (aa(1) lt (0.5 * aa(0))) or (aa(1) lt 50.0) then $
  goto,skip_data ; very low counts!



  if file(ff) eq '/data2/bruntt/wire/dat/HR2047/data/data_wire_005.idl' and $
   j gt 75400 and j lt 75495 then goto,skip_data

  if file(ff) eq '/data2/bruntt/wire/dat/betaLeo/data/data_wire_004.idl' and $
   i eq 1 and j gt 7555 and j lt 7715 then goto,skip_data

  if file(ff) eq '/data1/bruntt/wire/ZetaOph/ZetaOph_wire_ZetaOph_009.obj2.idlr' and $
     j ge 55600 and j le 55670 and i eq 4 then goto,skip_data


 aa1 = aa
 gg1 = gauss2dfit(ccd,aa1) ; fit all pixels

 ; first fit bad? ---> try again, but only if max counts in star > background

 fwhm_1 = avg(abs([aa1(2:3)]))
; if ( (fwhm_1 lt 0.5) or (fwhm_1) gt 5.5 or $
;      (abs((aa1(1)-aa(1))/aa(1)) gt 2.5) ) and $
;    (aa(1) gt aa(0)) then begin ; first fit bad, but there's plenty of signal!

; 29 MAR 2004: skip bad data for polaris
 if ( (fwhm_1 lt 0.5) or (fwhm_1) gt 5.5 or $
      (abs((aa1(1)-aa(1))/aa(1)) gt 2.5) ) then begin ; first fit bad, but there's plenty of signal!

; Exception for Procyon:
  if file(ff) eq '/data2/bruntt/wire/dat/ProcyonF5IV-V/data//data_wire_007.idl' and $
   abs(wire2(j).hjd(0) - 51450.891) lt .001D then goto,skip_data
  if file(ff) eq '/data2/bruntt/wire/dat/HR2047/data/data_wire_005.idl' and $
   abs(wire2(j).hjd(0) - 51795.19509D) lt .0001D then goto,skip_data

  if file(ff) eq '/data1/bruntt/wire/NuEri/ZetaOph/NuEri_wire_NuEri_009.obj1.idlr' then begin
    if i eq 1 and j gt 2775 and j lt 2850 then goto,skip_data
    if i eq 2 and j gt 8470 and j lt 8500 then goto,skip_data

  endif

; Feb 2005:
  if file(ff) eq $
   '/p6/bruntt/wire/data/gl845/gl845_wire_GL845_018.obj1.idlr' then begin

    if i eq 3 and j gt 5000 and j lt 10000 then goto,skip_data
   
  endif



; ccd = float( reform(wire3(j+20).d(i,*,*)) ) & surface,ccd

;  if file(ff) eq '/data1/bruntt/wire/ZetaOph/ZetaOph_wire_ZetaOph_009.obj2.idlr' then begin
; 
;   if j ge 41674 and j le 41690 and i eq 0 then goto,skip_data
;   if j ge 20650 and j le 20735 and i eq 3 then goto,skip_data
;   if j ge 50242 and j le 50320 and i eq 4 then goto,skip_data
;   if j ge 55600 and j le 55670 and i eq 4 then goto,skip_data
;;   if j ge 55600 and j le 55670 and i eq 4 then goto,skip_data
   
;  endif 

if file(ff) eq '/mnt/sdb6/wire/rawdata/nov2005/rfd/OmicronVel/OmicronVel_wire_OmicronVel_023.obj2.idlr' then begin
 if j ge 29393 and j le 29425 and i eq 3 then goto,skip_data
endif

; -----------------------------------------------------------------
  b9 = sort(ccd) & g9 = uniq(ccd(b9)) & nuniq = float(n_elements(g9)) / 64.
  if nuniq lt .05 then goto,skip_data ; data does not seem to be "real"

  aa2 = aa
  aa2(4) = aa2(4) - 1.
  aa2(5) = aa2(5) - 1. ; offset central position one pixel
  gg2    = gauss2dfit(ccd(1:6,1:6),aa2) ; fits only central 6x6 pixels
  fwhm_2 = avg(abs([aa2(2:3)]))

      if (fwhm_2 lt 0.5) or (fwhm_2 gt 5.5) or $
         (abs((aa2(1)-aa(1))/aa(1)) gt 2.5) then begin
         wire2(j).flux1(i) = -99.9 ; second fit was also bad
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
; -----------------------------------------------------------------

; -----------------------------------------------------------------
 background = aa(0) ; gaussian fit ==> sky background

 bb = [ccd(0,0),ccd(0,7),ccd(7,0),ccd(7,7)]
 bb2 = [ccd(0,0),ccd(0,7),ccd(7,0),ccd(7,7), $
        ccd(1,0),ccd(0,1),$
        ccd(6,7),ccd(7,6),$
        ccd(6,0),ccd(7,1),$
        ccd(0,6),ccd(1,7)]

; resistant_mean,bb,3,me,sd,nr
 background2 = avg(bb2) ; bg = "extented" corners of ccd = 12 pixels

 wire2(j).co(0,i)  = colight_x
 wire2(j).co(1,i)  = colight_y ; center of light

 wire2(j).gc(0,i)  = aa(4)
 wire2(j).gc(1,i)  = aa(5) ; gaussian center (x,y)

 wire2(j).flux1(i) = flux - 64. * background2
 wire2(j).flux2(i) = total(gg) - 64. * background2 ; subtract background

 wire2(j).backgr(i)  = background  ; gaussian background
 wire2(j).backgr2(i) = background2 ; use median of four corners of CCD!

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
 
 wire2(j).fwhm(i) = fwhm
 
 if fwhm lt 0.5 or fwhm gt 5. then begin ; currupt r_ap
   wire2(j).fwhm(i) = median(wire2(0:j-1).fwhm(0))
 endif
; -----------------------------------------------------------------

; -----------------------------------------------------------------
;  Select which aperture sizes to compute fluxes for
; -----------------------------------------------------------------
 doap = where(apuse eq 1,cdoap)
; -----------------------------------------------------------------

; for pp=0,nap-1 do begin ; for each aperture!
 for p2 = 0,cdoap-1 do begin
  pp = doap(p2)
  r_ap = aperture(pp) * fwhm

  ; print,' %%% fwhm = ' + strcompress(fwhm) + ' rad = ' + strcompress(r_ap)

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
  ; print, r_ap + r_adj * r_ap


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

;  plot,photres(0,*),photres(1,*),psym=4,xsty=3,ysty=3,$
;   xtit='Aperture Rad.',ytit='Flux',tit='Wire Phot.'
;  oplot,photres(0,*),lin_myfit,col=col.red,thick=2
;  plots,photres(0,n1),lin_myfit(n1),psym=6,symsi=2.0,col=col.green

;  print,'Hit me' & ghh9 = get_kbrd(1)

   ap_flux(pp) = lin_myfit(n1)

endif
; =========================================


;  contour,gg
;  plots,colight_x,colight_y,psym=4
;  plots,aa(4),aa(5),psym=2

  if (use_x + r_ap) gt 7. or (use_x - r_ap) lt 0. or $
     (use_y + r_ap) gt 7. or (use_y - r_ap) lt 0. then $
       wire2(j).a(i,*) = 1 ; mark aperture as being outside!

next_ap:

endfor
; -----------------------------------------------------------------

; Debug:
;  showim,ccd,background2*0.7,background2*1.3 
;  surface,ccd,charsi=2.0
;  s = robust_poly_fit(aperture, ap_flux, 2, myfit)

; --------------------------------------------------------------------
;  Store the flux in this aperture for this slot (i) & CCD image (j)
; --------------------------------------------------------------------
  wire2(j).p(i,*) = ap_flux ; j = frame number, i = star number
; --------------------------------------------------------------------

;  if ap_max eq 0 then ap_max = nap-1
;  wire2(j).a(i,*) = ap_max ; maximum aperture size used!

; -----------------------------------------------------------------
; >>> Progress plot >>>
; -----------------------------------------------------------------
if progress_on ge 2 then begin
 if (i ge 0)           and (j le 6000) and $
    ((j mod 500) eq 0) and j ne 0 then begin ; plot a progress plot of a LC!

  !P.multi = [0,1,3]
   best = ceil(nap/2.) ; select a good aperture size (?)
   wpp = where(apuse eq 1,c_wpp) & if c_wpp le 2 then best = max(wpp)

   ; USE A VERY LARGE APERTURE (number of apertures minus one):
   ptp_robust_fin,-2.5*alog10(wire2(0:j-1).p(i,nap-2))+25.,noise,0
   tt = wire2(0:j-1).hjd(i) & wt = where(tt gt 10000.,ct)
   mt = median(tt(wt))
   t1 = 86400.*(min(tt(wt))-mt) & t2 = 86400.*(max(tt(wt))-mt)
   plot,86400.*(wire2(0:j-1).hjd(i)-mt),$
                wire2(0:j-1).p(i,nap-1),$
        psym=3,symsi=.5,yr=[-2000,2000]+median(wire2(0:j-1).p(i,nap-2)),$
        tit='Star: '+string(i,format='(I2)')+$
        ' -- Aperture ' + strcompress(nap-2) + $ 
          ': !4!3 ='+string(noise,format='(F9.4)'),$
        charsi=1.5,xsty=3,ysty=3,xr=[t1,t2]

   ; PROBABLY A GOOD APERTURE SIZE: (0.5 * number of apertures):
   ptp_robust_fin,-2.5*alog10(wire2(0:j-1).p(i,best))+25.,noise,0
   plot,86400.*(wire2(0:j-1).hjd(i)-mt),$
                wire2(0:j-1).p(i,best),$
        psym=3,symsi=.5,yr=[-2000,2000]+median(wire2(0:j-1).p(i,best)),$
        tit='Aperture ' + strcompress(best+1) + $ 
          ': !4r!3 ='+string(noise,format='(F9.4)'),$
        charsi=1.5,xsty=3,ysty=3,xr=[t1,t2]

   ; SIMPLY SUM ALL THE LIGHT AND SUBTRACT BACKGROUND:
   ptp_robust_fin,-2.5*alog10(wire2(0:j-1).flux1(i))+25.,noise,0
   plot,86400.*(wire2(0:j-1).hjd(i)-mt),$
                wire2(0:j-1).flux1(i),$
        psym=3,symsi=.5,yr=[-2000,2000]+median(wire2(0:j-1).flux1(i)),$
        tit='Sum-All-Light Photometry: !4r!3 ='+string(noise,format='(F9.4)'),$
        charsi=1.5,xsty=3,ysty=3,xr=[t1,t2]
  !P.multi=[0,2,2]
 endif

endif
; <<< end of progress plot <<<

; plot,wire2(0:199).hjd(0),(wire2(0:199).p(0,6)-wire2(0:199).p(0,best))/wire2(0:199).flux2(0),yr=[-.1,.1],psym=1,symsi=.5
; plot,wire2(0:199).hjd(0),wire2(0:199).p(0,6),yr=[5.4,5.9]*1e4
; oplot,wire2(0:199).hjd(0),wire2(0:199).p(0,best)
; oplot,wire2(0:199).hjd(0),wire2(0:199).flux2(0),psym=1,symsi=.5
; oplot,wire2(0:199).hjd(0),wire2(0:199).flux1(0),psym=1,symsi=.5

  skip_data:

 endfor ; next data point
; -----------------------------------------------------------------

; -----------------------------------------------------------------
if progress_on eq 1 then begin
 print,''
 plot,xm,ym,xr=[0,1000],yr=[0,1000],psym=4,tit='CCD position of stars'

 ;plot,wire2.hjd(0),wire2.x(0,*),psym=3
 ;plot,wire2.hjd(0),wire2.x(1,*),psym=3
 ;plot,wire2.hjd(0),wire2.x(2,*),psym=3

 plot,wire2.co(1,0),wire2.co(0,0),psym=3,xr=[0,7],yr=[0,7],$
  tit='Center of Light Pos',xsty=1,ysty=1
  oplot,wire2.co(1,1),wire2.co(0,1),psym=3,col=col.red
  oplot,wire2.co(1,2),wire2.co(0,2),psym=3,col=col.yellow
  oplot,wire2.co(1,3),wire2.co(0,3),psym=3,col=col.green
  oplot,wire2.co(1,4),wire2.co(0,4),psym=3,col=col.sky 

 plot,wire2.hjd(0),wire2.flux2(0),psym=3,tit='Time vs. Flux for Main Target'

 plot,wire2.flux1(0),wire2.flux2(0),psym=3,tit='Flux1 vs. Flux2 for Main Target'
endif
; -----------------------------------------------------------------


fail_star:

endfor                          ; next star
; -----------------------------------------------------------------


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
  wire2 = wire2(0:cnt_max-1) ; remove unused data entries!
  
  ; ====================================================
  ; Correct the times for the new format

; FOR NEW FORMAT: two object with very different RA/DEC --- do

 print,'' & print,' %%%% WARNING: CONVERTING TO HJD ... !!! '
   for slot=0,nslot-1 do begin
   jd_temp = wire2.hjd(slot)
   wx  = where(jd_temp gt 4e4 and jd_temp lt 5.9e4,cx)
    ; wxb = where(jd_temp le 4e4 or jd_temp gt 5.9e4,cxb)

   if cx lt 5 then begin
     print,' %%% No valid times ... aborting ... '
     wire2(*).hjd(slot) = -1e9 
    goto, abort_hjd
   endif
 
   jd_temp = jd_temp(wx)
   hjd_temp = helio_jd( jd_temp, ra, dec)   ; Heliocentric Julian Date
   wire2(wx).hjd(slot) = hjd_temp

    ; debug:
    ; t000 = median(hjd_temp)
    ; plot, (hjd_temp-jd_temp)*86400.,ysty=3,psym=3 ; diff. in seconds
    ; plot,wire2(wx).hjd(0)-t000,wire2(wx).p(0,4),psym=3 ; ap 4, star 0    
    ; oplot,wire.hjd(0)-t000,wire.d(0,3,3)*4.0,psym=3,col=col.red; pix*4, star 0

   abort_hjd:

endfor ; next slot

 print,''

  outfile = ''
  a = strsplit(file(ff),'.',/extract) & na = n_elements(a)
  for i=0,na-2 do outfile = outfile + a(i) + '.'
  outfile = outfile + $
             'ph' + string(phot_mode,format='(I1)') + '.idl'

  save,filename=outfile,wire2,fl,xm,ym

  print,''
  print,' %%% Wire output file saved as: ' + outfile
  print,''
  ; ====================================================
endif else begin ; any valid data points to save?
  print,''
  print,' *** No valid data points for file: ' + file(ff)
  print,''
endelse


endfor      ; next wire file (from the program "wire_all.pro") is read
; -----------------------------------------------------------------

!P.multi = 0
; wdelete,0 ; delete window

; print, ' %%% Try to run .r wire_merge3.pro now!'

print,' %%% CCD stamps where 65000+ values were replaced: ',repair
m4_get_basedir, basedirr
repout = basedirr+'/wire_process/badpixels.idl'
print,' %%% Saved structure of bad pixels: '+repout
save,filename=repout, repair, repbad

nl = ' ====================================== '
print,nl
print, ' %%% Now launch wire_getpos.pro: '
print,nl


g = strsplit(file,'.',/extract)
ng = n_elements(g)
gg = ''
for l=0,ng-2 do gg = gg + g(l) + '.'
gg = gg + 'ph'
fp = findfile(gg + '*',Count=cnt)
if cnt ne 1 then begin
 print,' *** WARNING: File not found: ' + gg + '*'
 stop
endif

ref_file = fp(0)

print,' target = "' + target + '"'
print,' ref = "' + ref_file + '"
print,' wire_getpos, ref, target, $'
print,'              "'+ref_position_file+'", $'
print,'              "'+target_file+'"'

print,''
print,nl
print,''

END


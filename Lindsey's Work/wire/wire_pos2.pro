stop

; This version: I tried to fix x,y position and FWHM ... 
; no great improvement ... since x,y is very constant for 99% of the
; data points! (at least for Altair!!)

PRO wire_pos2,file,phot_mode,g_fwhm,e_fwhm,g_x,e_x,g_y,e_y

; Purpose:
; ---------
; Read wire result file(s) from the prg. wire_all.pro
; Then measure flux, sky, x-y centroid (2d-gauss fit), & center of light

; Examples:
; ----------
; Single wire result file:
;   wire_pos,'/ai38/bruntt/wire/data/991019.3142.1AltairA7V.idl',1
; Multiple wire result files:
;   spawn,'ls -1 /ai39/bruntt/wire/altair/altair_wire_*.idl',fil
;   wire_pos,fil,1

; 28/10/03
;   spawn,'ls -1 /ai39/bruntt/wire/altair/altair_wire_*.idl',fil
;   wire_pos2,fil,1,1.85895  , 0.004, 3.27182 ,  0.00657, 3.15866,   0.00906

; Experimental photometry technique (slower):
;   spawn,'ls -1 /ai39/bruntt/wire/altair/altair_wire_003.idl',fil
;   wire_pos,fil,2

; flux2 = gaussian - background2 * n_pix
; flux1 = total flux - background2 * n_pix
; p = aperture photometry ... ap. size scales with FWHM ("seeing")

nf = n_elements(file)
if nf eq 0 then return

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
progress_on = 2 ; 0 = all of, 1 = plot for every star, 2 = more progress plots (slower!)

if progress_on ge 1 then begin
  !P.multi = [0,2,2] ; multiple plots = progress
  window,1,xsize=450,ysize=450,title='WIRE - Basic CCD Reduction'
endif

for ff = 3,4 do begin
 ; for ff=0,nf-1 do begin
;for ff=9,nf-1 do begin

print,' %%% Restoring file '+file(ff)+' ...'
restore,file(ff)

ndp = n_elements(wire)
nslot = 5


nap = 9 ; number of apertures
inc = 1.4 & inc = sqrt(inc)
aperture = fltarr(nap)
aperture(0) = 0.6 ; size of the first aperture!
for i=0,nap-2 do aperture(i+1) = aperture(i) * inc
; The area of the aperture will increase by "inc", eg 1.25 ==> 25%.

;wire2 = replicate({x:fltarr(nslot),y:fltarr(nslot),hjd:dblarr(nslot),$
;                  co:fltarr(2,nslot),flux1:fltarr(nslot),flux2:fltarr(nslot),$
;                  gc:fltarr(2,nslot),backgr:fltarr(nslot),backgr2:fltarr(nslot),$
;                  p:fltarr(nslot,nap), a:bytarr(nslot,nap), fwhm:fltarr(nslot)}, ndp)

wire2 = replicate({x:fltarr(nslot),y:fltarr(nslot),hjd:dblarr(nslot),$
                  co:fltarr(2,nslot),flux1:fltarr(nslot),flux2:fltarr(nslot),$
                  gc:fltarr(2,nslot),backgr:fltarr(nslot),backgr2:fltarr(nslot),$
                  p:fltarr(nslot,nap), a:bytarr(nslot,nap), fwhm:fltarr(nslot), $
                  col:intarr(nslot), row:intarr(nslot)}, ndp)

;                  d:fltarr(nslot,8,8)}, ndp)
; wire2.d = wire.d

; In wire3 the saturated pixels are increased by 2^16.,
; thus the counts are correct, and the flux can be calculated!
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
    w = where(cut lt -100,c)
    if c ge 1 then begin
       cut(w) = cut(w) + add_digital_sat 
       wire3(j).d(i,*,*) = cut ; add 65536 if data is saturated (beware of low background counts)
    endif
 endfor 
endfor

xm = fltarr(nslot)
ym = fltarr(nslot)

for i=0,nslot-1 do begin ; overall x,y position of the 5 stars
  xm(i) = median(wire2.x(i,*))
  ym(i) = median(wire2.y(i,*))
endfor


; Loop to compute centre of light and overall flux level
print,''
for i=0,nslot-1 do begin ; for each star

; Progress indication:
 print,''
 print,'Star number = '+string(i,format='(I2)')
 print,'Data point out of '+string(ndp,format='(I6)') + ':'

 for j=0L,ndp-1 do begin
  if j mod 5000 eq 0 then print,j,format='(I7,$)'

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

 max_cen = max(ccd(2:5,2:5))
 max_oth = max(ccd)
 if max_oth / max_cen gt 1.1 then begin ; central pixels do not contain the maximum!
  wire2(j).flux1(i) = -19.9
  goto,skip_data
 endif

 aa = fltarr(7)
 aa(0) = avg([ccd(0,0),ccd(0,7),ccd(7,0),ccd(7,7)]) ; median(wire3(j).d(i,*,*))
 aa(1) = (max(wire3(j).d(i,2:5,2:5)) - aa(0)) * 0.95
 aa(2) = 0.9 & aa(3) = 0.9 ; typical gaussian sigma = FWHM / 2.35 
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

; a known bad ccd image ... hm.
; if (j eq 26374 and i eq 1 and $
;   file(ff) eq '/ai39/bruntt/wire/altair/altair_wire_010.idl') or $
;    (wire(j).stamp(i) eq '991103_211721.81') $
;     then begin
;   wire2(j).flux1(i) = -999.9
;   wire2(j).hjd(i) = wire2(j).hjd(i) * (-1.) ; mark bad data!
;   goto,skip_data
; endif

 aa1 = aa
 gg1 = gauss2dfit(ccd,aa1) ; fit all pixels

 fwhm_1 = avg(abs([aa1(2:3)]))

 if (fwhm_1 lt 0.5) or (fwhm_1) gt 5.5 or $
    (abs((aa1(1)-aa(1))/aa(1)) gt 2.5) then begin ; first fit bad?
      aa2 = aa
      aa2(4) = aa2(4) - 1.
      aa2(5) = aa2(5) - 1. ; offset central position one pixel
      gg2 = gauss2dfit(ccd(1:6,1:6),aa2) ; fits only central 6x6 pixels
       fwhm_2 = avg(abs([aa2(2:3)]))
        if (fwhm_2 lt 0.5) or (fwhm_2 gt 5.5) or $
           (abs((aa2(1)-aa(1))/aa(1)) gt 2.5) then begin
              wire2(j).flux1(i) = -99.9 ; second fit was also bad
              goto,skip_data
        endif
      aa = aa2 ; second fit was ok
      aa(4) = aa(4) + 1.
      aa(5) = aa(5) + 1. ; offset central position one pixel
      gg = gg2
 endif else begin
      aa = aa1 ; first fit was ok
      gg = gg1
 endelse


 background = aa(0) ; gaussian fit ==> sky background

 bb = [ccd(0,0),ccd(0,7),ccd(7,0),ccd(7,7)]
; resistant_mean,bb,3,me,sd,nr
 background2 = avg(bb) ; bg = corners of ccd

 wire2(j).co(0,i)  = colight_x
 wire2(j).co(1,i)  = colight_y ; center of light

 wire2(j).gc(0,i)  = aa(4)
 wire2(j).gc(1,i)  = aa(5) ; gaussian center (x,y)

 wire2(j).flux1(i) = flux - 64. * background2
 wire2(j).flux2(i) = total(gg) - 64. * background2 ; subtract background

 wire2(j).backgr(i)  = background  ; gaussian background
 wire2(j).backgr2(i) = background2 ; use median of four corners of CCD!

; ================================
; Do "fixed aperture" photometry:
; ================================
 ap_flux = fltarr(nap) ; fluxes in each aperture
  ss1 = abs( aa(2) ) ; may be negative ...
  ss2 = abs( aa(3) ) ; gaussian sigma
 
  fwhm     = g_fwhm
  fwhm_org = avg([ss1,ss2]) * 2.35 ; convert sigma to FWHM
 if abs(fwhm_org - g_fwhm) gt 4.0 * e_fwhm then goto,skip_data

  ap_max = 0

  use_x = aa(4)
  use_y = aa(5) ; x,y coordinates to use ... it seems the gaussian fit is better

  if use_x gt 5.5 or use_x lt 2.5 or $
     use_y gt 5.5 or use_y lt 2.5 then begin
       ; coordinates may be wrong!!?
       use_x = colight_x
       use_y = colight_y
  endif

; 28/10-03
  if i eq 0 then begin ; for altair == fixes x,y position
   if abs(use_x - g_x) gt 4.0 * e_x or $ 
      abs(use_y - g_y) gt 4.0 * e_y then goto,skip_data
   use_x = g_x
   use_y = g_y
  endif
 
 wire2(j).fwhm(i) = fwhm
 
 if fwhm lt 0.5 or fwhm gt 5. then begin ; currupt r_ap
   wire2(j).fwhm(i) = median(wire2(0:j-1).fwhm(0))
 endif

 for pp=0,nap-1 do begin ; for each aperture!
  r_ap = aperture(pp) * fwhm

;  print,' FWHM and aperture radii: ',fwhm, ' ap: ',r_ap
;  print,' Applied x,y center position: ',use_x, use_y

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

; Debug:
;  showim,ccd,background2*0.7,background2*1.3 
;  surface,ccd,charsi=2.0
;  s = robust_poly_fit(aperture, ap_flux, 2, myfit)

  wire2(j).p(i,*) = ap_flux ; j = frame number, i = star number

;  if ap_max eq 0 then ap_max = nap-1
;  wire2(j).a(i,*) = ap_max ; maximum aperture size used!

; >>> Progress plot >>>
if progress_on ge 2 then begin
 if (i eq 0) and (j le 6000) and ((j mod 500) eq 0) then begin ; plot a progress plot of a LC!
  !P.multi = [0,1,3]
   ptp_robust_fin,-2.5*alog10(wire2(0:j-1).p(0,8))+25.,noise,0
   tt = wire2(0:j-1).hjd(0) & wt = where(tt gt 50000.,ct)
   mt = median(tt(wt))
   t1 = 86400.*(min(tt(wt))-mt) & t2 = 86400.*(max(tt(wt))-mt)
   plot,86400.*(wire2(0:j-1).hjd(0)-mt),$
                wire2(0:j-1).p(0,8)-median(wire2(0:j-1).p(0,8)),$
        psym=3,symsi=.5,yr=[-2000,2000],tit='Aperture 8: '+string(noise,format='(F9.4)'),$
        charsi=1.5,xsty=3,ysty=3,xr=[t1,t2]
   ptp_robust_fin,-2.5*alog10(wire2(0:j-1).p(0,5))+25.,noise,0
   plot,86400.*(wire2(0:j-1).hjd(0)-mt),$
                wire2(0:j-1).p(0,5)-median(wire2(0:j-1).p(0,5)),$
        psym=3,symsi=.5,yr=[-2000,2000],tit='Aperture 5: '+string(noise,format='(F9.4)'),$
        charsi=1.5,xsty=3,ysty=3,xr=[t1,t2]
   ptp_robust_fin,-2.5*alog10(wire2(0:j-1).flux1(0))+25.,noise,0
   plot,86400.*(wire2(0:j-1).hjd(0)-mt),$
                wire2(0:j-1).flux1(0)-median(wire2(0:j-1).flux1(0)),$
        psym=3,symsi=.5,yr=[-2000,2000],tit='Buzasi Phot: '+string(noise,format='(F9.4)'),$
        charsi=1.5,xsty=3,ysty=3,xr=[t1,t2]
  !P.multi=[0,2,2]
 endif

endif
; <<< end of progress plot <<<

; plot,wire2(0:199).hjd(0),(wire2(0:199).p(0,6)-wire2(0:199).p(0,4))/wire2(0:199).flux2(0),yr=[-.1,.1],psym=1,symsi=.5
; plot,wire2(0:199).hjd(0),wire2(0:199).p(0,6),yr=[5.4,5.9]*1e4
; oplot,wire2(0:199).hjd(0),wire2(0:199).p(0,4)
; oplot,wire2(0:199).hjd(0),wire2(0:199).flux2(0),psym=1,symsi=.5
; oplot,wire2(0:199).hjd(0),wire2(0:199).flux1(0),psym=1,symsi=.5

  skip_data:

 endfor ; next data point

if progress_on eq 1 then begin
 print,''
 plot,xm,ym,xr=[0,1000],yr=[0,1000],psym=4,tit='CCD position of stars'

 ;plot,wire2.hjd(0),wire2.x(0,*),psym=3
 ;plot,wire2.hjd(0),wire2.x(1,*),psym=3
 ;plot,wire2.hjd(0),wire2.x(2,*),psym=3

 plot,wire2.co(1,0),wire2.co(0,0),psym=3,xr=[0,7],yr=[0,7],tit='Center of Light Pos'
  oplot,wire2.co(1,1),wire2.co(0,1),psym=3,col=col.red
  oplot,wire2.co(1,2),wire2.co(0,2),psym=3,col=col.yellow
  oplot,wire2.co(1,3),wire2.co(0,3),psym=3,col=col.green
  oplot,wire2.co(1,4),wire2.co(0,4),psym=3,col=col.sky 

 plot,wire2.hjd(0),wire2.flux2(0),psym=3,tit='Time vs. Flux for Main Target'

 plot,wire2.flux1(0),wire2.flux2(0),psym=3,tit='Flux1 vs. Flux2 for Main Target'
endif

endfor ; next star


fl = fltarr(nslot) ; median flux of each star
fl(*) = -99.9 ; assume bad data!

 for i=0,nslot-1 do begin
   fl_all = wire2.flux2(i)       ; all measured fluxes
   ww = where(fl_all gt 0.0,cok) ; reject bad points
   if cok ge (ndp*0.25) then fl(i) = median(fl_all(ww))
 endfor

; ====================================================
outfile = ''
a = strsplit(file(ff),'.',/extract) & na = n_elements(a)
for i=0,na-2 do outfile = outfile + a(i) + '.'
outfile = outfile + 'idl'+string(phot_mode,format='(I1)') + 'xfix'

save,filename=outfile,wire2,fl,xm,ym

print,''
print,' %%% Wire output file saved as: '+outfile
print,''
; ====================================================

endfor ; next wire file (from the program "wire_all.pro") is read

!P.multi = 0
; wdelete,0 ; delete window

print, ' %%% Try to run .r wire_merge3.pro now!'

end


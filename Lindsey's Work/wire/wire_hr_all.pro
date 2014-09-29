; Plot final HR diagram for all WIRE stars
; Optionally, plot certain WIRE stars with special symbols, e.g. data
; contained in the wire structure == for a paper?

; (c) April 2005 -- Hans Bruntt

; ===================================================================
m4_get_basedir,base
; solar = string(110B)
solar = '!D!9n!3'

outname = base+'wire_process/wire_params_Nov2006.idl' ; wire_comp_teff_calibr
restore,outname

pamyat_on = 0B

hdout = 0B ; print HD numbers?
masson = 0B ; print mass (only if hdout = 1)
fehon = 0B ; print Fe/H (only if hdout = 1)
metal_effect = 0B ; plot lejeune for model with Z = 0.008
diff_sym = 0B ; use different symbols for different kind of data / types of star
typical_error = 0B ; plot typical error bars in lower left corner?
hr_tit = 'Hertzsprung-Russell diagram'

hr_tit = ''
mark_psicen  = 0B ; mark the double star Psi Cen ?
only_mainseq = 0B ; plot only main seq stars, ie. lum-class >= 4
txt_out      = 0B ; explain what kinds of stars are in the diagram
explain_axis = 0B

mainsymbol = 0


; No Teff for these stars:
hd_avoid = [210839,149757,8890,  41361,113226,132905,161178]

; From the program wire_bstars.pro: work in B-type stars
if n_elements(wire) eq 0 then $
 restore,base+'wire_process/reduced_B.idl'
; ===================================================================

; ===================================================================
dops = 1
cz = 1.0
sz = 2.0 & if dops then sz = .8
evs = -6 ; *mark* points on evol. tracks
evs = 0 ; do not mark points on evol. tracks

; ===================================================================


; ===================================================================
mark_stars_below_zams = 0B
mark_stars_in_wire = 1B
indiv_err_bars = 0.
init_missing = 1B ; must start at 1
; ===================================================================

; ===================================================================
; Do not plot these stars:
; ===================================================================
avoid = [81188L ,127972, 210839, 2905, 63975, 173300,$
 218045,112185,119921]
;; avoid = [0,1]
reason = ['Kappa Vel == bad data',$ ; 
          'EtaCen slot 0 == bad data (artif. data)', 'x,y dependency', $
          'New star - not reduced','New star - not reduced','New star - not reduced',$
          'strong x-y dependence','Not reduced','Not reduced' ]
print,' %%% Not plotting these stars: '
for i=0,n_elements(avoid)-1 do $
 print,avoid(i),reason(i), format='(I10,A40)'

; ===================================================================
 
; ===================================================================
if dops eq 0 then $
 window, 1, title='WIRE: HR Diagram for B-type stars',xsize=700,ysize=700,xpos=750,ypos=500
; ===================================================================
easyps, keywords, kk, dops=dops, /prepare, dim = [15,15,-1,1], $
 fil = 'bstars_hr_fin.eps',$
 dir = base+'papers/wire/bstars/'
if dops then col=getcolor(/load)

; ===================
; FINAL HR PLOT:
; ======================================================================

x = 20 & xx = [4.67,3.52] & yy = [-.8,5.5] ; all stars
; x = 20 & xx = [4.55,3.90] & yy = [1.5,5.] ; zoom in on B-stars

; Default x + y titles:
xtt = 'log(!17T!Ieff!N!3)' & ytt = 'log(!17L/L!I'+solar+'!N!N!3)'

; Explanation of x,y axis included (for non-experts):
xtt = 'log(!17T!Ieff!N!3)' & ytt = 'log(!17L/L!I'+solar+'!N!N!3)'
if explain_axis then begin
 ytt = ytt + ' Brightness'
 xtt = 'Temperature = ' + xtt
endif

plot,alog10(wireobj3.teff),alog10(wireobj3.lumn),psym=7,xr=xx,yr=yy, $
 xtit=xtt, ytit=ytt, /nodata, tit=hr_tit

w = where(wireobj3.teff gt 500.,n)
if diff_sym then plotsym,0,/fill else plotsym,mainsymbol,/fill

if mark_stars_in_wire eq 0 then begin
  oplot,alog10(wireobj3(w).teff),alog10(wireobj3(w).lumn),psym=8,symsi=sz 
endif else begin

; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  for k=0,n-1 do begin
   ww_bad = where(avoid eq wireobj3(w(k)).hd,cc_bad)
   if cc_bad ge 1 then goto, bad_star

; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   ww = where(wire.hd eq wireobj3(w(k)).hd,cc)  ; IN WIRE STRUCT?
   if diff_sym then plotsym,0,/fill else plotsym,mainsymbol,/fill
   ; plotsym,0 ; open symbols = default

  if cc ge 1 then begin
     if diff_sym then plotsym,0,/fill ; bad data
       ww = ww(0)
       if wire(ww).tobs gt 10. and wire(ww).noise10 le 20 then $
         if diff_sym then plotsym,8,/fill ; good data syncronize with wire_bstars (easyplot ---> 'bstars_hr.ps')
   endif

   if cc eq 0 and alog10(wireobj3(w(k)).teff) gt 4. then begin
     if init_missing then begin
        print,' *** MISSING HOT STARS IN wire structure (run wire_bstars.pro to update): '
        print,'     HD          SPEC1       B-V      V   Teff  L/Ls'
        init_missing = 0B
     endif

     print,wireobj3(w(k)).hd, wireobj3(w(k)).spec1, wireobj3(w(k)).bv,wireobj3(w(k)).v, $
           wireobj3(w(k)).teff, wireobj3(w(k)).lumn, $
     format='(I9, A15, F8.2, F8.1, I6, F9.1)'

 endif
; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

; PSI CEN, HD 125473
   sz_use = sz ; symbol size is normal
   if mark_psicen then begin
    if wireobj3(w(k)).hd eq 125473L then begin
       plotsym,mainsymbol,/fill &      sz_use = 3
       xyouts,alog10(wireobj3(w(k)).teff)+.03,alog10(wireobj3(w(k)).lumn)-.25,'!4w!3Cen',charsi=.9
   endif

; Beta Hyi (not included yet, febr' 2006)
    if wireobj3(w(k)).hd eq 2151L then begin
       plotsym,mainsymbol,/fill &      sz_use = 3
   endif

; Alpha Cir (lumclass = 0)
    if wireobj3(w(k)).hd eq 128898L then begin
       plotsym,mainsymbol,/fill &      sz_use = 3
    endif


   endif

; Plot position of star if inside x,y limits:
   wav = where(wireobj3(w(k)).hd eq hd_avoid,cav) ; Teff not reliable?

   if only_mainseq then lum_limit = 4 else lum_limit = -1
   if cav eq 0 then $
   if wireobj3(w(k)).lumclass ge lum_limit then $
   if alog10(wireobj3(w(k)).teff) gt min(xx) and alog10(wireobj3(w(k)).teff) lt max(xx) and $
      alog10(wireobj3(w(k)).lumn) gt min(yy) and alog10(wireobj3(w(k)).lumn) lt max(yy) then $
   plots,alog10(wireobj3(w(k)).teff),alog10(wireobj3(w(k)).lumn),psym=8,symsi=sz_use ; was sz?
  
   txtout = strcompress(string(wireobj3(w(k)).hd,format='(I8)'),/remove_all)
   if masson then $
    if wireobj3(w(k)).mass_templogg gt .1 then $
   txtout = txtout + '(' + strcompress(string(wireobj3(w(k)).mass_templogg,format='(F9.1)'),/remove_all)+')'
   if fehon then $
    if wireobj3(w(k)).feh_nap lt 6. then $
   txtout = txtout + '[' + strcompress(string(wireobj3(w(k)).feh_nap,format='(F9.1)'),/remove_all)+']'
            

   dxhd = -0.01 & dyhd = 0.03
       if hdout then $
        if alog10(wireobj3(w(k)).teff) gt min(xx) and alog10(wireobj3(w(k)).teff) lt max(xx) and $
           alog10(wireobj3(w(k)).lumn) gt min(yy) and alog10(wireobj3(w(k)).lumn) lt max(yy) then $
        xyouts,alog10(wireobj3(w(k)).teff)+dxhd,alog10(wireobj3(w(k)).lumn)+dyhd, $
          txtout, $
          charsi=.6,charthick=1
  

   bad_star:
 endfor

endelse




if indiv_err_bars then begin
 ; wireobj3(w(k)).eteff
 err_teff = fltarr(n) & err_teff = wireobj3(w).teff * 0.05 
 for kx=0,n-1 do $
  oplot,[alog10(wireobj3(w(kx)).teff-err_teff(kx)), $
         alog10(wireobj3(w(kx)).teff+err_teff(kx))] , $
       [alog10(wireobj3(w(kx)).lumn) ,alog10(wireobj3(w(kx)).lumn) ]

  ; wireobj3(w(k)).elumn
  err_lumn = fltarr(n) & err_lumn =wireobj3(w).lumn *  0.2
  for kx=0,n-1 do $
   oplot,[alog10(wireobj3(w(kx)).teff), alog10(wireobj3(w(kx)).teff)] , $
        [alog10(wireobj3(w(kx)).lumn-err_lumn(kx)),$
         alog10(wireobj3(w(kx)).lumn+err_lumn(kx)) ]
endif

if typical_error then begin
 t = 10^4.6 & et = t*0.05 & l = 10^0.0  & el = l*0.20
 oplot, [alog10(t-et),alog10(t+et)], [alog10(l),alog10(l)],thick=3
 oplot, [alog10(t),alog10(t)], [alog10(l-el),alog10(l+el)],thick=3
 xyouts, 4.53, alog10(l) - 0.04, 'Typical error bars',charsi=cz,charthick=2.0
endif

import_lejeune, base+'evol/lejeune/modc020.dat', l
ev_max = n_elements(l)

for i=5,ev_max-1  do oplot, l(i).teff(0:x), l(i).l(0:x),psym=evs,symsi=.3
xo = 0 & aa = 1.0  ; xo = x & aa = 0.0
offtxt_y = -0.1
offtxt_x = 0.01
for i=5,ev_max-1 do $
 xyouts,l(i).teff(xo) + offtxt_x, l(i).l(xo) + offtxt_y,$
  strcompress(string(l(i).m,format='(F9.1)'),/remove_all), align=aa, $
  charsi=cz

xo20 = 10 & offtxt_x20 = 0.05 & offtxt_y20 = -0.7

if metal_effect then begin
 import_lejeune, base+'evol/lejeune/modc008.dat', l20
  oplot, l20(xo20).teff(0:x), l20(xo20).l(0:x),psym=evs,symsi=.3, col=col.sky
  xyouts,l20(xo20).teff(0) + offtxt_x20, l20(xo20).l(0) + offtxt_y20,$
   strcompress(string(l20(xo20).m,format='(F9.1)'),/remove_all)+ ' Z=0.008', align=aa,col=col.sky,$
  charsi=cz

xo04 = 10 & offtxt_x04 = 0.1 & offtxt_y04 = 0.1

import_lejeune, base+'evol/lejeune/modc004.dat', l04
 oplot, l04(xo04).teff(0:x), l04(xo04).l(0:x),psym=evs,symsi=.3, col=col.red
 xyouts,l04(xo04).teff(0)+ offtxt_x04, l04(xo04).l(0) + offtxt_y04,$
  strcompress(string(l04(xo04).m,format='(F9.1)'),/remove_all) + ' Z=0.004', align=aa,col=col.red,$
  charsi=cz

oplot,[l04(xo04).teff(0),l04(xo04).teff(0) + offtxt_x04], $
      [l04(xo04).l(0), l04(xo04).l(0) + offtxt_y04], $
 col=col.red

oplot,[l20(xo20).teff(0),l20(xo20).teff(0) + offtxt_x20], $
      [l20(xo20).l(0), l20(xo20).l(0) + offtxt_y20], $
 col=col.sky
endif ; plot isochrones with different metallicity


zams = transpose( [ [l.teff(0)], [l.l(0)] ] )
s = sort(zams(0,*)) & zams = zams(*,s)
lzams = interpol(zams(1,*), zams(0,*), alog10(wireobj3.teff))

if mark_stars_below_zams then begin
; wlow = where(10.^(lzams) gt wireobj3.lumn*1.8 and wireobj3.lumn gt .1,c)
; plotsym,0
; oplot,alog10(wireobj3(wlow).teff),alog10(wireobj3(wlow).lumn),psym=8,symsi=3,col=col.red
endif

g = where(wireobj3.teff gt 28000.,c)
; oplot,alog10(wireobj3(g).teff),alog10(wireobj3(g).lumn),psym=8,symsi=2,col=col.green

if pamyat_on then $
 pamyat,x,blue=3,spb=2,betacep=2, /txtout, cz=cz


; ===================================================================

; Explain where the stars are located?
if txt_out then begin
 dx = .1
 xyouts,3.9+dx,-0.3,'Solar-like',charthick=2,charsi=1,alignment=1
 xyouts,4.1+dx,.5,'!4d!3-Scuti',charthick=2,charsi=1,alignment=1
 xyouts,4.35+dx,2,'B-type',charthick=2,charsi=1,alignment=1

 dx1 = .02 & dx2 = -.17 & dy1 = .08
 arrow,/data,thick=2, 3.9+dx-dx1,-.3+dy1,3.9+dx+dx2, .0 ; solar-like
 arrow,/data,thick=2, 4.1+dx-dx1,.5+dy1,4.1+dx+dx2, 1. ; delta scuti
 arrow,/data,thick=2, 4.35+dx-dx1,2+dy1,4.35+dx+dx2, 2.5 ; delta scuti

if explain_axis then begin
 arrow,/data,thick=6, 4.47, -1.5, 4.63, -1.5 ; temperature increasing to the left
 arrow,/data,thick=6, 4.79, 4.2, 4.79, 5.3 ; temperature increasing to the left
endif


endif

easyps, keywords, kk, dops=dops, /close
col=getcolor(/load)

; Debugging
; wd = where(alog10(wireobj3.teff) gt 4.,c)
; oplot, alog10(wireobj3(wd).teff), alog10(wireobj3(wd).lumn),psym=7,col=col.green,symsi=3

END

; Compare the teff calitrations: ramirez vs. templogg
; This program must be run before wire_hr_all.pro

 slack_giant_bv=0.1
 slack_giant_by=0.1

; Notes in specific stars:
; HD 127972: B1.5Vne  99.00  99.00  99.00   2.58   2.32  -0.15
;            MS, bright, has B-V, but no stromgren indices
;            Teff could be guessed from a typical B1.5V type star with
;            similar B-V

plot_metal_effect = 0B ; plot ev. tracks with Z = 0.004 / 0.008

m4_get_basedir,base
 restore,base + 'wire/wire_essential/wire_sec_info.idl' ; from: .r wire_combine_info.pro
 wire_ramirez, wireobj, wireobj2,slack_giant_bv=slack_giant_bv
 ; wire_prep_templogg, wireobj2

; wire_read_templogg, wireobj2, base + 'wire/wiretargOUTcastelli.csv',calib='Cas',/debug
; wire_read_templogg, wireobj2, base + 'wire/wiretargOUTribas.csv',   calib='Rib',/debug
; wire_read_templogg, wireobj2, base + 'wire/wiretargOUT.csv',        calib='Nap',/debug

 wire_read_templogg, wireobj2, base + 'wire/wire_field/templogg_outCAS.txt',calib='Cas',/debug
 wire_read_templogg, wireobj2, base + 'wire/wire_field/templogg_outRIBAS.txt', calib='Rib',/debug
 wire_read_templogg, wireobj2, base + 'wire/wire_field/templogg_outNAP.txt', calib='Nap',/debug

plot,wireobj2.bv, wireobj2.v,psym=2,xr=[-1,3],yr=[7,-1]

; ================================================================
; B-V calibration valid for M-S stars:
; ================================================================
xm2 = where(wireobj2.bv gt 0.3 and wireobj2.bv lt 1.5 and $
          wireobj2.lumclass ge 4 and wireobj2.lumclass le 5 and $
          wireobj2.teff gt 500.,cx2)
oplot,wireobj2(xm2).bv,wireobj2(xm2).v,col=col.yellow,psym=2

; ================================================================
; b-y calibration valid for M-S stars:
; ================================================================
xm = where(wireobj2.by gt 0.23 and wireobj2.by lt 0.8 and $
            wireobj2.lumclass ge 4 and wireobj2.lumclass le 5 and $
          wireobj2.teff gt 500.,cx)

xm_nap_fail = where(wireobj2.by gt 0.23 and wireobj2.by lt 0.8 and $
            wireobj2.lumclass ge 4 and wireobj2.lumclass le 5 and $
            wireobj2.teff gt 500. and wireobj2.teff_nap lt 500.,cm_nap_fail)
 
oplot,wireobj2(xm).bv,wireobj2(xm).v,col=col.green,psym=6
; ================================================================

; ================================================================
; GIANTS:
; ================================================================


; B-V calibration valid for GIANT stars:
xg2 = where(wireobj2.bv gt 0.19 and wireobj2.bv lt (1.67+slack_giant_bv) and $
          wireobj2.lumclass ge 2 and wireobj2.lumclass le 3 and $
          wireobj2.teff gt 500.,cg2)

; b-y calibration valid for GIANT stars:
xg = where(wireobj2.by gt 0.05 and wireobj2.by lt 1.08 and $
          wireobj2.lumclass ge 2 and wireobj2.lumclass le 3 and $
          wireobj2.teff_nap gt 500.,cg)

xg_nap_fail = where(wireobj2.by gt 0.05 and wireobj2.by lt 1.08 and $
          wireobj2.lumclass ge 2 and wireobj2.lumclass le 3 and $
          wireobj2.teff gt 500. and wireobj2.teff_nap lt 500.,cx_nap_fail)

xg_ram_fail = where(wireobj2.by gt 0.05 and wireobj2.by lt 1.08 and $
          wireobj2.lumclass ge 2 and wireobj2.lumclass le 3 and $
          wireobj2.teff lt 500. and wireobj2.teff_nap lt 500.,cx_ram_fail)

if cg ge 2 then $
 oplot,wireobj2(xg).bv,wireobj2(xg).v,col=col.yellow,psym=4,symsi=2,thick=2
if cg2 ge 2 then $
 oplot,wireobj2(xg2).bv,wireobj2(xg2).v,col=col.green,psym=4,symsi=2,thick=2

; ================================================================

; ================================================================
; templogg calibration available (reg. b-y):
; ================================================================
tg = where(wireobj2.teff_nap gt 500,cnap)
if cnap ge 2 then $
 oplot,wireobj2(tg).bv,wireobj2(tg).v,col=col.sky,psym=6,symsi=1.5,thick=1
; ================================================================

; Adopt the preferred Teff / logg:
wireobj3 = wireobj2

wireobj3.teff = 0.
wireobj3.lcfile3 = ''
wireobj3.eteff = -100.
wireobj3.logg = -5.
wireobj3.elogg = -5.

; ===============================================
; TEMPERATURES:
; ===============================================

; Giants with Teff B-V calibr.
wireobj3(xg2).teff = wireobj2(xg2).teff
wireobj3(xg2).eteff = wireobj2(xg2).eteff
wireobj3(xg2).lcfile3 = wireobj2(xg2).lcfile3

; Giants with Teff from b-y calibr. preferred over B-V calibr.
wireobj3(xg).teff = wireobj2(xg).teff_nap
wireobj3(xg).eteff = wireobj2(xg).eteff_nap
wireobj3(xg).lcfile3 = wireobj2(xg).lcfile3

; M-S with Teff B-V calibr.
wireobj3(xm2).teff = wireobj2(xm2).teff
wireobj3(xm2).eteff = wireobj2(xm2).eteff
wireobj3(xm2).lcfile3 = wireobj2(xm2).lcfile3

; M-S with Teff from b-y calibr. preferred over B-V calibr.
wireobj3(xm).teff = wireobj2(xm).teff_nap
wireobj3(xm).eteff = wireobj2(xm).eteff_nap
wireobj3(xm).lcfile3 = wireobj2(xm).lcfile3

; Templogg calibration is the preferred calibration:
wireobj3(tg).teff = wireobj2(tg).teff_nap
wireobj3(tg).eteff = wireobj2(tg).eteff_nap
wireobj3(tg).lcfile3 = 'Teff from templogg / Napi calibr.'


; ===============================================
; LOG G:
; ===============================================

; Giants with Logg B-V calibr.
wireobj3(xg2).logg = wireobj2(xg2).logg
wireobj3(xg2).elogg = wireobj2(xg2).elogg
wireobj3(xg2).lcfile2 = wireobj2(xg2).lcfile2

; Giants with Logg from b-y calibr. preferred over B-V calibr.
wireobj3(xg).logg = wireobj2(xg).logg_nap
wireobj3(xg).elogg = wireobj2(xg).elogg_nap
wireobj3(xg).lcfile2 = wireobj2(xg).lcfile2

; M-S with Logg B-V calibr.
wireobj3(xm2).logg = wireobj2(xm2).logg
wireobj3(xm2).elogg = wireobj2(xm2).elogg
wireobj3(xm2).lcfile2 = wireobj2(xm2).lcfile2

; M-S with Logg from b-y calibr. preferred over B-V calibr.
wireobj3(xm).logg = wireobj2(xm).logg_nap
wireobj3(xm).elogg = wireobj2(xm).elogg_nap
wireobj3(xm).lcfile2 = wireobj2(xm).lcfile2

wireobj3(tg).logg = wireobj2(tg).logg_nap
wireobj3(tg).elogg = wireobj2(tg).elogg_nap
wireobj3(tg).lcfile2 = 'Logg from templogg / Napi calibr.'

wireobj3(xg_nap_fail).teff    = wireobj2(xg_nap_fail).teff
wireobj3(xg_nap_fail).lcfile3 = wireobj2(xg_nap_fail).lcfile3

wireobj3(xm_nap_fail).teff    = wireobj2(xm_nap_fail).teff
wireobj3(xm_nap_fail).lcfile3 = wireobj2(xm_nap_fail).lcfile3

; ===============================================
; Special cases:
; ===============================================

; ===============================================
sp1 = where(wireobj3.hd eq 127972,cp1)
; ===============================================
; Star has Hbeta, but no by,c1 etc, has B-V. 
; Get Teff from spectral type.
; Get logg from stars with similar teff
; ===============================================
plots,wireobj3(sp1).bv, wireobj3(sp1).v, psym=7, col=col.cyan,thick=2,symsi=3
bv_ref = wireobj3(sp1).bv
wsp1 = where(wireobj3.spec1 eq 'B2V',csp1)
plots,wireobj3(wsp1).bv, wireobj3(wsp1).v, psym=1, col=col.sky,thick=3,symsi=3
oplot,[wireobj3(wsp1).bv,wireobj3(sp1).bv],[wireobj3(wsp1).v, wireobj3(sp1).v],col=col.sky

wireobj3(sp1).teff = wireobj3(wsp1).teff
wireobj3(sp1).eteff = 750. ; wireobj3(wsp1).eteff
wireobj3(sp1).lcfile3 = 'Teff from spectral type B2V = HD 175191'
xx = where( abs(wireobj3(sp1).teff-wireobj3.teff) lt 1500. and $
            wireobj3.lumclass ge 4 and wireobj3.lumclass le 5 and $
            wireobj3.logg gt 2.,cxx)
wireobj3(sp1).logg = median(wireobj3(xx).logg)
wireobj3(sp1).lcfile2 = 'logg from stars with similar teff/spec. type n = ' + $
 strcompress(cxx)

; ===============================================
sp1 = where(wireobj3.hd eq 128898,cp1)
; ===============================================
; Star has Hbeta, but no by,c1 etc, has B-V. 
; Get Teff from spectral type.
; Get logg from stars with similar teff
; ===============================================
plots,wireobj3(sp1).bv, wireobj3(sp1).v, psym=7, col=col.red,thick=2,symsi=3
; ===============================================


; Debugging
; for i=0,174 do print,wireobj3(i).lcfile3             
; w = where(wireobj3.lcfile3 eq 'giant teff from B-V',c)
; print,wireobj3(w).by

print,' %%% Stars with no temperature: '

 w = where(wireobj3.lcfile3 eq '',c)

 w = where(wireobj3.lcfile3 eq '' and wireobj3.v lt 5.,c)

 w = where(wireobj3.teff lt 2000. and wireobj3.bv lt 1.67+slack_giant_bv,c)
 for i=0,c-1 do $
   print,wireobj3(w(i)).hd, wireobj3(w(i)).spec1, $
         wireobj3(w(i)).by, wireobj3(w(i)).c1, $
         wireobj3(w(i)).m1, $
         wireobj3(w(i)).hbeta, wireobj3(w(i)).v, wireobj3(w(i)).bv, $
         wireobj3(w(i)).lcfile3, $
   format='(I8, A8, 6F7.2, X,A25)'

plotsym,0,/fill
oplot,wireobj3(w).bv, wireobj3(w).v, psym=8,symsi=3,col=col.yellow,thick=3
print,' %%% Stars plotted with LARGE YELLOW CIRCLES have no Teff!'

w = where(abs(wireobj3.bv-0.7) lt .3 and wireobj3.teff lt 500 and wireobj3.v gt 4.,c)

plotsym,0
got = where(wireobj3.teff gt 500.,c)
oplot,wireobj3(got).bv,wireobj3(got).v,$
 psym=8,symsi=2.5,thick=1,col=col.red

print,' %%% Slack in colour for Ramirez et al. giant calibration:'
xslack = where(strmatch(wireobj3.lcfile3,'*SLACK*') eq 1,c)
for k=0,c-1 do print,wireobj3(xslack(k)).lcfile3

xslack = where(strmatch(wireobj3.lcfile3,'*SLACK-BV*') eq 1,c)
if c ge 2 then oplot,wireobj3(xslack).bv,wireobj3(xslack).v,$
 psym=8,symsi=3.2,thick=3,col=col.magenta
xslack = where(strmatch(wireobj3.lcfile3,'*SLACK-by*') eq 1,c2)
if c2 ge 2 then oplot,wireobj3(xslack).bv,wireobj3(xslack).v,$
 psym=8,symsi=3.2,thick=3,col=col.charcoal

wx = where(wireobj3.hd eq 206952,cx)
plots,wireobj3(wx).bv,wireobj3(wx).v,psym=2,col=col.sky,thick=4,symsi=3

plots,1.67+slack_giant_bv,!y.crange,line=2
plots,1.67,!y.crange,line=2

hitme,s9

plot,wireobj3.teff/1000.,wireobj3.logg,psym=7,xtit='Teff/1000',ytit='logg'

; Stars with logg determined:
w = where(wireobj3.logg gt 0. and wireobj3.lumclass eq 3,c) ; GIANTS
oplot,wireobj3(w).teff/1000.,wireobj3(w).logg,psym=2,col=col.red
mlogg_giant = round(median(wireobj3(w).logg)*10.) / 10.
emlogg_giant = round(median(wireobj3(w).elogg)*10.) / 10.
nmlogg_giant = c
plots,!x.crange,mlogg_giant,line=2,col=col.red
plots,!x.crange,mlogg_giant+emlogg_giant,line=5,col=col.red
plots,!x.crange,mlogg_giant-emlogg_giant,line=5,col=col.red

w = where(wireobj3.logg gt 0. and wireobj3.lumclass ge 4,c)
oplot,wireobj3(w).teff/1000.,wireobj3(w).logg,psym=2,col=col.sky


w = where(wireobj3.logg gt 1. and wireobj3.lumclass ge 4 and $
          strmatch(wireobj3.spec1,'*G*') eq 1,c)
mlogg_g_ms = round(median(wireobj3(w).logg)*10.) / 10.
emlogg_g_ms = round(median(wireobj3(w).elogg)*10.) / 10.
nmlogg_g_ms = c

; Main sequence stars with no/currupt logg:
w = where(wireobj3.logg lt 2. and wireobj3.lumclass ge 4,c)
oplot,wireobj3(w).teff/1000.,wireobj3(w).logg,psym=2,col=col.sky
wireobj3(w).logg = mlogg_g_ms
wireobj3(w).elogg = emlogg_g_ms
wireobj3(w).lcfile2 = wireobj3(w).lcfile2 + $
 'MS star: logg is mean val. for G-type MS stars with logg from templogg!'

; Giant stars with no logg determination: either since only B-V is
; available or because templogg failed: USE MEAN logg for giant stars
; WITH logg already determined by templogg (17 giants)

w = where(wireobj3.logg lt 0. and wireobj3.lumclass le 3,c) ; GIANTS
oplot,wireobj3(w).teff/1000.,wireobj3(w).logg,psym=2,col=col.red
for j=0,c-1 do print,$
 wireobj3(w(j)).hd, wireobj3(w(j)).lcfile3, wireobj3(w(j)).teff, format='(I9,A35,I9)'        
wireobj3(w).logg = mlogg_giant
wireobj3(w).elogg = emlogg_giant
wireobj3(w).lcfile2 = wireobj3(w).lcfile2 + $
 'Giant star: logg is mean val. for giants with logg from templogg!'

; Find BC from Bessell 1998:
w = where(wireobj3.teff gt 500. and wireobj3.logg gt 0.,c)
for j=0,c-1 do begin
 teff = wireobj3(w(j)).teff
 logg = min([wireobj3(w(j)).logg, 5.0]) ; max calibr. in Bessell ig logg = 5.0
 bessell, teff, logg, bc
 wireobj3(w(j)).bc = bc
endfor

plot,wireobj3.teff/1000.,wireobj3.bc,psym=7,$
 max_value=20,xtit='Teff/1000',ytit='B.C. (Bessell)',ysty=3,symsi=.5

w = where(wireobj3.teff gt 500. and wireobj3.logg gt 0. and wireobj3.par gt 1e-6,c)
for i=0,c-1 do begin
 extinc = wireobj3(w(i)).extby
 if extinc lt -0.01 then extinc = 0.0
 err_extinc = wireobj3(w(i)).e_extby
 if err_extinc lt -0.01 or err_extinc gt 0.1 then extinc = 0.02

 err_bc = wireobj3(w(i)).ebc
 if err_bc lt -0.01 or err_bc gt 0.1 then err_bc = abs(bc * 0.05)
 

 par = wireobj3(w(i)).par ; parallax in arc seconds
 epar = wireobj3(w(i)).epar ; error on parallax in arc seconds
 bc = wireobj3(w(i)).bc

; d = distance = 1/parallax (parsec <---> arcsconds)

 wireobj3(w(i)).mbol = wireobj3(w(i)).v + 4.3 * extinc + 5. - 5. * alog10(1000./par) + bc

 variance = 0.015^2. + (4.3 * err_extinc)^2. + $
   ( (5./alog(10.)) * (1000. /par) * (epar/1000.)  )^2. + (err_bc)^2.
 wireobj3(w(i)).embol = sqrt(variance)

 wireobj3(w(i)).lumn  = 10.^((4.75 - wireobj3(w(i)).mbol)/2.5)

; Determine error on luminosity by inserting +- 1-sig. error on Mbol !!
 elumn1 = 10.^((4.75 - (wireobj3(w(i)).mbol+wireobj3(w(i)).embol))/2.5)
 elumn2 = 10.^((4.75 - (wireobj3(w(i)).mbol-wireobj3(w(i)).embol))/2.5)
 wireobj3(w(i)).elumn = abs(elumn2 - elumn1) * 0.5

endfor

; elumn1 = 10.^((4.75 - (wireobj3.mbol+wireobj3.embol))/2.5)
; elumn2 = 10.^((4.75 - (wireobj3.mbol-wireobj3.embol))/2.5)

; ======================================================================
; FINAL PLOT:
; ======================================================================

x = 20 & xx = [4.67,3.52] & yy = [-.5,5.5]
plot,alog10(wireobj3.teff),alog10(wireobj3.lumn),psym=7,xr=xx,yr=yy, $
 xtit='log(!17T!Ieff!N!3)',ytit='log(!17L/L!Isun!N!N!3)',/nodata

w = where(wireobj3.teff gt 500.,n)
plotsym,0,/fill
 oplot,alog10(wireobj3(w).teff),alog10(wireobj3(w).lumn),psym=8

; wireobj3(w(k)).eteff
err_teff = fltarr(n) & err_teff = wireobj3(w).teff * 0.05 * 0.0
for k=0,n-1 do $
 oplot,[alog10(wireobj3(w(k)).teff-err_teff(k)), $
        alog10(wireobj3(w(k)).teff+err_teff(k))] , $
       [alog10(wireobj3(w(k)).lumn) ,alog10(wireobj3(w(k)).lumn) ]

; wireobj3(w(k)).elumn
err_lumn = fltarr(n) & err_lumn =wireobj3(w).lumn *  0.3 * 0.0
for k=0,n-1 do $
 oplot,[alog10(wireobj3(w(k)).teff), alog10(wireobj3(w(k)).teff)] , $
       [alog10(wireobj3(w(k)).lumn-err_lumn(k)),$
        alog10(wireobj3(w(k)).lumn+err_lumn(k)) ]

t = 10^4.6 & et = t*0.05 & l = 10^0.0  & el = l*0.20
oplot, [alog10(t-et),alog10(t+et)], [alog10(l),alog10(l)],thick=3
oplot, [alog10(t),alog10(t)], [alog10(l-el),alog10(l+el)],thick=3
xyouts, 4.53, alog10(l) - 0.04, 'Typical error bars',charsi=2.0,charthick=2.0

m4_get_basedir, base
import_lejeune, base + 'evol/lejeune/modc020.dat', l
ev_max = n_elements(l)

for i=5,ev_max-1  do oplot, l(i).teff(0:x), l(i).l(0:x),psym=-6,symsi=.3
xo = 0 & aa = 1.0  ; xo = x & aa = 0.0
offtxt_y = -0.1
offtxt_x = 0.01
for i=5,ev_max-1-2 do $
 xyouts,l(i).teff(xo) + offtxt_x, l(i).l(xo) + offtxt_y,$
  strcompress(string(l(i).m,format='(F9.1)'),/remove_all), align=aa

if plot_metal_effect then begin
   xo20 = 10 & offtxt_x20 = 0.05 & offtxt_y20 = -0.7
   
   import_lejeune, base + 'evol/lejeune/modc008.dat', l20
    oplot, l20(xo20).teff(0:x), l20(xo20).l(0:x),psym=-6,symsi=.3, col=col.sky
    xyouts,l20(xo20).teff(0) + offtxt_x20, l20(xo20).l(0) + offtxt_y20,$
     strcompress(string(l20(xo20).m,format='(F9.1)'),/remove_all)+ ' Z=0.008', align=aa,col=col.sky
   
   xo04 = 10 & offtxt_x04 = 0.1 & offtxt_y04 = 0.1
   
   import_lejeune, base + 'evol/lejeune/modc004.dat', l04
    oplot, l04(xo04).teff(0:x), l04(xo04).l(0:x),psym=-6,symsi=.3, col=col.red
    xyouts,l04(xo04).teff(0)+ offtxt_x04, l04(xo04).l(0) + offtxt_y04,$
     strcompress(string(l04(xo04).m,format='(F9.1)'),/remove_all) + ' Z=0.004', align=aa,col=col.red
endif

oplot,[l04(xo04).teff(0),l04(xo04).teff(0) + offtxt_x04], $
      [l04(xo04).l(0), l04(xo04).l(0) + offtxt_y04], $
 col=col.red

oplot,[l20(xo20).teff(0),l20(xo20).teff(0) + offtxt_x20], $
      [l20(xo20).l(0), l20(xo20).l(0) + offtxt_y20], $
 col=col.sky

zams = transpose( [ [l.teff(0)], [l.l(0)] ] )
s = sort(zams(0,*)) & zams = zams(*,s)
lzams = interpol(zams(1,*), zams(0,*), alog10(wireobj3.teff))

; MARK STARS WITH NO TEFF ESTIMATE?
wlow = where(10.^(lzams) gt wireobj3.lumn*1.8 and wireobj3.lumn gt .1,c)
plotsym,0
oplot,alog10(wireobj3(wlow).teff),alog10(wireobj3(wlow).lumn),psym=8,symsi=3,col=col.red

print,' %%% Stars with no TEFF ESTIMATE:'
for i=0,c-1 do print,wireobj3(wlow(i)).nam, wireobj3(wlow(i)).hd,format='(A20,I8)'

g = where(wireobj3.teff gt 28000.,c)
; oplot,alog10(wireobj3(g).teff),alog10(wireobj3(g).lumn),psym=8,symsi=3,col=col.green
pamyat,x,blue=3,spb=2,betacep=2, /txtout


outname = base + 'wire_process/wire_params_Nov2006.idl'
save,filename=outname,wireobj3

;; No Teff estimate?
;;         * lam Cep    210839 == O6I, V=5.1, lambda Ceph -- uvbyHbeta available
;;        V* zet Oph    149757 == O9V, V=2.6 -- uvbyHbeta available
;;           * 1 UMi      8890 == Polaris
;;
;;          * 63 Ori     41361 == V = 5.7, G7III
;; NAME VINDEMIATRIX    113226 == Eps Vir, G8III, V=2.8
;;         * eta Cir    132905 == G8III, V=5.2
;;         HD 161178    161178 == G9III, V=5.9

hd_avoid = [210839,149757,8890,  41361,113226,132905,161178]

END

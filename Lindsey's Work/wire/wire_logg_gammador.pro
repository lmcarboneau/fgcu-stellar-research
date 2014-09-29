; Get logg of gamma dor stars as determined from Teff, mass (from evolution
; tracks) and Hipparcos parallaxes + BC from Bessell et al. 1998
; Run .r wire_gamma_hr.pro to get masses

; History:
; 26/10/2006: 
; (a) Only the best gamma dor stars are plotted
; (b) Mass estimate is modified by the abundance from Napiwozki's
;     calibration of metallicity.
; (c) If not abundance is given, fe/h = 0.0 is assumed 

; 1/1/2007: 
; Option to insert Teff from spectroscopy.
; This will affect the Mass estimate and both teff/mass
; will affect the final logg found from the hipparcos parallax.
;

fudge_mass_125081 = 1B ; artificially shift mass of this F2II type star to 1.1 Msun

; +++++++++++++++++++++++++++++++++++++++++++++++
; insert_teff_from_spec = 1B ; insert Teff from spectroscopy for a few stars? 1/1/2007
insert_teff_from_spec = 0B   ; use Teff from photometry for Hipparcos logg. RECOMMENDED == NO CHANGE

use_teff_from_vwares = 1B ; spectr. results
use_teff_from_phot   = 0B ; photometry
use_teff_from_list   = 0B ; force results from a list (usually your spectroscopy)

if total([use_teff_from_vwares,use_teff_from_phot,use_teff_from_list]) ne 1 then stop
; +++++++++++++++++++++++++++++++++++++++++++++++
dops = 1B
subsolar_metal = 1B

plot_indiv_err = 0B ; plot error bars for each star?

m4_get_basedir, base
m4_get_basedir, basedir

; notplot_hd = [7455] ; HD 7455 has a wrong parallax in gam structure: 23.55 Simbad had 6.18+-0.94

adjust_mass_based_on_feh = 1B
corot_on = 0B ; mark corot targets?
 notplot_hd = [-1]
plot_these_hd = [  7455,  12901,  14940,  22001, 26298,  27290,  27604,  33262, $
                  40745,  48501,  65526,  81421, 85964, 110379, 125081, 126516, $
                 135825, 147787, 167858, 218225]

plot_these_hd = [  12901,  14940,  22001, 26298,  27290,  27604,  33262, $
                  40745,  48501,  65526,  81421, 85964, 110379, 125081, 126516, $
                 135825, 147787, 167858, 218225]

; if insert_teff_from_spec then 
plot_these_hd = [22001,125081,126516,147787,167858]

easyps, keywords, kk, dops=dops, /prepare, dim = [15,13.5,-1,1], $
 fil = 'gammador_logg.ps', dir = base + 'papers/gammador/gammador/'
if dops then col=getcolor(/load)


; readcol,'/home/bruntt/papers/wire/bstars/bmass.dat',$
;  hd,feh,teff,bc,logg,mass,emass

; Restore the VWA results -- teff's used for Logg_pi
; restore,basedir + '/VWA/GAMMA/vwares_24MAY2006.idl' ; 26 October 2006 (THIS WILL OVERWRITE THE gam STRUCTURE!)
restore,'~/VWA/GAMMA/vwares_02JAN2007.idl' ; ,vwares,gam, 2/1/2007

wire_replace_hdnumber, vwares
wg = where(vwares.hd ne 177177,cg)
vwares = vwares(wg) ; remove results for COROT target HD 177177

; restore the 'gam' structure:
; Apparently from the program: wire_gamma_combine_info.pro
restore,base + 'papers/gammador/gammador_data/gammador_info_Oct06.idl' ;; UPDATED Oct 2006

; Get "cor" struture for COROT stars:
 outfile99 = base + '/VWA/MEGA/info/corot_atype_info.idl' 
 restore,outfile99 ; cor structure

; Missing Nap teff's for these HD numbers: 81421 ,     218225
; Almost no data on these two stars...
w1 = where(gam.hd eq 81421,c1)
gam(w1).teff_nap = 6950  &   gam(w1).eteff_nap = 800.
gam(w1).logg_nap = 4.4   &   gam(w1).elogg_nap = 0.5

w2 = where(gam.hd eq 218225L,c2)
gam(w2).teff_nap = 6790  &   gam(w2).eteff_nap = 800.
gam(w2).logg_nap = 4.02  &   gam(w2).elogg_nap = 0.5


if insert_teff_from_spec then begin
 hd_ins   = [22001,125081,126516,147787,167858]
 teff_ins = [7010.,6590,6590,7230,7610] ; See GAMMADOR_RAPPORT, 1/1/2007
 nk = n_elements(hd_ins)  &  wins = lonarr(nk) & wins(*) = -1
 for k=0,nk-1 do begin
  wp = where(gam.hd eq hd_ins(k),cwp)
  if cwp eq 1 then begin
    gam(wp).teff = teff_ins(k) ; insert the Teff from spectroscopy!
    wins(k) = wp
  endif
 endfor
endif

; ==================================================
n = n_elements(gam.hd)
 logg_pi = fltarr(n)
 err_pi  = fltarr(n)
 err_phot = fltarr(n)

bc = fltarr(n)
for j=0,n-1 do begin

; teff_use = gam(j).teff_nap
; logg_use = gam(j).logg_nap

  found = where(vwares.hd eq gam(j).hd,cfound)
  if cfound ne 1 then print,' *** logg_pi failed for star (not in vwares): ',gam(j).hd else begin

  if use_teff_from_vwares then teff_use = float(vwares(found).teff)
  if use_teff_from_phot   then teff_use = gam(j).teff_nap
  if use_teff_from_list   then begin
    hd_ins   = [22001,125081,126516,147787,167858]
    teff_ins = [7010.,6590,6590,7230,7610] ; See GAMMADOR_RAPPORT, 1/1/2007
    teff_use = -1.
    wg = where(gam(j).hd eq hd_ins,cg) 
    if cg eq 1 then teff_use = teff_ins(wg)
  endif

  logg_use = vwares(found).logg
 
 if teff_use gt 4000 and teff_use lt 15000 and $
    logg_use gt 1.5  and logg_use lt 5.5 then begin
   bessell, teff_use, logg_use, bc1
   bc(j) = bc1
;
; STORE RESULTS:
;
   gam(j).bc    = bc1 ; store BC
   gam(j).teff  = teff_use ; from spectroscopy
   gam(j).eteff = 100. & if vwares(found).vsini ge 40 then gam(j).eteff = 150.
   gam(j).logg  = logg_use ; from spectroscopy

 endif

 endelse
endfor



; ==================================================

; ==================================================
; COROT
; ==================================================
n2 = n_elements(cor.hd)
 logg_pi2  = fltarr(n2)
 err_pi2   = fltarr(n2)
 err_phot2 = fltarr(n2)

bc2 = fltarr(n2)
for j=0,n2-1 do begin
 teff_use = cor(j).teff_nap
 logg_use = cor(j).logg_nap

 if teff_use gt 4000 and teff_use lt 15000 and $
    logg_use gt 1.5  and logg_use lt 5.5 then begin
   bessell, teff_use, logg_use, bc2temp
   bc2(j) = bc2temp
 endif
endfor
; ==================================================

; ==================================================
; Masses determined from Lejeune ev. tracks for Z=0.02 (solar abn.)
; ==================================================
mass  = fltarr(n) & mass(*)  = 1.90  ; Default stellar mass
emass = fltarr(n) & emass(*) = 0.20

; Masses based on Solar abundances:
; October 2006 Notes: 
;
; These stars are likely evolved beyond hook: this is based on
; spectral class AND logg < 4.0, mass is like a MS ev. track (before
; hook) and -0.10 Msun
; 22001, 27604, 27290, 147787, 40745, 218225,

; Slightly evolved: spec. class = V, but logg =< 4.0, mass is MS but -0.05 M_sun
; 48501, 26298, 167858

; Special cases:
; 7455: spectral class is G3V, spec=6200K, templogg=6000K. 
; Logg values also say it is not evolved. 
; But luminosity is very high for such a cool star. Likely,
; the luminosity is incorrect. If offset by 1 sigma from 1.37 to 1.27
; the mass of an unevolved star would be 1.9 M_sun

; 126516. Spec.class: F3V. logg around 4.2-4.0. So not too evolved?
; Not quite sure about this one. Mass = 1.95 ?

; 125081: F2II (highly evolved=Cepheid?) in agreement with logg. Must
; be in a stage after the hook region. Mass = 1.85 ?
; Peter de Cat paper: Hipparcos photometry + de Cat's rad-vel shows
; the same period at 0.154d = 3.70 hours. Interpreted as delta-Sct
; pulsation. Paunzen found evidence for the star being chem. peculiar
; in the elements: Sr, Cr, Eu. See Paunzen & Maitzen 1998 A&AS, 133, 1

; 85964: very difference Teffs from templogg/spectr. 
; F3IV/V, so may be slightly evolved. logg = 4.1/4.8. spec=very high metallicity?

;  12901: F0 = unknown ev. stage, logg = likely unevolved., teff_sp = 7100,logg=4.5
; 135825: F0 = unknown ev. stage. logg = likely unevolved., teff_sp = 7330,logg=4.3
;  81421: A0 = unknown ev. stage, logg = maybe unevolved., teff_sp = 7500,logg=4.1
;  65526: A3 = unknown ev. stage, logg = maybe unevolved., teff_sp = 7100,logg=4.2

; Outliers for comp. of spec/hipp logg's: 14940, 110379, 33262 --
; Test: offset by 0.2 in mass has little consequence.
; Spectr. classes: F0IV/V, F0V, F7V
; Metallicity, spec: 0.0, +0.1, +0.0 
; Metallicity, phot: ---, -0.2, -0.4 ; quite large difference in metallity


m195  = [126516, 26298, 85964, 135825,81421,14940, 12901 ]   &  n195 = n_elements(m195)  ; 1.95 solar masses
m190  = [  7455,218225, 167858, 65526                    ]   &  n190 = n_elements(m190)  ; 1.90 solar masses
m185  = [ 48501, 125081                                  ]   &  n185 = n_elements(m185)  ; 1.85 solar masses 
m180  = [110379, 33262,  27604, 147787,40745             ]   &  n180 = n_elements(m180)  ; 1.80 solar masses
m175  = [ 22001, 27290                                   ]   &  n175 = n_elements(m175)  ; 1.75 solar masses
; m170  = [-1                                              ]   &  n170 = n_elements(m170)  ; 1.70 solar masses

star_tot = n195 + n190 + n185 + n180 + n175  ; + n170
if abs(star_tot - n_elements(vwares)) gt 1 then begin
 hitme,mess=' >>> YOU NEED TO MANUALLY ASSIGN A MASS TO *EACH STAR* IN THIS PROGRAM! (x = stop)',s9 
 if s9 eq 'x' then stop
endif

for k=0,n195-1 do begin
 wh = where(gam.hd eq m195(k),ch) & if ch eq 1 then mass(wh) = 1.95
endfor
for k=0,n190-1 do begin
 wh = where(gam.hd eq m190(k),ch) & if ch eq 1 then mass(wh) = 1.90
endfor
for k=0,n185-1 do begin
 wh = where(gam.hd eq m185(k),ch) & if ch eq 1 then mass(wh) = 1.85
endfor
for k=0,n180-1 do begin
 wh = where(gam.hd eq m180(k),ch) & if ch eq 1 then mass(wh) = 1.80
endfor
for k=0,n175-1 do begin
 wh = where(gam.hd eq m175(k),ch) & if ch eq 1 then mass(wh) = 1.75
endfor

if fudge_mass_125081 then begin
 wspec = where(gam.hd eq 125081)
 mass(wspec) = 1.1
endif

; Adjust mass based on metallicity = [Fe/H]
if adjust_mass_based_on_feh then begin
   for k=0,n_elements(gam)-1 do begin


    feh_use = 1000. ; feh_use = gam(k).feh_nap
    wu = where(gam(k).hd eq vwares.hd,cu)
    if cu eq 1 then feh_use = vwares(wu).feh

   if abs(feh_use) gt 1. then feh_use = 0.0 ; use solar composition if corrupt FeH value
    z_value = 0.02 * 10^feh_use
    ; Adjust the mass:
;    print,' %%% Mass before Fe/H adjustment: ', mass(k)
    mass(k) = mass(k) - (1.9-1.75) * (0.02 - z_value)/0.008
;    print,' %%% Mass after  Fe/H adjustment: ', mass(k)
     ; diff btw Z=0.02 and Z=0.008 is 0.15 M_solar 
     ; ie. lower metallity has lower mass for the same position in the CMD
   endfor
endif
gam.t1 = mass ; store the mass in gam structure


; Z = 0.02  ; solar
; Z = 0.008 ; Fe/H  = -0.4
; Z = 0.01 ; 
; print,10^([-.4,-.3,-.2,-.1,0,.1]) * 0.02,format='(6F6.3)'
;
; Fe/H = -0.4  -0.3  -0.2  -0.1  0.0   +0.1 
; Z    = 0.008 0.010 0.013 0.016 0.020 0.025
;
; if subsolar_metal then mass = mass - (1.9-1.75) ; diff btw Z=0.02 and Z=0.008


; ==================================================

; ==================================================
; Masses for COROT stars
; ==================================================
mass2  = fltarr(n2) & mass2(*)  = 1.75
emass2 = fltarr(n2) & emass2(*) = 0.20
wl = where(cor.hd eq 99028,c) & mass2(wl) = 1.78
wh = where(cor.hd eq 45431,c) & mass2(wh) = 1.58                   
if subsolar_metal then mass2 = mass2 - 0.15 ; diff btw Z=0.02 and Z=0.008
; ==================================================

; Uncertainty on the temperatures:
err_teff = fltarr(n)
for j=0,n-1 do err_teff(j) = max([gam(j).eteff,100])  ; error on effective temperatures = 6% ?
err_teff2 = fltarr(n2)
for j=0,n2-1 do err_teff2(j) = max([cor(j).eteff,100])  ; error on effective temperatures = 6% ?


err_v    = 0.015         ; error on V magnitude ?
err_bc = fltarr(n)
for j=0,n-1 do err_bc(j)   = max([abs(gam(j).bc) * 0.05,0.01]) ; error on bolometric correction = 5% ?
err_bc2 = fltarr(n2)
for j=0,n2-1 do err_bc2(j)   = max([abs(cor(j).bc) * 0.05,0.01]) ; error on bolometric correction = 5% ?

err_teff = gam.eteff             ; gam.teff * 0.05 ; assume 5% error on Teff
err_bc   = abs(bc * 0.05)
err_teff2 = cor.teff_nap * 0.05 ; COROT: assume 5% error on Teff
err_bc2   = abs(bc2 * 0.05)

w  = where(gam.teff gt 1000.,c )
w2 = where(cor.teff_nap gt 1000.,c2)

if c lt 2 then stop

; ===============================================
for i=0,c-1 do begin

;; logg_pi(w(i)) = 4. * alog10(gam(w(i)).teff_nap/5778.) + alog10(mass(w(i))/1.0) + $
;;              2. * alog10(gam(w(i)).par/1e3) + $
;;              0.4 * (gam(w(i)).v + bc(w(i)) + 0.26) + 4.44
;;
;; err_pi(w(i)) = (4. * (1./alog(10.)) * err_teff(w(i)) / gam(w(i)).teff_nap)^2. + $
;;                  ((1./alog(10.)) * emass(w(i))/mass(w(i)))^2. + $
;;             (2. * (1./alog(10.)) * gam(w(i)).epar/gam(w(i)).par)^2. + $
;;             (0.4 * err_v)^2. + (0.4 * err_bc(w(i)))^2. + (0.03)^2.
; This is the variance of logg, sqrt taken later ...

; Use spectroscopy results, october 2006:
 logg_pi(w(i)) = 4. * alog10(gam(w(i)).teff/5778.) + alog10(mass(w(i))/1.0) + $
              2. * alog10(gam(w(i)).par/1e3) + $
              0.4 * (gam(w(i)).v + bc(w(i)) + 0.26) + 4.44

 err_pi(w(i)) = (4. * (1./alog(10.)) * gam(w(i)).eteff / gam(w(i)).teff)^2. + $
                  ((1./alog(10.)) * emass(w(i))/mass(w(i)))^2. + $
             (2. * (1./alog(10.)) * gam(w(i)).epar/gam(w(i)).par)^2. + $
             (0.4 * err_v)^2. + (0.4 * err_bc(w(i)))^2. + (0.03)^2.
; This is the variance of logg, sqrt taken later ...

 err_phot(w(i)) = gam(w(i)).elogg_nap ; error on the photometric log g value

endfor

err_pi = sqrt(err_pi)
; ===============================================

; ===============================================
for i=0,c2-1 do begin

 logg_pi2(w2(i)) = 4. * alog10(cor(w2(i)).teff_nap/5778.) + alog10(mass(w2(i))/1.0) + $
              2. * alog10(cor(w2(i)).par/1e3) + $
              0.4 * (cor(w2(i)).v + bc(w2(i)) + 0.26) + 4.44

 err_pi2(w2(i)) = (4. * (1./alog(10.)) * err_teff(w2(i)) / cor(w2(i)).teff_nap)^2. + $
                  ((1./alog(10.)) * emass(w2(i))/mass(w2(i)))^2. + $
             (2. * (1./alog(10.)) * cor(w2(i)).epar/cor(w2(i)).par)^2. + $
             (0.4 * err_v)^2. + (0.4 * err_bc(w2(i)))^2. + (0.03)^2.
; variance of logg, sqrt taken later ...

 err_phot2(w2(i)) = cor(w2(i)).elogg_nap ; error on the photometric log g value

endfor

err_pi2 = sqrt(err_pi2)
; ===============================================


plotsym,0,/fill ; triangle

; +++++++++++++++++++++++++++++++++++++++++++++++++++
plot,logg_pi,gam.logg_nap,psym=2,xr=[3.5, 4.8],yr=[3.5, 4.8],/nodata,$
 xtit='!17log !3g!Iphot!N', ytit='!17log !3g!I!4p!3!N'
oplot,[2,6],[2,6],thick=4,color=150,line=5
; +++++++++++++++++++++++++++++++++++++++++++++++++++



; +++++++++++++++++++++++++++++++++++++++++++++++++++
for k=0,n_elements(gam)-1 do begin
 wuse = where(gam(k).hd eq plot_these_hd,cuse)
 if cuse eq 1 then $
  plots,gam(k).logg_nap,logg_pi(k),psym=8,symsi=1.2
endfor
; +++++++++++++++++++++++++++++++++++++++++++++++++++

; +++++++++++++++++++++++++++++++++++++++++++++++++++
if corot_on then begin
  plotsym,3,/fill ; stars = COROT
  oplot,cor.logg_nap, logg_pi2, psym=8,symsi=1.2
endif
; +++++++++++++++++++++++++++++++++++++++++++++++++++

below = [33262,7455,26298,147787,40745,126516,48501,167858]
above = [81421,110379,135825,22001,85964]
; pointl = [10167,209295,48501] ; lower left ; pointr = [12901,187028,27604,125081]
; pointl2 = [216910,14940,5590,218225,167858] ; upper left ; pointr2 = [35416,214291,149989]
pointr = [125081,27290] & pointl = [-1]
pointr2 = [27604] & pointl2 = [-1,218225]


; Write HD number and (optionally) connect with a line
for i=0,c-1 do begin
 hd = gam(w(i)).hd

 wbad = where(hd eq notplot_hd,cbad)
 wplot = where(hd eq plot_these_hd,cgood)

 if cgood eq 1 and cbad eq 0 then begin

 offy = 0.03 & offy2 = 0.0 & offx = 0.00 & aln = 0.5 ; default offset values

 bb = where(below eq hd,cbb)
 if cbb eq 1 then begin
   offy = -0.065
 endif

 bb = where(above eq hd,cbb)
 if cbb eq 1 then begin
   offy = 0.025
 endif

 bb = where(pointl eq hd,cbb)
 if cbb eq 1 then begin
   offy = -0.03  & offy2 = -0.01 &  offx = -0.08 &    aln  = 1.0 
   oplot,[gam(w(i)).logg_nap,gam(w(i)).logg_nap+offx],[logg_pi(w(i)),logg_pi(w(i))+offy],thick=2
endif

 bb = where(pointl2 eq hd,cbb)
 if cbb eq 1 then begin
   offy = 0.03  & offy2 = -0.01 &  offx = -0.08 &    aln  = 1.0 
   oplot,[gam(w(i)).logg_nap,gam(w(i)).logg_nap+offx],[logg_pi(w(i)),logg_pi(w(i))+offy],thick=2
 endif

 dd = where(pointr eq hd,cdd)
 if cdd eq 1 then begin
   offy = -0.03  &  offy2 = -0.01 &  offx =  0.08 &    aln  = 0.0
   oplot,[gam(w(i)).logg_nap,gam(w(i)).logg_nap+offx],[logg_pi(w(i)),logg_pi(w(i))+offy],thick=2
 endif

 dd = where(pointr2 eq hd,cdd)
 if cdd eq 1 then begin
   offy = 0.03  &  offy2 = -0.01 &  offx =  0.08 &    aln  = 0.0
   oplot,[gam(w(i)).logg_nap,gam(w(i)).logg_nap+offx],[logg_pi(w(i)),logg_pi(w(i))+offy],thick=2
endif

  xyouts,gam(w(i)).logg_nap+offx,logg_pi(w(i))+offy+offy2,$
  strcompress(string(gam(w(i)).hd,format='(I8)'),/remove_all),charsi=0.9,charthick=1.0,$
  alignment=aln
 endif

endfor

; =======================================

if corot_on then begin

below = [999] 
pointl = [45431, 99028]
pointr = [99, 172230]
pointl2  = [99]
pointl2b = [99, 177177]
pointr2 = [99, 51332, 169725, 172588]

for i=0,c2-1 do begin
 hd = cor(w2(i)).hd

 offy = 0.03 & offy2 = 0.0 & offx = 0.00 & aln = 0.5 ; default offset values

 bb = where(below eq hd,cbb)
 if cbb eq 1 then begin
   offy = -0.065
 endif

 bb = where(pointl eq hd,cbb)
 if cbb eq 1 then begin
   offy = -0.03  & offy2 = -0.01 &  offx = -0.08 &    aln  = 1.0 
   oplot,[cor(w2(i)).logg_nap,cor(w2(i)).logg_nap+offx],[logg_pi2(w2(i)),logg_pi2(w2(i))+offy],thick=2
endif

 bb = where(pointl2 eq hd,cbb)
 if cbb eq 1 then begin
   offy = 0.03  & offy2 = -0.01 &  offx = -0.08 &    aln  = 1.0 
   oplot,[cor(w2(i)).logg_nap,cor(w2(i)).logg_nap+offx],[logg_pi2(w2(i)),logg_pi2(w2(i))+offy],thick=2
 endif

 bb = where(pointl2b eq hd,cbb)
 if cbb eq 1 then begin
   offy = 0.05  & offy2 = -0.01 &  offx = -0.05 &    aln  = 1.0 
   oplot,[cor(w2(i)).logg_nap,cor(w2(i)).logg_nap+offx],[logg_pi2(w2(i)),logg_pi2(w2(i))+offy],thick=2
 endif

 dd = where(pointr eq hd,cdd)
 if cdd eq 1 then begin
   offy = -0.03  &  offy2 = -0.01 &  offx =  0.08 &    aln  = 0.0
   oplot,[cor(w2(i)).logg_nap,cor(w2(i)).logg_nap+offx],[logg_pi2(w2(i)),logg_pi2(w2(i))+offy],thick=2
 endif

 dd = where(pointr2 eq hd,cdd)
 if cdd eq 1 then begin
   offy = 0.03  &  offy2 = -0.01 &  offx =  0.08 &    aln  = 0.0
   oplot,[cor(w2(i)).logg_nap,cor(w2(i)).logg_nap+offx],[logg_pi2(w2(i)),logg_pi2(w2(i))+offy],thick=2
 endif

 xyouts,cor(w2(i)).logg_nap+offx,logg_pi2(w2(i))+offy+offy2,$
 strcompress(string(cor(w2(i)).hd,format='(I8)'),/remove_all),charsi=0.9,charthick=1.0,$
 alignment=aln
endfor

endif ; corot on?

; =======================================
; END OF PRINTING HD FOR COROT STARS
; =======================================

if plot_indiv_err then begin
 for i=0,c-1 do $
  oplot,gam(w(i)).logg_nap + [-1,1.]*0.,logg_pi(w(i))+[-1.,1]*err_pi(w(i))

 for i=0,c-1 do $
  oplot,gam(w(i)).logg_nap + [-1,1.]*err_phot(w(i)),logg_pi(w(i))+[-1.,1]*0.
endif

wpi = where(logg_pi gt 2.5,c)
wpi2 = where(err_phot gt 0. and err_phot lt .5)
resistant_mean, err_phot(wpi2), 3, typ_phot, sd,nr
resistant_mean, err_pi(wpi),    3, typ_pi,   sd,nr

typ_phot = 0.2

; Plot typical error bars
oplot,3.85+[-1.,1]*typ_phot, 4.6 + [-1,1.] * 0.,thick=2
oplot,3.85+[-1.,1]*0.,       4.6 + [-1,1.] * typ_pi,thick=2

tick = .01 ; tick marks
oplot,3.85+[-1.,1]*tick,       4.6 + [ 1, 1.] * typ_pi,thick=2
oplot,3.85+[-1.,1]*tick,       4.6 + [-1,-1.] * typ_pi,thick=2
oplot,3.85+[-1., -1]*typ_phot, 4.6 + [-1,1.] * tick,thick=2
oplot,3.85+[ 1.,  1]*typ_phot, 4.6 + [-1,1.] * tick,thick=2

print,' %%% Avg error on logg for phot/pi: ',typ_phot, typ_pi

; logg_pi = 4.*teff + mass + 2*log_pi + 0.4*(V+BC+0.26)+4.44

easyps, keywords, kk, dops=dops, /close
col=getcolor(/load)


easyps, keywords, kk, dops=dops, /prepare, dim = [15,13.5,-1,1], $
 fil = 'gammador_logg2.ps', dir = base + 'papers/gammador/gammador/'
if dops then col=getcolor(/load)
plot,logg_pi,gam.logg_nap,psym=2,xr=[3.2, 5.0],yr=[3.2, 5.0],/nodata,$
 xtit='!17log !3g!Ispec!N', ytit='!17log !3g!I!4p!3!N'
oplot,[2,6],[2,6],thick=4,color=150,line=5
for k=0,n_elements(vwares)-1 do begin
 v = where(vwares(k).hd eq gam.hd,cv)
 if cv eq 1 then begin
   plotsym,0,/fill  &  if vwares(k).vsini ge 50 then plotsym,0
   plots, vwares(k).logg + 0.2 * 0.,      logg_pi(v), psym=8, symsi=1.2
;   xyouts,vwares(k).logg + 0.2 * 0.+0.02, logg_pi(v)+0.02, strcompress(string(vwares(k).hd,format='(I8)'),/remove_all)
 endif
endfor
easyps, keywords, kk, dops=dops, /close

; Plot differences in logg determinations:
ddy = 0.025

easyps, keywords, kk, dops=dops, /prepare, dim = [15,13.5,-1,1], $
 fil = 'gammador_logg_pi_spec.ps', dir = base + 'papers/gammador/gammador/'
if dops then col=getcolor(/load)
plot,alog10(gam.teff),logg_pi-gam.logg_nap,psym=2,xr=[3.9,3.75],yr=[-1,1]*.75,/nodata,$
 xtit='!17log !17T!3!Ieff!N', ytit='!17log !3g!I!4p!3!N - !17log !3g!Ispec!N',$
 xtickname=[' ','',' ','',' ','',' ','',' ','',' ',''],ysty=1
oplot,[2,6],[0,0],thick=4,color=150,line=5
for k=0,n_elements(vwares)-1 do begin
 vv = where(vwares(k).hd eq gam.hd,cv)
 vvp = where(vwares(k).hd eq plot_these_hd,cvp) ; plot all stars?
 if cvp eq 1 and cv eq 1 then begin
   plotsym,0,/fill  &  if vwares(k).vsini ge 50 then plotsym,0
   plots, alog10(vwares(k).teff),  logg_pi(vv) - vwares(k).logg, psym=8, symsi=1.2
   xyouts,alog10(vwares(k).teff),  logg_pi(vv) - vwares(k).logg + ddy, $
      strcompress(string(gam(vv).hd,format='(I8)'),/remove_all),charsi=.8,align=.5
 endif
endfor
easyps, keywords, kk, dops=dops, /close

easyps, keywords, kk, dops=dops, /prepare, dim = [15,13.5,-1,1], $
 fil = 'gammador_logg_pi_phot.ps', dir = base + 'papers/gammador/gammador/'
if dops then col=getcolor(/load)
plot,alog10(gam.teff),logg_pi-gam.logg_nap,psym=2,xr=[3.9,3.75],yr=[-1,1]*.75,/nodata,$
 xtit='!17log !17T!3!Ieff!N', ytit='!17log !3g!I!4p!3!N - !17log !3g!Iphot!N',$
 xtickname=[' ','',' ','',' ','',' ','',' ','',' ',''],ysty=1
oplot,[2,6],[0,0],thick=4,color=150,line=5
for k=0,n_elements(vwares)-1 do begin
 v = where(vwares(k).hd eq gam.hd,cv)
 vvp = where(vwares(k).hd eq plot_these_hd,cvp) ; plot all stars?
 if cvp eq 1 and cv eq 1 then begin
   plotsym,0,/fill  &  if vwares(k).vsini ge 50 then plotsym,0
   plots, alog10(vwares(k).teff),  logg_pi(v) - gam(v).logg_nap, psym=8, symsi=1.2
   xyouts,alog10(vwares(k).teff),  logg_pi(v) - gam(v).logg_nap + ddy, $
      strcompress(string(gam(v).hd,format='(I8)'),/remove_all),charsi=.8,align=.5
 endif
endfor
easyps, keywords, kk, dops=dops, /close



END

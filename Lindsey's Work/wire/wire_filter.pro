; Determine the filter response for the WIRE satellite star tracker

dops = 1

; ================================================================================
if dops then begin
   dir = '/home/bruntt/papers/procyon/'
   startFilename = 'wire_filter_response.ps'
   startFilename = strcompress(startFilename,/remove_all)

   x = 20. & y = 18.5 & xo = (21.5-x) * 0.5 & yo = (30.5-y)*.5

   keywords = PSConfig(Cancel=cancelled, Filename=startFilename, $
    /European, directory=dir, $
    xsize=x, ysize=y, xoffset=xo, yoffset=yo)

   IF cancelled THEN RETURN
      thisDevice = !D.Name
      thisFont   = !P.Font
      !P.Font = 0
    Set_Plot, 'PS'
    Device, _Extra=keywords
endif
; ================================================================================


csize = 1.5 & cz = 1.6
if dops then begin
 csize = 1.0
 sz = 1.0
endif


; ======================================================================
plot_oo,[0,1],psym=2,$
 xtit='WIRE flux [electrons]',ytit='Calculated flux [electrons]',$
 xr=[1e4,5e7],yr=[1e6,5e9],xsty=1,ysty=1,/nodata,charsi=csize, $
 xthick=2, ythick=2, charthick=2
; ======================================================================

; ======================================================================
oplot,[1e4,1e7],[1e4,1e7]*55*4.5 ; wire = allflux * 55. ; ~2 percent

plotsym,0 & plots,1.2e4,6e8,psym=8,symsi=2.5
xyouts, 1.3e4*1.1,0.95*6e8,'Main Sequence Stars',charsi=csize

plotsym,0,/fill & plots,1.2e4,0.7*6e8,psym=8,symsi=2.5
xyouts, 1.3e4*1.1,0.7*0.95*6e8,'Giant Stars',charsi=csize

xyouts, 1e6, 1.5*2.7e6,'Filters:',charsi=csize,charthick=2
; ======================================================================

; ======================================================================
szz = 0.9 ; overall symbol size
ssym = 8 ; use custom symbols 
; ======================================================================

debug = 0B

coln = 0

calib = ['stromgren', 'giants15_strom']
;fil = ['U 3200-4000','B 4000-5000','V 5100-6000','allflux']
fil = ['U 3200-4000','B 4000-5000','V 5100-6000','V+R 5100-7100']
 fil = ['B 4000-5000','B 4000-6000','B 4400-5400']

fcnt = 0B

; teff_calib = 'hbeta' & ssym = 2 & szz = 1.2 ; larger scatter
; teff_calib = 'johnsonBV' & ssym = 4 & szz = 0.8
; teff_calib = 'stromgren' & ssym = 6 & szz = 1.5
; teff_calib = 'giants15_strom' & ssym = 8 & szz = 2.0 ; Giant stars, b-y

ncal = n_elements(calib)
nfil = n_elements(fil)

plotsym,0,/fill ;
col=getcolor(/load)
colx = [col.sky, col.red, col.yellow, col.magenta, col.green]

; =========================================
;  Loop over every filter and calibration
; =========================================
for f=0,nfil-1 do begin
 filtername = fil(f)
; ==============================
   for l=0,ncal-1 do begin
    teff_calib = calib(l)
; ==============================

if strmatch(teff_calib,'*giant*',/fold_case) then $
 plotsym,0,/fill else plotsym,0 ; open symbols

; ================================================
; WIRE filter:
; ================================================
nw = 500.

if filtername eq 'allflux' then begin
 wire_wl = 300. + (findgen(nw)/(nw-1.)) * 21200. & coln = 1
endif


; Try the Johnson filters: U, B, V, V+R
if filtername eq 'U 3200-4000' then begin
 wire_wl = 3200. + (findgen(nw)/(nw-1.)) * 800. & coln = 3
endif

if filtername eq 'B 4000-5000' then begin
 wire_wl = 4000. + (findgen(nw)/(nw-1.)) * 1100. & coln = 0
endif

if filtername eq 'V 5100-6000' then begin
 wire_wl = 5100. + (findgen(nw)/(nw-1.)) *  900. & coln = 4
endif

if filtername eq 'V+R 5100-7100' then begin
 wire_wl = 5100. + (findgen(nw)/(nw-1.)) * 2000. & coln = 1 
endif

if filtername eq 'B 4000-6000' then begin
 wire_wl = 4000. + (findgen(nw)/(nw-1.)) * 2100. & coln = 2 
 if strmatch(teff_calib,'*giant*',/fold_case) then $
  plotsym,4,/fill else plotsym,4 ; open symbols
endif

if filtername eq 'wideB3' then begin
 wire_wl = 4500. + (findgen(nw)/(nw-1.)) * 1500. & coln = 4
 if strmatch(teff_calib,'*giant*',/fold_case) then $
  plotsym,5,/fill else plotsym,5 ; open symbols
endif

if filtername eq 'B 4400-5400' then begin
 wire_wl = 4400. + (findgen(nw)/(nw-1.)) * 1000. & coln = 4
 if strmatch(teff_calib,'*giant*',/fold_case) then $
  plotsym,5,/fill else plotsym,5 ; open symbols
endif


if filtername eq 'B 4400-4900' then begin
 wire_wl = 4400. + (findgen(nw)/(nw-1.)) * 1000. & coln = 4
 if strmatch(teff_calib,'*giant*',/fold_case) then $
  plotsym,5,/fill else plotsym,5 ; open symbols
endif


wire_fl = fltarr(nw)
wire_fl(2:nw-2) = 1.


; ================================================
solarlum = 3.846e33 ; erg/sec
; The flux on the WIRE CCD is about 4 by 4 pixels. 
; The amount of flux WIRE gets compared to the 4-PI steradian sphere is:
pix_wire = 4.0
wirefrac = 1. / ((4.*!PI) / (2.*!PI * pix_wire / 1440.)^2.0) ; 2.4e-5, ie 24 ppm
get_elec = 1.602e-12 ; erg per electron Volt
gain     = 15. ; CCD gain = 15 electrons per ADU
observing_time = 0.5 ; seconds integrated
; ================================================


; ================================================
wire_target_info, '~/wire/wire_essential/targets_wire.txt', info
restore,'~/wire/wire_essential/wire_sec_info.idl' ; 09 NOV 2004
; ================================================

; Stromgren parameters:
; plot,wireobj.by, wireobj.c1,psym=6,symsi=.5,xr=[-2,2],yr=[0,1]
; plot,wireobj.hbeta, wireobj.m1,xr=[3,2.4],psym=6,symsi=.5,yr=[0,3]

; c1 = m1 and vice versa !

;theta = 0.4399 +1.209*by -0.3541*by^2.+ 8.443e-2*X*FeH -0.1063*FeH -1.686e-2^2.
;teff = 5040. / theta

if teff_calib eq 'stromgren' then $
w = where(wireobj.lumclass ge 4 and wireobj.by gt -5. and wireobj.m1 lt 2.0 ) ; m1 <--> c1

if teff_calib eq 'hbeta' then $
w = where(wireobj.lumclass ge 4 and wireobj.hbeta ge 2.44 and wireobj.hbeta le 2.74 ) 


if teff_calib eq 'giants15_strom' then $
w = where(wireobj.lumclass eq 3 and wireobj.by ge 0.5 and wireobj.by lt 1.0,c) ; 15 stars
if teff_calib eq 'giants14_strom' then $
w = where(wireobj.lumclass eq 3 and wireobj.by ge 0.0 and wireobj.by lt 0.5,c) ; 1 star!

if teff_calib eq 'johnsonBV' then $
w = where(wireobj.lumclass ge 4 and $
          wireobj.bv ge 0.2 and wireobj.bv le 1.5,c)

; plot,wireobj.bv,wireobj.v,psym=2,yr=[8,-2],symsi=.6
; oplot,wireobj(w).bv,wireobj(w).v,psym=6,symsi=1.2

by = wireobj(w).by
c1 = wireobj(w).m1
feh = -0.2
hbeta = wireobj(w).hbeta
m1 = wireobj(w).c1 ; swapped !
bv = wireobj(w).bv

; =====================================================================================
if teff_calib eq 'stromgren' then begin
  ;4000K<T_eff_<8000K and 0>[Fe/H]>-2.5.
  ;MS stars: (residuals on theta = 0.019 --> 120 K)
  theta = 0.537 + 0.854*by + 0.196*by^2. -0.198*by*c1-0.026*by*FeH-0.014*FeH-0.009*FeH^2.
  teff = 5040. / theta
endif
; =====================================================================================

; =====================================================================================
if teff_calib eq 'giants14_strom' then begin
  theta = $
    0.5815 +0.7263*by + (6.856e-2)*by^2. -(6.832e-2)*by*feh -(1.062e-2)*feh -(1.079e-2)*feh^2.
  teff = 5040. / theta
endif
; =====================================================================================

; =====================================================================================
if teff_calib eq 'giants15_strom' then begin
  theta = 0.4399 + 1.209*by - 0.3541*by^2.0 + (8.443e-2)*by*feh -0.1063*feh -(1.686e-2)*feh^2.0
  teff = 5040. / theta
endif
; =====================================================================================

; =====================================================================================
if teff_calib eq 'hbeta' then begin
  theta = 47.7477 - 34.0506*hbeta + 6.1625*hbeta^2.0 - 0.1016*hbeta*FeH + $
           0.3054*FeH + 0.0083*FeH^2.0 ; rms on theta 0.025
  teff = 5040. / theta
endif
; =====================================================================================


; =====================================================================================
if teff_calib eq 'johnsonBV' then begin
  ;4000K<T_eff_<8000K and 0>[Fe/H]>-2.5.
  ;MS stars: (residuals on theta = 0.023 3.2% at 4000K, 1.7% at 7500K
  theta = 0.541 +0.533*bv +0.007*bv^2.0 - 0.019*bv*feh - 0.047*feh - 0.01*feh^2.0
  teff = 5040. / theta
endif
; =====================================================================================

; bessell,teff,logg,bolcor
mv = wireobj(w).v + 5. - 5 * alog10(1e3/wireobj(w).par) ; abs. magn, ignoring B.C. & A_V

; plot,by,mv,psym=2,yr=[6,-6],ytit='Mv',xtit='b-y'

n = n_elements(by)
np = 22500


; Init plot:

if debug then $
 plot,[30,0],/nodata,xr=[0,30000],yr=[30,0],xtit='WL [AA]',ytit='Mag'
col=getcolor(/load)

flux = fltarr(n)
wflux = fltarr(n)

for i=0,n-1 do begin

 v = wireobj(w(i)).v
 b = wireobj(w(i)).bv + v
 teff1 = teff(i)

 wire_counts, v, b, cnt, /out

 wflux(i) = cnt * gain / observing_time ; convert to electrons in one second

 wl = 500. + (findgen(np)/(np-1.)) * 35000.
 fl = planck(wl,teff1)
 fl = fl/total(fl) ; integral under planck function is 1.0000

; ==============================================================
; Determine the bolometric correction:
; ==============================================================
; GIANT STARS
; ==============================================================
if strmatch(teff_calib,'*giant*',/fold_case) eq 1 then begin
 logg = 2.5
endif
; ==============================================================
; MAIN SEQUENCE STARS:
; ==============================================================
if strmatch(teff_calib,'*giant*',/fold_case) ne 1 then begin
 logg = 4.4
 if teff1 gt 6000. then logg = 4.6
 if teff1 lt 5300. then logg = 4.2
endif
; ==============================================================
bessell,teff1,logg,bolcor & bolcor = bolcor(0)
mbol = mv(i) + bolcor
; ==============================================================

 lumn = 10.^((4.75-mbol)/(2.5)) ; in units of the Sun luminosity!
 flux2 = fl * lumn ; integral under planck function gives TOTAL luminosity!
 ; Total(fl)    == 1.000
 ; total(flux2) == total bolometric (== all wavelengths) flux in solar luminosity units!
 mall = -2.5 * alog10(flux2)

; But star is probably quite distant!
 distmod =  5. - 5 * alog10(1e3/wireobj(w(i)).par)
 flux3 = mall - distmod

if debug then begin
 oplot,wl,mall
 oplot,wl,flux3 
endif

 flux3dir = 10.^(-flux3/2.5)

 print,' %%% Obs flux / bolometric: ',total(flux3dir) / total(flux2)


 wok = where(wl gt min(wire_wl) and wl lt max(wire_wl),cok)
 val = interpol(wire_fl,wire_wl,wl(wok))
 

 percentage = total(fl(wok)) ; percentage of bolometric flux WIRE observed

; The star is moved to a distance of 10 parsec
 parsec10 = 4. * !PI * (3.0857e16)^2.0  ; parsec in meters
 ; area_ccd = (27e-6*4.)^2. ; 27micron pixels, around 4 by 4 pixels are used
 area_ccd = !PI * (2.5e-2)^2.  ; 5cm aperture lens! area is 2.5cm-squared time PI
 fraction_wire = area_ccd / parsec10

 filt = flux3dir(wok) * val
 flux(i) = total(filt) * solarlum * fraction_wire / (3.65 * get_elec) 
   ; abount 10eV to get electron!


endfor



oplot,wflux,flux,psym=ssym,col=colx(coln),symsi=szz


r = robust_poly_fit(wflux, flux, 1, myfit)
mfit_x = [1e4,1e7] & mfit_y = r(0) + r(1) * mfit_x
; oplot,mfit_x, mfit_y, col=colx(coln)


if strmatch(teff_calib,'*giant*',/fold_case) eq 1 then begin
 if fcnt eq 0 then xyouts, 1e6, 1.5*2e6,   filtername,col=colx(coln),charsi=sz
 if fcnt eq 1 then xyouts, 1e6, 1.5*0.8*2e6, filtername,col=colx(coln),charsi=sz
 if fcnt eq 2 then xyouts, 1e6, 1.5*0.8^2*2e6,   filtername,col=colx(coln),charsi=sz
 if fcnt eq 3 then xyouts, 1e6, 1.5*0.8^3*2e6,   filtername,col=colx(coln),charsi=sz
 fcnt = fcnt + 1
endif

 endfor
endfor


; ================================================================================
if dops then begin
      Device, /Close_File
      Set_Plot, thisDevice
      !P.Font = thisFont
      set_plot,'x'
   print,' $  gv ' + keywords.filename + '  & '
endif
; ================================================================================

END

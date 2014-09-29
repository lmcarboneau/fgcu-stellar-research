print,' %%% Use wire_bstars.pro'
stop











; Get information on each WIRE object

dops = 0B
plot_fit = 0B

detail_lc = 2B ; overplot zoomed version of LC ? 0,1 or 2
zoom_yy = 1B ; in the zoomed plots --- change y-axis range individually?
pzs  = 3 ; symbol style for LC
pzs2 = 3 ; symbol style, zoomed LC

; Parameters for ampllitude plot:
mark_freq = 0B
am = 0 ; plot raw ampltide spectrum
am = 1 ; removed orbital freq.
am = 2 ; remove all peaks
am = 3 ; removed significant peaks
mark_freq3 = 1B


; Parameters for H-R plot
hdout_hr = 0B

if n_elements(wire) eq 0 then begin
 restore,'/home/bruntt/wire/wire_process/reduced_B.idl'

 restore,'~/wire/wire_essential/wire_sec_info.idl' ; for wire_combine_info.pro
 wire_ramirez, wireobj, wireobj2
 ; wire_prep_templogg, wireobj2

 wire_read_templogg, wireobj2, '/mnt/hda5/data/wire/wiretargOUTcastelli.csv',calib='Cas',/debug
 wire_read_templogg, wireobj2, '/mnt/hda5/data/wire/wiretargOUTribas.csv',   calib='Rib',/debug
 wire_read_templogg, wireobj2, '/mnt/hda5/data/wire/wiretargOUT.csv',        calib='Nap',/debug
endif

; plot,wireobj2.teff_nap,wireobj2.teff_nap-wireobj2.teff,psym=2,yr=[-1,1]*2000.,xr=[0,50000]
; mbol = wireobj2.v + (5.-5*alog10(1e3/wireobj2.par))+wireobj2.dt
; lum = 10.^((4.75-mbol)/2.5)
; tef = alog10(wireobj2.teff)

extby = wireobj2.extby ; E(b-y)
w = where(wireobj2.extby lt -.6,c) ; bad values
extby(w) = 0.0 ; assume no extinction

; Update BC using the Nap teff/logg
n = n_elements(wireobj2)
for i=0,n-1 do begin
 teff9 =  wireobj2(i).teff_nap
 logg9 =  wireobj2(i).logg_nap
 if logg9 lt -.5 then logg9 = 4.0
 if logg9 gt 5.0 then logg9 = 5.0
 if teff9 gt 1000 and logg9 gt 0 then $
  bessell, teff9, logg9, bc
 wireobj2(i).bc = bc
endfor


mbol1 = wireobj.mv_templogg + (5.-5*alog10(1e3/wireobj2.par)) ; +wireobj2.bc

mbol2 = wireobj2.v + (5.-5*alog10(1e3/wireobj2.par))+wireobj2.bc + 4.3 * extby
mbol3 = wireobj2.v + (5.-5*alog10(1e3/wireobj2.par))+wireobj2.bc ; + 4.3 * extby

plot,alog10(wireobj2.teff_nap),mbol2,psym=1,xr=[4.6,3.6],yr=[7,-10]
; oplot,alog10(wireobj2.teff),mbol2,psym=1,col=col.sky
oplot,alog10(wireobj2.teff_nap),mbol3,psym=1,col=col.sky


plotsym,0,/fill

g = reverse( sort(wire.bv) )
g = ( sort(wire.bv) )
g = reverse( sort(wire.teff) ) ; possible after the first run of this prg.

; sort by luminosity:
if n_elements(lum2) eq 0 then begin
 lum2 = fltarr(n_elements(wire))
endif else begin
 g = reverse( sort(lum2) )
endelse

n = n_elements(wire) ; number of stars

xer=3 & yer=4 & n6791pos,xer,yer,0.07,0,0,pos

np = n_elements(pos(0,*)) ; number of plots per page
np1 = np

ymin = 0.
ymax = 15000.
xmax = 10. ; max freq do display in c/day



; ===================================================================
if dops eq 0 then $
 window, 1, title='WIRE B-type stars',xsize=700,ysize=700,xpos=750,ypos=0 ; 900
; ===================================================================
easyps, keywords, kk, dops=dops, /prepare, dim = [15,15,-1,1], $
 fil = 'bstars_hr.ps', dir = '/home/bruntt/papers/wire/bstars/'
if dops then col=getcolor(/load)

offx = 0.03
offy = 0.

xxx = [4.7,3.6] & yyy = [-.5,5] ; nice fig, all stars
; xxx = [4.6,4.1] & yyy = [2.5,4.5]
; xxx = [4.2,3.9] & yyy = [2,3]

plotsym,0 ; open
 plot,$
  tef,alog10(lum),xr=xxx,yr=yyy,xtit='!17log !3T!Ieff!N',ytit='!17log !3L/L!ISun!N',$
  psym=8,xsty=1,ysty=1,xthick=2,ythick=2,charsi=1.5,charthick=2,thick=2

; Print HD numbers of all stars:
;nall = n_elements(tef)
;for j=0,nall-1 do $
; xyouts, tef(j), alog10(lum(j)), $
;  strcompress(string(wireobj2(j).hd,format='(I8)'),/remove_all),$
;   col=col.sky,charsi=1,charthick=1

import_lejeune, '/home/bruntt/evol/lejeune/modc008.dat', l
x = 22 & xx = [4.7,3.5] & yy = [-.5,6]
for i=5,n_elements(l)-1  do oplot, l(i).teff(0:x), l(i).l(0:x),psym=-6,symsi=.3
xo = 0 & aa = 1.0  ; xo = x & aa = 0.0
offtxt_y = -0.1
for i=5,n_elements(l)-1 do $
 xyouts,l(i).teff(xo), l(i).l(xo) + offtxt_y,$
  strcompress(string(l(i).m,format='(F9.1)'),/remove_all), align=aa

plotsym,0,/fill
 for s2=0,n-1 do begin
  u = g(s2)
  x = where(wire(u).hd eq wireobj2.hd,cx)
  if cx eq 1 then $
   plots,tef(x),alog10(lum(x)),col=col.red,thick=3,psym=8
   lum2(u) = lum(x)
 endfor 

; Print mass of each evolution track:
 for s2=0,n-1 do begin
  u = g(s2)
  x = where(wire(u).hd eq wireobj2.hd,cx)
  if cx eq 1 then begin
   numout = s2 + 1
   numout = u
   numout = wire(u).hd

if hdout_hr then $
    xyouts,tef(x)+offx, alog10(lum(x)) + offy, $
     strcompress(string(numout,format='(I8)'),/remove_all), charsi=0.8, charthick=1.0
    teff = tef(x)
  endif else teff = -1.
;  print,s2,teff

   wire(u).teff = long(10.^teff)



endfor
; ===================================================================
easyps, keywords, kk, dops=dops, /close
; ===================================================================



; ===================================================================
if dops eq 0 then $
 window, 0, title='WIRE B-type stars: Amplitude Spectra',$
 xsize=750,ysize=1400 ; ,xpos=750,ypos=900
; ===================================================================
easyps, keywords, kk, dops=dops, /prepare, dim = [20,yyps,-1,1], $
 fil = 'bstars_ampl.ps', dir = '/home/bruntt/papers/wire/bstars/'


for m = 0,n-1 do begin
s = g(m)

xtt = [' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ']
ytt = xtt
xt2 = ''
yt2 = ''

if (m mod np1) eq (np/xer-1) then begin
 xtt=''  &  ytt=[' ','','','','','','','','','','']
 xt2 = 'Frequency [c/day]'
 yt2 = 'Amplitude [ppt]'
endif

if (m mod np1) eq (np-1) then begin
 xtt=[' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ']
 ytt=[' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ']
 xt2 = '' ; Frequency [c/day]'
 yt2 = ''
endif

if ( ((m+1) mod yer) eq 0) and (m ne 0) then begin
 xtt=''
 xt2 = '' ; Frequency [c/day]'
endif

if ((m mod np1) eq 0) or (m eq 0) then $
 plot,[0,1],xsty=6,ysty=6,/nodata ; new plot

yscale = (1e-3)/1.086 ; display ppt

if am eq 0 then begin
 ymax = ceil(max(wire(s).ampl(am,*)) * yscale / 2.) * 2.
 if ymax lt 2. then ymax = 2.
endif

if am eq 3 then begin
 ymax = ceil(max(wire(s).ampl(am,*)) * yscale / .5) * .5
 if ymax lt 1. then ymax = 1.
endif

; Exceptions
if am eq 0 then begin
 if wire(s).hd eq 125823 then ymax = 6.
 if wire(s).hd eq  37795 then ymax = 0.5
 if wire(s).hd eq  87901 then ymax = 1.0
 if wire(s).hd eq 152614 then ymax = 1.0
 if wire(s).hd eq  40494 then ymax = 1.5
 if wire(s).hd eq  68520 then ymax = 2.5
 if wire(s).hd eq 133242 then ymax = 0.5
 if wire(s).hd eq 193924 then ymax = 1.5
 if wire(s).hd eq 127381 then ymax = 3.0
 if wire(s).hd eq  66591 then ymax = 3.0
 if wire(s).hd eq 128345 then ymax = 3.0
 if wire(s).hd eq 101431 then ymax = 1.5
endif

nf  = wire(s).n   ; number of freq. detected in total spectrum (not orbital freq)
nf2 = wire(s).n2  ; number of freq. detected in total spectrum (not orbital freq)
nf3 = wire(s).n3  ; number of freq. left in partly cleaned spectrum

amuse = am
if am eq 3 then begin
 gunik = uniq(wire(s).freq(am,*))  ; no ampl. spectrum for partly cleaned spectrum
 if n_elements(gunik) eq 1 then amuse = 2 ; clean all freq
endif

plot,wire(s).freq(amuse,*),wire(s).ampl(amuse,*)*yscale,xr=[0,xmax], yr=[0,ymax],$
 xsty=1,ysty=1,xthick=2,ythick=2,charsi=1.2,$
 xtickname=xtt, ytickname=[' ','','','','','','','','','',''], $
 xtit=xt2, ytit=yt2,$
 /noerase,position=pos(*,m mod np1)

; Mark frequencies except orbital harmonics + low freq. spurious peaks
if mark_freq then $
   for l=0,wire(s).n-1 do $
    oplot,wire(s).f2(l)*[1,1],[0,wire(s).a2(l)*1e6]*yscale,col=200,thick=2

; Mark freq. for partially cleaned spectrum
if mark_freq3 and amuse eq 3 then $
   for l=0,wire(s).n3-1 do $
    if wire(s).f3(l) gt .1 then $
    oplot,wire(s).f3(l)*[1.,1.],[0.,wire(s).a3(l)*1e6]*yscale,col=200,thick=2

; print,wire(s).f(0:wire(s).n-1)   ; all freq
; print,wire(s).f2(0:wire(s).n2-1) ; all freq except low freq. peaks + orbital harms
; print,wire(s).f3(0:wire(s).n3-1) ; what's left after cleaning dominant peaks?

; Print star HD number on plot:
yy = !y.crange
y2 = ymax * 0.85
x2 = xmax * 0.47

numout = s + 1
numout = m + 1

; Is star present more than once?
addhd = ''
mm = where(wire.hd eq wire(s).hd, cmm)
if cmm ge 2 then begin
 add = ['A','B','C','D','E','F','G','H','I','J']
 xcd = where(s eq mm,cxcd)
 addhd = ' [' + add(xcd) + ']'
endif

xyouts,x2,y2,'HD' + strcompress(wire(s).hd,/remove_all)+addhd,$
 charsi=0.9,charthick=1

; ' / ' + strcompress(string(numout,format='(I4)'),/remove_all),/data

if ((m mod np1) eq (np-1)) and (m ne 0) and $
   (m ne np-1) and (dops eq 0) then hitme,s9

endfor

clear_tt  = [' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ']
clear_tt2 = [' ','',' ','',' ','',' ','',' ','',' ','']

clear_yy  = [' ','','','','','',' ']
clear_xx  = ['','','','',' ',' ',' ',' '] ; lower left plot = plot all time tick marks?

easyps, keywords, kk, dops=dops, /close
; ===================================================================
if dops eq 0 then $
 window, 2, title='WIRE B-type stars: Light curves',$
 xsize=750,ysize=1400 ; ,xpos=750,ypos=900

xer=3 & yer = 7 & yyps = 24
xer=3 & yer = 4 & yyps = 12

easyps, keywords, kk, dops=dops, /prepare, dim = [20,yyps,-1,1], $
 fil = 'bstars_lightcurves.ps', dir = '/home/bruntt/papers/wire/bstars/'

n6791pos,xer,yer,0,0,0,poslc ; number of lc's per plotting page?
np = n_elements(poslc(0,*)) ; number of plots per page
np1 = np

for m = 0,n-1 do begin
s = g(m)

if ((m mod np1) eq 0) or (m eq 0) then $
 plot,[0,1],xsty=6,ysty=6,/nodata ; new plot

; Default tickmarks: (all clear)
xtt = clear_tt
ytt = xtt
xtt2 = clear_tt
ytt2 = clear_tt & if zoom_yy then ytt2 = clear_yy
xt2 = ''
yt2 = ''

if (m mod np1) eq (np/xer-1) then begin
 xtt=clear_xx
 ytt=clear_yy
 xtt2 = clear_tt2
 ytt2 = clear_tt2 & if zoom_yy then ytt2 = clear_yy
 xt2 = 'HJD - t!I0!N'
 yt2 = '!4D!3flux [ppt]'
endif

if (m mod np1) eq (np-1) then begin
 xtt=clear_tt
 ytt=clear_tt
 xtt2 = clear_tt
 ytt2 = clear_tt & if zoom_yy then ytt2 = clear_yy
 xt2 = '' ; 'HJD - t!I0!N'
 yt2 = ''
endif

; s = where(wire.hd eq 160578)

lc  = wire(s).lc(0:wire(s).np-1)     ; MASSAGED LIGHT CURVE
lc1 = wire(s).lcsub2(0:wire(s).np-1) ; ORBITAL FREQ. WERE SUBTRACTED
lc2 = wire(s).lcsub(0:wire(s).np-1)  ; ALL FREQ REMOVED
tt  = wire(s).hjd(0:wire(s).np-1)    ; HJD TIMES (minus t0)
wei = wire(s).wei(0:wire(s).np-1)    ; WEIGHTS

fl = 1. ; set to 10. to round to first decimal
offt = round(median(tt) * fl) / fl
tt2 = tt - offt
; plot,tt2,lc,xr=[-15,15]

; X range for light curve
x1 = floor(min(tt) * 10.) / 10. - 1.
x2 =  ceil(max(tt) * 10.) / 10. + 1.
x2 = x1 + 25.

x1 = -15. & x2 = 35.
; x1 = -3  & x2 = 3.  ; zoomed lc plot
xmax_out = 12 ; do not plot data points at time > t0 + xmax_out (write warning on plot!)

t02 = wire(s).t0 + offt ; Nyt t0

yfac = 1e3 / 1.086 ; Remember: 1ppm = 1.086microMag: (ppm/microMag) * (1/1.086)
rr = 30. ; robust_sigma(lc) * yfac * 6.

wgood = where(wei/max(wei) gt .3 and tt2 lt xmax_out,npp)
wgood2 = where(wei/max(wei) gt .3,npp2)
every = 10.
pout = findgen(floor(npp/every)) * every

; Are all data points displayed in main window?
warn_rem = 0B
if (float(npp2) / npp) gt 1.001 then warn_rem = 1B ; more than 0.1% of data points removed?

plot,tt2(wgood(pout)),lc(wgood(pout))*yfac,$
 xr=[x1,x2], yr=[-1.,1]*rr, $
 xsty=1,ysty=1,xthick=2,ythick=2,charsi=1.2,charthick=2, $
 xtickname=xtt, ytickname=ytt, $
 xtit=xt2, ytit=yt2,$
 /noerase,position=poslc(*,m mod np1),psym=pzs,symsi=.1, /nodata

oplot,tt2(wgood(pout)),lc(wgood(pout))*yfac,psym=pzs,symsi=.2


if plot_fit then begin
 ;if mode_lc eq 'massage' then $
 ; oplot, wire(s).fitt-wire(s).t0, wire(s).fitd*yfac
  ; ALL FREQ. FITTED

 ;if mode_lc eq 'orb_sub' then $ 
 ; oplot, wire(s).fitt(wok(pl2))-wire(n).t0, wire(n).fitdorb(wok(pl2))*yfac
  ; ORBITAL FREQ SUBTRACTED
  oplot, wire(s).fitt-t02, wire(s).fitdorb*yfac,col=col.green,psym=3
endif


; Plot the LC:
; oplot,tt2(wgood(pout)),lc(wgood(pout))*yfac,psym=pzs,symsi=.2

xoff  = floor(x2 * 0.74)
xoff2 = floor(x2 * 0.1)

; Is star present more than once?
addhd = ''
mm = where(wire.hd eq wire(s).hd, cmm)
if cmm ge 2 then begin
 add = ['A','B','C','D','E','F','G','H','I','J']
 xcd = where(s eq mm,cxcd)
 addhd = ' [' + add(xcd) + ']'
endif

; t0 in bottom right corner:
;xyouts,x2- xoff,-rr*.83,charsize=0.9,charthick=1.0,$
; '24' + strcompress(string(t02,format='(D15.1)'),/remove_all)

; t0 in bottom left corner:
xyouts,x1+ xoff2,-rr*.83,charsize=0.9,charthick=1.0,$
 '24' + strcompress(string(t02,format='(D15.1)'),/remove_all)

; Top right corner:
;xyouts,x2- xoff,rr*.72,charsize=0.9,charthick=1.0,$
; 'HD' + strcompress(string(wire(s).hd,format='(I9)'),/remove_all)

; Lower left corner
;xyouts,x1+ xoff2,-rr*.83,charsize=0.9,charthick=1.0,$
; 'HD' + strcompress(string(wire(s).hd,format='(I9)'),/remove_all)

; Top left corner:
xyouts,x1+ xoff2,rr*.72,charsize=0.9,charthick=1.0,$
 'HD' + strcompress(string(wire(s).hd,format='(I9)'),/remove_all) + addhd

if warn_rem then xyouts,xmax_out-4.,-rr*.83,'!17(DPM)!3',charthick=2,charsi=.7

if detail_lc ge 1 then begin ; plot zoomed lc?
 nzoom = 3
 zpp = 0.25 ; zoom space in y (per plot)
 zps = 0.70 ; zoom plot start lower-y value

 for z=0,nzoom-1 do begin

 pos2 = poslc(*,m mod np1)
 xrr  = pos2(2) - pos2(0) ; x range
 yrr  = pos2(3) - pos2(1) ; y range
 poszoom = [pos2(0) + xrr * .60, pos2(1) + yrr * zps, $
            pos2(0) + xrr * .95, pos2(1) + yrr * (zps+zpp)] 
 poszoom = poszoom - float(z) * [0., yrr * zpp, $
                                 0., yrr * zpp]

 if zoom_yy then begin
   ptp_robust_fin, lc(wgood), noise, 1
   ; noise = robust_sigma(lc(wgood))
   noise = max(wire(s).a)  / 3. ; scale by maximum ampl. detected in star

; Exceptions for y-range:
   if wire(s).hd eq 158926 then noise = noise * 1.5 ; show eclipse!
   if wire(s).hd eq  40494 then noise = noise * 2.5 ; show eclipse!
   if wire(s).hd eq  35708 then noise = noise * 2. ; show eclipse!
   if wire(s).hd eq  68520 then noise = noise * 2. ; show eclipse!
   if wire(s).hd eq 133242 then noise = noise * 2.5 ; show eclipse!
   if wire(s).hd eq 127381 then noise = noise * 1.5 ; show eclipse!
   if wire(s).hd eq  66591 then noise = noise * 1.5 ; show eclipse!
   if wire(s).hd eq 101431 then noise = noise * 1.5 ; show eclipse!
   if wire(s).hd eq 152614 then noise = noise * 1.5 ; show eclipse!
   if wire(s).hd eq  37202 then noise = noise * 1.3 ; show eclipse!
   if wire(s).hd eq  37795 then noise = noise * 0.2 ; show eclipse!
   if wire(s).hd eq 128345 then noise = noise * 0.8 ; show eclipse!

   rr_zoom = ceil( (yfac * noise * 8.)  / 5.) * 5. 
 endif else $
   rr_zoom = rr

  x1z = 0. + z & x2z = 1. + z ; zoom range in time

  if z eq 0 then begin
   wz = where(tt gt x1z and tt le x2z,cz)
   if cz ge 1 then begin
   offy_lc = median(lc(wz)) * yfac
   if offy_lc lt 5 then offy_lc = 0.
   endif
  endif
 
  if z ge 1 then begin
    ytt2 = clear_tt
  endif
  if nzoom gt 2 then begin
       xtt2 = clear_tt
       if z eq (nzoom-1) then xtt2 = clear_tt2
  endif

 if detail_lc eq 1 then $ ; PLOT EVERY 'every' DATA POINT
  plot,/noerase,position=poszoom, $
    xtickname=xtt2, ytickname=ytt2, $ 
    tt2(wgood(pout)),lc(wgood(pout))*yfac,xr=[0,1],yr=[-1.,1]*rr_zoom-offy_lc,$
    psym=pzs,symsi=.2,charsi=.4,charthick=1
 if detail_lc eq 2 then $ ; PLOT ALL DATA POINTS
 plot,/noerase,position=poszoom, $
   xtickname=xtt2, ytickname=ytt2, $ 
   tt2(wgood),lc(wgood)*yfac,xr=[x1z,x2z],yr= [-1.,1]*rr_zoom-offy_lc,$
    psym=pzs2,symsi=.2,charsi=.4,charthick=1

; Mark eclipse in Lambda Sco ?
  if z eq 0 and wire(s).hd eq 158926 then begin
   if s eq 26 then $
   xyouts,.7,-25,'!17Ecl.!3',charsi=.5,charthick=2
  endif

endfor

 
endif ; any zoomed data points to plot?





endfor
; ===================================================================

easyps, keywords, kk, dops=dops, /close

if n_elements(keywords) ge 1 then $
 if keywords.color then col=getcolor(/load)

end


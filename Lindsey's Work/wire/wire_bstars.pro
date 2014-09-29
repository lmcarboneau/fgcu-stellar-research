; Create plots for Beta Ceph and SPB stars for WIRE targets:
; Run wire_bstars_known first?
; Requires input from wire_comp_teff_calibr.pro

; goto,here

m4_get_basedir,base

skiplc = 0B ; do not plot LC (takes a long time)

lcmode = 'zoomed'   & detail_lc = 0B & tobs_out = 1 
; lcmode = 'complete' & detail_lc = 2B & tobs_out = 0B

; Do not plot these WIRE stars: Bad stars, or not yet reduced
avoid = [81188L , 125473, 127972, 210839, 2905, 63975, 173300, 218045, 112185,119921] ; FROM: wire_hr_all.pro

; The different classes of B-type stars observed with WIRE:
betacep       = [158926L , 111123, 44743, 129056, 160578, 205021, 149757, 158408, 126341, 29248]
betacep_clean = betacep ; [158926L , 111123, 44743, 129056, 160578, 205021, 149757, 126341, 29248] ; change to betacep !!!

;; border = [127381, 175191, 35708] ; included in SPB: they look like
;; SPB in the ampl. spectra!
spb =[127381, 175191, 35708, 193924L , 158427, 125823, 66591, 130807, 128345, 87901, 152614, $
 101431, 40494, 37202, 133242, 30211, 37795, 68520 , 159217]  ; 159217 added 3nd of May 2005

x = sort(betacep) & betacep = betacep(x)
x = sort(betacep_clean) & betacep_clean = betacep_clean(x)
x = sort(spb)     & spb     = spb(x)


dops = 1B
plot_fit = 0B

;;; detail_lc = 2B ; overplot zoomed version of LC ? 0,1 or 2

zoom_yy = 1B ; in the zoomed plots --- change y-axis range individually?
pzs  = 3 ; symbol style for LC
pzs2 = 3 ; symbol style, zoomed LC

; Parameters for ampllitude plot:
mark_freq = 0B
 am = 0 ; plot raw ampltide spectrum
; am = 1 ; removed orbital freq.
; am = 2 ; remove all peaks
; am = 3 ; removed significant peaks
mark_freq3 = 0B


; Parameters for H-R plot
hdout_hr = 1B

if n_elements(wire) eq 0 then $
 restore,base + 'wire_process/reduced_B2.idl'

; wireobj3 struct:
 outname = base + 'wire_process/wire_params_Nov2006.idl' ; from .r wire_comp_teff_calibr.pro
 restore,outname


; plot,wireobj3.teff_nap,wireobj3.teff_nap-wireobj3.teff,psym=2,yr=[-1,1]*2000.,xr=[0,50000]
; mbol = wireobj3.v + (5.-5*alog10(1e3/wireobj3.par))+wireobj3.dt
; lum = 10.^((4.75-mbol)/2.5)
; tef = alog10(wireobj3.teff)

extby = wireobj3.extby ; E(b-y)
w = where(wireobj3.extby lt -.6,c) ; bad values
extby(w) = 0.0 ; assume no extinction

; Update BC using the Nap teff/logg
;   n = n_elements(wireobj3)
;   for i=0,n-1 do begin
;    teff9 =  wireobj3(i).teff_nap
;    logg9 =  wireobj3(i).logg_nap
;    if logg9 lt -.5 then logg9 = 4.0
;    if logg9 gt 5.0 then logg9 = 5.0
;    if teff9 gt 1000 and logg9 gt 0 then $
;     bessell, teff9, logg9, bc
;    wireobj3(i).bc = bc
;   endfor


;   mbol1 = wireobj.mv_templogg + (5.-5*alog10(1e3/wireobj3.par)) ; +wireobj3.bc
;   
;   mbol2 = wireobj3.v + (5.-5*alog10(1e3/wireobj3.par))+wireobj3.bc + 4.3 * extby
;   mbol3 = wireobj3.v + (5.-5*alog10(1e3/wireobj3.par))+wireobj3.bc ; + 4.3 * extby
;   
;   plot,alog10(wireobj3.teff_nap),mbol2,psym=1,xr=[4.6,3.6],yr=[7,-10]
;   ; oplot,alog10(wireobj3.teff),mbol2,psym=1,col=col.sky
;   oplot,alog10(wireobj3.teff_nap),mbol3,psym=1,col=col.sky


plotsym,0,/fill

g = reverse( sort(wire.bv) )
g = ( sort(wire.bv) )
g = reverse( sort(wire.teff) ) 
g = sort(wire.hd) 

; sort by luminosity:
if n_elements(lum2) eq 0 then begin
 lum2 = fltarr(n_elements(wire))
endif else begin
 g = reverse( sort(lum2) )
endelse


g = sort(wire.hd) 


n = n_elements(wire) ; number of stars

xer=3 & yer=12 & n6791pos,xer,yer,0.07,0,0,pos

np = n_elements(pos(0,*)) ; number of plots per page
np1 = np

ymin = 0.
ymax = 15000.
xmax = 7. ; max freq do display in c/day

; ===================================================================
;easyps, keywords, kk, dops=dops, /prepare, dim = [15,12,-1,1], $
; fil = 'bstars_noise.ps', dir = base + 'papers/wire/bstars/'
; ===================================================================
;if dops then col=getcolor(/load)
; plotsym,0,/fill
;  plot_io,wire.v,wire.noise10,psym=8,$
;   yr=[1,200],xr=[0,6], $
;   xtit='!18m!IV!N!3', ytit='!4r!3!Iampl!N (20d !4@!3 10mHz) [ppm]',charsi=1.5,charthick=2



; ===================================================================
easyps, keywords, kk, dops=dops, /prepare, dim = [15,12,-1,1], $
 fil = 'bstars_noise_20d.ps', dir = base + 'papers/wire/bstars/'
; ===================================================================

; n_pixels = 16.
; ff_noise = 1e3 * 0.01 / sqrt(n_pixels-1) ; ppt

tobs_ideal = 20.

duty = 0.35 ; duty cycle
points_pr_orbit = 96. * 60. * 0.35 * 2. ; = #seconds * 2
points_pr_day = points_pr_orbit * 15.
coverage = 0.9 ; big gaps in LC ?
points_pr_day_merged = round(coverage * points_pr_orbit * 15. / 31. / 10.) * 10. ; round to 10!

if dops then col=getcolor(/load)
 plotsym,0,/fill
  plot_io,wire.v,wire.noise10 * sqrt(wire.tobs/tobs_ideal),psym=8,symsi=1,$
   yr=[1,200],xr=[0,6], $
   xtit='!18m!IV!N!3', ytit='!4r!3!Iampl!N (' + $ 
      strcompress(string(tobs_ideal,format='(I5)'),/remove_all) + $
      'd !4@!3 10mHz) [ppm]',charsi=1.5,charthick=2


nw = n_elements(wire) & counts = fltarr(nw) & noise = counts & noise_amp = noise
for i = 0,nw-1 do begin
bv  = wire(i).bv
v   = wire(i).v
b   = bv + v

plotsym,0 ; open symbols
oplot,wire.v,wire.noise10,psym=8,symsi=1
for j=0,nw-1 do $
 oplot,wire(j).v*[1,1.], [wire(j).noise10, wire(j).noise10 * sqrt(wire(j).tobs/tobs_ideal)]

; Counts in one frame:
wire_counts,b, v, cnt8,/out
counts(i) = cnt8

gain = 15. & nmerge = 31. & data_points_pr_day_use = points_pr_day_merged

noise(i)     = 1e6 / sqrt(counts(i) * gain * (nmerge-1.) ) ; in ppt
noise_amp(i) = noise(i) *  sqrt(!PI / (data_points_pr_day_use * tobs_ideal) ) ; 

endfor

; oplot,wire.v,noise_amp,thick=2 ; observed

bv = -0.1 ; typical for these B-type stars
v1 = 0.0     & v2 = 6.0
b1 = bv + v1 & b2 = bv + v2

wire_counts,v1,b1, cnt_bright,/out
wire_counts,v2,b2, cnt_faint, /out
noise_ideal = 1e6 / sqrt([cnt_bright,cnt_faint] * gain * (nmerge-1.) ) $
               *  sqrt(!PI / (data_points_pr_day_use * tobs_ideal) ) ; 
oplot,[v1,v2],noise_ideal,thick=2 ; ,line=5
oplot,[v1,v2], noise_ideal*1.5, thick=2, line=5

; ===================================================================
easyps, keywords, kk, dops=dops, /close
; ===================================================================

; stop

; ===================================================================
if dops eq 0 then $
 window, 1, title='WIRE B-type stars',xsize=700,ysize=700,xpos=750,ypos=0 ; 900
; ===================================================================
easyps, keywords, kk, dops=dops, /prepare, dim = [15,15,-1,1], $
 fil = 'bstars_hr.ps', dir = base + 'papers/wire/bstars/'
if dops then col=getcolor(/load)

offx = 0.03
offy = 0.

xxx = [4.7,3.6] & yyy = [-.5,5] ; nice fig, all stars
xxx = [4.6,3.9] & yyy = [1.8,4.8] ; zoom in on hot stars

tef  = wireobj3.teff
lum  = wireobj3.lumn


plotsym,0 ; open

; =================================================================================
 plot,$
  alog10(wireobj3.teff),alog10(wireobj3.lumn),xr=xxx,yr=yyy,$
  xtit='!17log !3T!Ieff!N',ytit='!17log !3L/L!ISun!N',$
  psym=8,xsty=1,ysty=1,xthick=2,ythick=2,charsi=1.5,charthick=2,thick=2, /nodata
; =================================================================================

; Do not plot stars with bad data:
nout = n_elements(wireobj3)

; =================================================================================
for kk9=0,nout-1 do begin

 wav = where(wireobj3(kk9).hd eq avoid,cx_avoid)
 wwire = where(wireobj3(kk9).hd eq wire.hd,cx_wirestruct)

 t1 = alog10(wireobj3(kk9).teff) & l1 = alog10(wireobj3(kk9).lumn)
 plotsym,0 ; open symbols

 if cx_avoid eq 0 and cx_wirestruct eq 0 then begin
    if t1 gt min(xxx) and t1 lt max(xxx) and $
       l1 gt min(yyy) and l1 lt max(yyy) then $
    plots, t1, l1, psym=8,thick=2
 endif

endfor

; Print HD numbers of all stars:
;nall = n_elements(tef)
;for j=0,nall-1 do $
; xyouts, tef(j), alog10(lum(j)), $
;  strcompress(string(wireobj3(j).hd,format='(I8)'),/remove_all),$
;   col=col.sky,charsi=1,charthick=1

import_lejeune, base + 'evol/lejeune/modc020.dat', l
x = 22 & xx = [4.7,3.5] & yy = [-.5,6]
for i=5,n_elements(l)-1  do oplot, l(i).teff(0:x), l(i).l(0:x),psym=0,symsi=.3
xo = 0 & aa = 1.0  ; xo = x & aa = 0.0
offtxt_y = -0.1

; Print mass on ev. track:
off_ymass = -0.01
for i=5,n_elements(l)-1 do begin
 xo = 25
 targ_teff = 4.00 ; print mass around the same Teff
 dist = abs( l(i).teff - targ_teff)
 wd = where(dist eq min(dist),c_wd) & xo = wd(0)

 x1 = targ_teff ; l(i).teff(xo)
 y1 = l(i).l(xo) + off_ymass
 if x1 lt xxx(0) and x1 gt xxx(1) and y1 gt yyy(0) and y1 lt yyy(1) then $
 xyouts,x1,y1,$
  strcompress(string(l(i).m,format='(F9.1)'),/remove_all), align=aa, $
  charsi=1.0,charthick=2
endfor
; =================================================================================

plotsym,0,/fill

outfile_pdc = base + 'papers/wire/bstars/pdc.idl'
restore,outfile_pdc ; from " .r wire_bstars_known.pro " ---> TABLE FROM PETER DE CAT

if n_elements(pdc) ne n_elements(wire) then begin
 print,' %%% Need to relaunch wire_bstars_known ?? '
 stop
endif

; =================================================================================
 for s2=0,n-1 do begin
  u = g(s2)

  x = where(wire(u).hd eq wireobj3.hd,cx)
  x_aviod = where(wire(u).hd eq avoid,cx_avoid) ; do not plot stars with BAD data

  known = 0B & if pdc(u) ge 1 then known = 1 ; special symbol if star in known from Peter De Cat's website

; WIRE STRUCT DATA:
  plotsym,0,/fill
  if wire(u).noise10 lt 20 and wire(u).tobs gt 10. then plotsym,8,/fill ; wire_hr_all.pro

  if cx eq 1 and cx_avoid eq 0 then begin
   plots,alog10(tef(x)),alog10(lum(x)),col=col.red,thick=3,psym=8

   lum2(u) = lum(x)

; Store calibrated values: note Mv ---> luminosity!
   wire(u).mv   = wireobj3(x).lumn
   wire(u).teff = wireobj3(x).teff
   wire(u).eteff = wireobj3(x).eteff
   wire(u).logg = wireobj3(x).logg
   wire(u).elogg = wireobj3(x).elogg
   wire(u).feh = wireobj3(x).feh
   wire(u).efeh = wireobj3(x).efeh
 endif else begin
   print, ' *** FATAL MISMATCH: hd number: ',wire(u).hd
   print, ' *** NOT FOUND IN wireobj3 structure (parameter estimate strucutre)'
   print, ' *** Primary star Identification: ',wire(u).primnam,wire(u).entry
 endelse


 endfor 
; =================================================================================

; Pamyat. 1998 limits:
pamyat, x, spb=2, betacep=2, cz=1.0 ; txtout=txtout

; Print mass of each evolution track:
 for s2=0,n-1 do begin
  u = g(s2)

  x = where(wire(u).hd eq wireobj3.hd,cx)
  x_aviod = where(wire(u).hd eq avoid,cx_avoid) ; do not plot stars with BAD data

  known = 0B & if pdc(u) ge 1 then known = 1 ; special symbol if star in known from Peter De Cat's website

  if cx eq 1 and cx_avoid eq 0 then begin
   numout = s2 + 1
   numout = u
   numout = wire(u).hd

if hdout_hr then begin

below = [999] 

pointl = [68520,30211, 44743]
pointl2 = [111123,37795,40494,37202,133242]

pointr = [158926,130807,125823,35708 ]
pointr2 = [205021,129056]

 offy = 0.03 & offy2 = 0.0 & offx = 0.00 & aln = 0.5 ; default offset values

 bb = where(pointl eq numout,cbb)
 if cbb eq 1 then begin
   offy = -0.03  & offy2 = -0.01 &  offx = -0.04 &    aln  = 0.0 
   oplot,[alog10(tef(x)),alog10(tef(x))+offx],[alog10(lum(x)),alog10(lum(x))+offy],thick=2
endif

 bb = where(pointl2 eq numout,cbb)
 if cbb eq 1 then begin
   offy = 0.03  & offy2 = 0.01 &  offx = -0.04 &    aln  = 0.0 
   oplot,[alog10(tef(x)),alog10(tef(x))+offx],[alog10(lum(x)),alog10(lum(x))+offy],thick=2
 endif

 bb = where(pointr2 eq numout,cbb)
 if cbb eq 1 then begin
   offy = 0.03  & offy2 = 0.01 &  offx = 0.04 &    aln  = 1.0 
   oplot,[alog10(tef(x)),alog10(tef(x))+offx],[alog10(lum(x)),alog10(lum(x))+offy],thick=2
 endif

 bb = where(pointr eq numout,cbb)
 if cbb eq 1 then begin
   offy = -0.03  & offy2 = -0.01 &  offx = 0.04 &    aln  = 1.0 
   oplot,[alog10(tef(x)),alog10(tef(x))+offx],[alog10(lum(x)),alog10(lum(x))+offy],thick=2
 endif

   txtout = strcompress(string(numout,format='(I8)'),/remove_all)
   if known then txtout = txtout + '!EK!N'

    xyouts,alog10(tef(x))+offx, alog10(lum(x)) + offy + offy2, $
     txtout, charsi=0.8, charthick=1.0,align=aln

endif



endif

endfor






; ===================================================================
easyps, keywords, kk, dops=dops, /close
; ===================================================================

; stop

yyps = 24

; ===================================================================
if dops eq 0 then $
 window, 0, title='WIRE B-type stars: Amplitude Spectra',$
 xsize=750,ysize=1400 ; ,xpos=750,ypos=900
; ===================================================================

for amit =0,2 do begin

am = 0 ; default: plot raw amplitude spectrum
mark_freq_rem = 0B ; mark freq. that were removed

 if amit eq 0 then begin
     lcuse = betacep &    addps = '_betacep'
     xer=2 & yer = 5 & yyps = 14     
 endif
 if amit eq 1 then begin
     lcuse = spb    &    addps = '_spb'
     xer=2 & yer = 10 & yyps = 24
 endif
 if amit eq 2 then begin
     am = 3 ; cleaned ampl. spectrum
     lcuse = betacep_clean    &    addps = '_betacep_clean'
     xer=2 & yer = 5 & yyps = 14     
     wire_bstars_remfreq, remfreq ; get freq. to mark!
     mark_freq_rem = 1B
 endif

easyps, keywords, kk, dops=dops, /prepare, dim = [22,yyps,-1,1], $
 fil = 'bstars_ampl'+addps+'.ps', dir = base + 'papers/wire/bstars/'

n6791pos,xer,yer,0.07,0,0,pos
np = n_elements(pos(0,*)) ; number of plots per page
np1 = np

n2 = n_elements(lcuse)

; ===================================================================
for m = 0,n2-1 do begin
 s = where(wire.hd eq lcuse(m),ns)

 if ns ge 2 then begin ; select LC / ampl. with best data
  noise = wire(s).noise10
  wnoise = where(noise eq min(noise),cnoise)
  s = s(wnoise)
  print,' %%% Several spectra for star: ',lcuse(m), ' --- I will use: ',s
endif

; +++++++++++++++++++++
if s ne -1 then begin

xtt = [' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ']
ytt = xtt
xt2 = ''
yt2 = ''

if (m mod np1) eq (np/xer-1) then begin
 xtt=''  &  ytt=[' ','','','','','','','','','',''] ; only skip the first tick mark?
 xtt=''  &  ytt=[' ','',' ','',' ','',' ','',' ','',' '] ; print every second tick mark
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
if am eq 0 then begin ; RAW SPECTRA?
 if wire(s).hd eq  159217 then ymax = 0.5
 if wire(s).hd eq  37202 then ymax = 20.
 if wire(s).hd eq  29248 then ymax = 60.
 if wire(s).hd eq 125823 then ymax = 6.
 if wire(s).hd eq  37795 then ymax = 0.3
 if wire(s).hd eq  87901 then ymax = 1.0
 if wire(s).hd eq 152614 then ymax = 1.0
 if wire(s).hd eq  40494 then ymax = 1.5
 if wire(s).hd eq  68520 then ymax = 2.5
 if wire(s).hd eq 133242 then ymax = 0.5
 if wire(s).hd eq 193924 then ymax = 1.5
 if wire(s).hd eq 127381 then ymax = 3.0
 if wire(s).hd eq  66591 then ymax = 3.0
 if wire(s).hd eq 128345 then ymax = 3.0
 if wire(s).hd eq 101431 then ymax = 1.0
endif

; Exceptions
if am eq 3 then begin ; CLEANED spectra
 if wire(s).hd eq  29248 then ymax = 4.5
 if wire(s).hd eq  44743 then ymax = 0.6
 if wire(s).hd eq 111123 then ymax = 1.0
 if wire(s).hd eq 126341 then ymax = 1.5
 if wire(s).hd eq 129056 then ymax = 2.2
 if wire(s).hd eq 149757 then ymax = 2.0
 if wire(s).hd eq 158408 then ymax = 0.8
 if wire(s).hd eq 158926 then ymax = 2.5
 if wire(s).hd eq 160578 then ymax = 3.0
 if wire(s).hd eq 205021 then ymax = 2.0
endif

nf  = wire(s).n   ; number of freq. detected in total spectrum (not orbital freq)
nf2 = wire(s).n2  ; number of freq. detected in total spectrum (not orbital freq)
nf3 = wire(s).n3  ; number of freq. left in partly cleaned spectrum

amuse = am
if am eq 3 then begin
 gunik = uniq(wire(s).freq(am,*))  ; no ampl. spectrum for partly cleaned spectrum
 if n_elements(gunik) eq 1 then amuse = 2 ; clean all freq
endif

wgood = where(wire(s).freq(amuse,*) gt 0.,cgood)
plot,wire(s).freq(amuse,wgood),wire(s).ampl(amuse,wgood)*yscale,xr=[0,xmax], yr=[0,ymax],$
 xsty=1,ysty=1,xthick=2,ythick=2,charsi=1.2,$
 xtickname=xtt, ytickname=[' ','',' ','',' ','',' ','',' ','',' ','',' '], $
 xtit=xt2, ytit=yt2,$
 /noerase,position=pos(*,m mod np1)

; Mark frequencies except orbital harmonics + low freq. spurious peaks
if mark_freq then $
   for l=0,wire(s).n-1 do $
    oplot,wire(s).f2(l)*[1,1],[0,wire(s).a2(l)*1e6]*yscale,col=100,thick=2

; Mark freq. for partially cleaned spectrum
if mark_freq3 and amuse eq 3 then $
   for l=0,wire(s).n3-1 do $
    if wire(s).f3(l) gt .1 then $
    oplot,wire(s).f3(l)*[1.,1.],[0.,wire(s).a3(l)*1e6]*yscale,col=100,thick=2

if mark_freq_rem then begin
  rem = where(remfreq(0).freq gt 0.,crem)
  wrem = where(wire(s).hd eq remfreq.hd,crem2)
  if crem2 eq 1 then $
  for l=0,crem-1 do $
   oplot,remfreq(wrem).freq(rem(l))*[1.,1.],[0.,ymax],col=100,thick=2
endif

; print,wire(s).f(0:wire(s).n-1)   ; all freq
; print,wire(s).f2(0:wire(s).n2-1) ; all freq except low freq. peaks + orbital harms
; print,wire(s).f3(0:wire(s).n3-1) ; what's left after cleaning dominant peaks?

; Print star HD number on plot:
yy = !y.crange
y2 = ymax * 0.8
x2 = xmax * 0.5

numout = s + 1
numout = m + 1

; Is star present more than once?
addhd = ''
mm = where(wire.hd eq wire(s).hd, cmm)
if cmm ge 2 then begin
 add = strcompress(string(cmm,format='(I5)'),/remove_all)
 addhd = ' [' + add + ']'
; add = ['A','B','C','D','E','F','G','H','I','J']
; xcd = where(s eq mm,cxcd)
; addhd = ' [' + add(xcd) + ']'
endif

; Known B stars should be clearly identified:
add_decat = '' ; Star in De Cat's catalogue of known variable B stars?
if pdc(s) ge 1 then add_decat = '!IK!N'

outname = 'HD' + strcompress(wire(s).hd,/remove_all)+add_decat+addhd
xyouts,x2,y2,outname,charsi=0.9,charthick=1

; ' / ' + strcompress(string(numout,format='(I4)'),/remove_all),/data

if ((m mod np1) eq (np-1)) and (m ne 0) and $
   (m ne np-1) and (dops eq 0) then hitme,s9

endif
                           ; s NE -1 ?

endfor



clear_tt  = [' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ']
clear_tt2 = [' ','',' ','',' ','',' ','',' ','',' ','']

clear_yy  = [' ','','','','','',' ']
clear_xx  = ['','','','',' ',' ',' ',' '] ; lower left plot = plot all time tick marks?

easyps, keywords, kk, dops=dops, /close
endfor ; next ampl. spectrum plot

if skiplc then goto, jump_skiplc
; ===================================================================
if dops eq 0 then $
 window, 2, title='WIRE B-type stars: Light curves',$
 xsize=750,ysize=1400 ; ,xpos=750,ypos=900

xer=3 & yer = 12 & yyps = 24

; Three different LC plots:
for lcit=0,2 do begin

 if lcit eq 0 then begin
     lcuse = betacep &    addps = '_betacep'
     xer=2 & yer = 5 & yyps = 14     
 endif
 if lcit eq 1 then begin
     lcuse = spb    &    addps = '_spb'
     xer=2 & yer = 10 & yyps = 24
 endif

easyps, keywords, kk, dops=dops, /prepare, dim = [22,yyps,-1,1], $
 fil = 'bstars_lightcurves'+addps+'.ps', dir = base + 'papers/wire/bstars/'

n6791pos,xer,yer,0,0,0,poslc ; number of lc's per plotting page?
np = n_elements(poslc(0,*)) ; number of plots per page
np1 = np

n2 = n_elements(lcuse)

for m = 0,n2-1 do begin
 s = where(wire.hd eq lcuse(m),ns)

 if ns ge 2 then begin ; select LC / ampl. with best data
  noise = wire(s).noise10
  wnoise = where(noise eq min(noise),cnoise)
  s = s(wnoise)
  print,' %%% Several spectra for star: ',lcuse(m), ' --- I will use: ',s
endif

s = s(0)

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

if s ne -1 then begin
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

if lcmode eq 'complete' then begin
 x1 = -15.  &  x2 = 35.
endif

if lcmode eq 'zoomed' then begin
 x1 = -2    &  x2 = 2.  ; zoomed lc plot
endif

xmax_out = 12 ; do not plot data points at time > t0 + xmax_out (write warning on plot!)

t02 = wire(s).t0 + offt ; Nyt t0

; Y range for light curves:
yfac = 1e3 / 1.086 ; Remember: 1ppm = 1.086microMag: (ppm/microMag) * (1/1.086)
rr = 30. ; robust_sigma(lc) * yfac * 6.
if wire(s).hd eq 29248 then begin
 ytt= '' ; clear_tt
 rr = 80. ; exception in Y-range for HD 29248
endif


wgood = where(wei/max(wei) gt .3 and tt2 lt xmax_out,npp)
wgood2 = where(wei/max(wei) gt .3,npp2)
every = 10.
if lcmode eq 'zoomed' then every = 5
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

xoff  = floor(x2 * 0.74)
if lcmode eq 'complete' then xoff2 = floor(x2) * 0.1
if lcmode eq 'zoomed'   then xoff2 = floor(x2) * 0.1

; Is star present more than once?
addhd = ''
mm = where(wire.hd eq wire(s).hd, cmm)
if cmm ge 2 then begin
 add = strcompress(string(cmm,format='(I5)'),/remove_all)
 addhd = ' [' + add + ']'
; add = ['A','B','C','D','E','F','G','H','I','J']
; xcd = where(s eq mm,cxcd)
; addhd = ' [' + add(xcd) + ']'
endif

; t0 in bottom right corner:
;xyouts,x2- xoff,-rr*.83,charsize=0.9,charthick=1.0,$
; '24' + strcompress(string(t02,format='(D15.1)'),/remove_all)

timeout =  '24' + strcompress(string(t02,format='(D15.1)'),/remove_all)
if tobs_out then begin
 day = '  ('
 for k=0,cmm-1 do begin
   day = day + $
    strcompress(string(wire(mm(k)).tobs,format='(I9)'),/remove_all) 
   if k ne (cmm-1) then day = day + ', ' else day = day + 'd'
 endfor

 timeout = timeout + day + ')'
endif

; t0 in bottom left corner:
xyouts,x1+ xoff2,-rr*.83,charsize=0.9,charthick=1.0,timeout


; Known B stars should be clearly identified:
add_decat = '' ; Star in De Cat's catalogue of known variable B stars?
if pdc(s) ge 1 then add_decat = '!IK!N'
hdout = 'HD' + strcompress(string(wire(s).hd,format='(I9)'),/remove_all) + add_decat + addhd

; Top right corner:
;xyouts,x2- xoff,rr*.72,charsize=0.9,charthick=1.0,hdout

; Lower left corner
;xyouts,x1+ xoff2,-rr*.83,charsize=0.9,charthick=1.0,hdout

; Top left corner:
xyouts,x1+ xoff2,rr*.67,charsize=0.9,charthick=1.0, hdout
 

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

  offy_lc = 0.
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
col=getcolor(/load)

endfor ; next set of LCs
; ===================================================================
jump_skiplc:


 for s2=0,n-1 do begin
  u = g(s2)
  x = where(wire(u).hd eq wireobj3.hd,cx)
  if cx eq 1 then $
   plots,tef(x),alog10(lum(x)),col=col.red,thick=3,psym=8
   lum2(u) = lum(x)

; Store calibrated values: note Mv ---> luminosity!
   wire(u).mv   = wireobj3(x).lumn
   wire(u).teff = wireobj3(x).teff
   wire(u).eteff = wireobj3(x).eteff
   wire(u).logg = wireobj3(x).logg
   wire(u).elogg = wireobj3(x).elogg
   wire(u).feh = wireobj3(x).feh
   wire(u).efeh = wireobj3(x).efeh

 endfor 


; ===================================================================
here:

easyps, keywords, kk, dops=dops, /prepare, dim = [15,13.5,-1,1], $
 fil = 'bstars_teff_freq.ps', dir = base + 'papers/wire/bstars/'

sz = [.3, 2.] ; symbol size to use
ampl = [.5, 20] ; amplitude in ppt

pointr = [44743]
pointr2 = [99]

pointl = [129056 ]
pointl2 = [126341 ]



plotsym,0,/fill
plot_io,alog10(wire.teff), wire.f(0),psym=8,xr=[4.6,3.9],yr=[.01,40],/nodata, $
 xtit='!3log (T!17!Ieff!N!3)',ytit='!17f!3 [c/day]'
oplot,[4.33,4.47],[6.,6.],thick=2,line=2,col=100  ; Peter De Cat range
oplot,[4.28,4.28],[.01,50],thick=2,line=2,col=100 ; Boundary btw. SPB / Beta Ceph

for mode=0,1 do begin

 if mode eq 0 then begin
   staruse = betacep
   symbol = 4 ; triangle up
 endif
 if mode eq 1 then begin
   staruse = spb
   symbol = 5 ; triangle down
 endif
 
 nout = n_elements(staruse)

for g=0,nout-1 do begin

 s = where(wire.hd eq staruse(g),cs)
 if cs ge 2 then begin
   noise = wire(s).noise10
   wnoise = where(noise eq min(noise),cnoise)
   s = s(wnoise)
   print,' %%% Several TEFFPLOT for star: ',staruse(g), ' --- I will use: ',s
 endif

 amplitude   = wire(s).a2(0) ; avoid LOW FREQ + orbital peaks!
 frequency   = wire(s).f2(0)
 temperature = wire(s).teff

 good = 0B ; good data? long obs + now noise in the cleaned ampl. spectrum
 if wire(s).tobs ge 10. and wire(s).noise10 le 20 then good = 1
 if good then plotsym, symbol, /fill else plotsym, symbol

 szuse = interpol(sz, ampl, amplitude*1e3/1.086) ; symbol size
 plots,alog10(temperature), frequency, psym=8, symsi=szuse

 ; Known B stars should be clearly identified:
 add_decat = '' ; Star in De Cat's catalogue of known variable B stars?
 if pdc(s) ge 1 then add_decat = '!EK!N'
 hdout = 'HD' + strcompress(string(wire(s).hd,format='(I9)'),/remove_all) + add_decat + addhd



 offy = 1.10 & offy2 = 0.0 & offx = 0.02 & aln = 0.5 ; default offset values


 

 bb = where(pointl eq wire(s).hd,cbb)
 if cbb eq 1 then begin
   offy = 0.8  & offy2 = -0.01 &  offx = -0.04 &    aln  = 0.0 
   oplot,[alog10(temperature),alog10(temperature)+offx],[frequency,frequency*offy],thick=2
endif

 bb = where(pointl2 eq wire(s).hd,cbb)
 if cbb eq 1 then begin
   offy = 1.4  & offy2 = -0.01 &  offx = -0.04 &    aln  = 0.0 
   oplot,[alog10(temperature),alog10(temperature)+offx],[frequency,frequency*offy],thick=2
 endif

 bb = where(pointr eq wire(s).hd,cbb)
 if cbb eq 1 then begin
   offy = 0.8  & offy2 = 0.0 &  offx = 0.06 &    aln  = 1.0 
   oplot,[alog10(temperature),alog10(temperature)+offx],[frequency,frequency*offy],thick=2
endif

 bb = where(pointr2 eq wire(s).hd,cbb)
 if cbb eq 1 then begin
   offy = 1.2  & offy2 = 0.0 &  offx = 0.06 &    aln  = 1.0 
   oplot,[alog10(temperature),alog10(temperature)+offx],[frequency,frequency*offy],thick=2
 endif

 xyouts, alog10(temperature)+offx, frequency*offy, hdout, charsi=0.9, charthick=1.0,alignment=aln

endfor                          ; next star in this mode
endfor                          ; next mode of plotting



; ===================================

easyps, keywords, kk, dops=dops, /close
col=getcolor(/load)

end


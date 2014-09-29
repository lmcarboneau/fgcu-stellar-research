PRO wire_album, wire, $
 lumclass=lumclass, class=class, coloff=coloff, loglog=loglog, nops=nops

; Run this after wire_gather.pro
; This will produce a number of plots that present the WIRE data set
; See also wire_epsiloncep.pro for the B-type stars

default9, lumclass, [0,5]  ; evolution class
default9, class, [0,9]     ; temperature class

default9, synth, 6.        ; thickness of synthetic light curve on LC plot
default9, dops, 1B         ; create ps plot?
default9, nops, 0B
if nops then begin
  dops = 0B
  synth = 2.0
endif

; default9, mode_lc, 'massage' ; LC to be plotted: ORIGIAL LC
default9, mode_lc, 'orb_sub' ; LC to be plotted: ORBITAL + LOW-FREQ REMOVED

default9, cz, 1.5
default9, cth, 1.5
default9, titclear, [' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ']

default9, xleft, 0.05 ; starname, spectral type info 
default9, xleft1, 0.10 ; Tobs, hjd start X position
default9, xleft2, 0.15 ; frequencies start X position

default9, coloff, 0B
default9, loglog, 0B ; power spectra as log log plots?

default9, xm1,  .1 ; range x-axis
default9, xm2, 50

default9, gp, [1, 3, 2] ; ampl. spectra to plot: 
                        ; 0: raw, 
                        ; 1: -harmonics, 
                        ; 2: all freq. cleaned, 
                        ; 3: SN>10 og harm cleaned

w = where(wire.class ge min(class) and wire.class le max(class) and $
          wire.lumclass ge min(lumclass) and wire.lumclass le max(lumclass), c)
          
print,' %%% Album will include '+strcompress(c,/remove_all) + ' stars.'

; Add borders on plots:
bordx  = 0.08 & bordy = 0.02
bordx2 = 0.02 & bordy = 0.02

; Position of plots - array:
p = fltarr(8,4)
p(0,*) = [0.0, 0.6, 0.5, 1.0] ; txt info on star

p(1,*) = [0.55, 0.8-bordy, 1.0, 1.0] ; complete light curve
p(2,*) = [0.55, 0.6, 1.0, 0.8+bordy] ; complete light curve

p(3,*) = [0.0, 0.34-bordy, 1.0, 0.51+bordy] ; amplitude spectrum
p(4,*) = [0.0, 0.17-bordy, 1.0, 0.34+bordy] ; cleaned amplitude spectrum
p(5,*) = [0.0, 0.00-bordy, 1.0, 0.17+bordy] ; cleaned amplitude spectrum

; Add borders on plots:
 p(*,0) = p(*,0) + bordx & p(*,2) = p(*,2) - bordx2
 p(*,1) = p(*,1) + bordy & p(*,3) = p(*,3) - bordy
 p([3,4,5],1) = p([3,4,5],1) + bordy  & p([3,4,5],3) = p([3,4,5],3) + bordy




if dops then begin

   m4_get_basedir, dir
   dir = dir + 'papers/wire/'
   spec1a = strcompress(min(lumclass),/remove_all)
   spec1b = strcompress(max(lumclass),/remove_all)
   spec2a = strcompress(min(class),/remove_all)
   spec2b = strcompress(max(class),/remove_all)


   startFilename = 'wire_' + $
       'L' + spec1a + '-' + spec1b + '_' + $
       'T' + spec2a + '-' + spec2b + '.ps'
   startFilename = strcompress(startFilename,/remove_all)

   x = 20. & y = 25. & xo = (21.5-x) * 0.5 & yo = (30.5-y)*.5

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
    col = getcolor(/load) ; set up color table

colx = [col.black, col.red, col.sky, col.green] ; 0, 1, 2, 3
if coloff then colx = [0,50,100,150]


!P.multi=0 ; [0,2,2]
 !P.charsize=cz
 !P.charthick=cth


; For each star to be plotted:
for j=0,c-1 do begin

n = w(j)

; Basic properties




 jd1 = wire(n).t0 + min(wire(n).hjd)
 jd2 = wire(n).t0 + max(wire(n).hjd)
 daycnv, 2400000D + jd1, yr1, mn1, day1, nr1
 daycnv, 2400000D + jd2, yr2, mn2, day2, nr2

 if yr1 eq yr2 then $
   date_out = $
    strcompress(day1,/remove_all) + '/' + $
    strcompress(mn1,/remove_all) + ' - ' + $
    strcompress(day2,/remove_all) + '/' + $
    strcompress(mn2,/remove_all) + ' ' + strcompress(yr1,/remove_all) else $
   date_out = $
    strcompress(day1,/remove_all) + '/' + $
    strcompress(mn1,/remove_all) + ' ' + $
    strcompress(yr1,/remove_all) + ' - ' + $
    strcompress(day2,/remove_all) + '/' + $
    strcompress(mn2,/remove_all) + ' ' + strcompress(yr2,/remove_all) 

 plot,[0,1],/nodata,xsty=6,ysty=6,xmargin=[0,0],ymargin=[0,0], $
  position=[p(0,0),p(0,1),p(0,2),p(0,3)]

 spec_out = strcompress(wire(n).spec,/remove_all) + ' [ ' + $
                         strcompress(fix(wire(n).class),/remove_all) + ' / ' + $
                         strcompress(fix(wire(n).lumclass),/remove_all) + ' ]'

 lc = wire(n).lcsub(0:wire(n).np-1)
 
 ; ptp = robust_sigma(lc) * 1e3 / 1.086 ; point to point scatter in ppt
 ptp_robust_fin, lc * 1e3/1.086, ptp, 1

     gain = 15.0 ; electrons pr. ADU
     err_ff = 10000000. * gain ; data kept on the same pixels
     counts = 10.^((25. - wire(n).mmag) / 2.5) * gain ; number of electrons
     sky = 100. * gain ; ?
     ron = 5.0 ; I guess the read out noise for WIRE
     avg_fwhm = median(wire(n).fwhm(0:wire(n).np-1))
     r_aperture = avg_fwhm * 1.5
                                ; Variance in Ap. phot according to
                                ; Kjeldsen & Frandsen, PASP 104, 413, 192:

     var_ptp = (2. * alog(2.) ) / (avg_fwhm^2.0 * !PI * err_ff) + $
               1.0 / counts + $
               !PI * (r_aperture^2.0) * ( (sky + 5.0^2.0) / (counts^2.0) )

     nmerge = 31. ; every 31st data point is merged to a single data point
     sig_ptp = 1e3 * sqrt(var_ptp / (nmerge-1.) )

 st = strcompress(string(ptp,format='(F9.1)'),/remove_all) + ' ppt [' + $
 strcompress(string(ptp/sig_ptp,format='(F9.1)'),/remove_all) + ']'


 xyouts,xleft,1.0,'HD '+ strcompress(wire(n).hd,/remove_all) + ' / ' + wire(n).nam

 xyouts,xleft1,0.95, '(' + wire(n).primnam + '/' + wire(n).period + $
  '/' + strcompress(string(wire(n).entry,format='(I3)'),/remove_all) + ')',$
  charsi=cz*0.6

 xyouts,xleft1,0.88,spec_out + ' -- ' + st



 xyouts,xleft1,.77,$
 'T!Iobs!N = ' + strcompress(string(wire(n).tobs,format='(F9.1)'),/remove_all) + $
   ' d: ' + date_out
 xyouts,xleft1,.7, 'HJD: 24' + $
    strcompress(string(jd1,format='(F9.1)'),/remove_all) + ' - 24' + $
    strcompress(string(jd2,format='(F9.1)'),/remove_all) ,charsi=cz*0.7


  wf = where(wire(n).sn gt 0.01,cf)
  if cf ge 1 then begin
   fout = strarr(5, cf) 
   fcol = lonarr(cf) ; colours
   fout2 = strcompress(cf,/remove_all) + ' frequencies: '
   step  = 0.2
   stepy = 0.07

; Print frequency, amplitude, phase, and S/N
   for k=0,cf-1 do begin
    fout(0,k) = 'f!I' + strcompress(string(k+1,format='(I3)'),/remove_all) + '!N' 
;    fout(0,k) = strcompress(string(k+1,format='(I3)'),/remove_all)
    fout(1,k) = strcompress(string(wire(n).f(k),format='(F9.3)'),/remove_all)
    fout(2,k) = strcompress(string(wire(n).a(k)*1e6/1.086,format='(I9)'),/remove_all) 
    fout(3,k) = strcompress(string(wire(n).p(k),format='(F9.3)'),/remove_all) 
    fout(4,k) = strcompress(string(wire(n).sn(k),format='(F9.1)'),/remove_all)

    fcol(k) = colx(0) ; default plot color
    if wire(n).sn(k) le 4. then fcol(k) = colx(2) ; low SIGNAL-TO-NOISE
    wcc = where(abs(wire(n).f3-wire(n).f(k)) lt 1e-4,ccc)
    if ccc ge 1 then fcol(k) = colx(1) ; FREQUENCY = HARMONIC OR VERY LOW!

    xyouts,xleft2+.2-1.0*step,.5-k*stepy,charsi=cz*.8,fout(0,k),alignment=1.0, $
     col=fcol(k) ; number 
    xyouts,xleft2+.2+0.0*step,.5-k*stepy,charsi=cz*.7,fout(1,k),alignment=1.0, $
     col=fcol(k)  ; freq  
    xyouts,xleft2+.2+1.0*step,.5-k*stepy,charsi=cz*.7,fout(2,k),alignment=1.0, $
     col=fcol(k)  ; ampl  
    xyouts,xleft2+.2+2.0*step,.5-k*stepy,charsi=cz*.7,fout(3,k),alignment=1.0, $
     col=fcol(k)  ; phase 
    xyouts,xleft2+.2+3.0*step,.5-k*stepy,charsi=cz*.7,fout(4,k),alignment=1.0, $
     col=fcol(k)  ; S/N

   endfor

  endif
 
    xyouts,xleft2+.2-1.0*step,.6,charsi=cz*.65,'N'       ,alignment=1.0
    xyouts,xleft2+.2+0.0*step,.6,charsi=cz*.65,'f [c/d]' ,alignment=1.0
    xyouts,xleft2+.2+1.0*step,.6,charsi=cz*.65,'a [ppm]' ,alignment=1.0
    xyouts,xleft2+.2+2.0*step,.6,charsi=cz*.65,'p [0..1]',alignment=1.0
    xyouts,xleft2+.2+3.0*step,.6,charsi=cz*.65,'S/N'     ,alignment=1.0


lc  = wire(n).lc(0:wire(n).np-1)     ; MASSAGED LIGHT CURVE
lc1 = wire(n).lcsub2(0:wire(n).np-1) ; ORBITAL FREQ. WERE SUBTRACTED
lc2 = wire(n).lcsub(0:wire(n).np-1)  ; ALL FREQ REMOVED
tt  = wire(n).hjd(0:wire(n).np-1)    ; HJD TIMES (minus t0)
wei = wire(n).wei(0:wire(n).np-1)    ; WEIGHTS

; X range for light curve
x1 = floor(min(tt) * 10.) / 10.
x2 =  ceil(max(tt) * 10.) / 10.

yfac = 1e6 / 1.086 ; Remember: 1ppm = 1.086microMag: (ppm/microMag) * (1/1.086)
rr = robust_sigma(lc) * yfac

ttr   = 1.
ytxt  = 'dflux [ppm]'

if rr ge 1000. then begin
 yfac = 1e3 / 1.086
 ttr   = 1e3
 ytxt = 'dflux [ppt]'
endif

rr = robust_sigma(lc) * yfac
yy = ceil( (rr*2.) )

np = wire(n).np ; number of data points
np_plot = floor(np / 1000.) * 1000.
pl = findgen(np_plot/10) * 10

; ================================================
; Light curve: orig. light curve, harm. subtracted
; ================================================
plot,tt(pl), lc1(pl)*yfac,psym=1,symsi=.1,ytit=ytxt, $
 yr=[-1,1.] * yy, xr=[x1,x2], xsty=1,ysty=1,$
 xthick=2, ythick=2, $
  position=[p(1,0),p(1,1),p(1,2),p(1,3)],/noerase, $
  xtickname=titclear, ytickname=titclear,/nodata

; Compute a smoothed version of the time series:
temp_name = '~/temp/temp_lc.dat'
openw,1,temp_name
 if mode_lc eq 'massage' then for l=0L,np-1 do printf,1,tt(l),lc(l),wei(l)
 if mode_lc eq 'orb_sub' then for l=0L,np-1 do printf,1,tt(l),lc1(l),wei(l)
close,1
smooth_factor = 5.
wire_bin_ts,temp_name, smooth_factor, ssd, ssd2  ; ,/debug


; Overplot fitted light curve (not the same times as observed LC!)
 every_pt = 50L
 wok = where(wire(n).fitt gt -5000.,cok)
 np_plot2 = floor(cok / 1000.) * 1000.
 fact = floor(np_plot2/every_pt) & if fact lt 10 then fact = 10
 pl2 = findgen( fact ) * every_pt

if mode_lc eq 'massage' then $
 oplot, wire(n).fitt(wok(pl2))-wire(n).t0, wire(n).fitd(wok(pl2))*yfac, $
 col=colx(3),thick=synth ; ALL FREQ. FITTED
if mode_lc eq 'orb_sub' then $ 
 oplot, wire(n).fitt(wok(pl2))-wire(n).t0, wire(n).fitdorb(wok(pl2))*yfac, $
 col=colx(3),thick=synth ; ORBITAL FREQ SUBTRACTED

; Observed light curve -- smoothed:
oplot, ssd.t, ssd.d*yfac, psym=1,symsi=.2

; oplot, tt(pl), lc1(pl)*yfac, psym=1,symsi=.1
 

; ================================================
; Light curve: all freq subtracted:
; ================================================
plot,tt(pl), lc2(pl)*yfac,psym=1,symsi=.1,$
 yr=[-1,1.] * yy, xr=[x1,x2], xsty=1,ysty=1,$
 xtit='HJD - 24'+strcompress(string(wire(n).t0,format='(F9.1)'),/remove_all), $
 xthick=2, ythick=2, $
  position=[p(2,0),p(2,1),p(2,2),p(2,3)],/noerase,/nodata
 
; Compute a smoothed version of the time series:
openw,1,temp_name
for l=0L,np-1 do printf,1,tt(l),lc2(l),wei(l)
close,1
smooth_factor = 5.
wire_bin_ts,temp_name, smooth_factor, ssd_sub, ssd2_sub  ; ,/debug

oplot, ssd_sub.t, ssd_sub.d*yfac, psym=1,symsi=.2


; device,/close & set_plot,'x'
; stop






; ================================================
; Power spectrum
; ================================================

; Y range: log log plot or ordinary plot?
if loglog then begin
 yp1 = [1.0,1.0,1.0]
 yp2 = max(wire(n).ampl(0,*)) * 1.1 * [1,1,1.]
 yp2 = ceil(yp2/100.) * 100.
 xm1 = 0.1
 xm2 = 50.
endif else begin
 yp1 = [0.0,0.0,0.0]
 yp2 = [max(wire(n).ampl(gp(0),*)),  $
        max(wire(n).ampl(gp(1),*)),$
        max(wire(n).ampl(gp(2),*)) ] * 1.1
 yp2 = ceil(yp2/10.) * 10. ; Round to the nearest 10 ppm

 xm1 = 0.0
 min_freq_pp = 2.0 ; New plot below 2 c/day
 xm2 = ceil(max([max(wire(n).f2), 1.0]) / min_freq_pp) * min_freq_pp

 if wire(n).hd eq 40932 then print,wire(n).hd,' : ' , xm1,xm2
endelse


; for i=0,wire(n).n-1 do print,wire(n).f(i), wire(n).a(i), wire(n).sn(i)


if loglog then $
plot_oo,[0,1],$
  xr=[xm1,xm2],xsty=1,ysty=1,psym=-6,symsi=.4,yr=[yp1(0),yp2(0)], $
  position=[p(3,0),p(3,1),p(3,2),p(3,3)],/noerase,/nodata,$
  xtickname=titclear,ytickname=titclear else $
plot, [0,1], $
  xr=[xm1,xm2],xsty=1,ysty=1,psym=-6,symsi=.4,yr=[yp1(0),yp2(0)], $
  position=[p(3,0),p(3,1),p(3,2),p(3,3)],/noerase,/nodata,$
  xtickname=titclear,ytickname=[' ']

 dsm = reform(wire(n).smoo(2,*))
 oplot,wire(n).freq(0,*),dsm,col=colx(2),thick=2,min_value=1e-5
 oplot,wire(n).freq(0,*),dsm*4.,col=colx(2),thick=2,line=2,min_value=1e-5

wamp = where(wire(n).freq(gp(0),*) gt 1e-4 and wire(n).ampl(gp(0),*) gt 1e-6,camp)
if camp ge 10 then begin
 f = reform(wire(n).freq(gp(0),wamp))
 d = reform(wire(n).ampl(gp(0),wamp))
 wire_exclude_orbital, f,d, fnew, dnew, df=.5, forb=wire(n).orbital
 oplot,f,d,col=colx(1),min_value=1e-6
 oplot,fnew,dnew,min_value=1e-6
endif

; MARK THE SIGNIFICANT FREQUENCIES:
wover = where(d gt 1e-6,c) & a = min(d(wover))*.5
for i=0,wire(n).n2-1 do $
 oplot,[1.,1.]*wire(n).f2(i),[a,wire(n).a2(i)*1e6],col=colx(3),thick=2
; MARK THE DETECTED HARMONICS:
for i=0,wire(n).n3-1 do $
 oplot,[1.,1.]*wire(n).f3(i),[a,wire(n).a3(i)*1e6],col=colx(1),thick=2

 oo = 270 & oo = 0.
 for i=0,wire(n).n-1 do $
  if wire(n).sn(i) gt 8. and wire(n).f(i) gt .3 then $
  xyouts,wire(n).f(i),wire(n).a(i)*1e6,$
  '' + strcompress(string(i+1,format='(I4)'),/remove_all)+'',$
  orientation=oo,alignment=1.0,charsi=cz*0.7

; ============================================================================


if loglog then $
plot_oo, [0,1], $
  xr=[xm1,xm2],xsty=1,ysty=1,psym=-6,symsi=.4,yr=[yp1(1),yp2(1)], $
  position=[p(4,0),p(4,1),p(4,2),p(4,3)],/noerase,/nodata,$
  xtickname=titclear,ytickname=titclear  else $
plot, [0,1], $
  xr=[xm1,xm2],xsty=1,ysty=1,psym=-6,symsi=.4,yr=[yp1(1),yp2(1)], $
  position=[p(4,0),p(4,1),p(4,2),p(4,3)],/noerase,/nodata,$
  xtickname=titclear,ytickname=[' ']


wamp = where(wire(n).freq(gp(1),*) gt 1e-4 and wire(n).ampl(gp(1),*) gt 1e-6,camp)
if camp ge 10 then begin
 dsm = reform(wire(n).smoo(2,*))
 oplot,wire(n).freq(1,wamp),dsm,col=colx(2),thick=2,min_value=1e-5
 oplot,wire(n).freq(1,wamp),dsm*4.,col=colx(2),thick=2,line=2,min_value=1e-5

 f = reform(wire(n).freq(gp(1),wamp))
 d = reform(wire(n).ampl(gp(1),wamp))
 wire_exclude_orbital, f,d, fnew, dnew, df=.5, forb=wire(n).orbital
 oplot,f,d,col=colx(1),min_value=1e-6
 oplot,fnew,dnew,min_value=1e-6

; MARK THE SIGNIFICANT FREQUENCIES:

wover = where(d gt 1e-6,c_over)
if c_over ge 1 then begin
 a = min(d(wover))*.5
 for i=0,wire(n).n2-1 do $
  oplot,[1.,1.]*wire(n).f2(i),[a,wire(n).a2(i)*1e6],col=colx(3),thick=2
; MARK THE DETECTED HARMONICS:
 for i=0,wire(n).n3-1 do $
  oplot,[1.,1.]*wire(n).f3(i),[a,wire(n).a3(i)*1e6],col=colx(1),thick=2
endif

 oo = 270 & oo = 0.
 for i=0,wire(n).n2-1 do begin

  wid = where(wire(n).f2(i) eq wire(n).f,cid)
  if (wire(n).a2(i)*1e6) lt yp2(1) then $
  xyouts,wire(n).f2(i),wire(n).a2(i)*1e6,$
  '' + strcompress(string(wid(0)+1,format='(I4)'),/remove_all)+'',$
  orientation=oo,alignment=1.0,charsi=cz*0.7

 endfor

endif ; any data computed?

; ============================================================================



if loglog then $
plot_oo, [0,1],xr=[xm1,xm2],$
  xsty=1,ysty=1,psym=-6,symsi=.4,yr=[yp1(2),yp2(2)], $
  position=[p(5,0),p(5,1),p(5,2),p(5,3)],/noerase,/nodata else $
plot, [0,1],$
  xr=[xm1,xm2],xsty=1,ysty=1,psym=-6,symsi=.4,yr=[yp1(2),yp2(2)], $
  position=[p(5,0),p(5,1),p(5,2),p(5,3)],/noerase,/nodata

 dsm = reform(wire(n).smoo(2,*))
 oplot,wire(n).freq(gp(0),*),dsm,col=colx(2),thick=2,min_value=1e-5
 oplot,wire(n).freq(gp(0),*),dsm*4.,col=colx(2),thick=2,line=2,min_value=1e-5

wamp = where(wire(n).freq(gp(2),*) gt 1e-4 and wire(n).ampl(gp(2),*) gt 1e-6,camp)
if camp ge 10 then begin
 f = reform(wire(n).freq(gp(2),wamp))
 d = reform(wire(n).ampl(gp(2),wamp))
 wire_exclude_orbital, f,d, fnew, dnew, df=.5, forb=wire(n).orbital
 oplot,f,d,col=colx(1),min_value=1e-6
 oplot,fnew,dnew,min_value=1e-6
endif

; MARK THE SIGNIFICANT FREQUENCIES:
wover = where(d gt 1e-6,c) & a = min(d(wover))*.5
for i=0,wire(n).n2-1 do $
 oplot,[1.,1.]*wire(n).f2(i),[a,wire(n).a2(i)*1e6],col=colx(3),thick=2
; MARK THE DETECTED HARMONICS:
for i=0,wire(n).n3-1 do $
 oplot,[1.,1.]*wire(n).f3(i),[a,wire(n).a3(i)*1e6],col=colx(1),thick=2

 oo = 270 & oo = 0.
 for i=0,wire(n).n2-1 do begin
  wid = where(wire(n).f2(i) eq wire(n).f,cid)
  if (wire(n).a2(i)*1e6) lt yp2(2) then $
  xyouts,wire(n).f2(i),wire(n).a2(i)*1e6,$
  '' + strcompress(string(wid(0)+1,format='(I4)'),/remove_all)+'',$
  orientation=oo,alignment=1.0,charsi=cz*0.7
 endfor

;  'f!I' + strcompress(string(i+1,format='(I4)'),/remove_all)+'!N',$
 

; ============================================================================

if nops then begin
 hitme, s999
 if s999 eq 'x' then stop
endif

endfor ; next star


if dops then begin
      Device, /Close_File
      Set_Plot, thisDevice
      !P.Font = thisFont
      set_plot,'x'
   print,' $  gv ' + keywords.filename + '  & '
endif


!P.multi = 0

end

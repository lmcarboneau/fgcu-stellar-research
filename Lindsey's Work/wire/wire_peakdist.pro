; Explore the distibution of peak heights in ampl. spectra:

newplot = 1B

dops = 1B

skipbump = 1B ; do not plot the white noise with a bump (for paper)

most10day = 0B ; skip most 10day simulation
addn = 'most10day'
if most10day eq 0 then addn ='_Neutral'

f1 = 700. & f2 = 1800.
; f1 = 3000. & f2 = 4000.
; f1 = 400. & f2 = 700.

; f1 = 1500. & f2 = 2400.

ffit1 =  400.
ffit2 = 2500.

colsim = 100        ; simulations
colwhite = 50     ; pure white noise
colwhitebump = 200 ; white noise with a bump
col10 = 150 ; most simulation for 10 days
colobs = col.black  &  if dops eq 0 then colobs = col.sky


if dops and newplot then begin
   dir = '/home/bruntt/papers/procyon/'
   startFilename = 'Procyon_PeakDist'+addn+'.ps'
   startFilename = strcompress(startFilename,/remove_all)

   x = 20. & y = 10. & xo = (21.5-x) * 0.5 & yo = (30.5-y)*.5

   keywords = PSConfig(Cancel=cancelled, Filename=startFilename, $
    /European, directory=dir, $
    xsize=x, ysize=y, xoffset=xo, yoffset=yo)

   IF cancelled THEN RETURN
      thisDevice = !D.Name
      thisFont   = !P.Font
      !P.Font = 0
    Set_Plot, 'PS'
    Device, _Extra=keywords
    !P.font = -1
endif

if most10day eq 0 then n_sim = 3
if most10day then n_nim = 4

for j=0,n_sim-1 do begin
 if j ge 1 then newplot = 0B

; Simulations with the 'best' parameters:
if j eq 0 then begin
 densdir = '/home/bruntt/wire/procyon/simul/peakdist2000/density/'
 ren = 1 & colx = colsim
endif

; White noise with a bump:
if j eq 1 then begin
 densdir = '/home/bruntt/wire/procyon/simul/purewhite2000/density/'
 ren = 0 & colx=colwhite & exc=1
 if skipbump then goto,skip_s
endif

; Pure white noise:
if j eq 2 then begin
  densdir = '/home/bruntt/wire/procyon/simul/purewhite2000/density/'
  ren = 0 & colx=colwhitebump & exc = 0
  xyouts,4,.00037, 'White noise',charsi=1.0,charthick=2
endif

; Most simulations for the first 10 days:
if most10day then begin
 if j eq 3 then begin
   densdir = '~/wire/procyon/simul/mostfirst10/density/'
   ren = 1 & colx=col10 & exc = 0
   xyouts,15,.035, '10 d / 99% duty cycle',charsi=1.0,charthick=2
 endif
endif

spawnrob,'ls -1 ' + densdir + '*.density',l
nl = n_elements(l)

if j eq 0 then $
 restore,'~/wire/procyon/power/Procyon2000_lowsub.idl' ; p00l


debug = 0B


fr1 = p00l.f   &  dr1 = p00l.d
w1 = where(fr1 gt f1 and fr1 lt f2,c1)
fr2 = fr1(w1)  &  dr2 = dr1(w1)


wire_excessfit, p00l.f2,p00l.d2, ffit1, ffit2, fall, dall, /fudge
excessfit_obs = interpol(dall, fall, p00l.f )


if debug then begin
 hitme,s9
 plot_oo,p00l.f2,p00l.d2
 oplot,p00l.f,excessfit_obs,col=col.green
 hitme,s9
 plot,p00l.f,p00l.d/excessfit_obs,xr=[f1*.5,f2*1.5],/nodata
 oplot,p00l.f,p00l.d,col=col.red
 oplot,p00l.f,p00l.d/excessfit_obs
 hitme,s9
endif

w=where(p00l.f gt f1 and p00l.f lt f2,c)
pobs=p00l(w).d / excessfit_obs(w)
;sorter array
sp_obs=pobs(sort(pobs))
n       =n_elements(sp_obs)
nab_obs = 1-(findgen(n)+1)/n

if newplot then $
plot_io,[0,1],yr=[.0003,1.5],xr=[0,25],$
ysty=1,xsty=1,tit='!6',xtit='Power Density [ppm!E2!N/!4l!3Hz]!N',ytit='N!Dabove!N/N!Dtotal!N',$
 XTHICK=2, YTHICK=2,charsize=1.2,/nodata


if j eq 0 then $ 
 xyouts,11,.00037, 'WIRE 2000 Simul.',charsi=1.0,charthick=2


; ==========================================
; For each simulation:
; ==========================================
for i=0,nl-1 do begin
; ==========================================
 restore,l(i)

; wire_power_getval2, psim.f, pstore, harvey2
wire_excessfit, psim.f2,psim.d2, ffit1, ffit2, fall, dall, /fudge, /silent
wmost = where(strmatch(densdir,'*most*') eq 1,cmost)
if cmost eq 1 then wire_excessfit, psim.f2,psim.d2, 200, 2200, fall, dall, /silent
excessfit = interpol(dall, fall, psim.f )

if debug ge 2 then begin
 plot_oo,psim.f2,psim.d2
 oplot,fall,dall
 oplot,[f1,f1],[1,100]
 oplot,[f2,f2],[1,100]
endif

w=where(psim.f gt f1 and psim.f lt f2,c)

if ren then p=psim(w).d / excessfit(w) else begin
 fwhm = 800.
 sig = fwhm / 2.35
 fcen = 1400.
 g = 1. + exc * 1.8 * exp( -(psim.f - fcen)^2. / (2. * sig^2.) )
 p = psim(w).d * g(w)

; Debug:
goto,sk
 plot,psim.f,psim.d,yr=[0,20],xr=[0,2500],/nodata
 oplot,psim(w).f,p,col=col.green
 oplot,psim.f,psim.d,col=col.red
 oplot,psim.f,psim.d,/nodata
 oplot,psim.f,g,col=col.red,thick=2
 oplot,p00l.f,p00l.d/excessfit_obs,col=col.sky
 for k=1,100 do plots,174.19*k,!y.crange,line=2
 daybar,0,174.19
sk:

endelse



sp=p(sort(p))
n=n_elements(sp)
nab = 1-(findgen(n)+1)/n

oplot,sp,nab,col=colx

; =================================================
endfor ; for each simulation
; =================================================


goto,sl
  plot_oo,psim.f2,psim.d2,xr=[1,20000],yr=[.5,100]
  oplot,fall,dall
  oplot,p00l.f2,p00l.d2,col=col.green
  oplot,p00l.f,excessfit_obs,col=col.green
  oplot,[f1,f1],[1,100] &  oplot,[f2,f2],[1,100]
sl:

if newplot then $
xyouts, 10.5,.7,'Frequency range: ' + $
 strcompress(string(f1,format='(I8)'),/remove_all) + ' - ' + $
 strcompress(string(f2,format='(I8)'),/remove_all) + ' !4l!3Hz',$
 charsi=1.3, charthick=2

skip_s:

endfor

oplot,sp_obs, nab_obs, thick=4, col=colobs ; over plot observations

; ====================================================================
print,''

if dops then begin
      Device, /Close_File
      Set_Plot, thisDevice
      !P.Font = thisFont
      set_plot,'x'
   print,' $  gv ' + keywords.filename + '  & '
endif
; ====================================================================


END


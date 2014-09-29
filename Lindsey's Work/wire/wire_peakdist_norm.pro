; Explore the distibution of peak heights in ampl. spectra:
; Plot showing the normalisation



dops = 1B

skipbump = 0B ; do not plot the white noise with a bump (for paper)

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


if dops then begin
   dir = '/home/bruntt/papers/procyon/'
   startFilename = 'Procyon_PeakDist_Norm.ps'
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


for j=0,3 do begin

comment2 = ''

; Simulations with the 'best' parameters:
if j eq 0 then begin
 densdir = '/home/bruntt/wire/procyon/simul/peakdist2000/density/'
 ren = 1 & colx = colsim
 comment2 = 'WIRE 2000'
 comment = 'Gran + p-mode simulations'
endif

; White noise with a bump:
if j eq 1 then begin
 densdir = '/home/bruntt/wire/procyon/simul/purewhite2000/density/'
 ren = 0 & colx=colwhite & exc=1
 comment = 'White noise w/ bump'
 if skipbump then goto,skip_s
endif

; Pure white noise:
if j eq 2 then begin
  densdir = '/home/bruntt/wire/procyon/simul/purewhite2000/density/'
  ren = 0 & colx=colwhitebump & exc = 0
  comment = 'White noise'
endif

; Most simulations for the first 10 days:
if j eq 3 then begin
  densdir = '~/wire/procyon/simul/mostfirst10/density/'
  ren = 1 & colx=col10 & exc = 0
  comment= 'Simul: 10 d / 99% duty cycle'
endif


; Make list of all available simulations:
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

w=where(p00l.f gt f1 and p00l.f lt f2,c)
pobs=p00l(w).d / excessfit_obs(w)
;sorter array
sp_obs=pobs(sort(pobs))
n       =n_elements(sp_obs)
nab_obs = 1-(findgen(n)+1)/n

colspec=100

; ==========================================
; For each simulation:
; ==========================================
; for i=0,nl-1 do begin
for i=0,0 do begin ; only do this for one simulation, ok!
; ==========================================
 restore,l(i)

plot_oo,[0,1],yr=[.1,100],xr=[100,10000],$
ysty=1,xsty=1,tit='!6',ytit='Power Density [ppm!E2!N/!4l!3Hz]!N',xtit='Frequency [!4l!3Hz]',$
 XTHICK=2, YTHICK=2,charsize=1.2,/nodata
; xyouts, 120., .13, '' + densdir + ' / ' + comment, charsi=1.0, charthick=2,/data
 if comment2 eq '' then begin
   ww = where(strmatch(comment,'*White*',/fold) eq 1,cww)
   if cww eq 0 then $
    xyouts, 120., .13, comment, charsi=1.0, charthick=2,/data else $
    xyouts, 120., 70., comment, charsi=1.0, charthick=2,/data 
 endif
 if comment2 ne '' then xyouts, 120., .2, comment2, charsi=1.0, charthick=2,/data

; Observations:
if j eq 0 then begin
 oplot,p00l.f,p00l.d,col=colspec
 oplot,p00l.f2,p00l.d2
 oplot,p00l.f,excessfit_obs,thick=2
 oplot,[f1,f1],[interpol(excessfit_obs,p00l.f,f1),100],line=2,thick=2
 oplot,[f2,f2],[interpol(excessfit_obs,p00l.f,f2),100],line=2,thick=2
 
; Inserted plot shows the 'normalized' spectrum:
 plot,position=[.7,.7,.93,.9],/noerase,$
   p00l.f,p00l.d/excessfit_obs,xr=[f1-200,f2+300],$
   charsize=0.8,yr=[0,25]

; plot,p00l.f,p00l.d/excessfit_obs,xr=[f1*.5,f2*1.5],/nodata
; oplot,p00l.f,p00l.d,col=col.red
; oplot,p00l.f,p00l.d/excessfit_obs

; Make new plot for first set of simulations:
plot_oo,[0,1],yr=[.1,100],xr=[100,10000],$
ysty=1,xsty=1,tit='!6',ytit='Power Density [ppm!E2!N/!4l!3Hz]!N',xtit='Frequency [!4l!3Hz]',$
 XTHICK=2, YTHICK=2,charsize=1.2,/nodata

 xyouts, 120., .13, comment, charsi=1.0, charthick=2,/data

endif 

; ============================
; Simulations:
; ============================
; Artificial noise bump:
g = findgen(n_elements(psim.f)) & g(*) = 1.0 ; default: no bump
if j eq 1 then begin
 fwhm = 800.
 sig = fwhm / 2.35
 fcen = 1400.
 g = 1. + exc * 1.8 * exp( -(psim.f - fcen)^2. / (2. * sig^2.) )
endif

if ren eq 1 then begin
 wire_excessfit, psim.f2,psim.d2, ffit1, ffit2, fall, dall, /fudge, /silent
 wmost = where(strmatch(densdir,'*most*') eq 1,cmost)
 if cmost eq 1 then wire_excessfit, psim.f2,psim.d2, 200, 2200, fall, dall, /silent
 excessfit = interpol(dall, fall, psim.f )
endif else begin
 excessfit = findgen(n_elements(psim.f)) & excessfit(*) = 1.0 ; default: no bump
endelse

 oplot,psim.f,psim.d*g,col=colspec
 oplot,psim.f2,psim.d2*g
 if ren eq 1 then oplot,fall,dall
 oplot,[f1,f1],[interpol(psim.d2*g,psim.f2,f1),100],line=2,thick=2
 oplot,[f2,f2],[interpol(psim.d2*g,psim.f2,f2),100],line=2,thick=2

if j eq 1 then oplot,psim.f,g,line=2,thick=2 ; over plot gaussian noise bump!

 addy = 0.
 if j eq 3 then addy = 50. ; expand y axis ?
 plot,position=[.7,.7,.93,.9],/noerase,$
   psim.f,(psim.d/excessfit)*g,xr=[f1-200,f2+300],$
   charsize=0.8,yr=[0,25+addy],/nodata

; overplot input amplitudes:
 if j eq 3 then begin
  granulation_get_pmodes, 60., 9.9*sqrt(2.), frq, amp, pha
  for i=0,19 do oplot,frq(i)*[1.,1],[0,amp(i)^2.]/2.,col=150,thick=2
  for i=20,39 do oplot,frq(i)*[1.,1],[0,amp(i)^2.]/2. ,col=150,thick=2,line=2
  for i=40,59 do oplot,frq(i)*[1.,1],[0,amp(i)^2.]/2. ,col=150,thick=2,line=5
 endif
 oplot,psim.f,(psim.d/excessfit)*g




endfor ; for each simulation


skip_s:

endfor

oplot,sp_obs, nab_obs, thick=3, col=colobs ; over plot observations

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


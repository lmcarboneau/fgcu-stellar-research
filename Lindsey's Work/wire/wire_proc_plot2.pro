; Koer efter wire_proc_sim.pro

dops = 1

two = 0B ; two plots? set to 1
fmin = 50.

!P.multi = 0
if two then !P.multi=[0,1,2]
col=getcolor(/load)


colx=col.green
if dops eq 1 then colx=150

freqtxt = 'Frequency [!4l!3Hz]'
powertxt = 'Power [ppm!E2!N]'
logfreqtxt = 'Frequency [!4l!3Hz]' ; log!I10!N (
logpowertxt = 'Power Density [ppm!E2!N/!4l!3Hz]' ; log!I10!N (
logpowertxt2 = 'Power Density' 
ampltxt = 'Amplitude [ppm]'

restore,'/ai40/bruntt/wire/wire_simul/wire_granul_spec_00_2.gran.idl' ; batch 2, 2000
sgran00 = s

restore,'/ai40/bruntt/wire/wire_simul/wire_granul_spec_00_2.dat.idl' ; batch 2, 2000
sgranmode00 = s

restore,'/ai40/bruntt/wire/wire_simul/wire_granul_spec_MOST_2.dat.idl'
smost = s


; restore,'~/wire/wire_simul/wire_granul_spec_99.idl'
restore,'/ai40/bruntt/wire/wire_simul/wire_granul_spec_99_2.pmode.idl' ; batch 2, 1999, pmodes
spmode = s
;restore,'/ai40/bruntt/wire/wire_simul/wire_granul_spec_99_.dat.idl' ; batch 1, 1999, pmodes
;sall = s

; ================ ================
resol99 = 3.672 ; 1999 data set
resol00 = 9.502D ; 2000 data set

resol99a = 1.
resol00a = 1.
; ================ ================


restore,'/ai40/bruntt/wire/wire_simul/wire_granul_spec_99_2.dat.idl' ; 
sgranmode = s


; After cleaning 43 frequencies:
restore,'/ai40/bruntt/wire/wire_simul/procyon_1999_clean43_ampl.idl'



plot,smost(w).fs,smost(w).as^2.0,xr=[0,2500],yr=[0,100],/nodata
; oplot,freq1999clean43,(amp1999clean43)^2.0,col=col.red
wo = where(spmode.fo gt 0.,co) & po = findgen(co / 3) * 3. & po2 = findgen(co / 6) * 6.
 oplot,spmode(wo).fo,(spmode(wo).ao/1.086)^power
oplot,smost(w).fs,smost(w).as^2.0,col=col.sky

restore,'/ai40/bruntt/wire/wire_simul/wire_granul_spec_99_2.gran.idl' ; batch 2, 1999, pmodes
sgran = s

plot1 = '/ai40/bruntt/wire/wire_eps/wire_procyon_gran_pmode_power.ps'
setpl,15,8,plot1,1,1,encap=1,color=1
col=getcolor(/load)
power = 1.0

plot,[0,1],xr=[0,2500],yr=[0,35^(power)],$
 xtit=freqtxt,ytit=ampltxt,xthick=2,ythick=2,charsi=1.1,charthick=2,/nodata, $
 xsty=1,ysty=1

wg = where(sgran.fs gt 0,cg) & pg = findgen(cg/3.) * 3.
wp = where(spmode.fs gt 0,cp) & pp = findgen(cp/3.) * 3.
oplot,sgran(wg(pg)).fs,(sgran(wg(pg)).as^power),thick=1 ; only granulation
; oplot,sgran(wg).fs,(sgran(wg).as^power),thick=1 ; only granulation
; oplot,spmode(wp(pp)).fs,(spmode(wp(pp)).as^power),thick=1,col=col.sky ; only p-modes
oplot,spmode(wp).fs,(spmode(wp).as^power),thick=1,col=col.sky ; only p-modes

  ;oplot,sgran.fsn,(sgran.asn^power),col=colx,thick=1 ;granulation + noise
  ;oplot,sgran.an,(sgran.an^power),col=colx,thick=1 ; only granulation

power = 2.0
plot,position=[.5,.6,.93,.88],sgran.fs,(sgran.as^power),xsty=1,ysty=1,thick=1,$;  /nodata, $
 /noerase,xr=[300,1800], xtickname=['400',' ','800',' ','1200',' ','1600',' '], $
 xtit=freqtxt,ytit=powertxt,xthick=2,ythick=2,charsi=0.75,charthick=2,yr=[0,200]
oplot,spmode.fs,(spmode.as^power),thick=1,col=col.sky ; only p-modes

device,/close & set_plot,'x'
print,' gv ' + plot1 + ' &  '
col=getcolor(/load)




plot2 = '/ai40/bruntt/wire/wire_eps/wire_procyon_observ_power.ps'
setpl,15,8,plot2,1,1,encap=1,color=1
power = 1.0
col = getcolor(/load)

plot,[0,1],xr=[0,2500],yr=[0,35^(power)],xtit=freqtxt,ytit=ampltxt,xthick=2,ythick=2,charsi=1.1
; plot,[0,1],xr=[1000,1300],yr=[0,35^(power)],charthick=2,/nodata, xsty=1,ysty=1

wo = where(spmode.fo gt 0.,co) & po = findgen(co / 3) * 3. & po2 = findgen(co / 6) * 6.
 oplot,spmode(wo(po)).fo,(spmode(wo(po)).ao/1.086)^power,thick=2 ; observed spectrum
; oplot,spmode(wo).fo,((spmode(wo).ao/1.086)^power),thick=1,col=col.sky
wg = where(sgran.fs gt 0.,cg) & pg = findgen(cg / 3) * 3. & pg2 = findgen(cg / 6) * 6.
oplot,sgran(wg(pg)).fs,(sgran(wg(pg)).as^power),thick=1,col=col.sky ; gran. simulation

plot,position=[.5,.6,.93,.88],spmode(wo(po2)).fo,((spmode(wo(po2)).ao/1.086)^power),$
 /noerase,xr=[10000,20000],xtickname=['10000',' ','14000',' ','18000',' '], $
 xtit=freqtxt,ytit=ampltxt,xthick=2,ythick=2,charsi=0.75,charthick=2,yr=[0,6],ysty=1,thick=2
oplot,sgran(wg(pg2)).fs,(sgran(wg(pg2)).as^power),col=col.sky

whigh = where(spmode.fo gt 10000. and spmode.fo lt 20000.,c)
mm = median(spmode(whigh).ao/1.086)
plots,!x.crange,mm,thick=4,col=col.red
 xyouts,10500,5.1,'!4r!3=' + string(mm,format='(F4.1)') + ' ppm',charthick=2,charsi=1.0
;xyouts,10500,mm*4.1,'4 !4r!3',charthick=2,charsi=1.1

device,/close & set_plot,'x'
print,' gv ' + plot2 + ' &  '



sm = 30
power = 2.0

x = spmode.fo & y = smooth(((spmode.ao/1.086)^power)/resol99a,sm) 
wfit = where(y gt 0. and x gt fmin and x lt 35000.,cfit)
wire_power_weights, x(wfit), y(wfit), wei1

fmin = 10.
wlow  = where(x lt (200) and x gt fmin,clow)
whigh = where(x lt 1e4 and x gt 5e3,chigh)
pars = [median(y(wlow)), .001, 2.0, median(y(whigh))]

wmid = where(abs(x-1500.) lt 200,cmid) & frq = median(y(wmid))
best = (pars(0)-frq+pars(3)) / ( (frq-pars(3)) * 1500.^pars(2) )
best = best ^ (1. / pars(2)) & pars(1) = best

pow = curvefit(x(wfit),y(wfit), wei1, pars, sigma, $
 function_name='wire_power_lawk')
fre = x(wfit)
pow = curvefit(x(wfit),y(wfit), wei1, pars, sigma, $
 function_name='wire_power_lawk')
fre = x(wfit)

plot3 = '/ai40/bruntt/wire/wire_eps/wire_procyon_observ_logpower.ps'
setpl,15,8,plot3,1,1,encap=1,color=1

col = getcolor(/load)

plot_oo,[1,3],xr=[100,2e4],yr=[.01,600],xsty=1,ysty=1,$
 xtit=logfreqtxt,ytit=logpowertxt,xthick=2,ythick=2,charsi=1.1,/nodata

xyouts,150,1,'WIRE 1999',charthick=2,charsi=1.1
xyouts,150,.45,'Simulation of granulation',charthick=2,charsi=1.1,col=col.sky
xyouts,150,.18,'Fit to WIRE 1999',charthick=2,charsi=1.1,col=col.red

;wn = where(sgran.fn gt 0.,cn) & pn = findgen(cn / 3) * 3. & pn2 = findgen(cn / 6) * 6.
;oplot,sgran(wg(pn)).fn,smooth((sgran(wg(pn)).an^power)/resol99,sm),thick=1,col=col.red
wo = where(spmode.fo gt 0.,co) & po = findgen(co / 3) * 3. & po2 = findgen(co / 6) * 6.
oplot,spmode.fo,smooth(((spmode.ao/1.086)^power)/resol99,sm),thick=1
wg = where(sgran.fs gt 0.,cg) & pg = findgen(cg / 3) * 3. & pg2 = findgen(cg / 6) * 6.
oplot,sgran.fs,smooth((sgran.as^power)/resol99,sm),thick=1,col=col.sky ; gran. simulation
oplot,fre,pow/resol99,thick=4,col=col.red


overpl = 0B
if overpl then begin
plot,position=[.65,.65,.93,.88],$
 spmode.fo,smooth(((spmode.ao/1.086)^power)/resol99,sm), $
 /noerase,xr=[300,2100],$ ; xtickname=['10000',' ','14000',' ','18000',' '], $
 xtit=logfreqtxt,ytit='Power Density', $
 xthick=2,ythick=2,charsi=0.75,charthick=2,yr=[0,25],xsty=1,ysty=1,thick=2,/nodata
;oplot,sgran(wg(pn)).fn,smooth((sgran(wg(pn)).an^power)/resol99,sm),thick=1,col=col.red
oplot,spmode.fo,smooth(((spmode.ao/1.086)^power)/resol99,sm),thick=1
oplot,sgran.fs,smooth((sgran.as^power)/resol99,sm),thick=1,col=col.sky ; gran. simulation
oplot,x(wfit),pow,thick=4,col=col.red
xyouts,1350,20,'Observed',charthick=2,charsi=.8
xyouts,1350,15,'Granulation',charthick=2,charsi=.8,col=col.sky

plot,position=[.25,.27,.53,.50],$
 spmode.fo,smooth(((spmode.ao/1.086)^power)/resol99,sm), $
 /noerase,xr=[300,2100],$ ; xtickname=['10000',' ','14000',' ','18000',' '], $
 xtit=logfreqtxt,ytit='Power Density', $
 xthick=2,ythick=2,charsi=0.75,charthick=2,yr=[0,25],xsty=1,ysty=1,thick=2,/nodata
;oplot,sgran(wg(pn)).fn,smooth((sgran(wg(pn)).an^power)/resol99,sm),thick=1,col=col.red
oplot,spmode.fo,smooth(((spmode.ao/1.086)^power)/resol99,sm),thick=1
; oplot,sgran.fs,smooth((sgran.as^power)/resol99,sm),thick=1,col=col.sky ; gran. simulation
oplot,x(wfit),pow,thick=4,col=col.red

oplot,sgranmode.fs,smooth((sgranmode.as^power)/resol99,sm),$
 thick=1,col=col.sky ; gran. simulation + pmodes
xyouts,1350,20,'Observed',charthick=2,charsi=.8
xyouts,1350,15,'Gran + Osc.',charthick=2,charsi=.8,col=col.sky
endif

device,/close & set_plot,'x'
print,' gv ' + plot3 + ' &  '

; ========== === ========== ========== ======= ==================== ==========

; ========== === ========== ========== ======= ==================== ==========

; below 1999 power spectra, OBS +  gran + gran/osc
xx1 = 800
xx2 = 1300

plot4 = '/ai40/bruntt/wire/wire_eps/wire_procyon_observ_power4.ps'
setpl,15,8,plot4,1,1,encap=1,color=1
power = 2.0
col = getcolor(/load)

sm2 = 1
plot,[0,1],xr=[xx1,xx2],xtit=logfreqtxt,ytit=powertxt, $
 xthick=2,ythick=2,charsi=1,charthick=2,yr=[0,150],xsty=1,ysty=1,thick=2,/nodata
;oplot,sgran(wg(pn)).fn,smooth((sgran(wg(pn)).an^power)/resol99a,sm2),thick=1,col=col.red
wsp = where(sgran.fo gt 0,cfs)
wgg = where(sgran.fs gt 0.,cgg)
oplot,spmode(wsp).fo,smooth(((spmode(wsp).ao/1.086)^power)/resol99a,sm2),thick=1
oplot,sgran(wgg).fs,smooth((sgran(wgg).as^power)/resol99a,sm2),$
 thick=1,col=col.sky ; gran. simulation
oplot,fre,pow,thick=4,col=col.red ; overplot Harvey 1985 fit

xyouts,1060,135,'WIRE 1999',charthick=3,charsi=1.1
xyouts,1060,125,'Granulation',charthick=3,charsi=1.1,col=col.sky
xyouts,1060,115,'Orbital Frequency',charthick=3,charsi=1.1,col=col.red

; Mark orbital freqs.
forb99 = 15.001
forb00 = 15.050
fac = 1e6 / 86400D
for i=1,20 do $
 if i*(forb99*fac) gt xx1 and i*(forb99*fac) lt xx2 then $
   plots,i*(forb99*fac),!y.crange,line=0,thick=5,col=col.red

device,/close & set_plot,'x'
print,' gv ' + plot4 + ' &  '




plot5 = '/ai40/bruntt/wire/wire_eps/wire_procyon_observ_power5.ps'
setpl,15,8,plot5,1,1,encap=1,color=1
power = 2.0
col = getcolor(/load)

plot,[0,1],xr=[xx1,xx2],xtit=logfreqtxt,ytit=powertxt, $
 xthick=2,ythick=2,charsi=1,charthick=2,yr=[0,150],xsty=1,ysty=1,thick=2,/nodata
;oplot,sgran(wg(pn)).fn,smooth((sgran(wg(pn)).an^power)/resol99a,sm2),thick=1,col=col.red
oplot,spmode(wsp).fo,smooth(((spmode(wsp).ao/1.086)^power)/resol99a,sm2),thick=2
; oplot,sgran.fs,smooth((sgran.as^power)/resol99a,sm2),thick=1,col=col.sky ; gran. simulation
wfs = where(sgranmode.fs gt 0,cfs)
oplot,sgranmode(wfs).fs,smooth((sgranmode(wfs).as^power)/resol99a,sm2),$
 thick=1,col=col.sky ; gran. simulation + pmodes
 oplot,fre,pow,thick=4,col=col.red; overplot Harvey 1985 fit

xyouts,1060,135,'WIRE 1999',charthick=3,charsi=1.1
xyouts,1060,125,'Granulation + oscillation',charthick=2,charsi=1.1,col=col.sky
xyouts,1060,115,'Orbital Frequency',charthick=3,charsi=1.1,col=col.red
; oplot,fre,pow,thick=4,col=col.red; overplot Harvey 1985 fit

; Mark orbital freqs.
forb99 = 15.001
forb00 = 15.050
fac = 1e6 / 86400D
for i=1,20 do $
  if i*(forb99*fac) gt xx1 and i*(forb99*fac) lt xx2 then $
   plots,i*(forb99*fac),!y.crange,line=0,thick=5,col=col.red


device,/close & set_plot,'x'
print,' gv ' + plot5 + ' &  '

; above WIRE 1999 power spectra + gran

; ========== === ========== ========== ======= ==================== ==========

; below WIRE 2000 power spectra + gran

plot4_99 = '/ai40/bruntt/wire/wire_eps/wire_procyon_observ_power4_00.ps'
setpl,15,8,plot4_99,1,1,encap=1,color=1
power = 2.0
col = getcolor(/load)

; sm2 = 9
plot,[0,1], xr=[xx1,xx2],xtit=logfreqtxt,ytit=powertxt, $
 xthick=2,ythick=2,charsi=1,charthick=2,yr=[0,350],xsty=1,ysty=1,thick=2,/nodata
;oplot,sgran(wg(pn)).fn,smooth((sgran(wg(pn)).an^power)/resol00a,sm2),thick=1,col=col.red
wsp = where(sgran00.fo gt 0,cfs)
wgg = where(sgran00.fs gt 0.,cgg)
oplot,sgran00(wsp).fo,smooth(((sgran00(wsp).ao/1.086)^power)/resol00a,sm2),thick=1
oplot,sgran00(wgg).fs,smooth((sgran00(wgg).as^power)/resol00a,sm2),$
 thick=1,col=col.sky ; gran. simulation
 oplot,fre00,pow00,thick=4,col=col.red; overplot Harvey 1985 fit

xyouts,1060,175+140,'WIRE 2000',charthick=3,charsi=1.1
xyouts,1060,150+140,'Granulation',charthick=3,charsi=1.1,col=col.sky
xyouts,1060,125+140,'Orbital Frequency',charthick=3,charsi=1.1,col=col.red

; Mark orbital freqs.
forb99 = 15.001
forb00 = 15.050
fac = 1e6 / 86400D
for i=1,20 do $
  if i*(forb00*fac) gt xx1 and i*(forb00*fac) lt xx2 then $
   plots,i*(forb00*fac),!y.crange,line=0,thick=5,col=col.red

device,/close & set_plot,'x'
print,' gv ' + plot4_99 + ' &  '



plot5 = '/ai40/bruntt/wire/wire_eps/wire_procyon_observ_power5_00.ps'
setpl,15,8,plot5,1,1,encap=1,color=1
power = 2.0
col = getcolor(/load)



plot,[0,1],xr=[xx1,xx2],xtit=logfreqtxt,ytit=powertxt, $
 xthick=2,ythick=2,charsi=1,charthick=2,yr=[0,350],xsty=1,ysty=1,thick=2,/nodata
wsp = where(sgran00.fo gt 0,cfs)
oplot,sgran00(wsp).fo,smooth(((sgran00(wsp).ao/1.086)^power)/resol00a,sm2),thick=2
wfs = where(sgranmode00.fs gt 0,cfs)
oplot,sgranmode00(wfs).fs,smooth((sgranmode00(wfs).as^power)/resol00a,sm2),$
 thick=1,col=col.sky ; gran. simulation + pmodes

xyouts,1060,175+140,'WIRE 2000',charthick=3,charsi=1.1
xyouts,1060,150+140,'Granulation + oscillation',charthick=2,charsi=1.1,col=col.sky
xyouts,1060,125+140,'Orbital Frequency',charthick=3,charsi=1.1,col=col.red
 oplot,fre00,pow00,thick=4,col=col.red; overplot Harvey 1985 fit

; Mark orbital freqs.
forb99 = 15.001
forb00 = 15.050
fac = 1e6 / 86400D
for i=1,20 do $
   if i*(forb00*fac) gt xx1 and i*(forb00*fac) lt xx2 then $
   plots,i*(forb00*fac),!y.crange,line=0,thick=5,col=col.red


device,/close & set_plot,'x'
print,' gv ' + plot5 + ' &  '

; above WIRE 2000 power spectra + gran

; ========== === ========== ========== ======= ==================== ==========

resol00a = 1. ; fit the POWER spectrum
x = sgran00.fo & y = smooth(((sgran00.ao/1.086)^power)/resol00a,sm) 
wfit = where(y gt 0. and x gt fmin and x lt 35000.,cfit)
wire_power_weights, x(wfit), y(wfit), wei1

fmin = 10.
wlow  = where(x lt (200) and x gt fmin,clow)
whigh = where(x lt 1e4   and x gt 5e3,chigh)
pars00 = [median(y(wlow)), .001, 2.0, median(y(whigh))]

wmid = where(abs(x-1500.) lt 200,cmid) & frq = median(y(wmid))
best = (pars00(0)-frq+pars00(3)) / ( (frq-pars00(3)) * 1500.^pars00(2) )
best = best ^ (1. / pars00(2)) & pars00(1) = best

pow00 = curvefit(x(wfit),y(wfit), wei1, pars00, sigma00, $
 function_name='wire_power_lawk')
fre00 = x(wfit)
pow00 = curvefit(x(wfit),y(wfit), wei1, pars00, sigma00, $
 function_name='wire_power_lawk')
fre00 = x(wfit)



plot8 = '/ai40/bruntt/wire/wire_eps/wire_procyon_observ_log99_00.ps'
setpl,15,8,plot8,1,1,encap=1,color=1

col = getcolor(/load)

plot_oo,[1,3],xr=[100,2e4],yr=[.2,100],xsty=1,ysty=1,$
 xtit=logfreqtxt,ytit=logpowertxt,xthick=2,ythick=2,charsi=1.1,/nodata

xyouts,150,1,'WIRE 1999',charthick=2,charsi=1.1
xyouts,150,.45,'WIRE 2000',charthick=2,charsi=1.1,col=col.sky
;xyouts,150,.2,'Fit to WIRE 1999',charthick=2,charsi=1.1,col=col.red

 wo = where(spmode.fo gt 0.,co) & po = findgen(co / 3) * 3. & po2 = findgen(co / 6) * 6.
 oplot,spmode(wo(po)).fo,smooth(((spmode(wo(po)).ao/1.086)^power)/resol99,sm),thick=1

 wo2 = where(sgran00.fo gt 0.,co2) & 
  p00a = findgen(co2 / 3) * 3. & p00b = findgen(co2 / 6) * 6.
 oplot,sgran00(wo2(p00a)).fo,$
  smooth(((sgran00(wo2(p00a)).ao/1.086)^power)/resol00,sm),thick=1,col=col.sky


plot,[1,3],xr=[200,2200],yr=[0,100],xsty=1,ysty=1,$
 xtit=logfreqtxt,ytit=powertxt,xthick=2,ythick=2,charsi=0.7,/nodata ,/noerase,$
 position=[.6,.6,.92,.87]
 oplot,spmode(wo).fo,smooth(((spmode(wo).ao/1.086)^power)/resol99a,sm),thick=1
 oplot,sgran00(wo2).fo,$
  smooth(((sgran00(wo2).ao/1.086)^power)/resol00a,sm),thick=1,col=col.sky

plusminus = '' + string(177B) + ''
t99 = pars(1) * 1e6 / (2D * !DPI) & s99 = sigma(1) * 1e6 / (2D * !DPI)
t00 = pars00(1) * 1e6 / (2D * !DPI) & s00 = sigma00(1) * 1e6 / (2D * !DPI)
xyouts,1250,85,'!4s!3='+string(t99,format='(I3)')+$
 plusminus+string(s99,format='(I2)')+'s',charsi=.9
xyouts,1250,70,'!4s!3='+string(t00,format='(I3)')+$
 plusminus+string(s00,format='(I2)')+'s',col=col.sky,charsi=.9


oplot,fre,pow,thick=4,col=col.red
oplot,fre00,pow00,thick=4,col=col.red,line=2


device,/close & set_plot,'x'
print,' gv ' + plot8 + ' &  '


print,' Time scales: ', pars(1) * 1e6 / (2D * !DPI), '+-',  sigma(1) * 1e6 / (2D * !DPI)
print,' Time scales: ', pars00(1) * 1e6 / (2D * !DPI), '+-',  sigma00(1) * 1e6 / (2D * !DPI)


end

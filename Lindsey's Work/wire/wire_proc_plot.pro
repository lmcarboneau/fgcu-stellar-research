; Koer efter wire_proc_sim.pro

two = 0B ; two plots? set to 1
fmin = 50.

!P.multi = 0
if two then !P.multi=[0,1,2]
ff = 30  &  col=getcolor(/load)

restore,'~/wire/wire_simul/wire_granul_spec_99.idl'
resol = 3.672

 plot_oo,[0,1],xr=[50,35000],/nodata,yr=[.01,200],ysty=1,xsty=3, $
  xtit='log (Frequency)',ytit='log (Power)',tit='WIRE 1999 + 2000'
 oplot,s.fo,smooth(s.ao^2.0/resol,ff),col=col.sky
 oplot,s.fs,smooth(s.as^2.0/resol,ff),col=col.red
; oplot,s.fn,smooth(s.an^2.0/resol,ff),col=col.magenta

x = s.fo & y = smooth(((s.ao)^2.0)/resol,ff) 
wfit = where(y gt 0. and x gt fmin and x lt 35000.,cfit)
wire_power_weights, x(wfit), y(wfit), wei1

wlow  = where(x lt (fmin+500.) and x gt fmin,clow)
whigh = where(x lt 1e4 and x gt 5e3,chigh)
pars = [median(y(wlow)), .001, 2.0, median(y(whigh))]

wmid = where(abs(x-1500.) lt 200,cmid) & frq = median(y(wmid))
best = (pars(0)-frq+pars(3)) / ( (frq-pars(3)) * 1500.^pars(2) )
best = best ^ (1. / pars(2)) & pars(1) = best


pow = curvefit(x(wfit),y(wfit), wei1, pars, sigma, $
 function_name='wire_power_lawk')
oplot,x(wfit),pow

; =====================================================================

restore,'~/wire/wire_simul/wire_granul_spec_00.idl'
resol = 9.502

ll = 0
if not two then ll = 2

if two then $
 plot_oo,[0,1],xr=[50,5000],/nodata,yr=[.4,800],ysty=1,xsty=3, $
  xtit='log (Frequency)',ytit='log (Power)',tit='WIRE '+epoch
 oplot,s.fo,smooth(s.ao^2.0/resol,ff),col=col.navy,line=ll,psym=3
 oplot,s.fs,smooth(s.as^2.0/resol,ff),col=col.magenta,line=ll,psym=3
; oplot,s.fn,smooth(s.an^2.0/resol,ff),col=col.magenta,line=ll


end

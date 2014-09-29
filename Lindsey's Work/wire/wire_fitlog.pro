kilde = 'simul'
; kilde = 'obs'

fminfit = 350.

restore,'~/wire/wire_simul/wire_granul_spec_00.idl'
col=getcolor(/load)

if kilde eq 'simul' then begin
 w = where(s.fs gt 0.,c)
 weipow = fltarr(c) & weipow = 1./c
 x = s(w).fs & y = smooth((s(w).as)^2.0,15)
endif

if kilde eq 'obs' then begin
 w = where(s.fo gt 0.,c)
 weipow = fltarr(c) & weipow = 1./c
 x = s(w).fo & y = smooth((s(w).ao)^2.0,15)
endif


wire_power_weights, x, y, weinew

w2 = 300 + findgen(200) * 200L - round(100 * randomn(seed))
x2 = x(w2) & y2 = y(w2) 
wire_power_weights, x2, y2, wei2

wlow = where(x lt 800 and x gt 10.,clow)
pars = [median(y(wlow)), .001, 2.0]

guess = ( ( (pars(0)/y2) -1.)^(1./pars(2)) ) / x2
wg = where(finite(guess))
resistant_mean, guess(wg), 3, me, sd,nr
pars(1) = me

pow2 = curvefit(x2,y2, wei2, $
 pars, function_name='wire_power_law')

plot_oo,x,y,xr=[1,25000],yr=[.1,1e3]
oplot,x2,y2,col=col.sky,psym=2,symsi=.7
oplot,x2,pow2,col=col.red

wfit = where(x gt fminfit,clow)
wire_power_weights, x(wfit), y(wfit), wei3


pow = curvefit(x(wfit),y(wfit), wei3, $
 pars, sigma, function_name='wire_power_law')

pars2 = fltarr(6)
pars2(0:2) = pars
wm = where(x(wfit) gt 3e3 and x(wfit) lt 6e3,cm)
pars2(3) = median(y(wfit(wm)))
pars2(4) = 0.001
pars2(5) = 1.0

guess = ( ( (pars2(3)/y(wfit(wm))) -1.)^(1./pars2(5)) ) / x(wfit(wm))
wg2 = where(finite(guess))
resistant_mean, guess(wg2), 3, me, sd,nr
pars2(4) = me

pow2 = curvefit(x(wfit),y(wfit), wei3, $
 pars2, sigma2, function_name='wire_power_law2')

oplot,x(wfit),pow2,col=col.yellow,thick=3
oplot,x(wfit),pow,col=col.green


end



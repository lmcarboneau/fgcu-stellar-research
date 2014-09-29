; June 2004: after wire_acen_sim.pro, use this prg. to plot the
; log/log power spectrum to try and detect granulation / modes

col=getcolor(/load)

; epoch = '2004'
epoch = '1999'

restore,'/ai40/bruntt/wire/wire_simul/wire_granul_acen_'+epoch+'_NA.idl'

w = where(s.fo gt 0.,c)
plot_oo,s(w).fo,smooth(s(w).ao^2.0,61),xr=[.1,7e4],yr=[.1,5e4],xsty=1,ysty=1


forb99 = 15.001
forb00 = 15.050
forb04 = 15.34
case epoch of
 '2004': forb = forb04
 '1999': forb = forb99
endcase


xx1 = 100 & xx2 = 2e4
fac = 1e6 / 86400D
for i=1,40 do $
 if i*(forb*fac) gt xx1 and i*(forb*fac) lt xx2 then $
   oplot,[1,1.]*i*(forb04*fac),[1,1000],line=0,thick=1,col=col.red

fmin = 4. & sm = 20 & power = 2.0 & resol04a = 1. ; fit the POWER spectrum
x = s.fo & y = smooth(((s.ao/1.086)^power)/resol04a,sm) 
wfit = where(y gt 0. and x gt fmin and x lt 50000.,cfit)
wire_power_weights, x(wfit), y(wfit), wei1

wlow  = where(x lt (20) and x gt fmin,clow)
whigh = where(x gt 1.5e4   and x lt 5e4,chigh)
pars04 = [median(y(wlow)), .002, 2.0, median(y(whigh))]

wmid = where(abs(x-3000.) lt 500,cmid) & frq = median(y(wmid))
best = (pars04(0)-frq+pars04(3)) / ( (frq-pars04(3)) * 3000.^pars04(2) )
best = best ^ (1. / pars04(2)) & pars04(1) = best * 1.5

fc = pars04(3) + pars04(0) / (1.0 + (pars04(1) * x(wfit))^(pars04(2)))
oplot,x(wfit),fc,col=col.magenta ; first guess

wfit2 = where(y(wfit) lt fc*6.,cfit2)
wfit = wfit(wfit2)
wire_power_weights, x(wfit), y(wfit), wei1


pow04 = curvefit(x(wfit),y(wfit), wei1, pars04, sigma04, $
 function_name='wire_power_lawk')

stop

pow04 = curvefit(x(wfit),y(wfit), wei1, pars04, sigma04, $
 function_name='wire_power_lawk')

plot_oo,x,y,thick=3,xr=[10,4e4],yr=[.01,1000]
oplot,x(wfit),pow04,col=col.sky
oplot,x(wfit),pow04*2.,col=col.sky

stop

wfit2 = where(y(wfit) lt pow04*1.,cfit2)
wfit3 = wfit(wfit2)
 oplot,x(wfit3),y(wfit3),col=col.sky

wire_power_weights, x(wfit3), y(wfit3), wei3

wlow  = where(x(wfit3) lt (200) and x(wfit3) gt fmin,clow)
whigh = where(x(wfit3) gt 1e4   and x(wfit3) lt 4e4,chigh)
pars04n = [median(y(wfit3(wlow))), .002, 2.0, median(y(wfit3(whigh)))]

pow04n = curvefit(x(wfit3),y(wfit3), wei3, pars04n, sigma04n, $
 function_name='wire_power_lawk')
pow04n = curvefit(x(wfit3),y(wfit3), wei3, pars04n, sigma04n, $
 function_name='wire_power_lawk')
oplot,x(wfit3),pow04n,col=col.red,thick=2 ; totalt i orden fit!


plot_oo,x(wfit3),y(wfit3)
oplot,x(wfit3),pow04n,col=col.red,thick=2 ; totalt i orden fit!


print,' Time scale: ', pars04n(1) * 1e6 / (2D * !DPI), '+-',  sigma04n(1) * 1e6 / (2D * !DPI)
print,' Time scale: ', pars04(1) * 1e6 / (2D * !DPI), '+-',  sigma04(1) * 1e6 / (2D * !DPI)

t = [200, 302, 500] ; sun, acen, procyon time scales
g = [2.7, 1.97, 1.08] ; * 1e4 ---> gravities

end


PRO wire_density_analysis, lc, fmax=fmax

default9, fmax, 15000. ; microHz

; ====================================================================================
;  Import light curve - set up frequency range for FFT
; ====================================================================================
readcol, lc, torg, dorg, worg

tt2 = torg & dat2 = dorg & dat2 = dat2 - median(dat2) & wei2 = worg / total(worg)
minfreq = 1.5 / (max(tt2)-min(tt2)) & maxfreq = fmax / fj
; ====================================================================================

; ====================================================================================
; Calculate power density factor by inserting a sinus wave at 1 milliHz
; ====================================================================================
wire_power_res, tt2, wei2, density ; /debug
; ====================================================================================

; ====================================================================================
;  Calculate ampl. spectrum including weights
; ====================================================================================
ampl_spec_calc_wire_rv,tt2,dat2,wei2,minfreq,maxfreq,freq,amp,phase
; ====================================================================================

; ====================================================================================
;  Store the ampl. spectrum and the power density
; ====================================================================================
np = n_elements(freq)
cen = replicate( {f:0., d:0., a:0., n:0., f2:0., a2:0., d2:0., n2:0., d2fit:0.}, 400000)
cen(0:np-1).f = freq * fj    ; frequency in microHz
cen(0:np-1).a = amp          ; amplitude
cen(0:np-1).d = (amp^2.0) / density ; power density
cen(0:np-1).n = np-1         ; number of points
cen = cen(0:np-1)
; ====================================================================================

; ====================================================================================
;  Remove orbital periods before smoothing the power density curve!
; ====================================================================================
wire_exclude_orbital,cen(0:cen(0).n).f, cen(0:cen(0).n).d,fny,dny,forb=forb,df=35,inc=.001

if debug then begin
 plot_oo,cen(0:cen(0).n).f, smooth(cen(0:cen(0).n).d,10),xr=[100,10000],xsty=1,ysty=3
 oplot,fny,dny,min_value = 1e-3,col=col.green
 hitme, s
endif
; ====================================================================================

w = where((dny gt 1e-3),c) ;  or cen(0:cen(0).n2).f gt 2500.),c) ;  and abs(fny-1200.) gt 400,c)
f1 = cen(0:cen(0).n).f & d1 = cen(0:cen(0).n).d & f1 = f1(w) & d1 = d1(w)
oplot,f1,d1,col=col.red,psym=3
wire_power_weights, f1, d1, ww

stop


; Testing on a small freq. window
w = where((dny gt 1e-3) and cen(0:cen(0).n2).f gt 1500. and cen(0:cen(0).n2).f lt 3000.,c)
f1 = cen(0:cen(0).n).f & d1 = cen(0:cen(0).n).d & f1 = f1(w) & d1 = d1(w)

wire_power_weights, f1, d1, ww,va=va,fr=fr,overlap=0.001,fcen=2000,fwid=200,gwid=20. ; ,/gauss_plot

plot,f1,smooth(d1,5,/edge),xr=[2000,3000],xsty=1
oplot,fr,va,col=col.sky,psym=-1
; oplot,obs(0:obs(0).n).f, smooth(obs(0:obs(0).n).d,50,/edge),col=col.sky,thick=2

plot,f1,smooth(d1,5,/edge),xr=[2000,2500],xsty=1,psym=-2,symsi=.1
 oplot,fr,va,col=col.sky,psym=-1,thick=3


wire_power_logfit, f1, d1, log_cen, loga_cen,fmin=200.,fmid=3000.,p=p
wire_power_getval, cen(0:cen(0).n).f, p, harvey
oplot,cen(0:cen(0).n).f, harvey, col=col.magenta,thick=2

cen(0:np-1).d2fit = harvey

oplot,cen(0:cen(0).n).f, smooth(cen(0:cen(0).n).d,50),col=col.yellow

; Compute smoothed density + ampl. spectra:
wire_power_weights, cen(0:cen(0).n).f, cen(0:cen(0).n).d, ww,$
 va=va,fr=fr,overlap=0.01, fcen=4000.
wire_power_weights, cen(0:cen(0).n).f, cen(0:cen(0).n).a, ww,$
 va=va2,fr=fr2,overlap=0.01, fcen=4000. ,/gauss_plot

np = n_elements(fr2)
cen(0:np-1).n2 = np-1       ; number of points
cen(0:np-1).f2 = cen(0:cen(0).n2).f
cen(0:np-1).a2 = interpol(va2, fr2, cen(0:cen(0).n2).f)
cen(0:np-1).d2 = interpol(va,  fr,  cen(0:cen(0).n2).f)


; ====================================================================================

END

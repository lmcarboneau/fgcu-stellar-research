PRO wire_density_analysis, lc, cen, fmax=fmax, forb=forb, $
 debug=debug, fmin=fmin, $
 fskip=fskip, skip2=skip2, skip3=skip3, df=df, density=density, $
 skip_smooth=skip_smooth, highres=highres, $
 dont_sub_median=dont_sub_median

default9, fmax, 15000. ; microHz ... used for calc. area pr. res. element in power spectrum
default9, fj, 1e6/86400D ; conversion factor
default9, debug, 0B
default9, fmin, 50. ; exclude frequencies below fmin microHz in HARVEY fit!
default9, fskip, 0B
default9, skip2, 0B
default9, skip3, 0B
default9, df, 35.    ; parameter for excluding oribital power (set to 1e-7 to ignore)
default9, skip_smooth, 0B
default9, highres, 1.
default9, dont_sub_median, 0B

if n_elements(forb) eq 0 then begin
 print,' >>> Parameter forb is missing ... setting to default value!'
 forb = 173.6 ; microHz
endif

; ====================================================================================
;  Import light curve - set up frequency range for FFT
; ====================================================================================
readcol, lc, torg, dorg, worg, format='D,D,D'

tt2 = torg & dat2 = dorg
if dont_sub_median eq 0 then dat2 = dat2 - median(dat2) ; subtract median? (not for window func)
wei2 = worg / total(worg)
minfreq = 1.5 / (max(tt2)-min(tt2)) & maxfreq = fmax / fj
; ====================================================================================

; ====================================================================================
; Calculate power density factor by inserting a sinus wave at 1 milliHz
; ====================================================================================
if n_elements(density) eq 0 then $
 wire_power_res, tt2, wei2, density ; ,/debug
; ====================================================================================

; ====================================================================================
;  Calculate ampl. spectrum including weights
; ====================================================================================
ampl_spec_calc_wire_rv,tt2,dat2,wei2,minfreq,maxfreq,freq,amp,phase,$
 highres=highres,dont_sub_median=dont_sub_median
; ====================================================================================

; ====================================================================================
;  Store the ampl. spectrum and the power density
; ====================================================================================
np = n_elements(freq)
cen = replicate( {f:0., d:0., a:0., n:0., f2:0., a2:0., d2:0., n2:0., d2fit:0., dkfit:0., $
                  f1:0., d1:0.}, np)
cen.f = freq * fj    ; frequency in microHz
cen.a = amp          ; amplitude
cen.d = (amp^2.0) / density ; power density
cen.n = np-1         ; number of points
; cen = cen(0:np-1)
; ====================================================================================

; ====================================================================================
;  Remove orbital periods before smoothing the power density curve!
; ====================================================================================
wire_exclude_orbital,cen.f, cen.d,fny,dny,$
 forb=forb,df=df,inc=.001, fskip=fskip, skip2=skip2, skip3=skip3

if debug then begin
 col=getcolor(/load)
 plot_oo,cen.f, smooth(cen.d,10),xr=[1000,10000],xsty=1,ysty=3
 oplot,fny,dny,min_value = 1e-3,col=col.green
 hitme, s
 plot_io,cen.f, smooth(cen.d,10),xr=[4000,8000],xsty=1,ysty=3
 hitme, s
 oplot,fny,dny,min_value = 1e-3,col=col.green
 hitme, s
endif
; ====================================================================================


if skip_smooth then goto, skipsm
; ====================================================================================
; Computed gaussian-smoothed spectrum: gaussian has width (df) at freq
; (fref). At other frequencies that width scales with the number of
; points in each logarithmic frequency interval.
; ====================================================================================
w = where((dny gt 1e-6),c)
f1 = cen.f & d1 = cen.d & f1 = f1(w) & d1 = d1(w)
wire_power_smooth, f1, d1, ds, df=150.,fref=1000.,res=0.1 ; ,/debug

cen(0:c-1).f1 = f1 ; The freq + power that you are fitting the data to!
cen(0:c-1).d1 = d1 

np = n_elements(freq)
cen.n2 = np-1       ; number of points
cen.f2 = cen.f
cen.d2 = interpol(ds, f1, cen.f)

wb = where((dny lt 1e-6),c)
if c ge 1 then cen(wb).d2 = -1. ; BAD DATA!

; ============================================================================
                                                                              
; ============================================================================
; Fit Harvey (1986) profile to the smoothed spectrum:                         
; ============================================================================
goto,skip_smooth_fit
f1s = cen.f2 & d1s = cen.d2

wire_exclude_orbital,f1s,d1s, fny_s,dny_s,$
 forb=forb,df=df,inc=.001, fskip=fskip, skip2=skip2, skip3=skip3

wire_power_logfit, fny_s, dny_s, log_cen, loga_cen,fmin=fmin,fmid=1000.,p=p,flow_up=200.,kpower=1B 
wire_power_getval, cen.f2, p, harvey_s
cen.d2fit = harvey_s

wire_power_logfit, fny_s, dny_s, log_cen, loga_cen,fmin=fmin,fmid=1000.,p=p,flow_up=200.,kpower=0B 
wire_power_getval2, cen.f2, p, harvey_s
cen.dkfit = harvey_s


if debug then begin
 plot_oo,f1,smooth(d1,5),xr=[10,fmax],xsty=1,xtit='Frequency',ytit='Power'
 oplot,cen.f2,cen.d2,col=col.sky
 oplot,cen.f2,cen.d2fit,col=col.red
endif
skip_smooth_fit:

; goto,skip_fit_direct
; ====================================================================================
; Fit Harvey profil to the original power spectrum: but avoid orbital periods
; ====================================================================================
w = where((dny gt 1e-6),c)
f1 = cen.f & d1 = cen.d & f1 = f1(w) & d1 = d1(w)

; Harvey fit but with variable exponent
wire_power_logfit, f1, d1, log_cen, loga_cen,fmin=fmin,fmid=2000.,p=p,flow_up=200.,kpower=1B ; ,/debug
wire_power_getval, cen.f, p, harvey_dir
cen.d2fit = harvey_dir

; Harvey fit with fixed exponent == 2.0
wire_power_logfit, f1, d1, log_cen, loga_cen,fmin=fmin,fmid=2000.,p=p,flow_up=200.,kpower=0B ; ,/debug
wire_power_getval2, cen.f, p, harvey_dir
cen.dkfit = harvey_dir

; ====================================================================================

; ====================================================================================
if debug then begin
 col=getcolor(/load)
 plot_oo,f1,d1,xr=[10,35000],xsty=1
 oplot,cen.f, harvey, col=col.magenta,thick=2
 oplot,cen.f, smooth(cen.d,50),col=col.yellow
 hitme, s
endif
; ====================================================================================
skip_fit_direct:
skipsm:




; ====================================================================================

END

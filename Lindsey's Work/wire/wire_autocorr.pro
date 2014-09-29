PRO wire_autocorr, freq, amp, center, fwhm, damping_time, auto, $
 p=p, unit=unit, over=over,f_orbit=f_orbit,sep=sep,out=out, $
 thresh = thresh, lagmax=lagmax, nlag=nlag

; Eksempel:
;  freq = fc(0).freq & amp = fc(0).amp/1e6
;  wire_autocorr, freq, amp, 0.9, 0.9, 0.25, a, p=1, unit='micro'

wg = where(freq gt 0.,cg) & amp2 = amp(wg) & freq2=freq(wg)
col = getcolor(/load)

if n_elements(f_orbit) eq 0 then f_orbit = 173.652 ; microHz
if n_elements(sep) eq 0 then sep = 54.7 * 0.5 ; microHz
if n_elements(out) eq 0 then out = 0

; ==================================================================
; DEFINE FREQUENCY UNIT:
; ==================================================================
if n_elements(unit) eq 0 then unit = 'micro'
if strmatch(unit,'*micro*',/fold_case) eq 1 then begin
 freq_unit = 1D
 freq_name = 'microHz'
 fac = 86400D / 1e6
endif
if strmatch(unit,'*milli*',/fold_case) eq 1 then begin
 freq_unit = 1D * 1e-3
 freq_name = 'milliHz'
 fac = 0
endif
if strmatch(unit,'*day*',/fold_case) eq 1 then begin
 freq_unit = 86400D / 1e6
 freq_name = 'c/day'
 fac = 1D
endif

hitme,mess = ' %%% Frequencies are measured in ' + freq_name + ' right?',s9


; ==================================================================

; ==================================================================
; Daming time ... fold by gaussian
; ==================================================================
if damping_time gt 0. then begin
; damping_time = 1.5 ; damping time in days
fwhm_damping = 1./(86400. * damping_time * freq_unit)

fwhm_damping = 1./(damping_time * fac)


print,' %%% Damping "frequency" set to: ' + $
 string(fwhm_damping,format='(F7.4)') + ' ' + freq_name

; Construct a gaussian with a certain width
gauss_fwhm = fwhm_damping & gauss_sigma = gauss_fwhm / 2.35
nf = n_elements(freq2)
df = median(freq2(1:nf-1) - freq2(0:nf-2))
ng = ceil(10. * round(6. * gauss_fwhm / df)) / 10.
gg = fltarr(2,ng)
gg(0,*) = findgen(ng) * df & x0 = gg(0,round(ng*0.5)-1)
gg(1,*) = exp(- ( (gg(0,*)-x0)^2.0) / (2. * (gauss_sigma)^2.0 ) )
gg(1,*) = gg(1,*) / total(gg(1,*))

; Debug, please don't delete:
; plot,gg(0,*),gg(1,*) / max(gg(1,*)),xtit='Frequency',$
;   tit='Convolving spectrum by this Gaussian'
; oplot,fwhm_damping*0.5*[-1,1]+x0,[1,1]*.5,thick=3

amp_conv = convol(reform(amp2), reform(gg(1,*)), /edge_truncate)
endif else begin
 amp_conv = amp2
 print,' %%% No convolution of amp. spectrum due to damping time contrast enhancement ... '
endelse

if n_elements(p) eq 1 then begin
 if p eq 2 then begin
  plot,freq2,amp2,xr=[0.5,1.5] & oplot,freq2,amp_conv,col=col.red
  wait,1
 endif
endif

; ==================================================================

n = 8. ; Order of super gaussian
sigma    = fwhm/( 2 * (ALOG(2))^(1/n) )
window   = EXP(-0.5*((freq2-center)/sigma)^n)

power = amp_conv * amp_conv * window & freq3 = freq2

; Remove data outside threshold
if n_elements(thresh) ne 0 then begin
 w = where(amp_conv lt thresh,c) 
 if c ge 1 then power(w) = 0.
 print,' %%% Removing points below threshold ... '
endif

if n_elements(p) ne 0 then begin
 if p ne 0 then begin
  plot,freq3,power
  oplot,freq2,window * max(!y.crange)
  wait,1
 endif
endif

; Lag range in micro Hz --> Range will cover TWO times the orbial period!
lagmin = 0.0
if n_elements(lagmax) eq 0 then lagmax = 180. * 1.2 * freq_unit ; two times the orbital period of WIRE (~174 microHz)

nee = n_elements(freq3)
dfreq = median(freq3(1:nee-1) - freq3(0:nee-2))

if n_elements(nlag) eq 0 then $
 nlag = ceil(0.01 * (lagmax - lagmin) / dfreq) * 100L 

print,' %%% lag freq. range = 0.000 to ' + $
   strcompress(string(lagmax,format='(F8.3)'),/remove_all) + ' ' + $
   freq_name + ' / ' + $
   'nlag = ' + strcompress(string(nlag,format='(I8)'),/remove_all)

; stop

lag = findgen(nlag)
auto3 = a_correlate(power, lag) ; , /double)

auto = fltarr(2,n_elements(auto3))
auto(0,*) = lag * dfreq 
auto(1,*) = auto3

if n_elements(over) eq 1 then begin
 if over eq 1 then begin
  plot,auto(0,*),auto(1,*)/max(auto(1,*)),xtit='Frequency Lag ['+freq_name+']',$
   ytit='Normalized Auto Correlation Power',$
   xthick=2,ythick=2,yr=[0,1]
;  wire_peakcomb,173.652*freq_unit,54.7*freq_unit,dummy,p=1
;  sep = 32.372633 ; 33.1325 ; 54.7 * 0.5 ; large separation in microHz (deltaNu / 2)
  wire_peakcomb,f_orbit*freq_unit,sep*freq_unit,dummy,p=1,out=out
 endif
 if over ge 2 then begin
  oplot,auto(0,*),auto(1,*)/max(auto(1,*)),line=over,thick=2
  print,' %%% Overplotting ... '
 endif
endif


end


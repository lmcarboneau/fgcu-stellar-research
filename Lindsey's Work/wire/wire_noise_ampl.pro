PRO wire_noise_ampl, tt, dat, f1, f2, $
 noise=noise, freq=freq, amp=amp, wei=wei, highres=highres, debug=debug


n = n_elements(tt)
if n_elements(wei) eq 0 then begin
 wei    = fltarr(n)
 wei(*) = 1D / double(n)
endif

; Make sure f2 does not exceed the theoretical sampling limit:
tstep = median(tt(1:n-1) - tt(0:n-2))
nyquist = 1. / (2. * tstep)

fmax = nyquist * 0.5
if f2 gt fmax then begin
 print, ''
 print, ' *** The max freq. you specify is close to nyquist frequency: ',nyquist, ' c/day'
 print, ''
 stop
endif

default9, highres, 2.0
default9, debug, 0B

; Frequency in c/day
minfreq=f1
maxfreq=f2

; Avoid changing the input data:
tt2  = tt
dat2 = dat
wei2 = wei

ampl_spec_calc_wire_rv,tt2,dat2,wei2,minfreq,maxfreq,freq,amp,phase,highres=highres

x  = median(amp)
resistant_mean, amp, 3, me, sd, nr

noise = me / 1.086

print,' %%% Noise in amplitude spectrum: '
print,' %%% Median noise level in ppm: ' ,  x / 1.086
print,' %%% Robust averg level in ppm: ' , me / 1.086

print,''
print,' %%% Estiamted white noise component in light curve: '
print,' %%% sqrt(PI/N) * sigma(highfreq) = ', sqrt(float(n)/!PI) * (me / 1.086), ' ppm'
print,' %%% sqrt(PI/N) * sigma(highfreq) = ', sqrt(float(n)/!PI) * (me / 1e3), ' mmag'

if debug then begin
 plot,freq,amp,ysty=3
 plots,!x.crange,me,line=2,thick=2,color=200
endif

END

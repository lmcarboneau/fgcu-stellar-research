
restore,'~/wire/wire_simul/wire_granul_spec_MOST.idl'

col=getcolor(/load)

w = where(s.ds gt -.5,c)
t = s(w).ts & d = s(w).ds & wei = s(w).ws

fac = 1e6/86400D
f = 1e3 / fac ; 1 milli Hz is the input frequency

tt2 = t & dat2 = d & wei2 = wei / total(wei)
dat2(*) = 0. & dat2 = 1.0 * sin(2.*!DPI*(f*tt2 + .5))
; wei(*) = 1. ; equal weights

minfreq = 0.005 & maxfreq = 300.

ampl_spec_calc_wire_rv,tt2,dat2,wei2,minfreq,maxfreq,$
 freq2,amp2,phase2,highres=1.0,nmax=nmax
freq = reform(freq2)*1e6/86400D & amp = reform(amp2)/1e6

plot,freq,amp,xtit='Frequency [!4l!3Hz]',ytit='Amplitude'

n = n_elements(amp)
a = 0.
for i=0,n-2 do $
  a = a + (0.5 * (amp(i) + amp(i+1))) * (freq(i+1) - freq(i))

n = n_elements(amp)
a2 = 0.
for i=0,n-2 do $
  a2 = a2 + (0.5 * (amp(i)^2. + amp(i+1)^2.)) * (freq(i+1) - freq(i))


print,' %%% Area of window function: ' + $
 string(a,format='(F9.4)') + ' microHz'
print,' %%% Area of window function in power: ' + $
 string(a2,format='(F9.4)') + ' microHz'

end

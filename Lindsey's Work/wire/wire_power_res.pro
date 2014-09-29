PRO wire_power_res, tt, w2, res, debug=debug

; Input: times(tt) and weights(w2)

default9, debug, 0B

fj = 1e6/ 86400D

; Generate light curve for a perfect sinusoid
fcen = 1000. / fj ; frq. at 1000 microHz = 1 milliHz
d2 = sin(2.*!PI * (fcen*tt + 0.45))
; Use this sinusoid to calculate the area pr. resolution element
; in the power spectrum

tt2 = tt & dat2 = d2 & wei2 = w2

s = sort(tt2) & tt3 = tt2(s) & ntt = n_elements(tt3)
steps = tt3(1:ntt-1) - tt3(0:ntt-2) & tstep = median(steps)
fmax  = 1e6/(2.*tstep*86400D) ; fmax in microHz
fmax  = fmax / fj ; MUST CALC. UP TO NYKVIST FREQ. in c/day


; fmax = 4000. / fj ; calculate up to 4000 microHz
minfreq = (2. * fj) / (max(tt2) - min(tt2)) ; min freq in microHz
maxfreq = fmax 
minfreq2 = (2. * fj) / (max(tt2) - min(tt2)) ; min freq in microHz
maxfreq2 = fmax 
nmax = 550000. ; available on manowar


; Calculate ampl. spec in ppm, using weights = Hans Ks code
ampl_spec_calc_wire_rv,tt2,dat2,wei2,minfreq,maxfreq,$
 freq2res,amp2res,phase2res,highres=1.0,nmax=nmax

if debug then begin
 ; Plot the POWER spectrum:
 plot_oo, freq2res*fj,(amp2res/1e6)^2.0,$
  ysty=3,yr=[0,1.2],xtit='Frequency [!4l!3Hz]',$
  ytit='Power [ppm!E2!N]',charsi=2
endif

; Calc. area pr. resolution element in MICRO HZ
l1 = 100. &  l2 = 4500. ; range of freqs. to consider
l1 = minfreq2*fj &  l2 = maxfreq2*fj*.5 ; range of freqs. to consider
wa = where(freq2res*fj gt l1 and freq2res*fj lt l2,ca)

if debug then begin
 oplot,l1*[1.,1],[1e-12,1],line=2,col=250,thick=4
 oplot,l2*[1.,1],[1e-12,1],line=2,col=250,thick=4
endif

res = 0. ; calculate area of power spectrum
for i=0L,ca-2 do $
 res = res + $
   (freq2res(wa(i+1))-freq2res(wa(i))) * fj * $
   (0.5 * (amp2res(wa(i+1)) + amp2res(wa(i))) * 1e-6)^2.0 ; power!



; if debug then $
print,$
 ' %%% Area pr. resolution element in power spectrum ',res, ' microHz'



END

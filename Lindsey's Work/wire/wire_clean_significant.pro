PRO wire_clean_significant, fc, unit, siglim, significance

; Number of frequencies to be removed before calc. noise level
nf = n_elements(fc)

if strmatch(unit,'*day*')   eq 1 then fsec = 3.
if strmatch(unit,'*milli*') eq 1 then fsec = 0.3 * 1e3
if strmatch(unit,'*micro*') eq 1 then fsec = 0.3

; Calculate the noise level in boxes
fmax = max(fc.freq) * 1.2
fmin = min(fc.freq) * 0.8
nsec = ceil(1. + (fmax - fmin) * fsec)


wire_calc_ampl_noise, fc(nf-1).freq, fc(nf-1).amp, fmin, fmax, nsec, noise

; Smooth the noise profile:
noise2 = noise
noise2(1,*) = smooth(noise(1,*),7,/edge_truncate)
noise2 = noise

; Calculate noise at each frequency found with clean
noise_at_freq = interpol( noise(1,*), noise(0,*), fc.f )

; Store the output from the program
significance = fltarr(2,nf)
significance(0,*) = fc.a / noise_at_freq
w = where(significance(0,*) gt siglim,c)
if c ge 1 then significance(1,w) = 1


END

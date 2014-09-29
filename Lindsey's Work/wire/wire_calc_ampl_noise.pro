PRO wire_calc_ampl_noise, freq, power, f1, f2, nsec, noise

w = where(freq ge f1 and freq le f2,c)

freq2 = freq(w) & power2 = power(w)

r1 = min(freq2)
r2 = max(freq2)
ra = r2 - r1

step = ra / (nsec)
border = step * 0.15
secs = step * findgen(nsec)  + r1


noise = fltarr(3,nsec)

for i=0,nsec-1 do begin

ff1 = secs(i)-border
ff2 = secs(i)+border+step
w2 = where(freq2 gt 0. and $
           freq2 gt ff1 and $
           freq2 lt ff2,c2)

if c2 ge 5 then begin
 resistant_mean,power2(w2),3,me,sd,nr
 noise(0,i) = avg([ff1,ff2])
 noise(1,i) = me
 noise(2,i) = robust_sigma(power2(w2))
 ; print,ff1,ff2,noise(*,i)
endif

endfor


END

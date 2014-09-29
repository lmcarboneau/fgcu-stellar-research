print,'Low dec:'

 w = where(dec lt 4.)
 print,sqrt( (ra(w(0)) - ra(w))^2. + (dec(w(0))-dec(w))^2.)
 print,sqrt( (ra_proj(w(0)) - ra_proj(w))^2. + (dec(w(0))-dec(w))^2.)

print,''
print,'High dec:'
 w = where(dec gt 14)
 print,sqrt( (ra(w(0)) - ra(w))^2. + (dec(w(0))-dec(w))^2.)
 print,sqrt( (ra_proj(w(0)) - ra_proj(w))^2. + (dec(w(0))-dec(w))^2.)

print,'Synth:'
f_ra = replicate(300,6.)
f_dec = findgen(6) * 15. 
f_ra_proj = f_ra / cos(f_dec * !pi / 180.)
 print,sqrt( (f_ra(0) - f_ra)^2. + (f_dec(0)-f_dec)^2.)
 print,sqrt( (f_ra_proj(0) - f_ra_proj)^2. + (f_dec(0)-f_dec)^2.)



end


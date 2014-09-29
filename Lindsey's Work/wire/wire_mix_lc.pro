PRO wire_mix_lc,lc1, lc2, lcout, t0_1, t0_2

readcol, lc1, t1,d1,w1, format='(D,D,D)'
readcol, lc2, t2,d2,w2, format='(D,D,D)'

n1 = n_elements(t1)
n2 = n_elements(t2)

t3 = dblarr(n1+n2)
d3 = dblarr(n1+n2)
w3 = dblarr(n1+n2)
o3 = bytarr(n1+n2)

; Make sure zero points times are double precission
t0_1 = double(t0_1)
t0_2 = double(t0_2)

; WEIGHTS: Default: Equal weights
w3(0:n1-1) = 1./n1
w3(n1:n1+n2-1) = 1./n2
w3 = w3 / total(w3)

; TIMES
dt = t0_2 - t0_1
t3(0:n1-1)     = t1 
t3(n1:n1+n2-1) = t2 + dt

; Data points:
submed = 1.0 ; subtract median for each lc?
d3(0:n1-1)     = d1 - median(d1) * submed
d3(n1:n1+n2-1) = d2 - median(d2) * submed

; Observatory:
o3(0:n1-1)     = 0B
o3(n1:n1+n2-1) = 1B

; Write the light curve
get_lun,u
openw,u,lcout
for i=0L,n1+n2-1 do $
 printf,u,t3(i),d3(i),w3(i),o3(i), format='(D18.7,D10.6, D10.7, I3)'
close,u
free_lun,u

print,' %%% Light curve saved as: ' + lcout

END

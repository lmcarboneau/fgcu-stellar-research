PRO wire_power_lawk, x, a, f, pder

; Fit function of the type: g(x) = a / (1.0 + (b x)^c)
t = ( 1.0 + (a(1) * x)^a(2) )
f = a(3) + a(0) / t ; function value at each x

np = n_elements(x)
na = n_elements(a)

pder = fltarr(np,na)

fac = (-1.0 * a(0)) / t^2.

pder(*,0) = 1./t ; dg / da
pder(*,1) = a(2) * x^(a(2)) * a(1)^(a(2)-1.0) * fac ; dg/db
pder(*,2) = ((a(1) * x)^a(2)) * alog(x) * fac  ; dg / dc
pder(*,3) = 1. ; konstant



END

PRO wire_power_lawk2, x, a, f, pder

; Fit function of the type: g(x) = a / (1.0 + (b x)^c)
t = ( 1.0 + (a(1) * x)^2 )
f = a(2) + a(0) / t ; function value at each x

np = n_elements(x)
na = n_elements(a)

pder = fltarr(np,na)

fac = (-1.0 * a(0)) / t^2.

pder(*,0) = 1./t ; dg / da
pder(*,1) = 2. * x^(2.) * a(1) * fac ; dg/db
pder(*,2) = 1. ; konstant



END

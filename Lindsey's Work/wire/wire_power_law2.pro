PRO wire_power_law2, x, a, f, pder

; Fit function of the type: g(x) = a / (1.0 + (b x)^c)
t  = ( 1.0 + (a(1) * x)^a(2) )
t2 = ( 1.0 + (a(4) * x)^a(5) )

f = a(0) / t + a(3) / t2 ; function value at each x




np = n_elements(x)
na = n_elements(a)

pder = fltarr(np,na)

fac  = (-1.0 * a(0)) / t^2.
fac2 = (-1.0 * a(3)) / t2^2.

pder(*,0) = 1./t ; dg / da
pder(*,1) = a(2) * x^(a(2)) * a(1)^(a(2)-1.0) * fac ; dg/db
pder(*,2) = ((a(1) * x)^a(2)) * alog(x) * fac       ; dg / dc

pder(*,3) = 1./t2 ; dg / da
pder(*,4) = a(5) * x^(a(5)) * a(1)^(a(5)-1.0) * fac2 ; dg/db
pder(*,5) = ((a(4) * x)^a(5)) * alog(x) * fac2       ; dg / dc


END

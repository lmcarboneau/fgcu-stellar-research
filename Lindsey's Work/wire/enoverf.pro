PRO enoverf, x, pars, func, pder

 func = pars(0) + pars(1) / x

 nx = n_elements(x)
 na = n_elements(pars)

 pder = fltarr(nx,na)
 pder(*,0) = 1.0
 pder(*,1) = 1.0 / x

END

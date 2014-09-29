PRO wire_power_getval, f, p, harvey

; Input: Harvey paramteres from wire_power_logfit.pro:

; harvey = fltarr(n_elements(f)
harvey = p(3) + ( p(0) / ( (( p(1) * f)^p(2)) + 1.) )

END

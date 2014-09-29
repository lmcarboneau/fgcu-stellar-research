PRO wire_power_getval2, f, p, harvey

; Input: Harvey paramteres from wire_power_logfit.pro:

; harvey = fltarr(n_elements(f)
harvey = p(2) + ( p(0) / ( (( p(1) * f)^2.) + 1.) )

END

PRO wire_guess_unit, fc, unit, conv

; Guess the frequency unit:
 ff = max(fc(0).freq)
 if ff gt 40. and ff lt 80 then begin
   conv =  1. ; c/day
   unit = 'cday'
 endif

 if ff lt 10.  then begin
   conv =  11.574 / 1e3 ; milliHz
   unit = 'milli'
 endif

 if ff gt 500. then begin
   conv =  11.574  ; microHz
   unit = 'micro'
 endif

;   f_orbit = 15.345 * conv ; 15.345 = orbital freq. in spring 2004.

END

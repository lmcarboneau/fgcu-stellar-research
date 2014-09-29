PRO wire_count_rate, vv, bb, cr, cr_err

bv = vv - bb
nn = n_elements(vv)

cr = fltarr(nn)
cr_err = fltarr(nn)

; !P.multi=[0,1,3]

w = where(abs(bv) lt 10. and $
          vv gt -5 and vv lt 20 and $
          bb gt -5 and bb lt 20,c)

if c ge 1 then begin
 mag = 11.6607 + 1.01643 * vv(w) + 0.410385 * (bv(w)) -0.373560 * (bv(w))^2
 ee = 0.9
 cr(w) = 10.^( ( 25.0 - mag) / 2.5 )
 err1  = 10.^( ( 25.0 - mag + ee) / 2.5 )
 err2  = 10.^( ( 25.0 - mag - ee) / 2.5 )
 dsig  = (err2 - err1) * 0.5
 cr_err(w) = dsig

; ploterr,cr(w),dsig

endif

w_vv = where(abs(bv) gt 10. and $
          vv gt -5 and vv lt 20,c_vv)

if c_vv ge 1 then begin
 mag = 11.6671 +       1.10432 * vv(w_vv)
 ee = 0.81
 cr(w_vv) = 10.^( ( 25.0 - mag) / 2.5 )
 err1 = 10.^( ( 25.0 - mag + ee) / 2.5 )
 err2 = 10.^( ( 25.0 - mag - ee) / 2.5 )
 dsig = (err1 - err2) * 0.5
 cr_err(w_vv) = dsig

; ploterr,cr(w_vv),dsig

endif




w_bb = where(abs(bv) gt 10. and $
          bb gt -5 and bb lt 20,c_bb)

if c_bb ge 1 then begin
 mag = 11.4238 +      0.983660 * bb(w_bb) ; 
 ee = 0.92
 cr(w_bb) = 10.^( ( 25.0 - mag) / 2.5 )
 err1 = 10.^( ( 25.0 - mag + ee) / 2.5 )
 err2 = 10.^( ( 25.0 - mag - ee) / 2.5 )
 dsig = (err2 - err1) * 0.5
 cr_err(w_bb) = dsig

; ploterr,cr(w_bb),dsig

endif


;Hvis du kun har V magnituden:
;      11.6671 +       1.10432 * V   ( 0.804239 )

;Hvis du kun har B magnituden:
;      11.4238 +      0.983660 * B   ( 0.911448 )


; !P.multi=0 & s = get_kbrd(1)

cr_err = abs(cr_err)

end

PRO wire_get_apertures, nap, aperture, apuse

; The original aperutes ... first Altair reduction (must be the same
; as wire_pos.pro !
nap = 9 ; number of apertures
inc = 1.25 & inc = sqrt(inc)

aperture = fltarr(nap)
aperture(0) = 0.7 ; size of the first aperture!

apuse = intarr(nap) & apuse(*) = 1 ; do all apertures == DEFAULT
for i=0,nap-2 do aperture(i+1) = aperture(i) * inc
; The area of the aperture will increase by "inc", eg 1.25 ==> 25%.

END

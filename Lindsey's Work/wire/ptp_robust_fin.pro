PRO ptp_robust_fin,dat,noise, type

; This version is for daophot-like magnitudes, type = 0

noise = robust_sigma(dat)

if n_elements(type) eq 0 then type = 0

if type eq 0 then $
 w2 = where(dat gt 5. and dat lt 25.,cok)
if type eq 1 then $
 w2 = where(abs(dat) lt 2.5,cok)

if cok le 5 then begin
 ; print, 'Less than 5 points to calc. ptp scatter!'
 ; noise = 99.9
 RETURN
endif

dat2 = reform(dat(w2) - median(dat(w2)))

r = robust_sigma(dat2)
w = where(abs(dat2) lt 3.5*r,nn) ; 3.5 - sigma limit

if nn le 20 then begin
 ; noise = 99.99
 goto,ee
endif

dat22 = reform(dat2(w))

noise = 0.

for i=0L,nn-2 do $
 noise = noise + (dat22(i) - dat22(i+1))^2.

noise = noise / (2. * (nn-1.))

noise = sqrt(noise)

; noise = robust_sigma(dat22)

ee:

end

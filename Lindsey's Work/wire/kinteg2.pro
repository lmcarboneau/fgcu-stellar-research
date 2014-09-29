function kinteg2, a, corners=corners, dimension=dimension, average=average, $
                     renormalize=renormalize
; Integrates the 2D float array a(kx,ky) in k-rings
; kinteg assumes the standard kx,ky ordering coming out of IDL's FFT routine.
; 
; If dimension is set integrate only along last dimension.
;
; In case of renormalization the weight for DC component is not changed!

  s  = size(a) & nkx = s(1) & nky = s(2) 

  if keyword_set(dimension) then goto, lastd

  k    = dist(nkx,nky)
  ki   = floor(k)
  w    = 1.0-k+float(ki)
  if keyword_set(corners) then begin
    kmax = max(ki)
  endif else begin
    kmax = min(nkx,nky)/2-1
  endelse
  nk   = kmax+2      ; one "buffer" k to avoid index check in loop
  integral = fltarr(nk) 
  for j=0,kmax do begin
    ii = where(ki eq j)
    d1  = w(ii)
    d2  = a(ii)
    integral(j)   = integral(j)   + total(     d1 *d2)
    integral(j+1) = integral(j+1) + total((1.0-d1)*d2)
  endfor
  if keyword_set(corners) then integral(kmax)=integral(kmax)+integral(kmax+1)

  if ( keyword_set(average) or keyword_set(renormalize)) then begin
    ; calculate average over k-ring
    winteg   = fltarr(nk)
    for j=0,kmax do begin
      ii = where(ki eq j, nii)
      d1            = total(w(ii))
      winteg(j)     = winteg(j)   + d1
      winteg(j+1)   = winteg(j+1) + float(nii) - d1
    endfor
    if keyword_set(corners) then winteg(kmax)=winteg(kmax)+winteg(kmax+1)
   integral = integral/winteg
  endif

  if keyword_set(renormalize) then begin
    ; enforce exact surface weighting, except DC component
    ki = findgen(nk-1)+1.0
    integral(1:*) = integral(1:*)*!pi*((ki+0.5)^2-(ki-0.5)^2)
  endif

  return, integral(0:kmax)

lastd:
; Not my most elegant implementation ...
  ky  =  dist(nky,1)
  nka = nky/2+1
  pa  = fltarr(nkx,nka)
  for k = 0L,nky-1 do begin
    j = ky(k) & pa(*,j) = pa(*,j) + a(*,k)  
  endfor 
  return, pa 
end

pro powerfft,a,oplt=oplt,title=title,xtitle=xtitle,ytitle=ytitle,$
  size=size,spectrum=spectrum,wavenumbers=wavenumbers,$
  renormalize=renormalize,corners=corners,debug=debug,noplot=noplot

; Author: H.G. Ludwig

;  Calculates 2D or 1D power spectrum based on FFT estimate
;  As long size is not set the routine returns power per resolution
;  element, wavenumber(1)=1.
;  If size is set it returns power per unit wavenumber.
;
;  The resulting spectrum satisfies aver(a^2)=wavenumber(1)*total(spectrum).


default9, title, ''
default9, xtitle, 'Wavenumber'
default9, ytitle, 'Power'

s    = size(a)
ndim = s(0)
if n_elements(size) eq 0 then size=2.0*!pi
k0=2.0*!pi/size

ta = fft(a,-1) & ta=float(ta*conj(ta))

case ndim of 
  1: begin
       nx = s(1)
       ta = reform(ta,1,nx)
       spectrum=reform(kinteg2(ta,renormalize=renormalize,corners=corners,/dimension))
     end
  2: begin
       spectrum=kinteg2(ta,renormalize=renormalize,corners=corners)
     end
  else: begin
     print, 'Number of dimensions too large'
     return
     end
endcase

imax = n_elements(spectrum)
wavenumbers=indgen(imax,/long)*k0               ; dimensional wavenos
spectrum=spectrum/k0                            ; normalize to k0

if keyword_set(noplot) then return

if n_elements(oplt) eq 0 then begin
  plot_oo,wavenumbers(1:*),spectrum(1:*),$      ; skip DC in log-log
    title=title,xtitle=xtitle,ytitle=ytitle
endif else begin
  oplot,wavenumbers(1:*),spectrum(1:*),$
    line=oplt
endelse

return
end
